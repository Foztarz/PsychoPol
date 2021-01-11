// generated with brms 2.9.0
// I've tried to edit it in Stan to do what I need
functions {
    //prior for a unit_vector estimate of mean angle
   real von_mises_unitvector_lpdf(vector in_vec, real mu, real kappa) {
        real angle = atan2(in_vec[2], in_vec[1]);
        //extreme kappa correction for numeric stability
     if (kappa < 100) {
       return von_mises_lpdf(angle | mu, kappa);
     } else {
       return normal_lpdf(angle | mu, sqrt(1 / kappa));
     }
   }
   // calculate the mean angle of a circular distribution (in radians)
   real mean_circular(vector y, int N){
      real sumsin = 0;
      real sumcos = 0;
     for (n in 1:N){
       sumsin += sin(y[n]);
       sumcos += cos(y[n]);
     }
     sumsin = sumsin/N;
     sumcos = sumcos/N;
     return(atan2(  sumsin, sumcos) );
   }
    // calculate the mean vector length for a circular distribution
   real rho_circular(vector y, int N){
      real sumsin = 0;
      real sumcos = 0;
     for (n in 1:N){
       sumsin += sin(y[n]);
       sumcos += cos(y[n]);
     }
     sumsin = sumsin/N;
     sumcos = sumcos/N;
     return(sqrt(  sumsin^2 + sumcos^2)/N );
   }
}
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable, angles
  // data for group-level effects of ID 1
  // effects on mu (added to temp_Intercept)
  int<lower=1> N_1;//number of groups
  int<lower=1> M_1;//number of random effects SD
  int<lower=1> J_1[N];//group identity
  vector[N] Z_1_1;//offset from population mean in SDs
  // data for group-level effects of ID 2
  // effects on kappa (added to temp_kappa_Intercept)
  int<lower=1> N_2;
  int<lower=1> M_2;
  int<lower=1> J_2[N];
  vector[N] Z_2_kappa_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  //A unit_vector is used in place of temp_Intercept
  // this stops estimates of mu from walking around the circle multiple times
  //real temp_Intercept;  // temporary intercept
  unit_vector [2] mu_vec;  //  I'll use mu_vec as a temporary intercept
  real temp_kappa_Intercept;  // temporary intercept
  // vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector<lower=0, upper=pi()>[M_1] sd_1;  // ±arc containing group-level means
  // there is only one random effect (animal),
  //starting values may fall outside these of (-pi,pi)
  vector<lower=-1, upper=1>[N_1] z_1[M_1];  // unscaled group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // unscaled group-level effects
}
transformed parameters {
	// some kind of angle
	real temp_angle = atan2(mu_vec[2], mu_vec[1]);
  // group-level effects (mu)
  vector[N_1] r_1_1 = (sd_1[1] * (z_1[1]));
  // group-level effects (kappa)
  vector[N_2] r_2_kappa_1 = (sd_2[1] * (z_2[1]));
}
model {
  //population level circular mean estimate for mu_vec prior
  real cmean = mean_circular(Y,N);
  vector[N] mu = temp_angle + rep_vector(0, N);
  vector[N] kappa = temp_kappa_Intercept + rep_vector(0, N);

  for (n in 1:N) {
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
    kappa[n] += r_2_kappa_1[J_2[n]] * Z_2_kappa_1[n];
    kappa[n] = exp(kappa[n]); // kappa is estimated via the log link, avoiding kappa < 0
  }
  // priors including all constants
  target += uniform_lpdf(temp_angle | -pi(), pi()); //anything within one circle
  target += normal_lpdf(temp_kappa_Intercept | 0, 3); //most likely values of log(kappa)
  // target += normal_lpdf(sd_1 | 0, 1.814)  //sd of a uniform circular distribution
  //   - 1 * normal_lccdf(0 | 0, 1.814);
  // Multimodality in the posterior suggests that there is more than one way
  // to combine SD, z and mu_vec to fit the same population of random effects means.
  // I'll try a prior on smaller SDs to reduce the number of good fits.
  target += uniform_lpdf(sd_1 | 0, pi())  //any half_arc in one half circle
    - 1 * uniform_lccdf(0 | 0, pi());
    // target += von_mises_lpdf(sd_1 | 0, 1)  //weak bias towards smaller SD.
    ;//  - 1 * von_mises_lcdf( 0 |  0,  1); // N.B. sd_1 is already bounded to (0,pi())
  // Multimodality in the posterior suggests that there is more than one way
  // to combine SD, z and mu_vec to fit the same population of random effects means.
  // I'll try a weak prior on larger z to avoid bias towards the pop mean at 0.
  // target += normal_lpdf(z_1[1] | 0, 1); //95% probability (-135.6°  135.6°)
  // An inverted normal. Strong prior that individual means are not at population mean.
  target += uniform_lpdf(z_1[1] | -1, 1) - normal_lpdf(z_1[1]  | 0, 0.25);
  // No prior reason to expect small deviations, remove bias
  // target += uniform_lpdf(z_1[1] | -1, 1); //rng values have 95% probability (-135.6°  135.6°)
  target += normal_lpdf(sd_2 | 0, 3)
    - 1 * normal_lccdf(0 | 0, 3);
  target += normal_lpdf(z_2[1] | 0, 1);
  //mu_vec needs some kind of prior, but I'm not entirely sure what
  // My current theory, the population mean needs to be anchored to estimate
  // random effects means well. Because the von Mises distribution wraps,
  // it is possible to get good fits for multiple combinations of
  // population mean, random effects SD and random effects offsets.
  // A strong prior that the population mean should fall at the
  // circular mean of the dataset could help the choice of SD and z.
  target += von_mises_unitvector_lpdf(mu_vec| mean_circular(Y,N), 5);
  // this has been proposed (https://discourse.mc-stan.org/t/von-mises-documentation-suggestion/1133/31)
  // but I don't understand how it would help estimating mu
          // real t1;
          // real t2;
          // mu ~ von_mises (mu_prior, kappa_fixed);
          // //  angles ~ von_mises (mu, kappa);  // where one would use mu
          // t1 = 1 + (mu_vec[2] * mu_vec[2]);
          // target += - log (t1);
          // t2 = 1 + (mu_vec[1] * mu_vec[1]);
          // target += - log (t2);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += von_mises_lpdf(Y[n] | mu[n], kappa[n]);
    }
  }
}
generated quantities {
  // return population-level unit_vector sin component
  real b_sin1 = mu_vec[2];
  // return population-level unit_vector cos component
  real b_cos2 = mu_vec[1];
  // actual population-level intercept
  real b_Intercept = temp_angle;//normally this would be temp_Intercept
  // actual population-level intercept
  real b_kappa_Intercept = temp_kappa_Intercept;
  // actual group-level mu
  // real animal_mu_offset = r_1_1;//how do I extract this?
  // actual group-level kappa
  // real animal_kappa_offset = r_2_kappa_1;
}
