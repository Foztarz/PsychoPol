# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2024 08 06
#     MODIFIED:	James Foster              DATE: 2024 08 06
#
#  DESCRIPTION: Toy example of a binomial experiment spread over 7 contrasts
#               with consistent individual biases
#               
#       INPUTS: Starting parameters for the simulation
#               file to load.
#               
#      OUTPUTS: Saves model summary
#
#	   CHANGES: - post-hoc tests
#	            - plot predictions
#
#   REFERENCES: Bates D., Maechler M., Bolker B. & Walker S. (2015). 
#               Fitting Linear Mixed-Effects Models Using lme4.
#               Journal of Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.
#               
#               Lenth R. V. (2021). emmeans: Estimated Marginal Means, 
#               AKA Least-Squares Means. R package version 1.5.4. 
#               https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
#
#               Šidák, Z. K. (1967). "Rectangular Confidence Regions for the 
#               Means of Multivariate Normal Distributions". J. Am. Statist. Assoc. 
#               62 (318): 626–633. doi:10.1080/01621459.1967.10482935.
#       USAGE:  
#TODO   ---------------------------------------------
#TODO   
#- Set up simulation  
#- Test analysis      
#- Individual slopes  
#- Plotting 



# Install and set up Bayesian modelling -----------------------------------

#make sure modelling packages are installed

#Open file with default program on any OS
# https://stackoverflow.com/a/35044209/3745353
shell.exec.OS = function(x){
  # replacement for shell.exec (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
  {return(base::shell.exec(x))}else
  {comm <- paste0('open "',x,'"')
  return(system(comm))}
}

#first download and install Rtools4.3 
shell.exec.OS('https://cran.r-project.org/bin/windows/Rtools/')

#install the package for Bayesian modelling on Windows
if(!any(installed.packages() %in% 'remotes'))
{install.packages('remotes')}#install the package for installing remote packages

if(!any(installed.packages() %in% 'cmdstanr'))
{remotes::install_github("stan-dev/cmdstanr")} #follow all instructions

#install accompanying executable program
require(cmdstanr)
install_cmdstan(cores = parallel::detectCores()-1, overwrite = FALSE)# may take a while & print a lot!
#if this fails, try https://github.com/stan-dev/cmdstan/archive/refs/tags/v2.30.1.zip

#install package for writing Bayesian models
if(!any(installed.packages() %in% 'brms'))
{install.packages('brms')}

#test cmdstanr
cmdstanr::check_cmdstan_toolchain()
cmdstanr::set_cmdstan_path(path = cmdstanr::cmdstan_path())

#load packages
require(cmdstanr)
require(brms)


# Seed random number generator --------------------------------------------
#simulation should be repeatable
set.seed(20240806)# date this script was started

# Starting parameters -----------------------------------------------------
n_indiv = 20 # total individuals
n_contrasts = 7 # contrasts per individual
n_treat = 2 # number of treatments (different individuals)
contrast_max = 0.9 # maximum contrast tested

dt_sim = data.frame(indiv = sort(rep(x = 1:n_indiv, 
                                     times = n_contrasts)), #n_indiv each perform n_contrasts, sort by indiv number
                    treat = rep(x = sapply(X = 1:n_treat,
                                           FUN = rep,
                                           times = n_contrasts), #there are n treatments, shared equally across n_indiv
                                times = n_indiv/n_treat),
                    contrast = contrast_max*rep(x = 1:n_contrasts / n_contrasts,
                                          times = n_indiv)  #each indiv performs contrasts 1 to n
)

# Simulate curve -----------------------------------------------------------
zero_crossing = qlogis(0.65)#  probability for treatment 1 would reach 0.5 between the 1st and 2nd contrast
pop_contrast_slope = 1.80 #change in log odds with log unit contrast (5x increase ≈ exp(1.60)-1)
pop_treat_slope = 1.10 #change in log odds with treatment 2 (tripled odds ≈ exp(1.10))
pop_treat_contrast_slope = 0.45 #change in contrast effect with treatment 2 (quite large)
pop_intercept = -zero_crossing*pop_contrast_slope # set intercept to match zero crossing (inflection point of sigmoid)
ind_intercept_sd = 0.5 #standard deviation of individual intercepts (log odds)
ind_contrast_sd = 0.3 #standard deviation of individual contrast slopes
residual_sd = 0.05#small source of unnaccounted error
lapse_rate = 0.07#failure rate at maximum performance
base_rate = 0.02#response rate at minimum performance
# ind_treat_sd = 0.1 #standard deviation of individual treatment slopes #indistinguishable from intercept
ind_intercepts = rnorm(n = 1:n_indiv,
                       mean = 0,
                       sd = ind_intercept_sd)
ind_contrast_bias = rnorm(n = 1:n_indiv,
                       mean = 0,
                       sd = ind_contrast_sd)
ind_contrast_lapse = plogis( rlogis(n = 1:n_indiv,
                       location = qlogis(lapse_rate),
                       scale = ind_contrast_sd) )
ind_contrast_LogitBase = plogis( rlogis(n = 1:n_indiv,
                       location = qlogis(base_rate),
                       scale = ind_contrast_sd) )
#simulate continuous contrast sequence
xx = seq(from  = 0, to  = contrast_max, length.out = 1e3)#simulate continuous contrast sequence
lx = qlogis(xx) # logistic scaled contrast
#simulate log odds for treatment 1
yy_lodds = pop_contrast_slope*lx + pop_intercept 
#convert to average choice probability
yy_prob = plogis(yy_lodds)*(1-lapse_rate-base_rate)+base_rate
#convert to probability
#simulate log odds for treatment 2
yy_lodds2 = (pop_contrast_slope + pop_treat_contrast_slope)*lx + 
  pop_intercept +pop_treat_slope
#convert to average choice probability
yy_prob2 = plogis(yy_lodds2)*(1-lapse_rate-base_rate)+base_rate

#plot function
plot(x = xx, 
     y = yy_prob,
     ylim = c(0,1),
     xlim = c(0,1),
     type = 'l',
     col = 2,
     lwd = 3,
     ylab = 'probability',
     xlab = 'contrast')
lines(x = xx,
      y = yy_prob2,
      col = 7,
      lwd = 3)
abline(h = c(1,0.5*(1-lapse_rate-base_rate)+base_rate,0),
       lty = c(1,3,1))
abline(v = plogis(
  c(zero_crossing, #original p(50%)
             -(pop_intercept + pop_treat_slope) / 
               (pop_contrast_slope + pop_treat_contrast_slope)) ), #treatment 2 p(50%)
       lty = 3,
       col = c(2,3))


# . Generate simulated data -----------------------------------------------

# #Make an empty vector
# response_y = rep(x = NA,
#                  length = n_indiv * n_contrasts)

dt_sim = within(dt_sim,
                {
                  #each data point is generated from the animal's intercept
                  #plus the effect of treatment
                  #plus the effect of successive contrasts
                  #plus the animal's response (slope) to changing treatment
                  #scaled by the effect of successive contrasts
                  #plus a small additional source or error
                  
                  response_y = pop_intercept + ind_intercepts[indiv] +
                    qlogis(contrast) * (pop_contrast_slope + 
                                 (treat-1) * pop_treat_contrast_slope) + 
                    ind_contrast_bias[indiv] +
                    (treat-1) *  +pop_treat_slope
                  rnorm(n = n_indiv * n_contrasts,
                        mean = 0,
                        sd = residual_sd)
                }
)
#convert to probability and simulate binomial
dt_sim = within(dt_sim,
                {
                  #each response on the log-odds scale gives us
                  #response probability on a "logit" scale
                  p_response = plogis(response_y) *
                                  (1 - ind_contrast_lapse[indiv] - ind_contrast_LogitBase[indiv]) + ind_contrast_LogitBase[indiv]
                  #that response probabilty determines the sting event
                  #which we observe once for each contrast
                  response = rbinom(n = n_indiv * n_contrasts, #total contrast number
                                 size = 1, # one observation per contrast
                                 prob = p_response)
                }
)


# . Inspect simulated data ------------------------------------------------

# View(dt_sim)
summary(dt_sim)

#plot the expected probabilities for each individual (after added noise)
plot(x = xx, 
     y = yy_prob,
     ylim = c(0,1),
     xlim = c(0,1),
     type = 'l',
     col = 2,
     lwd = 3,
     ylab = 'probability',
     xlab = 'contrast',
     main = 'true probability per individual')
lines(x = xx,
      y = yy_prob2,
      col = 7,
      lwd = 3)
with(subset(x = dt_sim,
            subset = treat == 1),
     points(x = contrast,
            y = p_response,
            col = 2,
            pch = 3)
)

with(subset(x = dt_sim,
            subset = treat == 2),
     points(x = contrast,
            y = p_response,
            col = 7,
            pch = 4)
)

abline(h = c(1, base_rate+ (1-lapse_rate-base_rate)*0.5,0),
       lty = c(1,3,1))
abline(v = 
         plogis(c(zero_crossing, #original p(50%)
             -(pop_intercept + pop_treat_slope) / 
               (pop_contrast_slope + pop_treat_contrast_slope)) ), #treatment 2 p(50%)
       lty = 3,
       col = c(2,3))
legend(x = 'bottomright',
       legend = 'inflection point',
       lty = 3)

# . aggregate simulated data to summarise ---------------------------------
dt_agg = aggregate(response ~ contrast*treat,
                   data = dt_sim,
                   FUN = mean)


#plot the expected probabilities for each individual (after added noise)
plot(x = xx, 
     y = yy_prob,
     ylim = c(0,1),
     xlim = c(0,1),
     type = 'l',
     col = 2,
     lwd = 3,
     ylab = 'probability',
     xlab = 'contrast',
     main = 'aggregate across individuals')
lines(x = xx,
      y = yy_prob2,
      col = 7,
      lwd = 3)
abline(h = c(1, base_rate+ (1-lapse_rate-base_rate)*0.5,0),
       lty = c(1,3,1))
with(subset(x = dt_agg,
            subset = treat == 1),
     points(x = contrast,
            y = response,
            col = 2,
            pch = 3,
            lwd = 5)
)

with(subset(x = dt_agg,
            subset = treat == 2),
     points(x = contrast,
            y = response,
            col = 7,
            pch = 4,
            lwd = 5)
)



# Set up non-linear model -------------------------------------------------
#make treatment categorical for this example
dt_sim = within(dt_sim,
                {treatment = c('alpha','beta')[treat]}
                )

# . Define model formula --------------------------------------------------

#for our model, we would like to define width as 80% of the curve rise
print(round( 
  2*log(2/
          (1- 0.8) -1),
  digits = 2 )) # coeficient to rescale curve to width))
#this is approximately 4.39

#set up model fit
formula_nl = bf(
  #set up a formula for the curve as a whole,
  #including parameters found in the data (correct_incorrect, stimulus)
  #and parameters that we wish to estimate (LogitBaseline, lapse rate, inflection point, width).
  #Most of these are subject to further fixed (type) and random (animal) effects,
  #these need to be defined for each parameter.
  
  #Two parameters need special transformations
  #To keep the output between 0 and 1, additional effects of lapse rate
  #will be added on the "logit" scale (Lapse = inv_logit(LogitLapse)).
  #To avoid curve widths of 0, we can assume a positive slope (≥0)
  #and add additional effects to width on a log scale (Width = exp(LogWidth))
  formula = response ~ 
    inv_logit(LogitBase) + (1 - inv_logit(LogitLapse) - inv_logit(LogitBase) ) *#curve region
    inv_logit( 4.39*(contrast - inv_logit(Inflex) ) / exp(LogWidth) ) , #inflection-width curve
  # for each of these parameters, we can set up a separate formula 
  # that describes how to predict them from the data 
  #LogitBase rate of correct choices: "
  LogitBase ~ treatment + (1 + treatment|indiv), #LogitBase rate of correct choices: "~ 1" gives the instruction "estimate the mean across all data"
  #Lapse rate on a log(odds) scale:
  LogitLapse ~ treatment + (1 + treatment|indiv), #this is similar to the formula in our LMM example
  #inflection point of the initial curve:
  Inflex ~ treatment + (1 + treatment|indiv), #N.B. this is similar to the intercept, so it does not include effects of stimulus level
  #log 80% width of the curve:
  LogWidth ~ treatment + (1 + treatment|indiv), #N.B. this is similar to the slope, so all of its effects depend on stimulus level
  family = bernoulli("identity"),
  nl = TRUE)#the joint distribution for these parameters is undefined, and therefore the parameters themselves are "nonlinear"

# #N.B. for plotting we will need to also definte "inv_logit" in our the R environment
inv_logit = inv_logit_scaled # this can be found in brms, but with an additional specifier

#set up a null model
formulaNull_nl = bf(
  #set up a formula for the curve as a whole,
  #including parameters found in the data (correct_incorrect, stimulus)
  #and parameters that we wish to estimate (LogitBaseline, lapse rate, inflection point, width).
  #Most of these are subject to futher fixed (type) and random (animal) effects,
  #these need to be defined for each parameter.
  
  #Two parameters need special transformations
  #To keep the output between 0 and 1, additional effects of lapse rate
  #will be added on the "logit" scale (Lapse = inv_logit(LogitLapse)).
  #To avoid curve widths of 0, we can assume a positive slope (≥0)
  #and add additional effects to width on a log scale (Width = exp(LogWidth))
  formula = response ~ 
    inv_logit(LogitBase) + (1 - inv_logit(LogitLapse) - inv_logit(LogitBase)) *#curve region
    inv_logit( LogitMean ) , #inflection-width curve
  LogitBase ~ 1  + (1|indiv), #LogitBase rate of correct choices
  LogitLapse ~ 1 + (1|indiv), #Lapse rate on a log(odds) scale
  LogitMean ~ 1 + (1|indiv), #mean log(odds) of a correct choice (intercept of an LMM)
  family = bernoulli("identity"),
  nl = TRUE)#the joint distribution for these parameters is undefined, and therefore the parameters themselves are "nonlinear"


# . Define model priors ---------------------------------------------------

# For many types of model with multiple parameters, the search for the correct
# parameter values needs to focus on the right range of potential values.
# This is especially true for non-linear models, where the wrong value of one
# parameter can exclude the possiblity of the right value for other parameters.
# In Bayesian modelling, we define prior (expected) distributions for each parameter.
# This informs the modelling algorithm when parameter estimates become too different
# from the values we expect, which prevents it from returning implausible estimates
# (or failing to converge on any estimates).

# By default, expected distributions (prior) for non-linear parameters (nlpar) 
# for fixed effects coefficients (class: b) are uniform (flat): 
# any value is equally plausible!
# For random effects standard deviations (class: sd) the expected distribution
# is a very wide student t distribution: (3 degrees of freedom, centre 0, st. dev. 2.5):
# the differences in parameter values can vary by any amount, 
# but small variations are more plausible.

# Because we have random effects of animal on both Intercept and stimulus type
# brms adds a correlation parameter, with a Lewandowski-Kurowicka-Joe distributed 
# prior, for potential correlation between these two effects within each animal. 

prior_nl = get_prior(formula = formula_nl,
                     data = dt_sim)

# To make sure the model converges, and returns plausible parameter estimates
# it would be a good idea to define some expected distributions for our parameters.
# A normal distribution with the mean around the centre of possible values
# and a moderate standard deviation (1-3) indicates a range of values not more than
# 2-6 units of the parameter 
#set priors to ensure convergence


# . . LogitBase rate prior -----------------------------------------------------
#for the LogitBaseline, we will use a beta distribution with a bias towards 0

#set the prior distribution
prior_nl = within(prior_nl, 
                  { prior[
                    class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogitBase' &
                      coef %in% 'Intercept' 
                  ] = 'normal(-5,3)' #a logistic distribution centred on 0.007
                  })

#for all other fixed effects on LogitBase rate, we'll suggest values around 0 (no effect)
prior_nl = within(prior_nl, 
                  { prior[
                    class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogitBase' &
                      coef %in% 'treatmentbeta' 
                  ] = 'normal(0,1)' #a normal distribution:mean 0, sd 1
                  })


# . . Lapse rate priors ---------------------------------------------------
#for the lapse rates, we will use a normal distribution with a strong bias 
# towards log(odds) = -3, prob ≈0.05
# (see qlogis(0.05) for conversion from prob to log odds)
# this expectation is still very broad, 95% probability between 0.0001 and 0.9468

# set the prior distribution
prior_nl = within(prior_nl, 
                  { prior[
                    class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogitLapse' &
                      coef %in% 'Intercept' 
                  ] = 'normal(-3,3)' #a normal distribution centred on -3
                  })

#for all other fixed effects on lapse rate, we'll suggest values around 0 (no effect)
prior_nl = within(prior_nl, 
                  { prior[
                    class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogitLapse' &
                      coef %in% 'treatmentbeta' 
                  ] = 'normal(0,1)' #a normal distribution:mean 0, sd 1
                  })



# . . Inflection point priors ---------------------------------------------
#for the population level inflection point, we expect it to be somewhere
#in the middle of our stimulus range (0-1) = 0.5
#on a logit scale we might expect somewhere around qlogis(0.5) = 0
# this expectation is broad, 95% probability of inflection points between 0.003 and 0.997
plogis( 
  round(qnorm(p = c(0,1) + #lower & upper
              (c(1,-1)/2) * #add and subtract half the interval
              (1 - 0.95), #proportion of probability density outside interval
            mean = 0, 
            sd = 3),
      digits = 2)
)

#set the priors
prior_nl = within(prior_nl, 
                  { prior[
                    class %in% 'b' & #just the fixed effects
                      nlpar %in% 'Inflex' &
                      coef %in% 'Intercept'
                  ] = 'normal(0,1)' #a normal distribution:mean 0, sd 1
                  })
#for all coefficients, we'll suggest values around 0 (no effect)
prior_nl = within(prior_nl, 
                  { prior[
                    class %in% 'b' & #just the fixed effects
                      nlpar %in% 'Inflex' &
                      coef %in% 'treatmentbeta' 
                  ] = 'normal(0,1)' #a normal distribution:mean 0, sd 1
                  })


# . . Rise region width priors --------------------------------------------
#for the population level width, we expect it to be a positive value (>exp(-Inf))
#somewhat smaller than the range of stimulus values
#it might be reasonable to also expect an 80% rise region of around 3 ≈ exp(1)
#this expectation is broad, 95% probability of widths between 0.01 and 972.52
#but with the strong assumption the curves are either positive (increase with stimulus level)
#or nearly flat (requiring many times the range of available stimuli to rise).
#This would not be the case in experiments where correct response rates _decrease_ 
#as a function of increasing stimulus (estimate Width instead of LogWidth).

#set prior distribution
prior_nl = within(prior_nl, 
                  { prior[
                    class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogWidth' &
                      coef %in% 'Intercept' 
                  ] = 'normal(1,1)' #a normal distribution:mean 1, sd 1
                  })
#for all coefficients, we'll suggest values around 0 (no effect)
prior_nl = within(prior_nl, 
                  { prior[
                    class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogWidth' &
                      coef %in% 'treatmentbeta'
                  ] = 'normal(0,1)' #a normal distribution:mean 0, sd 1
                  })

# . . Set flat priors to wide ---------------------------------------------
#for other priors, we'll suggest values around 0 (no effect)
prior_nl = within(prior_nl, 
                  { prior[
                    prior %in% '' & !(class %in% c('b','sd','cor'))
                  ] = 'normal(0,1)' #a normal distribution:mean 0, sd 1
                  })




# . Define null priors ----------------------------------------------------

priorNull_nl = get_prior(formula = formulaNull_nl,
                         data = dt_sim)
# . . LogitBase rate prior -----------------------------------------------------
priorNull_nl = within(priorNull_nl, 
                      { prior[
                        class %in% 'b' & #just the fixed effects
                          nlpar %in% 'LogitBase' &
                          coef %in% 'Intercept' 
                      ] = with(prior_nl, #use the same prior as the full model
                               {
                                 prior[
                                   class %in% 'b' & #just the fixed effects
                                     nlpar %in% 'LogitBase' &
                                     coef %in% 'Intercept' 
                                 ]
                               }
                      ) 
                      })


# . . Lapse rate priors ---------------------------------------------------
priorNull_nl = within(priorNull_nl,
                      { prior[
                        class %in% 'b' & #just the fixed effects
                          nlpar %in% 'LogitLapse' &
                          coef %in% 'Intercept' 
                      ] = with(prior_nl, #use the same prior as the full model
                               {
                                 prior[
                                   class %in% 'b' & #just the fixed effects
                                     nlpar %in% 'LogitLapse' &
                                     coef %in% 'Intercept' 
                                 ]
                               }
                      )
                      })
#for all other fixed effects on lapse rate, we'll suggest values around 0 (no effect)
priorNull_nl = within(priorNull_nl, 
                      { prior[
                        class %in% 'b' & #just the fixed effects
                          nlpar %in% 'LogitLapse' &
                          coef %in% 'typebeta' 
                      ] = with(prior_nl, #use the same prior as the full model
                               {
                                 prior[
                                   class %in% 'b' & #just the fixed effects
                                     nlpar %in% 'LogitLapse' &
                                     coef %in% 'typebeta' 
                                 ] 
                               }
                      )
                      })

# . . Mean rate priors ---------------------------------------------------
#for the mean rates, we will use a normal distribution with a weak bias 
# towards log(odds) = 0, prob  = 0.5
# (see qlogis(0.5) for conversion from prob to log odds)

# this expectation is very broad, 95% probability between 0.0028 and 0.9972
round(plogis(qnorm(p = c(0,1) + #lower & upper
                     (c(1,-1)/2) * #add and subtract half the interval
                     (1 - 0.95), #proportion of probability density outside interval
                   mean = 0, 
                   sd = 3)
),
digits = 4)
xseq = seq(from  = 0, to = 1, by  = 0.01)


#set prior distribution
priorNull_nl = within(priorNull_nl, 
                      { prior[
                        class %in% 'b' & #just the fixed effects
                          nlpar %in% 'LogitMean' &
                          coef %in% 'Intercept' 
                      ] = 'normal(0,1)' #a normal distribution centred on 0
                      })

# . . Set flat priors to wide ---------------------------------------------
#for other priors, we'll suggest values around 0 (no effect)
priorNull_nl = within(priorNull_nl, 
                  { prior[
                    prior %in% '' & !(class %in% c('b','sd','cor'))
                  ] = 'normal(0,1)' #a normal distribution:mean 0, sd 1
                  })

# Fit model ---------------------------------------------------------------
#!be prepared to wait!
#The Markov Chain Monte-Carlo method used for Bayesian estimation requires
#a long series of iterations, between which the algorithm attempts to improve the fit.
#This process takes longer the more iterations, parameters and data are used.

# . Short dummy run to check the influence of the priors ------------------


#double check that the prior distribution is viable by first setting up a short dummy run
# Dummy run
system.time(
  {
    dummy_fit = brm( formula = formula_nl, # using our nonlinear formula
                     data = dt_sim, # our data
                     prior = prior_nl, # our priors 
                     sample_prior = 'only', #ignore the data to check the influence of the priors
                     iter = 300, # short run for 300 iterations
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <60s, each chain running for <1 seconds (mainly compile time)


#the default plot shows the values estimated for each parameter
# in each chain for each iteration
#fixed effects
plot(dummy_fit, 
     nvariables = 10,
     variable = "^b_", 
     regex = TRUE)

#random effects
plot(dummy_fit, 
     nvariables = 10,
     variable = "^sd", 
     regex = TRUE )

# We can see that for this formula with the priors we've set 
# most parameters would eventually arrive at a value of zero.
# Because we didn't add any information from the data we
# see only the values the priors expect.


# As a sanity test, we can see that the priors alone
# are not biasing estimates away from the range of 
# plausible models. 
# We can see that for this formula there is an expectation 
# that proportion correct increases as a function of the stimulus
# but it is still possible to observe wide range of correct choice rates
# for all stimulus levels.
#as we intended, the combined prior distribution allows for a range of curve shapes
#N.B. the "inv_logit" function needs to be in your R environment (renamed to "inv_logit_scaled" in BRMS)
plot(
  conditional_effects(x = dummy_fit, 
                      spaghetti = TRUE, 
                      ndraws = 2e2,
                      effects = 'contrast')
)

#and effects of stimulus type (mean correct choice rates just slightly above LogitBaseline)
plot(
  conditional_effects(x = dummy_fit, 
                      effects = 'treatment')
)
# For stimulus types, there is no expected difference, though the expected range
# is wider for beta since its coefficient is added to whatever alpha is.
# Generally, the priors accept most plausible models.

# N.B. We specified " sample_prior = 'only' " for this fit. We therefore see
# _only_ the effects of our prior expectations on the fitted model, but _no_
# influence from the data. These estimates should fit with our expectations,
# but should not limit the range of possible interpretations of the data by
# limiting the range of possible estimates to a narrow set of predictions.
# See: github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations for more details.

# . To check that estimation works  ---------------------------------------
# Run the model for a small number of iterations to check that it is possible to 
# estimate all parameters. This may be the point where we encounter numerical errors
# if the formula or priors are misspecified.
# e.g. if the formula returns estimates of correct choice rate outside of [0,1].

# Short run
system.time(
  {
    short_fit = brm( formula = formula_nl, # using our nonlinear formula
                     data = dt_sim, # our data
                     prior = prior_nl, # our priors 
                     iter = 500, # short run for 500 iterations (less than 300 gives insufficient warmup time)
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <3 minutes, each chain running for <90 seconds
#!Pay attention to any warning messages! A fix may be just one web search away.

#inspect
#fixed effects
plot(short_fit, 
     nvariables = 10,
     variable = "^b_", 
     regex = TRUE)



# . Full runs -------------------------------------------------------------


# Full run
system.time(
  {
    full_fit = brm( formula = formula_nl, # using our nonlinear formula
                    data = dt_sim, # our data
                    prior = prior_nl, # our priors 
                    iter = 2000, # long run for 2000 iterations
                    chains = 4, # 4 chains in parallel
                    cores = 4, # on 4 CPUs
                    refresh = 0, # don't echo chain progress
                    backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <8 min, each chain running for 4-6 minutes

# null model
system.time(
  {
    null_fit = brm( formula = formulaNull_nl, # using our nonlinear formula
                    data = dt_sim, # our data
                    prior = priorNull_nl, # our priors 
                    iter = 2000, # long run for 2000 iterations
                    chains = 4, # 4 chains in parallel
                    cores = 4, # on 4 CPUs
                    refresh = 0, # don't echo chain progress
                    backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <2 min, each chain running for 50-80 seconds


# . . Inspect the fits ----------------------------------------------------
#fixed effects
plot(full_fit, 
     nvariables = 10,
     variable = "^b_", 
     regex = TRUE)

#random effects
plot(full_fit, 
     nvariables = 10,
     variable = "^sd", 
     regex = TRUE )

#summary of parameter estimates
full_sm = summary(full_fit,
                  robust = TRUE)#use the median estimate

# Model comparison --------------------------------------------------------

#How robust is the model to changes in the data structure?
#Would the model make good predictions refitted the model but dropped one datapoint, would it still give good predictions?
# calculate the Leave-One-Out (LOO) cross validation metric for the model
loo_full = loo(full_fit)#calculate for full model 
loo_null = loo(null_fit)#calculate for null model

loo_compare(loo_full,
            loo_null)
# Expected log predictive density (ELPD) is higher for the full fit than for the
# null model, and the difference is larger than both the standard error of the
# ELPD and the difference in the number of parameters.


# Plot model predictions --------------------------------------------------

# We can get a general idea of the shape of the fitted curve by looking at a
# subsample (here only 200) of the draws for stimulus.
plot(
  conditional_effects(x = full_fit, 
                      spaghetti = TRUE, 
                      ndraws = 2e2,
                      effects = 'contrast')
)

# to predict all effects, we can use the 'posterior_epred' method
#by default 100 predictions per continuous variable (but fewer for their interactions)
system.time(
  {
    full_cond =brms::conditional_effects(full_fit, 
                                         method = 'posterior_epred', # posterior epred not working
                                         cores =  parallel::detectCores()-1,
                                         effects = c('contrast:treatment')
    )
  }
)#takes <2 seconds


pred_data = full_cond$`contrast:treatment`
#plot each stimulus type
plot(
  NULL,
  xlab = 'Contrast',
  ylab = 'Response proportion',
  xlim = c(0,1),
  ylim = c(0,1),
)
abline(h = c(0,1,base_rate, 1- lapse_rate), 
       lty = c(1,1,3,3))

#plot total prediction intervals
with(subset(pred_data, treatment == 'beta'), 
     {
       polygon(x = c(sort(contrast), rev(sort(contrast))), 
               y = c(lower__[order(contrast)],
                     rev(upper__[order(contrast)])
               ), 
               col = adjustcolor('orange', alpha.f = 50/256),
               border = NA,
               lwd = 0.1
       )
     }
)
with(subset(pred_data, treatment == 'alpha'), 
     {
       polygon(x = c(sort(contrast), rev(sort(contrast))), 
               y = c(lower__[order(contrast)],
                     rev(upper__[order(contrast)])
               ), 
               col = adjustcolor('darkblue', alpha.f = 50/256),
               border = NA,
               lwd = 0.1
       )
     }
)
#plot the median prediction lines
with(subset(pred_data, treatment == 'alpha'), 
     lines(x = sort(contrast), y = estimate__[order(contrast)], 
           col = 'darkblue',
           lwd = 5)
)
with(subset(pred_data, treatment == 'beta'), 
     lines(x = sort(contrast), y = estimate__[order(contrast)], 
           col = 'orange',
           lwd = 5)
)

lines(x = xx, 
     y = yy_prob,
     col = 'cyan',
     lwd = 5,
     lty = 3)
lines(x = xx,
      y = yy_prob2,
      col = 'red',
      lwd = 5,
      lty = 3)

with(subset(x = dt_agg,
            subset = treat == 1),
     points(x = contrast,
            y = response,
            col = 'cyan',
            pch = 3,
            lwd = 5)
)

with(subset(x = dt_agg,
            subset = treat == 2),
     points(x = contrast,
            y = response,
            col = 'red',
            pch = 4,
            lwd = 5)
)



