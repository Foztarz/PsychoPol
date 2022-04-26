# Set up calibration data -------------------------------------------------
calib_data = data.frame(
  p = c( # left dial setting
        0,
        0,   #p0.00u1
        0.75,#p0.75u1
        1.00,#p1.00u1
        1.50,#p1.50u1
        2.50,#p2.50u0
        5.00 #p5.00u0
        ),
  u = c( # right dial setting
        0,
        1,#p0.00u1
        1,#p0.75u1
        1,#p1.00u1
        1,#p1.50u1
        0,#p2.50u0
        0 #p5.00u0
        ),
  counts.per.sec = c( #total uncalibrated counts in final position
        1,
        256.948879897621,#p0.00u1
        247.730512651038,#p0.75u1
        261.807272387137,#p1.00u1
        914.268516538193,#p1.50u1
        2928.81619823986,#p2.50u0
        7493.23115662601 #p5.00u0
        ),
  degree.of.polarization = c( # degree of polarization in final position
        1e-4,
        0.00919859070663204,#p0.00u1
        0.0146431912688974,#p0.75u1
        0.0679534163563893,#p1.00u1
        0.27757914833085,#p1.50u1
        0.364751965583388,#p2.50u0
        0.347130195020875 #p5.00u0
        ),
  pre.counts.per.sec = c( # counts per second in pre-experiment position 
        1,
        NA,#p0.00u1
        NA,#p0.75u1
        NA,#p1.00u1
        NA,#p1.50u1
        NA,#p2.50u0
        34105.075950159 #p5.00u0
        ),
  pre.photon.flux = c( # photons cm^-2 s^-1 in pre-experiment position
        804440869229.885,#p0.00u0
        11925973801393.3,#p0.00u1
        NA,#p0.75u1
        10536785041662.5,#p1.00u1
        NA,#p1.50u1
        NA,#p2.50u0
        38930132594500.4 #p5.00u0
        )
)

# Calculate correction between 2nd and 3rd calibration --------------------
pre.post.correction = with(calib_data,
                           {
                             mean(
                               counts.per.sec[!is.na(pre.counts.per.sec) & !is.na(counts.per.sec)] / 
                                 pre.counts.per.sec[!is.na(pre.counts.per.sec) & !is.na(counts.per.sec)] ,
                               na.rm = TRUE
                             )
                           }
                           )
photons.per.count = with(calib_data,
                         {
                           mean( 
                               pre.photon.flux[!is.na(pre.photon.flux) & !is.na(counts.per.sec)] / 
                               counts.per.sec[!is.na(pre.photon.flux) & !is.na(counts.per.sec)] ,
                             na.rm = TRUE
                           )
                         }
)
calib_data = within(calib_data,
                    {
                    estimated.photon.flux = photons.per.count* counts.per.sec 
                    log10.photon.flux = log10(estimated.photon.flux)
                    p_u = paste('p',p,'u',u)
                    }
                    )

# Inspect data ------------------------------------------------------------


with(
  calib_data,
  {
    plot(x =1:length(log10.photon.flux),
        y = log10.photon.flux,
        pch = 19,
        col = 'blue',
        axes = FALSE
        )
    axis(side = 1,
         at = 1:length(p_u),
         labels = p_u)
    axis(side = 2) 
  }
)

p_only = subset(calib_data,
                subset = u == 0)

with(
  p_only,
  {
    plot(x = p ,
         y = estimated.photon.flux,
         pch = 19,
         col = 'blue'
    )
  }
)
with(subset(calib_data,
            subset = u == 1 & 
                      p == 0),
     {
points(x = u,
       y = estimated.photon.flux,
       col = 'red',
       pch = 19)
     }
     )
with(subset(calib_data,
            subset = u == 1 & 
                      p == 1),
     {
points(x = u,
       y = estimated.photon.flux,
       col = 'magenta',
       pch = 19)
     }
     )

# Predict intensity -------------------------------------------------------

intensity_points = rbind(p_only, 
      data.frame(p = 1,
                 u = 0,
                 counts.per.sec = NA,
                 degree.of.polarization = subset(calib_data,
                                                 subset = p == 2.5, 
                                                          u = 0)$degree.of.polarization,
                 pre.counts.per.sec = NA,
                 pre.photon.flux = NA,
                 p_u = "p 1 u 0",
                 log10.photon.flux = log10(
                                           subset(calib_data,
                                                  subset = u == 1 & 
                                                            p == 1
                                                  )$estimated.photon.flux -
                                             subset(calib_data,
                                                    subset = u == 1 & 
                                                              p == 0
                                                    )$estimated.photon.flux 
                                           ),
                 estimated.photon.flux = subset(calib_data,
                                                  subset = u == 1 & 
                                                            p == 1
                                                  )$estimated.photon.flux -
                                             subset(calib_data,
                                                    subset = u == 1 & 
                                                              p == 0
                                                    )$estimated.photon.flux 
      
                  )
          )
spl1 = with(intensity_points,
            smooth.spline(x = p,
                          y = estimated.photon.flux,
                          all.knots = TRUE,
                          spar = 0.25)
            )
spl2 = with(intensity_points,
            smooth.spline(x = log(p+1e-4),
                          y = log10.photon.flux,
                          all.knots = TRUE,
                          spar = 1e-2)
            )
spl3 = lm(formula = log10.photon.flux~log(p),
               data = subset(intensity_points,
                             subset = p>0))
xx = seq(from = 0,
         to = 7,
         length.out = 1e3)
with(intensity_points,
     plot(x = p,
          y = log10.photon.flux,
          pch = 19, 
          col = 'blue')
     )
lines(x = xx,
      y = log10(
              predict(object = spl1,
                  x = xx)$y
              ),
      col = 'orange')
lines(x = xx,
      y = (
              predict(object = spl2,
                  x = log(xx+1e-4))
              $y
              ),
      col = 'green')
lines(x = xx,
      y = coef(spl3)[1] + coef(spl3)[2]*log(xx),
      col = 'magenta')


plot(x = xx,
      y = (
        predict(object = spl2,
                x = log(xx+1e-4))
        $y
      ),
     type = 'l',
      col = 'green')
#identify useful range
abline(v = xx[which.min(predict(object = spl2, x = log(xx+1e-4))$y)], col = 'red')

PredPIntensity = function(p)
{
  p_transformed = log( p +1e-4)
  x_data = log( c(0,    1,        2.5,       5.0) +1e-4 )
  y_data = c( 11.35039, 12.03688, 14.81708, 15.22506 )
  spl = smooth.spline(x = x_data,
                      y = y_data,
                      all.knots = TRUE,
                      spar = 1e-2
                      )
  return(
      ifelse(p > 0.1,
             yes = 
                10^(
                    predict(object = spl,
                            x = p_transformed)$y
                    ),
             no = 0
            )
        )
}

with(intensity_points,
     plot(x = p,
          y = estimated.photon.flux,
          pch = 19,
          col = 'blue')
     )
lines(x = xx,
      y = PredPIntensity(p = xx),
      col = 'orange'
      )

#check unpolarized contribution
unpol_intensity = within(calib_data,
                         {
                           p_intensity = PredPIntensity(p)
                       u_intensity = estimated.photon.flux - p_intensity
                         }
                       )
with(unpol_intensity,
     plot(x = u,
          y = log10(u_intensity),
          pch = 19,
          col = 'red')
     )
u_intens_roffset = with(subset(unpol_intensity, 
                              subset = u>0 & p>0),
                       median(u_intensity-PredPIntensity(p))
                       )
u_intens_ratio = with(subset(unpol_intensity, 
                              subset = u>0 & p>0),
                       median(u_intensity/PredPIntensity(p))
                       )

PredUIntensity = function(u)
{
  u_transformed = log( u +1e-4)
  x_data = log( c(0,    1,        2.5,       5.0) +1e-4 )
  y_data = c( 11.35039, 12.03688, 14.81708, 15.22506 )
  spl = smooth.spline(x = x_data,
                      y = y_data,
                      all.knots = TRUE,
                      spar = 1e-2
  )
  return(
    ifelse(u > 0.1,
           yes = 
             50.8837*
             10^(
               predict(object = spl,
                       x = u_transformed)$y
             ),
           no = 0
    )
  )
}

PredI = function(p,u)
{
  PredUIntensity(u)+PredPIntensity(p)
}

with(calib_data,
     {
     plot(x = 1:length(estimated.photon.flux),
          y = log10.photon.flux,
          pch = 19,
          col = 'blue',
          axes = F)
       axis(side = 1,
            at = 1:length(estimated.photon.flux),
            labels = p_u)
       axis(side = 2)
       lines(x = 1:length(estimated.photon.flux),
             y = log10(PredI(p,u)),
             col = 'red')
     }
     )


# Predict DoP -------------------------------------------------------------


with(calib_data,
     {
       plot(x = 1:length(p_u),
            y = degree.of.polarization,
            pch = 19,
            col = 'blue',
            axes = F)
       axis(side = 1,
            at = 1:length(p_u),
            labels = p_u)
       axis(side = 2)
       lines(x = 1:length(p_u),
             y = 1.04*max(degree.of.polarization)*PredPIntensity(p)/
                             PredI(p,u),
             col = 'red'
       )
     }
)

dop_ratio = with(subset(calib_data,
                        subset = p>0 & u>0),
                 {
                     degree.of.polarization/
                       (
                     max(degree.of.polarization)*PredPIntensity(p)/
                       PredI(p,u)
                     )
                 }
)

dop_vs_int = with(subset(calib_data,
                        subset = p>0 & u>0),
                 {
                     degree.of.polarization/
                       (
                     max(degree.of.polarization)*PredPIntensity(p)/
                       PredI(p,u)
                     )
                 }
)

SSpredDoP = function(cf)
{
  with(subset(calib_data,
              subset = p>0),
       {
         sum(
           (
         qlogis(degree.of.polarization) - 
           qlogis( 
             ifelse(test =  p>0 & u>0,
                    yes = 
                           max(degree.of.polarization)*
                             (cf[1]*PredPIntensity(p)+
                             cf[2]*PredUIntensity(u))/
                             PredI(p,u),
                    no = ifelse(test = p > 0,
                                yes = max(degree.of.polarization),
                                no = 0)
                   )
                 )
           )^2
         )
       }
  )
}

opt1 = optim(par = c(1,0.1),
             fn = SSpredDoP,
             control = list(trace = 2)
             )

PredDoP = function(p,u,cf)
{
  ifelse(p>0 & u>0,
         yes = 
                0.36475197*
                  (cf[1]*PredPIntensity(p)+
                     cf[2]*PredUIntensity(u))/
                  PredI(p,u),
         no = ifelse(test = p > 0,
                     yes = 0.36475197,
                     no = 0)
        )
}

with(calib_data,
     {
       plot(x = 1:length(p_u),
            y = degree.of.polarization,
            pch = 19,
            col = 'blue',
            axes = F)
       axis(side = 1,
            at = 1:length(p_u),
            labels = p_u)
       axis(side = 2)
       lines(x = 1:length(p_u),
             y = PredDoP(p,u,cf = opt1$par),
             col = 'green'
       )
     }
)



Dial_predictor = function(p,
                          u)
{
  data.frame(
    dial_setting = paste('p',p,'u',u),
    DoP = round(
                PredDoP(p = p,
                            u = u,
                            cf = opt1$par
                        ),
              digits = 4
              ),
    Photon_flux = PredI(p = p,
                        u = u)
  )
}
# Predict stimulus properties from dial settings --------------------------

#e.g.
Dial_predictor(p = 0.5,
               u = 1.0
               )
 