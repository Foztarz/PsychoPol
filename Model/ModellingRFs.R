# Set up variables --------------------------------------------------------
fwhm = 5.74#maximum rf for honeybee DRA
lin_fwhm2 = 15.33333#estimated fwhm of witches' hat brim
fwhm2sd = 2*sqrt(2*log(2))#fwhm/sd ratio
x1 = seq(from = -30, to  = 30, length.out = 1e4)# sequence of angles to plot

# Set up functions --------------------------------------------------------
normGauss = function(xx = seq(from = -30, to  = 30, length.out = 1e4),
                     ...)
{
  dnorm(x = xx,...)/dnorm(x = 0,...)
}

normMixGauss = function(xx = seq(from = -30, to  = 30, length.out = 1e4),
                        sd1 = 1,
                        sd2 = 2,
                        wt = 1.0,
                        ...)
{
  g1 = dnorm(x = xx, sd = sd1, ...)
  g2 = wt*dnorm(x = xx, sd = sd2, ...)
  mx = dnorm(x = 0,sd=sd1)+wt*dnorm(x = 0,sd=sd2)
  return((g1+g2)/mx)
}

DMixGauss = function(xx = seq(from = -30, to  = 30, length.out = 1e4),
                        mu = 0,
                        sd1 = 1,
                        sd2 = 2,
                        wt1 = 0.5,
                        ...)
{
  g1 = wt1*dnorm(x = xx, mean = mu, sd = sd1, ...)
  g2 = (1-wt1)*dnorm(x = xx, mean = mu, sd = sd2, ...)
  mx = g1+g2
  return(mx)
}
xx = seq(from = -30, to  = 30, length.out = 1e4)
plot(xx,
     DMixGauss(xx)/DMixGauss(0),
     type = 'l')
# Plot and check y crossings ----------------------------------------------


plot(x = x1,
     y = normGauss(xx = x1,
                    sd = fwhm/fwhm2sd),
     type = 'l',
     ylab = 'Rel. Sensitivity',
     xlab = 'Off-axis angle'
     )
lines(x = x1,
     y = 0.08*normGauss(xx = x1,
                    sd = 30/fwhm2sd),
     col = 4
     )

# First guess -------------------------------------------------------------
plot(x = x1,
     y = normMixGauss(xx = x1,
                      sd1 = fwhm/fwhm2sd,
                      sd2 = 40/fwhm2sd,
                      wt = 0.5),
     col = 2,
     type = 'l',
     ylab = 'Rel. Sensitivity',
     xlab = 'Off-axis angle',
     main = 'A. mellifera DRA photoreceptor',
     xlim = c(0,35)
     )
abline(h = c(0,1))
abline(v = c(c(-1,-1,1,1)*c(7,30),0), lty = 2, col = 1)
abline(h = c(8,2)/100, lty = 3, col = 4)
text(x = c(7,30), y = c(0.08,0.02), adj = c(0,0), col = 4,
     labels = c('8%','2%'))


# Monarch butterfly -------------------------------------------------------
dplex = data.frame(angle = c(6,
                        5,
                        4,
                        3,
                        2,
                        1,
                        0,
                        1,
                        2,
                        3,
                        4,
                        5,
                        6,
                        6,
                        5,
                        4,
                        3,
                        2,
                        1,
                        0,
                        1,
                        2,
                        3,
                        4,
                        5,
                        6),
          rsens= c(6.332,#6,
                   9.34,#5,
                   8.151,#4,
                   33.409,#3,
                   53.394,#2,
                   77.247,#1,
                   99.166,#0,
                   60.056,#1,
                   42.004,#2,
                   25.243,#3,
                   14.928,#4,
                   4.613,#5,
                   1.819,#6
                   3.157,#6,
                   6.871,#5,
                   16.74,#4,
                   30.939,#3,
                   48.455,#2,
                   74.072,#1,
                   99.519,#0,
                   76.636,#1,
                   54.704,#2,
                   27.359,#3,
                   14.928,#4,
                   4.26,#5,
                   2.877)#6),
          )
# Field cricket -------------------------------------------------------
gcamp = data.frame(photorcp = c(rep(1,length.out = 39),
                                rep(2,length.out = 50),
                                rep(3,length.out = 37) ),
                   xy = c(rep('X',length.out =24), rep('y',length.out = 15),
                          rep('X',length.out =27), rep('y',length.out = 23),
                          rep('X',length.out =23), rep('y',length.out = 14) ),
              rel_angle = c(15,#1X
                        20,#1X
                        25,#1X
                        30,#1X
                        35,#1X
                        40,#1X
                        45,#1X
                        50,#1X
                        55,#1X
                        60,#1X
                        65,#1X
                        70,#1X
                        75,#1X
                        80,#1X
                        85,#1X
                        90,#1X
                        95,#1X
                        100,#1X
                        105,#1X
                        110,#1X
                        115,#1X
                        120,#1X
                        125,#1X
                        130,#1X
                        20,#1Y
                        25,#1Y
                        30,#1Y
                        35,#1Y
                        40,#1Y
                        45,#1Y
                        50,#1Y
                        55,#1Y
                        60,#1Y
                        65,#1Y
                        70,#1Y
                        75,#1Y
                        80,#1Y
                        85,#1Y
                        90,#1Y
                        37.5,#2X
                        39.33333333,#2X
                        41.33333333,#2X
                        44,#2X
                        46.66666667,#2X
                        48.66666667,#2X
                        51.33333333,#2X
                        53.33333333,#2X
                        56,#2X
                        58,#2X
                        60,#2X
                        62.66666667,#2X
                        65.33333333,#2X
                        67.33333333,#2X
                        70,#2X
                        72.66666667,#2X
                        74.66666667,#2X
                        77.33333333,#2X
                        80,#2X
                        82,#2X
                        84.666,#2X
                        86.66666667,#2X
                        89.33333333,#2X
                        91.33333333,#2X
                        94,#2X
                        96,#2X
                        99.33333333,#2X
                        18,#2X
                        20,#2Y
                        23.33333333,#2Y
                        25.33333333,#2Y
                        28,#2Y
                        30.666,#2Y
                        32.66666667,#2Y
                        34.66666667,#2Y
                        36.66666667,#2Y
                        39.33333333,#2Y
                        42,#2Y
                        44,#2Y
                        46.66666667,#2Y
                        49.33333333,#2Y
                        52,#2Y
                        54,#2Y
                        56.66666667,#2Y
                        58.66666667,#2Y
                        63.33333333,#2Y
                        65.33333333,#2Y
                        67.33333333,#2Y
                        70.66666667,#2Y
                        72.66666667,#2Y
                        39.33333333,#2Y
                        45,#3X
                        50,#3X
                        55,#3X
                        60,#3X
                        65,#3X
                        70,#3X
                        75,#3X
                        80,#3X
                        85,#3X
                        90,#3X
                        95,#3X
                        100,#3X
                        105,#3X
                        110,#3X,
                        115,#3X
                        120,#3X
                        125,#3X
                        130,#3X
                        135,#3X
                        140,#3X
                        145,#3X
                        150,#3X
                        25,#3y
                        30,#3y
                        35,#3Y
                        40,#3Y
                        45,#3Y
                        50,#3Y
                        55,#3Y
                        60,#3Y
                        65,#3Y
                        70,#3Y
                        75,#3Y
                        80,#3Y
                        85,#3Y
                        90#3Y
              ),
              logsense = c(
              0.160300449,
              0.343500962,
              0.503801411,
              0.572501603,
              0.618301731,
              0.732802052,
              1.122103142,
              1.488504168,
              1.809105065,
              1.83200513,
              1.55720436,
              1.603004488,
              1.877805258,
              1.374003847,
              0.984702757,
              0.755702116,
              0.549601539,
              0.435101218,
              0.480901347,
              0.412201154,
              0.366401026,
              0.366401026,
              0.320600898,
              0.297700834,
              0.068700192,
              0.091600256,
              0.274800769,
              0.526701475,
              0.572501603,
              0.755702116,
              1.190803334,
              1.259503527,
              1.671704681,
              1.992305578,
              1.580104424,
              1.694604745,
              1.671704681,
              1.305303655,
              1.305303655,
              0.137400385,
              0.251900705,
              0.480901347,
              0.595401667,
              0.709901988,
              0.801502244,
              0.801502244,
              0.870202437,
              0.870202437,
              1.030502885,
              1.122103142,
              1.122103142,
              1.213703398,
              1.145003206,
              1.076303014,
              1.099203078,
              1.099203078,
              1.030502885,
              0.938902629,
              0.847302372,
              0.687001924,
              0.572501603,
              0.572501603,
              0.412201154,
              0.229000641,
              0.274800769,
              0.022900064,
              0.045800128,
              0.595401667,
              0.595401667,
              0.458001282,
              0.572501603,
              0.77860218,
              0.870202437,
              0.916002565,
              0.984702757,
              1.030502885,
              1.007602821,
              1.05340295,
              1.030502885,
              1.122103142,
              1.16790327,
              1.259503527,
              1.465604104,
              1.625904553,
              1.763304937,
              1.55720436,
              1.511404232,
              1.44270404,
              1.259503527,
              1.076303014,
              1.488504168,
              1.671704681,
              1.877805258,
              1.923605386,
              1.717504809,
              2.038105707,
              1.94650545,
              1.900705322,
              1.603004488,
              1.374003847,
              1.030502885,
              0.824402308,
              0.732802052,
              0.732802052,
              0.641201795,
              0.435101218,
              0.343500962,
              0.480901347,
              0.114500321,
              0.320600898,
              0.297700834,
              0.068700192,
              0.068700192,
              0.206100577,
              0.458001282,
              0.641201795,
              0.893102501,
              1.282403591,
              1.854905194,
              2.015205643,
              1.94650545,
              1.923605386,
              1.923605386,
              1.83200513,
              1.465604104,
              1.328203719)
              
          )
gcamp_maxes = aggregate(logsense~photorcp*xy, data = gcamp, max )
gcamp_relsense = aggregate(logsense~photorcp*xy, 
                           data = gcamp, 
                           FUN = function(x)
                             {10^(x - max(x))} 
                           )$logsense
gcamp$rsens = unlist(gcamp_relsense)
gcamp_angle = lapply(unique(gcamp$photorcp),
                     function(pt)
                     {
                       lapply(unique(gcamp$xy),
                             function(xxyy)
                             {
                               dt = subset(gcamp, photorcp == pt & xy == xxyy)
                             return(with(dt, rel_angle - rel_angle[which.max(rsens)]))
                             }
                             )
                     }
                    )
gcamp$angle = unlist(gcamp_angle)
gcamp$absangle = abs(unlist(gcamp_angle))
with(gcamp,
   plot(angle,
        rsens)
   )
lapply(unique(gcamp$photorcp),
       function(pt)
       {
         lapply(unique(gcamp$xy),
                function(xxyy)
                {
                  dt = subset(gcamp, photorcp == pt & xy == xxyy)
                  with(dt, lines(angle,rsens, col = round(runif(1,min=1,max=10))) )
                }
         )
       }
)
with(gcamp,
   plot(absangle,
        rsens)
   )
lapply(unique(gcamp$photorcp),
       function(pt)
       {
         lapply(unique(gcamp$xy),
                function(xxyy)
                {
                  dt = subset(gcamp, photorcp == pt & xy == xxyy)
                  with(dt, lines(absangle,rsens, col = round(runif(1,min=1,max=10))) )
                }
         )
       }
)
# Fit curves --------------------------------------------------------------


FitGaussSens = function(x,
                        y,
                        sd0 = range(x, na.rm = T), 
                        b0 = max(y, na.rm = T)*sd0/dnorm(0))
{
  if(any(c(sd0,b0)<1e-16))
  {return(1e19)}
  # if(b0>1.5*max(y,na.rm = T))
  # {return(1e19)}
  #normalise y
  ypred = b0*dnorm(x,mean =0, sd = sd0)
  yrat = y/(ypred)
  abs_log_ratio = abs(log(yrat))#when the curves are closest, the log ratio approaches 0
  to_optimise = sum(abs_log_ratio, na.rm = T)
  # to_optimise = sum((y-ypred)^2, na.rm = T)
  return(to_optimise)
}
FitMixGaussSens = function(x,
                        y,
                        sd1 = range(x, na.rm = T)/2, 
                        sd2 = range(x, na.rm = T), 
                        wt1 = 0.5,
                        b0 = max(y, na.rm = T)/
                              DMixGauss(0,sd1=sd1,sd2=sd2,wt1=wt1)
                        )
{
  if(any(c(sd1,sd2,wt1,b0)<1e-16))
  {return(1e19)}
  # if(sd2<sd1)
  # {return(1e19)}
  if(wt1<0.10|wt1>1-0.10)
  {return(1e19)}
  #normalise y
  if(any(is.nan(
    c(dnorm(x=x, mean = 0, sd = sd2),
      dnorm(x=x, mean = 0, sd = sd1))
    )))
  {stop(paste(sapply(c(sd1, sd2, wt1), round, digits = 3),' '))}
  ypred = b0*DMixGauss(xx=x,mu =0, sd1 = sd1, sd2 = sd2, wt1 = wt1)
  yrat = y/(ypred+1e-16)#floating point rounding can predict zeroes
  abs_log_ratio = abs(log(yrat))#when the curves are closest, the log ratio approaches 0
  to_optimise = sum(abs_log_ratio)
  # to_optimise = sum((y-ypred)^2)
  if(is.na(to_optimise))
  {return(1e19)}
  return(to_optimise)
}
opt_norm <- with(subset(dplex, angle <8),
     {
       optimize(interval = c(lower = 0, upper = 90),
             f = function(prm)
             {
               FitGaussSens(x = angle,
                           y = rsens,
                           sd0 = prm
                           )
             }
       )
       }
     )
#More efficient sampling required
nchains = 1e3
mx_startpar <- data.frame(sd1 = 10^runif(n = nchains,min = 0, max = log10(90/3)),
                          sd2 = 10^runif(n = nchains,min = 0, max = log10(90/3)),
                          wt1 = runif(n= nchains, min = 0.25, max = 0.75)
                          )
lst <- vector(length = nchains, mode = 'list')
for(cn in 1:nchains)
  {
# opt_mxnorm <- with(subset(dplex, angle <8),
lst[[cn]] <- with(subset(dplex, angle <8),
     {
       # optim(par = c(sd1 = diff(range(angle, na.rm = T))/2, 
       #               sd2 = diff(range(angle, na.rm = T)),
       #               wt1 = 0.5),
       tryCatch(
         {
         optim(par = c(sd1 = mx_startpar$sd1[cn], 
                       sd2 = mx_startpar$sd2[cn],
                       wt1 = mx_startpar$wt1[cn]),
               function(prm)
               {
                 FitMixGaussSens(x = angle,
                             y = rsens,
                             sd1 = prm[1],
                             sd2 = prm[2],
                             wt1 = prm[3]
                             )
               },
               control = list(trace = F,
                              maxit = 1e3)
         )
         },
         error = function(e){stop(round(mx_startpar[cn,],3))}
       )
       }
     )
}#for(cn in 1:nchains)
opt_val <- sapply(lst, function(x){x$value})
opt_mxnorm <- lst[[which.min(opt_val)]]
xx = seq(from = 0, to = 6, length.out= 1e3)
plot(dplex$angle,
     dplex$rsens,
     xlab = c('Off axis angle'),
     ylab = c('Relative sensitivity (%)'))
with(opt_norm,
     lines(xx,
           minimum*max(dplex$rsens)*dnorm(xx,sd = minimum)/dnorm(0),
      col = 2)
)
with(data.frame(t(opt_mxnorm$par)),
     lines(xx,
           max(dplex$rsens)*DMixGauss(xx,sd1 = sd1, sd2 = sd2, wt1 = wt1)/
             DMixGauss(0,sd1 = sd1, sd2 = sd2, wt1 = wt1),
           col = 4
          )
      )
# for(cn in 1:nchains)
# {
# with(data.frame(t(lst[[cn]]$par)),
#      lines(xx,
#            max(dplex$rsens)*DMixGauss(xx,sd1 = sd1, sd2 = sd2, wt1 = wt1)/
#              DMixGauss(0,sd1 = sd1, sd2 = sd2, wt1 = wt1),
#            col = rgb(0,0,1,1/256)
#           )
#       )
# }
#Two distinct solutions
#Average the best ones?
mx_all = sapply(which(opt_val < quantile(opt_val,1.0)),
               function(cn)
                 {with(data.frame(t(lst[[cn]]$par)),
                       {
                         max(dplex$rsens)*DMixGauss(xx,sd1 = sd1, sd2 = sd2, wt1 = wt1)/
                               DMixGauss(0,sd1 = sd1, sd2 = sd2, wt1 = wt1)
                       }
                       )
                 }
               )
# mx_med = apply(mx_all, 1, median)
mx_mean = apply(mx_all, 1, mean)
# lines(xx, mx_med, col = 3)
lines(xx, mx_mean, col = 6)

gcamp_opt_norm <- with(subset(gcamp, absangle <20),
                 {
                   optimize(interval = c(lower = 0, upper = 90),
                            f = function(prm)
                            {
                              FitGaussSens(x = angle,
                                           y = rsens,
                                           sd0 = prm
                              )
                            }
                   )
                 }
)

gcamp_lst <- vector(length = nchains, mode = 'list')
for(cn in 1:nchains)
{
  # opt_mxnorm <- with(subset(dplex, angle <8),
  gcamp_lst[[cn]] <- with(subset(gcamp, absangle <90),
                    {
                      # optim(par = c(sd1 = diff(range(angle, na.rm = T))/2, 
                      #               sd2 = diff(range(angle, na.rm = T)),
                      #               wt1 = 0.5),
                      
                      tryCatch(
                        {
                          optim(par = c(sd1 = mx_startpar$sd1[cn], 
                                        sd2 = mx_startpar$sd2[cn],
                                        wt1 = mx_startpar$wt1[cn]),
                                function(prm)
                                {
                                  FitMixGaussSens(x = angle,
                                                  y = rsens,
                                                  sd1 = prm[1],
                                                  sd2 = prm[2],
                                                  wt1 = prm[3]
                                  )
                                },
                                control = list(trace = F,
                                               maxit = 1e3)
                          )
                        },
                        error = function(e){stop(paste(round(mx_startpar[cn,],3),' '))}
                      )
                    }
  )
}#for(cn in 1:nchains)
gcamp_opt_val <- sapply(gcamp_lst, function(x){x$value})
gcamp_opt_mxnorm <- gcamp_lst[[which.min(gcamp_opt_val)]]
xx = seq(from = -90, to  = 90, length.out = 1e4)
plot(gcamp$angle,
     gcamp$rsens,
     xlab = c('Off axis angle'),
     ylab = c('Relative sensitivity (%)'))
with(gcamp_opt_norm,
     lines(xx,
           minimum*max(gcamp$rsens)*dnorm(xx,sd = minimum)/dnorm(0),
           col = 2)
)
with(data.frame(t(gcamp_opt_mxnorm$par)),
     lines(xx,
           max(gcamp$rsens)*DMixGauss(xx,sd1 = sd1, sd2 = sd2, wt1 = wt1)/
             DMixGauss(0,sd1 = sd1, sd2 = sd2, wt1 = wt1),
           col = 4
     )
)
# Honeybee ----------------------------------------------------------------


