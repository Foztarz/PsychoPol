#FOR A 'CLEAN' RUN, PRESS ctrl+shift+F10 to RESTART Rstudio
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2025 04 07
#     MODIFIED:	James Foster              DATE: 2025 08 21
#
#  DESCRIPTION: Inspect and summarise individual dances.
#               Adapted from GUV_inspectdata.R
#               
#       INPUTS: 
#               
#      OUTPUTS: Plots and test statistics
#
#	   CHANGES: - 
#
#   REFERENCES: 
#
#    EXAMPLES:  
#
# 
#TODO   ---------------------------------------------
#TODO   
#- Visualise individual dances  +
#- Summarise individual dances  +
#- Visualise condition differences  +
#- Subtract sun azimuth
#- Summarise condition differences  


# Set up workspace --------------------------------------------------------


## Load packages ----------------------------------------------------------
#needs installing before first use (in Rstudio, see automatic message)
suppressMessages(#these are disturbing users unnecessarily
  {
    # require(CircStats)#package for circular hypothesis tests #Is this being used at all?
    require(circular)#package for handling circular data
    require(CircMLE)#package for circular mixture models
    require(brms)#package for preparing Stan models
    require(oce)#package for calculating sun azimuth
  }
)


## Plot spacing function -------------------------------------------------
#generates the same spacing as R's default barplot function
BarSpacer = function(n, 
                     spa = 0.2,
                     wdt = 1.0)
{
  seq(from = spa+1-wdt/2,
      to = n*(1+spa)-wdt/2,
      length.out = n )
}

## General functions -----------------------------------------------------

#Open file with default program on any OS
# https://stackoverflow.com/a/35044209/3745353
shell.exec.OS = function(x){
  # replacement for shell.exec (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
  {return(base::shell.exec(x))}else
  {comm <- paste0('open "',x,'"')
  return(system(comm))}
}

#convert angles to signed angles in (-180, 180)
Mod360.180 = function(x)
{#use atan2 to convert any angle to the range (-180,180)
  deg(
    atan2(y = sin(rad(x)),
          x = cos(rad(x))
    )
  )
}

#the radian equivalent
mod_circular = function(x)
{
  atan2(y = sin(x),
        x = cos(x))
}

#the unwrapped (no discontinuities)
unwrap_circular = function(x)
{
  mux = mean.circular(x = circular(x = x, template = 'none'))
  centx = atan2(y = sin(x - mux),
                x = cos(x  - mux))
  unwrx = centx + mux
}

#degree version
unwrap_circular_deg = function(x)
{
  mux = mean.circular(x = circular(x = x, template = 'none'))
  centx = atan2(y = sin(x - mux),
                x = cos(x  - mux))
  unwrx = centx + mux
  return(deg(unwrx))
}

#invert the softplus link
#https://en.wikipedia.org/wiki/Softplus
#we are using this as our _inverse_ link function for kappa,
#maps almost 1:1 but keeps values >0 for low estimates
softplus = function(x)
{
  log(exp(x)+1) 
}
#this would return our kappa estimates back to the original scale
inv_softplus = function(x)
{
  log(exp(x)-1) 
}

#convert inv_softplus scaled kappa to mean vector estimate
Softpl_to_meanvec = function(x)
{
  circular::A1(
    softplus(x)
  )
}

#convert circular to normalised
NormCirc = function(x,
                    plusmean = TRUE)
{
  mn = mean.circular(x) * as.numeric(plusmean)
  return(mod_circular(x - mn) + mn)
}

## Circular plotting functions -------------------------------------------

#plot circular data
PCfun = function(angles,
                 col = 'darkblue',
                 shrink = 1.5,
                 title = '',
                 lw = 3
                 )
{
  ca = circular(x = angles,
                units = 'degrees',
                rotation = 'clock')
  plot.circular(x = ca,
                col = col,
                stack = TRUE,
                sep = 0.1,
                bins = 355/5,
                units = 'degrees',
                rotation = 'clock',
                zero = pi/2,
                shrink = shrink)
  mtext(text = title,
        side = 1,
        line = -2)
  lines(x = c(0,0),
        y = c(-1,1),
        col = 'gray')
  arrows.circular(x = mean.circular(ca),
                  y = rho.circular(ca),
                  zero = pi/2,
                  rotation = 'clock',
                  col = col,
                  length =0.1,
                  lwd = lw)
}


#generic mean angle simulator
MeanRvm = function(n, #representative sample size
                   mu = circular(0), #mean (defaults to 0rad)
                   kappa, #kappa required
                   au = 'degrees', #units
                   ar = 'clock') #rotation direction
{
  mean.circular(rvonmises(n = n, 
                          mu = circular(mu, units = au, rotation = ar), 
                          kappa = kappa,
                          control.circular = list(units = au, rotation = ar)))
}


#Simulate confidence intervals for a unimodal or bimodal distribution
#fitted to a vector of "angles"
#Simulate confidence intervals for a unimodal or bimodal distribution
#fitted to a vector of "angles"
CI_vM = function(angles, #vector of angles fitted (used for sample size)
                 m1, #primary mean
                 k1, #primary concentration
                 m2 = NA, #secondary mean (ignored if NULL or NA)
                 k2 = NA, #secondary kappa
                 w1 = 1, #weighting of primary mean
                 force_mu = FALSE, #force median at true mu?
                 n = 1e4, #number of simulations
                 au = 'degrees', 
                 ar = 'clock',
                 calc_q = TRUE,
                 alternative = 'one.sided', #two.sided less conservative
                 interval = 0.95, #confidence interval to calculate
                 speedup_parallel = TRUE
)
{
  if(speedup_parallel) #3x faster
  {
    cl = parallel::makePSOCKcluster(parallel::detectCores()-1)
    parallel::clusterExport(cl = cl, 
                            varlist = c('mean.circular',
                                        'circular',
                                        'rvonmises'),
                            envir = .GlobalEnv
    )
    parallel::clusterExport(cl = cl, 
                            varlist = c('MeanRvm',
                                        'angles',
                                        'm1',
                                        'k1',
                                        'm2',
                                        'k2',
                                        'w1',
                                        'n',
                                        'au',
                                        'ar'),
                            envir = environment()
    )
    #simulate primary mean
    m1_est = 
      parallel::parSapply(cl = cl,
                          X = 1:n,
                          FUN = function(i)
                          {
                            eval.parent(
                              {
                                MeanRvm(n = round(length(angles)*w1), #estimate number of observations at primary mean
                                        mu = m1, 
                                        kappa = k1,
                                        au = au,
                                        ar = ar)
                              }
                            )
                          },
                          simplify = 'array' #return an array of simulated angles
      )
    if(!is.na(m2)) #if there is a valid secondary mean
    {
      m2_est = 
        parallel::parSapply(cl = cl,
                            X = 1:n,
                            FUN = function(i)
                            {
                              eval.parent(
                                {
                                  MeanRvm(n = round(length(angles)*(1-w1)), #estimate number of observations at secondary mean
                                          mu = m2, 
                                          kappa = k2,
                                          au = au,
                                          ar = ar)
                                }
                              )
                            },
                            simplify = 'array' #return an array of simulated angles
        )
    }
    parallel::stopCluster(cl)
  }else
  { #if not using parallel, use the slower version via replicate()
    m1_est = replicate(n = n, 
                       MeanRvm(n = round(length(angles)*w1), 
                               mu = m1, 
                               kappa = k1,
                               au = au,
                               ar = ar)
    )
    if(!is.na(m2))
    {
      m2_est = replicate(n = n, 
                         MeanRvm(n = round(length(angles)*(1-w1)), 
                                 mu = m2, 
                                 kappa = k2,
                                 au = au,
                                 ar = ar)
      )
    }
  }
  return(
    if(calc_q) #calculate quantiles only if requested
    {
      #either two-sided, symmetrical around mean change
      #or one-sided, from zero change towards mean change
      probs1 = switch(alternative,
                      two.sided = sort(c(c(0,1)+c(1,-1)*(1-interval)/2, 0.5)),
                      one.sided = sort(c(c(0,1)+
                                           (if(Mod360.180(m1)>0) #N.B. quantile.circular counts anticlockwise
                                           {c(1,0)}else
                                           {c(0,-1)}
                                           )*(1-interval), 0.5)),
                      sort(c(c(0,1)+ #default to one-sided
                               (if(Mod360.180(m1)>0)
                               {c(1,0)}else
                               {c(0,-1)}
                               )*(1-interval), 0.5))
      )
      if(is.na(m2))
      {
        if(force_mu)
        {
          Mod360.180(
            quantile( Mod360.180(as.numeric(m1_est) - m1),
                      probs = probs1) + m1
          )
        }else
        {
          Mod360.180(
            quantile.circular(x = circular(x = m1_est,
                                           units = au,
                                           rotation = ar),
                              probs = probs1)
          )
        }
      }else
      {
        probs2 = switch(alternative,
                        two.sided = sort(c(c(0,1)+c(1,-1)*(1-interval)/2, 0.5)),
                        one.sided = sort(c(c(0,1)+
                                             (if(Mod360.180(m2)>0)
                                             {c(1,0)}else
                                             {c(0,-1)}
                                             )*(1-interval), 0.5)),
                        sort(c(c(0,1)+ #default to one-sided
                                 (if(Mod360.180(m2)<0)
                                 {c(1,0)}else
                                 {c(0,-1)}
                                 )*(1-interval), 0.5))
        )
        list(m1 = if(force_mu)
        {
          Mod360.180(
            quantile( Mod360.180(as.numeric(m1_est) - m1),
                      probs = probs1) + m1
          )
        }else
        {
          Mod360.180(
            quantile.circular(x = circular(x = m1_est,
                                           units = au,
                                           rotation = ar),
                              probs = probs1)
          )
        },
        m2 = if(force_mu)
        {
          Mod360.180(
            quantile( Mod360.180(as.numeric(m2_est) - m2),
                      probs = probs2) + m2
          )
        }else
        {
          Mod360.180(
            quantile.circular(x = circular(x = m2_est,
                                           units = au,
                                           rotation = ar),
                              probs = probs2)
          )
        }
        )
      }
    }else
    { #if quantiles not requested, return the simulations (mainly for troubleshooting)
      if(is.na(m2))
      {
        m1_est = 
          sapply(X = m1_est, FUN = Mod360.180)
      }else
      {
        list(
          m1_est = 
            sapply(X = m1_est, FUN = Mod360.180),     
          m2_est = 
            sapply(X = m2_est, FUN = Mod360.180),
        )
      }
    }
  )
}


PlotCI_vM = function(ci_vec,
                     col = 'salmon',
                     lwd = 2,
                     radius = 0.95,
                     ...)#passed to lines()
{
  ci_vec = as.numeric(ci_vec)#remove circular formatting!
  #changed on 20250815, plotting issues near median
  angle_seq1.1 = 
    seq(from = ci_vec[1], #lower
        to = ci_vec[1] +
          Mod360.180(ci_vec[2]-ci_vec[1]), #median
        length.out =1e2/2)
  angle_seq1.2 = 
    seq(from = ci_vec[2], #median
        to = ci_vec[2] +
          Mod360.180(ci_vec[3]-ci_vec[2]) , #upper
        length.out =1e2/2)
  lines(x = radius*sin( rad(angle_seq1.1) ),
        y = radius*cos( rad(angle_seq1.1) ),
        col = col,
        lwd = lwd,
        lend = 'butt',
        ...
  )
  lines(x = radius*sin( rad(angle_seq1.2) ),
        y = radius*cos( rad(angle_seq1.2) ),
        col = col,
        lwd = lwd,
        lend = 'butt',
        ...
  )
  if(!is.na(ci_vec[4]))
  {
    #changed on 20250815
    angle_seq2.1 = 
      seq(from = ci_vec[1+3],
          to = ci_vec[1+3] +
            Mod360.180(ci_vec[2+3]-ci_vec[1+3]),
          length.out =1e2/2)
    
    angle_seq2.2 = 
      seq(from = ci_vec[2+3],
          to = ci_vec[2+3] +
            Mod360.180(ci_vec[3+3]-ci_vec[2+3]) ,
          length.out =1e2/2)
    lines(x = radius*sin( rad(angle_seq2.1) ),
          y = radius*cos( rad(angle_seq2.1) ),
          col = col,
          lwd = lwd,
          lend = 'butt',
          ....)
    lines(x = radius*sin( rad(angle_seq2.1) ),
          y = radius*cos( rad(angle_seq2.1) ),
          col = col,
          lwd = lwd,
          lend = 'butt',
          ...)
  }
}


## Add sun azimuth ---------------------------------------------------------
# Directions to Pullen
# GPS Coordinates
# 25° 34' 19" S
# 31° 10' 53" E
# -25.571944, 31.181389

#Convert video name in format
#2024-02-25 12-47-55 
#to
#

IDtocode = function(x)
{
  tp = strsplit(x = as.character(x),
                split = '\\-')[[1]]
  tcode = paste0(tp[1], '-', tp[2], '-',
                 tp[3], ':', tp[4], ':', tp[5]
  )
  return(tcode)
}

#handling function (sunAngle is bad with NAs and vectors)
GetSaz = function(tm,
                  lon,
                  lat)
{
  if(!is.na(tm))
  {
    oce::sunAngle(
      t = tm,
      longitude = lon,
      latitude = lat,
      useRefraction = TRUE)$azimuth
  }else
  {
    tm
  }
}
#handling function (sunAngle is bad with NAs and vectors)
GetSel = function(tm,
                  lon,
                  lat)
{
  if(!is.na(tm))
  {
    oce::sunAngle(
      t = tm,
      longitude = lon,
      latitude = lat,
      useRefraction = TRUE)$altitude
  }else
  {
    tm
  }
}

# Input Variables ----------------------------------------------------------

all_plots = FALSE # to speed up
#  .  User input -----------------------------------------------------------



# . System parameters -----------------------------------------------------

#Check the operating system and assign a logical flag (T or F)
sys_win = Sys.info()[['sysname']] == 'Windows'
#User profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp  =  Sys.getenv('HOME')#Life was easier on Mac
}

# . Select file ---------------------------------------------------------

# set path to file
if(sys_win){#choose.files is only available on Windows
  message('\n\nPlease select the ".csv" file\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file  <- choose.files(
    default = file.path(ltp,'Documents', "*.csv"),#For some reason this is not possible in the "root" user
    caption = 'Please select the "colour_dance_reorg.csv" file'
  )
}else{
  message('\n\nPlease select the "colour_dance_reorg.csv" file\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file <- file.choose(new=F)
}
#show the user the path they have selected
if(is.null(path_file) | !length(path_file))
{stop('No file selected.')}else
{print(path_file)}


# Read in the data and format ---------------------------------------------

#select the reorganised data
dd = read.table(file = path_file, 
                header = T, 
                sep  = ',')
View(dd)

dd = within(dd,
            {
              ID = as.factor(Bee) # bee identifier as a factor
              Video = as.factor(Video) # video as a factor
              signed_angle = Mod360.180(Angle)  # bearing between -180 and 180
              angle = circular(rad(signed_angle),# bearing between -pi and pi
                               rotation = 'clock') # circular format suppresses later warnings
            }
)

u_id = with(dd, unique(ID)) # unique beedances
length(u_id)#18 bees


# Add sun position --------------------------------------------------------


#add time
dd = within(dd,
            {
              time = sub(pattern = '^...........',
                         x = Video,
                         replacement = '')
              time_code = sapply(X = Video, FUN = IDtocode)
              sast_time =  as.POSIXct(time_code, tz = "Africa/Johannesburg", format = "%Y-%m-%d %H:%M:%OS")#South Africa Standard Time
              utc_time =  as.POSIXct(sast_time, tz = "UTC")#UTC time
              rm(time_code)
            }
)

#add sun azimuth
dd = within(dd, 
            {
              sun_az = mapply(
                FUN = GetSaz,
                tm = sast_time,
                lon = 31.181389,
                lat = -25.571944)
            }
)

#add sun elevation
dd = within(dd, 
            {
              sun_el = mapply(
                FUN = GetSel,
                tm = sast_time,
                lon = 31.181389,
                lat = -25.571944)
            }
)


#add sun azimuth
dd = within(dd, 
            {
              rel_angle = angle - sun_az
            }
)



# Inspect individual dances -----------------------------------------------

#set up a sequence for the axes
xc = seq(from = -pi, to = pi-1e-16, length.out = 1e3)
#function for calculating mean vector params
Mvec = function(angle, units = 'radians', rotation = 'counter', type = 'angles')
{
  return(
    c(
      mu = as.numeric(mean.circular(circular(angle,units = units, rotation = rotation, type = type),
                                    control.circular = list(units = units, rotation = rotation, type = type))), 
      rho = rho.circular(circular(angle,units = units, rotation = rotation, type = type))
    )
  )
}

#set up plot
pdf_fl = file.path(dirname(path_file), 'all_dances.pdf')
pdf(file = pdf_fl)
par(mfrow = c(2, 2), mar = c(0,0,0,0))
par(pty = 's')

#add legend to 1st page
plot(x = NULL,
     xlim = c(-1,1),
     ylim = c(-1,1),
     pch = 19,
     axes = FALSE,
     xlab = '',
     ylab = '',
     main = ''
)
legend(x = 'center',
       legend = c('Antisolar',
                  'Solar'),
       col = c('cyan',
               'orange'),
       pch = c(21,21),
       lty = c(NA, NA),
       lwd = c(2,2)
)
#loop through individuals
for(ii in u_id)
{
  mnv_as = Mvec(subset(dd, ID %in% ii & Stimulus %in% 'antisolar')$angle)
  mnv_so = Mvec(subset(dd, ID %in% ii & Stimulus %in% 'solar')$angle)
  plot(x = NULL,
       xlim = 22*c(-1,1),
       ylim = 22*c(-1,1),
       pch = 19,
       axes = FALSE,
       xlab = '',
       ylab = '',
       main = ''
  )
  abline(a = 0, b = 1, col = 'gray90')
  abline(a = 0, b = -1, col = 'gray90')
  abline(h = 0, v = 0, col = 'gray75')
  lines(x = 10*sin(xc), y = 10*cos(xc), lty = 3)
  lines(x = 20*sin(xc), y = 20*cos(xc), lty = 3)
  text(x = 1:4*20,
       y = c(0,0,0,0),
       labels = paste(' run', 1:4*20), 
       cex = 0.3,
       adj = c(0,1))
  with(subset(dd, ID %in% ii),
       {
         points(x = Run*sin(as.numeric(angle)),
                y = Run*cos(as.numeric(angle)),
                bg = gray(level = 1.0,
                          alpha =  0.4),
                col = c('cyan', 'orange')
                [ifelse(Stimulus %in% 'Antisolar',
                        yes = 1, no = 2)],
                pch = 21,
                lwd = 2
         )
       }
  )
  lines(x = c(0,100*sin(mnv_as['mu'])*mnv_as['rho']), 
        y = c(0,100*cos(mnv_as['mu'])*mnv_as['rho']), 
        col = 'cyan4', 
        lwd = 1)
  lines(x = c(0,100*sin(mnv_so['mu'])*mnv_so['rho']), 
        y = c(0,100*cos(mnv_so['mu'])*mnv_so['rho']), 
        col = 'orange4', 
        lwd = 1)
  mtext(ii,side = 3,line = -2)
  legend(x = 'bottomright',
         col = c('cyan3',
                 'orange2'),
         pch = 15,
         legend = c('Antisolar skylight',
                    'Solar skylight'))
}
#save and open
dev.off()
shell.exec.OS(pdf_fl)


# Plot by video -----------------------------------------------------------


#set up plot
pdf_fl = file.path(dirname(path_file), 'all_dances_videos.pdf')
pdf(file = pdf_fl)
par(mfrow = c(2, 2), mar = c(0,0,0,0))
par(pty = 's')

#add legend to 1st page
plot(x = NULL,
     xlim = c(-1,1),
     ylim = c(-1,1),
     pch = 19,
     axes = FALSE,
     xlab = '',
     ylab = '',
     main = ''
)
legend(x = 'center',
       legend = c('Antisolar',
                  'Solar'),
       col = c('cyan',
               'orange'),
       pch = c(21,21),
       lty = c(NA, NA),
       lwd = c(2,2)
)
#loop through individuals
for(vv in unique(dd$Video))
{
  mnv_as = Mvec(subset(dd, Video %in% vv & Stimulus %in% 'antisolar')$angle)
  mnv_so = Mvec(subset(dd, Video %in% vv & Stimulus %in% 'solar')$angle)
  plot(x = NULL,
       xlim = 22*c(-1,1),
       ylim = 22*c(-1,1),
       pch = 19,
       axes = FALSE,
       xlab = '',
       ylab = '',
       main = ''
  )
  abline(a = 0, b = 1, col = 'gray90')
  abline(a = 0, b = -1, col = 'gray90')
  abline(h = 0, v = 0, col = 'gray75')
  lines(x = 10*sin(xc), y = 10*cos(xc), lty = 3)
  lines(x = 20*sin(xc), y = 20*cos(xc), lty = 3)
  text(x = 1:4*20,
       y = c(0,0,0,0),
       labels = paste(' run', 1:4*20), 
       cex = 0.3,
       adj = c(0,1))
  with(subset(dd, Video %in% vv),
       {
         points(x = Run*sin(as.numeric(angle)),
                y = Run*cos(as.numeric(angle)),
                bg = gray(level = 1.0,
                          alpha =  0.4),
                col = c('cyan', 'orange')
                [ifelse(Stimulus %in% 'Antisolar',
                        yes = 1, no = 2)],
                pch = 21,
                lwd = 2
         )
       }
  )
  lines(x = c(0,100*sin(mnv_as['mu'])*mnv_as['rho']), 
        y = c(0,100*cos(mnv_as['mu'])*mnv_as['rho']), 
        col = 'cyan4', 
        lwd = 1)
  lines(x = c(0,100*sin(mnv_so['mu'])*mnv_so['rho']), 
        y = c(0,100*cos(mnv_so['mu'])*mnv_so['rho']), 
        col = 'orange4', 
        lwd = 1)
  mtext(paste(unique(subset(dd, Video %in% vv)$ID), vv),
        side = 3,line = -2)
  # legend(x = 'bottomright',
  #        col = c('cyan3',
  #                'orange2'),
  #        pch = 15,
  #        legend = c('Antisolar skylight',
  #                   'Solar skylight'))
}
#save and open
dev.off()
shell.exec.OS(pdf_fl)

# Calculate mean vectors --------------------------------------------------


#calculate mean vectors
mean_vectors = aggregate(angle~ID*Stimulus*Video*sun_az*sun_el, # N.B. Including sun azimuth drops some cases without a time stamp
                         data = dd,
                         FUN = rho.circular
)
#correct names
mean_vectors = within(mean_vectors,
                      {mean_vector = angle; rm(angle)} # anlge now indicates a mean vector, not an angle
)
#add kappa
MLE_est = function(x)
{
  with(mle.vonmises(circular(x,
                             template = 'none'),
                    bias = TRUE),
       c(mu = as.numeric(mu), 
         kappa = kappa))
  
}

mle_estimates = aggregate(angle~ID*Stimulus*Video,
                          data = dd,
                          FUN = MLE_est
)
#add to the summary table and 
#calculate inverse softplus kappa
mean_vectors = within(mean_vectors,
                      {
                        mu = deg(mle_estimates$angle[,'mu'])
                        kappa = mle_estimates$angle[,'kappa']
                        iskappa  = inv_softplus(kappa)
                      }
)


# Plot all vectors for solar and antisolar dances -------------------------

par(mfrow = c(1, 1), mar = c(0,0,0,0))
par(pty = 's')
plot(x = NULL,
     xlim = c(-1,1),
     ylim = c(-1,1),
     pch = 19,
     axes = FALSE,
     xlab = '',
     ylab = '',
     main = ''
)
lines(x = 1*sin(xc), y = 1*cos(xc), lty = 3)
abline(h = 0, v = 0)
#TODO fix video with two vectors
for(vv in unique(mean_vectors$Video))
       {
  with(subset(mean_vectors, Video %in% vv),
       {
lines(x = c(0,mean_vector*sin(rad(mu)) ),
      y = c(0, mean_vector*cos(rad(mu)) ),
      col = adjustcolor(col = ifelse(test = Stimulus %in% 'Antisolar', 
                    yes = 'cyan3',
                    no = 'orange2'),
                    alpha.f = 0.5),
      lwd = 3
      )
     }
  )
}
legend(x = 'bottomright',
       col = c('cyan3',
               'orange2'),
       pch = 15,
       legend = c('Antisolar skylight',
                  'Solar skylight'))


# Just W28 ----------------------------------------------------------------
mv28 = subset(mean_vectors, ID %in% 'W28')
#TODO check this, these look different from the raw data
par(mfrow = c(1, 1), mar = c(0,0,0,0))
par(pty = 's')
plot(x = NULL,
     xlim = c(-1,1),
     ylim = c(-1,1),
     pch = 19,
     axes = FALSE,
     xlab = '',
     ylab = '',
     main = ''
)
lines(x = 1*sin(xc), y = 1*cos(xc), lty = 3)
abline(h = 0, v = 0)
for(vv in unique(mv28$Video))
{
  with(subset(mv28, Video %in% vv),
       {
         lines(x = c(0,mean_vector*sin(rad(mu)) ),
               y = c(0, mean_vector*cos(rad(mu)) ),
               col = adjustcolor(col = ifelse(test = Stimulus %in% 'Antisolar', 
                                              yes = 'cyan3',
                                              no = 'orange2'),
                                 alpha.f = 0.5),
               lwd = 3
         )
         text(x = mean_vector*sin(rad(mu)) + runif(1, -0.05, 0.05),
              y = mean_vector*cos(rad(mu)) + runif(1, -0.05, 0.05),
              labels = Video,
              cex = 0.5,
              adj = c(0,1))
       }
  )
}
legend(x = 'bottomleft',
       col = c('cyan3',
               'orange2'),
       pch = 15,
       legend = c('Antisolar skylight',
                  'Solar skylight'),
       bty = 'n',
       inset = c(0.05,0.3)
      )

# circular version


par(mfrow = c(1,2), mar = c(0,0,0,0))
par(pty = 's')
PCfun(angles = subset(dd, Stimulus %in% 'Antisolar' & ID %in% 'W28')$Angle,
      col = 'cyan3')
PCfun(angles = subset(dd, Stimulus %in% 'Solar' & ID %in% 'W28')$Angle,
      col = 'orange2')
mtext(text = 'W28 Dances 2024-02-20 & 21',
      side = 3, outer = TRUE, line =-2)
legend(x = 'bottomright',
       legend = c('Antisolar skylight',
                  'Solar skylight'),
       col = c('cyan3',
               'orange2'),
       pch = 15,
       cex = 0.7,
       bty = 'n')

# W28 relative to sun ----------------------------------------------------------------
mv28 = within(mv28,
              {
                mu_sun = mu - sun_az
              }
              )

par(mfrow = c(1, 1), mar = c(0,0,0,0))
par(pty = 's')
plot(x = NULL,
     xlim = c(-1,1),
     ylim = c(-1,1),
     pch = 19,
     axes = FALSE,
     xlab = '',
     ylab = '',
     main = ''
)
lines(x = 1*sin(xc), y = 1*cos(xc), lty = 3)
abline(h = 0, v = 0)
for(vv in unique(mv28$Video))
{
  with(subset(mv28, Video %in% vv),
       {
         lines(x = c(0,mean_vector*sin(rad(mu_sun)) ),
               y = c(0, mean_vector*cos(rad(mu_sun)) ),
               col = adjustcolor(col = ifelse(test = Stimulus %in% 'Antisolar', 
                                              yes = 'cyan3',
                                              no = 'orange2'),
                                 alpha.f = 0.5),
               lwd = 3
         )
         text(x = mean_vector*sin(rad(mu_sun)) + runif(1, -0.05, 0.05),
              y = mean_vector*cos(rad(mu_sun)) + runif(1, -0.05, 0.05),
              labels = Video,
              cex = 0.5,
              adj = c(0,1))
       }
  )
}
legend(x = 'bottomright',
       col = c('cyan3',
               'orange2'),
       pch = 15,
       legend = c('Antisolar skylight',
                  'Solar skylight'),
       bty = 'n',
       inset = c(0.05,0.3)
      )

# circular version

par(mfrow = c(1,2), mar = c(0,0,0,0))
par(pty = 's')
PCfun(angles = with(data = subset(dd, Stimulus %in% 'Antisolar' & ID %in% 'W28'),
                    expr = {Angle - sun_az}),
      col = 'cyan3')
PCfun(angles = with(data = subset(dd, Stimulus %in% 'Solar' & ID %in% 'W28'),
                    expr = {Angle - sun_az}),
      col = 'orange2')
mtext(text =
'W28 Dances 2024-02-20 & 21
relative to sun azimuth',
      side = 3, outer = TRUE, line =-2)
legend(x = 'bottomright',
       legend = c('Antisolar skylight',
                  'Solar skylight'),
       col = c('cyan3',
               'orange2'),
       pch = 15,
       cex = 0.7,
       bty = 'n')

# by dance
u_vid = mv28$Video
par(mfrow = c(ceiling(length(u_vid)/2),2), mar = c(0,0,0,0))
par(pty = 's')
for(uv in u_vid)
{
  subs = subset(dd,
                            subset = 
                              ID %in% 'W28' &
                              Video %in% uv)
  PCfun(angles = with(data = subs,
                      expr = {Angle - sun_az}),
        col = if(subs$Stimulus[1] %in% 'Antisolar')
                {'cyan3'}else
                {'orange2'},
        title = uv
        )
}
plot(x = NULL,
     xlim = c(0,0),
     ylim = c(0,0),
     xlab = '',
     ylab = '',
     main = '',
     axes = FALSE
     )
mtext(text =
        paste('W28 Dances relative to sun azimuth'),
      side = 3, outer = TRUE, line = -2)
legend(x = 'center',
       legend = c('Antisolar skylight',
                  'Solar skylight'),
       col = c('cyan3',
               'orange2'),
       pch = 15,
       cex = 1.0,
       bty = 'n')


# Load selected data from 20250128 ----------------------------------------
here_path = tryCatch(expr = #look in the folder containing this file: sys.frame(1)$ofile
                       {file.path(dirname(sys.frame(1)$ofile))},
                     error = function(e)
                     {#if that fails, try to find the "Documents" folder
                       file.path(ltp,'Documents/GitHub/PsychoPol/Dances')
                     }
                    )

fiji_angles = list.files(path = file.path(here_path),
                         pattern = '*.csv',
                         recursive = TRUE,
                         full.names = TRUE)

B66 = lapply(X = fiji_angles,
             FUN = read.table,
             header = TRUE, 
             sep = ',')
B66 = mapply(FUN = cbind,
             B66, 
             filename = basename(fiji_angles),
             SIMPLIFY = FALSE
             )
B66data = do.call(what = rbind,
                     args = B66)
B66data = within(B66data,
                 {bearing = 90-Angle}
                 )

par(mfrow = c(2,2), mar = c(0,0,0,0))
for(fl in unique(B66data$filename))
{
  b66d = subset(B66data,
                filename %in% fl
                )
  with(b66d,
       {
        PCfun(angles = bearing,
              col = 'green4',
              title = fl,
              # title = '',
              lw = 2.0)
        mlvm = mle.vonmises(x = circular(bearing, units = 'degrees'),
                            bias = TRUE) 
        #50% CI
        PlotCI_vM(ci_vec = CI_vM(angles = bearing,
                                 m1 = mlvm$mu,
                                 k1 = mlvm$kappa,
                                 interval = 0.5,
                                 alternative = 'two.sided'),
                  radius = 1.25,
                  col = 'green4',
                  lwd = 3)
        #95% CI
        PlotCI_vM(ci_vec = CI_vM(angles = bearing,
                                 m1 = mlvm$mu,
                                 k1 = mlvm$kappa,
                                 interval = 0.95,
                                 alternative = 'two.sided'),
                  radius = 1.25,
                  col = 'green4',
                  lwd = 0.25)
       }
  )
}
