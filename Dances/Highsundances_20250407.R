#FOR A 'CLEAN' RUN, PRESS ctrl+shift+F10 to RESTART Rstudio
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2025 04 07
#     MODIFIED:	James Foster              DATE: 2025 08 19
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

#add sun azimuth
dd = within(dd, 
            {
              sun_el = mapply(
                FUN = GetSel,
                tm = sast_time,
                lon = 31.181389,
                lat = -25.571944)
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

PCfun = function(angles,
                 col,
                 shrink = 1.5,
                 title = '')
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
                  length =0.1)
}

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

PCfun = function(angles,
                 col,
                 shrink = 1.5,
                 title = '')
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
                  length =0.1)
}

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
