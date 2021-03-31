rm(list = ls())
graphics.off()
#R versions <4.0.0 convert strings to factors, specify default behaviour
formals(data.frame)$stringsAsFactors <- FALSE
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2020 11 26
#     MODIFIED:	James Foster              DATE: 2021 03 31
#
#  DESCRIPTION: Adapted from "Darkroom Lund 119Nov2016 Irradiance.R"
#               Loads text files in µW/nm and calculates photon irradiance for
#               across a predetermined interval.
#               
#      OUTPUTS: Absolute irradiance as plain text (txt).
#
#	     CHANGES: - Works on JMU Würzburg computers 
#             
#             
#
#   REFERENCES: Johnsen S. (2016) How to Measure Color Using Spectrometers and 
#               Calibrated Photographs. Journal of Experimental Biology, 219(6)
#               772–78. https://doi.org/10.1242/jeb.124008.
#
#               Stavenga, D.G., et al. (1993) Simple Exponential Functions
#               Describing the Absorbance Bands of Visual Pigment Spectra. 
#               Vision Research, 33(8), 1011–17.
#               https://doi.org/10.1016/0042-6989(93)90237-Q.
#
#    EXAMPLES: 
#
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data +
#- Handle ignored names +
#- Estimate relative photon flux  +
#- General-use version  

# Input Variables ----------------------------------------------------------

#  .  User input -----------------------------------------------------------

#Measurement name
mnm <- "APol03 top layer 31 Mar 2021"
#Measurement distance
mds <- 50#mm
#Lambda max of photoreceptor class of interest
lmx <- 344
#Wavelengths to sum photons across range with high signal
awl <- c(350,400)

#ignore files with this string
ignore_st <- 'dark'
#subsistute this string from filenames
subst_st <- 'pc'
#with this string
repl_st <- '%'

#plot params
xlm <- c(300, 700)#Wavelength range of interest
lw <- 4#Line width
li <- 2.00

#check the operating system and assign a logical flag (TRUE or FALSE)
sys_win <- Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  ltp <- gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp <- Sys.getenv('HOME')#Life was easier on Mac
}

#  .  Select files ---------------------------------------------------------

#	directories														#
# ltp <- Sys.getenv('HOME') #Base level in environment
# set path to files
if(sys_win){#choose.files is only available on Windows
  message('\n\nPlease select the master folder\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  spc  <- choose.dir(
    default = file.path(ltp,'Documents'),#For some reason this is not possible in the "root" user
    caption = 'Please select the master folder'
  )
}else{
  message('\n\nPlease select any file in a subfolder of the master folder\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  spc <- dirname(dirname(file.choose(new=F)))
}
print(spc)

#  .  Derive variables	----------------------------------------------
#wavelength range label
wlr <- paste0(awl[1],'–',awl[2])

#folders containing measurements
setnm <- list.dirs(spc, recursive = F)
names(setnm) <- basename(setnm)
#basic sorting, works for 20201024 data
setnm <- setnm[order(nchar(names(setnm)))]
#Ignore extra stuff
setnm <- setnm[!(names(setnm) %in% c('zExtra'))]

#file names (minus ".txt")
stm <- lapply(setnm, dir, pattern = '.txt')
names(stm) <- names(setnm)
#replace undesirable characters in names
snm <-  sub(subst_st,  repl_st, basename(setnm))
#repalce minus with a space
snm <- lapply(strsplit(snm, '-'), paste, collapse = ' ')
names(snm) <- names(setnm)


# Useful Functions											 ---------------------------------------------

#	Save current plot with dimensions displayed on screen and label accordingly
PDFsave <- function(Directory = file.path(Sys.getenv('HOME'),'Documents'),
                    Experiment = '_',
                    Species = '_',
                    PlotName = date(),
                    Dim = par('din')
                    )
  {
  #Copy to invisible device to save
  suppressWarnings(
    dev.copy(pdf,
             file.path(Directory, 
                            paste0(Experiment, Species,PlotName, '.pdf'
                                   )
                       ),
             width = Dim[1], height = Dim[2],
             useDingbats = F
             )
  )
  #Save the image actual size
  dev.off()#Doesn't save until invisible copy is closed
  dev.set(dev.prev())
}

# Make a spline template for a visual pigment
StavengaSpline <- function(spec.range = c(300, 700),
                           lambda.max,
                           a.type = 'a1'){
  wlns <- seq(min(spec.range),max(spec.range), length.out = 1e3) #
  #Stavenga, D. G. (2010). On visual pigment templates and the spectral shape of invertebrate rhodopsins and metarhodopsins. Journal of Comparative Physiology A: Neuroethology, Sensory, Neural, and Behavioral Physiology, 196(11), 869–878. doi:10.1007/s00359-010-0568-7
  # modified lognormal
  m.lognorm <- function(wl,l.max, a0, a1)
  {
    x = log10(wl/l.max)
    return(exp(-a0*x^2 * (1+a1*x+3*a1^2*x^2)))
  }
  if(a.type == 'a1')
  {
    #alpha band
    a.band <- m.lognorm(wlns,lambda.max, 380, 6.09)
    #beta band
    b.band <- 0.29*m.lognorm(wlns, 340, 247, 3.59)
    #gamma band
    g.band <- 1.99*m.lognorm(wlns, 276, 647, 23.4)
  }else
  {stop('a2 and a3 pigments not yet implemented')}
  # N.B. Stavenga normalises to max(a.band), I normalise to function max
  r.stav <- (a.band + b.band + g.band)/max(a.band + b.band + g.band)
  return(	smooth.spline(wlns, r.stav)	)
}#StavengaSpline <- function(spec.range, lambda.max){

#Convert Absolute Irradiance measurements into Photons/cm2/s/nm
AItoFlux <- function(ai, nm){
  # Absolute Irradiance is measured in microWatts/cm2/nm
  # 1microWatt = 10^-3 Joules / s
  # to convert from Joules/cm2/s/nm to Photons/cm2/s/nm we must calculate
  # how many Joules/Photon = hc/lambda; all in the same units.
  #	Planck's constant	#
  planck <- 6.62606957*10^-34	#		Joules x seconds
  #	speed of light	#
  lightSpeed <- 299792458#			meters / second
  #	Photon energy per wavelength	#
  hc = planck * lightSpeed#			Joule x meters (/ meter wavelength)
  #	wavelength factor in photon energy	#
  wavelengthM <- nm*10^-9#	       	wavelength in meters
  photonEnergy <- hc/(wavelengthM)#	Joules / photon
  #	ai should be absolute irradiance in  micro Joules/cm2/s/nm
  #	divided by photon energy in micro Joules 
  # (multiplied by 1000000µJ/J, divided by something?)
  #	=   photons/cm2/s/nm
  ph <- ai/(photonEnergy*1000000)#	photons/cm2/s/nm
  return(ph)
}#AItoFlux

#Convert Absolute Irradiance spectrum into Photons/cm2/s across a wavelength band
AItoNph <-  function(ai, nm, mi, mx){	
  #set maximum limits
  rag <- range(nm)
  if(missing(mi)){mi <- rag[1]}
  if(missing(mx)){mx <- rag[2]}
  #Install integrate.xy if not already loaded
  if(!( sum(rownames(installed.packages()) %in% 'sfsmisc', na.rm = T) )){
    mirrors <- getCRANmirrors()
    chooseCRANmirror(graphics=FALSE, 
                     ind = which(mirrors$Country == 'Germany'))
    install.packages('sfsmisc')
  }#if(!( sum(rownames(installed.packages()) %in% 'sfsmisc', na.rm=T) ))
  library('sfsmisc')
  # Absolute Irradiance is measured in microWatts/cm2/nm
  # 1microWatt = 10^-3 Joules / s
  # to convert from Joules/cm2/s/nm to Photons/cm2/s/nm we must calculate
  # how many Joules/Photon = hc/lambda; all in the same units.
  #	Planck's constant	#
  planck <- 6.62606957*10^-34	#		Joules x seconds
  #	speed of light	#
  lightSpeed <- 299792458#			meters / second
  #	Photon energy per wavelength	#
  hc = planck * lightSpeed#			Joule x meters (/ meter wavelength)
  #	wavelength factor in photon energy	#
  wavelengthM <- nm*10^-9#	       	wavelength in meters
  photonEnergy <- hc/(wavelengthM)#	Joules / photon
  #	ai should be absolute irradiance in  micro Joules/cm2/s/nm
  #	divided by photon energy in micro Joules 
  # (multiplied by 1000000µJ/J, divided by something?)
  #	=   photons/cm2/s/nm
  ph <- ai/(photonEnergy*1000000)#	photons/cm2/s/nm
  return(	integrate.xy(nm, ph, mi, mx)	)
}#AItoNph

#load the package with integrate.xy
require('sfsmisc')

# Read In Spectra													 --------------------------------------------
#construct paths to each file
file_path <- lapply(names(setnm), function(i){file.path(setnm[[i]], stm[[i]])})
#label them with the correct measurement names
names(file_path) <- names(setnm)
#Load each file starting at Ocean Optics' ">>>Begin" flag
ai_files <- lapply(file_path, 
                   function(s)
                   {
                     lapply(s,
                            function(fl)
                            {
                               read.table(
                                 file = fl, header = F, sep = '\t',
                                 skip = grep(readLines(fl),pattern = '>>>>Begin')#,
                                 # nrows = 1028
                                 )
                            }
                           )
                   }
                  )
# num <- 5
# for(st in setnm){
#   flb <- paste0('Apol01-irradiance', stm[which(setnm == st)]) #file label
#   for(nn in 1:num){
#     i <- which(setnm == st)
#     filenm <- paste0(ltp, spc, st,'/', flb,'0',sprintf('%02d',nn),'.txt')
#     assign( paste0(snm[i],'.',nn),
#             read.table(filenm, header = F, sep  = '\t', 
#                        skip = 17, nrows = 1028) )
#   }#for(nn in 1:num)
# }#for(st in setnm)
RangeFUN <- function(x,v = 'V2'){subset(x, V1 >299 & V1 <701)[,v]}

wln <- RangeFUN(ai_files[[1]][[1]], 'V1')#get(paste0(snm[i],'.',nn))$V1

ai_files <- lapply(ai_files, 
                   sapply,
                   RangeFUN
                  )
#MOVED
solar.sky <- read.table(
                file.path(ltp, '/Dropbox/Spec/',
                          'Sun11degIrradianceJohnsenetal2006C.txt'
                          ),
                        header = F, sep  = '\t'
              )
sun.sky.sp <- with(solar.sky, smooth.spline(V1, V2))


# Median of Each Measurement								 --------------------------------------


# #Collect measurements

# for(sn in snm){
#   assign( paste0('all.',sn), get(paste0(sn,'.',1))*1 )
#   for(nn in 2:num){
#     assign(paste0('all.',sn), cbind( get(paste0('all.',sn)), get(paste0(sn,'.',nn))*1 ))
#   }#for(nn in 2:num)
# }#for(sn in snm)

#Take median across repeats
ai_medians <- sapply(ai_files,
                     apply,
                     1,
                     median
                     )

# for(sn in snm){
#   assign( paste0(sn), apply(get(paste0('all.',sn)), 1, median) )
# }#for(sn in snm)
if(dim(ai_medians)[1]<length(wln))
{
  warning('measurement array shorter than number of wavelength bins\n',
          'rotating array'
          )
  ai_medians <- t(ai_medians)
}
if(dim(ai_medians)[1] == length(wln))
# {ai_medians <- within(data.frame(ai_medians), {wavelength <- wln})}else
{}else
{
  stop('measurement array is not the same length as the wavelength vector',
      'perhaps not all measurements were recorded with the same device?')
}
#column names cannot contain certain characters, replace these with '_' for readability
names(ai_medians) <- sub('^X', '_', names(ai_medians))#starting with number, gets 'X'
names(ai_medians) <- sub('.', '_', names(ai_medians))#'-' replaced with '.', use '_'


#check that they look ok
#	Pre-transform plot											#
#colours	 for plotting each
clz <- sapply(
  dim(ai_medians)[2],
  colorRampPalette(
            c(		
            'gray30',
            'gray20',
            'purple4',
            'magenta4',
            'steelblue',
            'darkblue',
            'orange3',
            'orange4',
            'darkred',
            'red3',
            'cyan3',
            'cyan4',
            'pink3',
            'salmon4',
            'seagreen',
            'darkgreen',
            'gray50',
            'gray70'
            )
  )
)


# Plot raw absolute irradiance --------------------------------------------


upAI <- max(ai_medians[,1:length(snm)])
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(NULL, xlim = c(300,700), ylim = c(0,upAI), ann = F, type = 'l', col = 'red4')
for(sn in snm){
  i <- which(snm == sn)
  lines(wln, ai_medians[,i], col = clz[i] )
}#for(sn in snm)
title(xlab = 'Wavelength (nm)')
title(ylab = expression('Absolute Irradiance ('~mu*'Watts'*'cm'^{'-2'}*'nm'^{'-1'}*')'))
title(main = paste(mnm, 'as measured at', mds,'mm'))
legend(450, upAI, rev(snm), col = rev(clz[1:length(snm)]), lty = 1, lwd = 2)
# polygon(c(350, 400, 400, 350), c(0,0,upAI, upAI), col = rgb(1,0,1,0.1), border = NA)
PDFsave(Directory = spc, PlotName = paste(mnm, 'AI uncorrected'))

# Transform to Estimated True Counts per Meter Squared	 -------------------

photon_medians <- apply(ai_medians[,-length(ai_medians)],2, AItoFlux, wln)

photon.solarsky <-  predict(sun.sky.sp, x = wln)$y 

    # #try smoothing below 350
    # for(sn in snm){
    #   tmphoton <- get(paste0('photon.',sn))
    #   sptmp <- smooth.spline(wln, tmphoton)
    #   tmphoton[wln<350] <- predict(sptmp, x = wln[wln<350])$y
    #   assign( paste0('photon.',sn), tmphoton )
    # }#for(sn in snm)

#make a honeybee UV response curve (Menzel & Blakers, 1976)
spln.lmx <- StavengaSpline(c(300, 700), lmx) # an a1 opsin with a 344nm max absorption
whole.lmx <- predict(spln.lmx, x = wln)$y


# Plot photon flux --------------------------------------------------------


dev.new()
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(wln, whole.lmx, type = 'l', col = 'purple', lwd = 3,
     xlab = 'Wavelength (nm)', ylab = 'Relative Sensitivity',
     main = paste0('Stavenga Template', lmx,'nm peak')
     )
# polygon(c(350, 400, 400, 350), c(0,0,1, 1), col = rgb(1,0,1,0.1), border = NA)
# lines(wln, predict(StavengaSpline(c(300, 700), 344), x = wln)$y, col = 'seagreen', lwd = 3)
PDFsave(Directory = spc, PlotName = paste('Stavenga template lambda max 344'))

#Weight by Honeybee UV response curve
rel.photon_medians <- apply(photon_medians,2, function(x){x*whole.lmx})

rel.photon.solarsky <-  photon.solarsky*whole.lmx
#	Post-transform plot											#
up.photon <- max(photon_medians)
dev.new()
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(NULL, xlim = c(300,700), ylim = c(0, up.photon),
     ann = F, type = 'l', col = 'red4')
for(sn in snm){
  i <- which(snm == sn)
  lines(wln, photon_medians[,i], col = clz[i] )
}#for(sn in snm)
title(xlab = 'Wavelength (nm)')
title(ylab = expression('Photons cm'^{'-2'}*'s'^{'-1'}*'nm'^{'-1'}))
title(main = paste(mnm, 'as measured at ', mds,' mm'))
legend(500, up.photon, rev(snm), col = rev(clz[1:length(snm)]), lty = 1, lwd = 2)
# polygon(c(350, 400, 400, 350), c(0,0, up.photon, up.photon), col = rgb(1,0,1,0.1), border = NA)
PDFsave(Directory = spc, PlotName = paste(mnm, 'Photon Illumination'))

dev.new()
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(NULL, xlim = c(300,700), ylim = c(10, log10(up.photon)), ann = F, type = 'l', col = 'red4')
for(sn in snm){
  i <- which(snm == sn)
  lines(wln, log10( photon_medians[,i] ), col = clz[i] )
}#for(sn in snm)
title(xlab = 'Wavelength (nm)')
title(ylab = expression('log'[10]~'Photons cm'^{'-2'}*'s'^{'-1'}*'nm'^{'-1'}))
title(main = paste(mnm, 'as measured at ',mds,' mm'))
# PDFsave(Directory = paste0(ltp,spc), PlotName = paste(mnm, 'log10 Photon Illumination', '4Lana', 'scaled'))
lines(wln, log10(photon.solarsky), col = 'skyblue', lwd = 3)
legend(550, log10(up.photon), rev(snm), col = rev(clz[1:length(snm)]), lty = 1, lwd = 2)
legend(550, 9.5, 'Sunlit sky \nfrom Johnsen et al. 2008', col = 'skyblue', lty = 1, lwd = 5, bty = 'n')
# polygon(c(350, 400, 400, 350), c(0,0, log10(up.photon), log10(up.photon)), col = rgb(1,0,1,0.1), border = NA)
PDFsave(Directory = spc, PlotName = paste(mnm, 'Sunlit sky and', 'log10 Photon Illumination', 'scaled'))

dev.new()
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(NULL, xlim = c(300,700), ylim = c(10, log10(up.photon)), ann = F, type = 'l', col = 'red4')
for(sn in snm){
  i <- which(snm == sn)
  lines(wln, log10( rel.photon_medians[,i] ), col = clz[i] )
}#for(sn in snm)
title(xlab = 'Wavelength (nm)')
title(ylab = expression('log'[10]~'UV photopigment relative absorbance'~'•'~'Photons cm'^{'-2'}*'s'^{'-1'}*'nm'^{'-1'}))
title(main = paste(mnm, 'as measured at ',mds,' mm'))
# PDFsave(Directory = paste0(ltp,spc), PlotName = paste(mnm, 'log10 Relative Photon Illumination', '4Lana', 'scaled'))
lines(wln, log10(rel.photon.solarsky), col = 'skyblue', lwd = 3)
legend(550, log10(up.photon), rev(snm), col = rev(clz[1:length(snm)]), lty = 1, lwd = 2)
legend(550, 8.0, 'Sunlit sky \nfrom Johnsen et al. 2008', col = 'skyblue', lty = 1, lwd = 5, bty = 'n')
# polygon(c(350, 400, 400, 350), c(0,0, log10(up.photon), log10(up.photon)), col = rgb(1,0,1,0.1), border = NA)
PDFsave(Directory = spc, PlotName = paste(mnm, 'relative sunlit sky and ', 'log10 Photon Illumination', '4Lana', 'scaled'))



# # MEASUREMENT SPECIFIC edge distance
# 
# # dev.new(width=7, height = 5)
# par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
# plot(NULL, xlim = c(300,450), ylim = c(10, log10(up.photon)), ann = F, type = 'l', col = 'red4')
# for(sn in snm[length(snm) - 0:2]){
#   i <- which(snm == sn)
#   lines(wln, log10( photon_medians[,i] ), col = clz[i] )
# }#for(sn in snm)
# title(xlab = 'Wavelength (nm)')
# title(ylab = expression('log'[10]~'Photons cm'^{'-2'}*'s'^{'-1'}*'nm'^{'-1'}))
# title(main = paste(mnm, 'as measured at',mds,'mm'))
# legend(400, log10(up.photon),
#        rev(snm[length(snm) - 0:2]), 
#        col = rev(clz[length(snm) - 0:2]), 
#        lty = 1, lwd = 2
#        )

#nothing I can do here, no dropoff measured!


# Summed Photon Flux										 --------------------------------------------


#integrate under the spectral irradiance curve across the least noisy range (i.e. not the UV and far red where measured irradiance is probably noise).
#this gives total number of photons, regardless of wavelength
#needs integrate.xy() from 'sfsmisc' package, which performs a spline interpolated integration
#c(340,400)#;c(350,400)#Let's use the spline and guess values 325--400#too messy try from 340nm#now smoothed, try again

#Take integral in preferred range
sm.photon_medians <- apply(photon_medians,
                           2,
                            function(p)
                              {
                              integrate.xy(x = wln,
                                           fx = p,
                                           a = min(awl),
                                           b = max(awl)
                                           )
                              }
                            )
sm.photon.solarsky <- integrate.xy(x = wln,
                                   fx = photon.solarsky,
                                   a = min(awl),
                                   b = max(awl)
                                  )
sm.rel.photon_medians <- apply(rel.photon_medians,
                               2,
                               function(p)
                               {
                                 integrate.xy(x = wln,
                                              fx = p,
                                              a = min(awl),
                                              b = max(awl)
                                 )
                               }
                            )
sm.rel.photon.solarsky <- integrate.xy(x = wln,
                                   fx = rel.photon.solarsky,
                                   a = min(awl),
                                   b = max(awl)
                                   )


#not as close as I would like
polflux <-  sapply(sm.photon_medians,
                  function(p)
                  {
                    round( log10(p), 2)
                  }
                  )
solarflux <- round( log10(sm.photon.solarsky), 2)

estimates <- matrix( c(wln,
                       photon.solarsky,
                       photon_medians    
                       ),
                     ncol = length(snm)+1 +1,
                     dimnames = list(c(wln), 
                                     c('wavelength_nm', 'spline Sunlit Sky Johnsen 2008', snm) 
                                     )
                     ) 

rel.estimates <- matrix( c(wln,
                           rel.photon.solarsky,   
                           rel.photon_medians
                           ), 
                         ncol = length(snm)+1 +1, 
                         dimnames = list(c(wln), 
                                         c('wavelength_nm', 'spline Sunlit Sky Johnsen 2008', snm) 
                                         ) 
                         ) 
fluxes <- matrix( c(sm.photon.solarsky,
                    sm.photon_medians    
                    ),
                  nrow = 1,
                  dimnames = list(NULL,
                                  c('spline Sunlit Sky Johnsen 2008',
                                     paste(snm,'photons per cm squared per second', 'from', paste0(awl[1],'-', awl[2]) ) 
                                    )
                                  )
                  )


rel.fluxes <- matrix( c(sm.rel.photon.solarsky,
                           sm.rel.photon_medians    
                        ),
                      nrow = 1,
                      dimnames = list(NULL,
                                      c('spline Sunlit Sky Johnsen 2008',
                                        paste(snm,'photons per cm squared per second', 'from', paste0(awl[1],'-', awl[2]) ) 
                                      )
                      )
                    )

barplot(log10(fluxes), col = c('skyblue',clz),
        beside = T,
        xlab = '',
        ylab = expression('log'[10]~'Photons cm'^{'-2'}*'s'^{'-1'}),
        main = 'condition',
        # main = '',#paste0(awl[1],'-', awl[2],'nm'),
        ylim = c(10, log10(up.photon)+2),
        cex.names = 0.33,
        axisnames = F
        )
axis(1,
     at = 1:(length(snm)+1)*2-0.5,
     labels = c('solar sky', snm),
     col.axis = 'white',
     line = -12,
     las = 2,
     tick = F, lty = 0
     )

abline(h = log10(sm.photon.solarsky), col = 'skyblue')		
PDFsave(Directory = spc, PlotName = paste('log10 Photon Sum', mnm))


# MEASUREMENT SPECIFIC jog-wheel setting ----------------------------------
#logistic fit
lm.1 <- lm( y~x,
            data = data.frame(x = c(50, 50, 50, 50, 50, 100, 100),
                              y = qlogis(
                                  (sm.photon_medians -
                                     0.99*min(sm.photon_medians))/
                                    (1.01*max(sm.photon_medians))
                                )
                              )
          )
#spline fit
  # sp.1 <- smooth.spline(x = c(50, 50, 50, 50, 50, 100, 100),
  #                       y = sm.photon_medians
  #                       )
  # #gamma fit a+b*(x)^g
  # #ideally "a" should be 0 here
  # #so just b*(x)^g
  # scale.x <- scale(sm.photon_medians, center = F)
  # gammaFUN <- function(x, p)
  #               {
  #                 p[1]*x^p[2]
  #               }
  # gammaSS <- function(p, x = c(0,0.33, 0.50, 1.0), y = scale.x)
  #               {
  #                sum(
  #                  ( y - gammaFUN(x, p) )^2
  #                ) 
  #               }
  # ga.1 <- lapply(1:10,#in 10 chains
  #                function(i)
  #                {#start with random coefficients drawn from integers 1 to 10
  #                 optim(sample(1:10,2,T), gammaSS,
  #                       )
  #                }
  #               )
  # ga.val <- sapply(ga.1, function(x){x$value})
  # ga.fit <- ga.1[[which.min(ga.val)]]
  # xx <- seq(0,1,length.out = 1e3)
  # lm.fit <- 1.01*max(sm.photon_medians[1:4])*
  #             plogis(
  #               predict(lm.1, newdata = data.frame(x = xx)) 
  #               )+
  #             0.99*min(sm.photon_medians[1:4])
  # sp.fit <- predict(sp.1, x = xx)
  # plot(c(0,0.33, 0.50, 1.0),
  #      log10(sm.photon_medians[1:4]),
  #      pch = 3
  #      )
  # # lines(xx, log10(lm.fit), col = 'darkblue', lwd = 3)
  # lines(sp.fit$x, log10(sp.fit$y), col = 'darkgreen', lwd = 3)
  # lines(xx,
  #       log10((ga.fit$par[1]*xx^ga.fit$par[2])*attr(scale.x, 'scaled:scale')),
  #       col = 'purple', lwd = 3
  #       )
  # #Gamma fit looks good (though for this data a linear fit would probably also be fine)

# Save everything ---------------------------------------------------------
fnm <- file.path(spc, mnm)

summed <- data.frame(total.flux = sm.photon_medians,
                     rel.flux = sm.rel.photon_medians
                       )
write.csv(x = cbind(wavelength = wln, ai_medians),
          file = paste0(fnm, '-rawAI','.csv'),
          )
write.csv(x = cbind(wavelength = wln, photon_medians),
          file = paste0(fnm, '-photon.flux','.csv'),
          )
write.csv(x = cbind(wavelength = wln, rel.photon_medians),
          file = paste0(fnm, '-rel.photon.flux','.csv'),
          )
write.csv(x = summed,
          file = paste0(fnm, '-summed.photon.flux_',awl[1],'-', awl[2],'nm','.csv'),
          )


