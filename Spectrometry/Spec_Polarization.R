rm(list = ls())
graphics.off()

# Useful Functions											 ---------------------------------------------

# source(paste0(Sys.getenv('HOME'),'/Dropbox/R scripts/', 'TMUF.R') )
# this needed a lot of tidying up
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
  if(missing(mi)){mi <- min(nm,na.rm=T)}
  if(missing(mx)){mx <- max(nm,na.rm=T)}
  #Install integrate.xy if not already loaded
  if(!any(rownames(installed.packages()) %in% 'sfsmisc', na.rm = T) ){
    mirrors <- getCRANmirrors()
    chooseCRANmirror(graphics=FALSE, 
                     ind = which(mirrors$Country == 'Germany')[1])
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
  return(	sfsmisc::integrate.xy(nm, ph, mi, mx)	)
}#AItoNph

require('sfsmisc')

# Input Variables											 ----------------------------------------------


#	directories														#
ltp <- Sys.getenv('HOME') #Base level in environment

spc <- '/Dropbox/Spec/Apol01-IrradianceLund100mm/'#Folder containing measurements

setnm <- 'Apol01-IrradianceLund100mm'#folder containing measurements
num <- 1#5	#number of repeats for each measurement
# I measured from closer to the light when it was very dim and used an inverse square law to accound for distance
#	Measurements at 10mm											#
#file names (minus ".txt")
setnm <- c(	"Apol01-max", 
            "Apol01-maxnotrace", 
            "Apol01-6dp1d",
            "Apol01-5dp2d",
            "Apol01-4dp3d",
            "Apol01-3dp4d",
            "Apol01-2dp5d",
            "Apol01-1dp6d",
            "Apol01-minnotrace",
            "Apol01-min")#,

stm <- c(	"-tracediff7pol0diff-1000ms1stabox0-",
          "-diff7pol0diff-1000ms1stabox0-",
          "-diff6pol1diff-1000ms1stabox0-",
          "-diff5pol2diff-1000ms1stabox0-",
          "-diff4pol3diff-1000ms1stabox0-",			 
          "-diff3pol4diff-1000ms1stabox0-",
          "-diff2pol5diff-1000ms1stabox0-",
          "-diff1pol6diff-1000ms1stabox0-",
          "-diff0pol7diff-1000ms1stabox0-",
          "-diff0pol7difftrace-1000ms1stabox0-")#,
#
snm <- c(	'draft paper diff x 7 pol diff x 0',
          'diff x 7 pol diff x 0',
          'diff x 6 pol diff x 1',
          'diff x 5 pol diff x 2',
          'diff x 4 pol diff x 3',
          'diff x 3 pol diff x 4',
          'diff x 2 pol diff x 5',
          'diff x 1 pol diff x 6',
          'diff x 0 pol diff x 7',
          'diff x 0 pol diff x 7 draft paper')#

#plot params
xlm <- c(300, 700)#Wavelength range of interest
lw <- 4#Line width
li <- 2.00

mnm <- "Darkroom Lund 19Nov2016 revisited"


# Read In Spectra													 --------------------------------------------

num <- 5
for(st in setnm){
  flb <- paste0('Apol01-irradiance', stm[which(setnm == st)]) #file label
  for(nn in 1:num){
    i <- which(setnm == st)
    filenm <- paste0(ltp, spc, st,'/', flb,'0',sprintf('%02d',nn),'.txt')
    assign( paste0(snm[i],'.',nn),
            read.table(filenm, header = F, sep  = '\t', 
                       skip = 17, nrows = 1028) )
  }#for(nn in 1:num)
}#for(st in setnm)

wln <- get(paste0(snm[i],'.',nn))$V1
rng <- wln >299 & wln < 701

for(sn in snm){
  for(nn in 1:num){
    assign( paste0(sn,'.',nn),
            get(paste0(sn,'.',nn))$V2[rng]	)
  }#for(nn in 1:num)
}#for(sn in snm)

wln <- wln[rng]
#MOVED
lunar.sky <- read.table(paste0(ltp, '/Dropbox/My Papers/DL in Orientation/', 'FullMoonIrradianceJohnsenetal2006C','.txt'), header = F, sep  = '\t')
lun.sky.sp <- smooth.spline(lunar.sky$V1, lunar.sky$V2)


# Median of Each Measurement								 --------------------------------------


#Collect measurements

for(sn in snm){
  assign( paste0('all.',sn), get(paste0(sn,'.',1))*1 )
  for(nn in 2:num){
    assign(paste0('all.',sn), cbind( get(paste0('all.',sn)), get(paste0(sn,'.',nn))*1 ))
  }#for(nn in 2:num)
}#for(sn in snm)

#Take median across repeats
for(sn in snm){
  assign( paste0(sn), apply(get(paste0('all.',sn)), 1, median) )
}#for(sn in snm)



#check that they look ok
#	Pre-transform plot											#
#colours	 for plotting each
clz <- c(		
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
upAI <- .025
dev.new(width=7, height = 5)
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(NULL, xlim = c(300,700), ylim = c(0,upAI), ann = F, type = 'l', col = 'red4')
for(sn in snm){
  i <- which(snm == sn)
  lines(wln, get(paste0(sn)), col = clz[i] )
}#for(sn in snm)
title(xlab = 'Wavelength (nm)')
title(ylab = expression('Absolute Irradiance ('~mu*'Watts'*'cm'^{'-2'}*'nm'^{'-1'}*')'))
title(main = paste(mnm, 'as measured at 100 mm'))
legend(600, upAI, rev(snm), col = rev(clz[1:length(snm)]), lty = 1, lwd = 2)
polygon(c(350, 400, 400, 350), c(0,0,upAI, upAI), col = rgb(1,0,1,0.1), border = NA)
PDFsave(Directory = paste0(ltp,spc), PlotName = paste(mnm, 'AI uncorrected'))

# Transform to Estimated True Counts per Meter Squared	 -------------------


for(sn in snm){
  assign( paste0('photon.',sn), AItoFlux(get(paste0(sn)), wln) )
}#for(sn in snm)
assign( paste0('photon.','fullmoon'), predict(lun.sky.sp, x = wln)$y )

#try smoothing below 350
for(sn in snm){
  tmphoton <- get(paste0('photon.',sn))
  sptmp <- smooth.spline(wln, tmphoton)
  tmphoton[wln<350] <- predict(sptmp, x = wln[wln<350])$y
  assign( paste0('photon.',sn), tmphoton )
}#for(sn in snm)

#make a Scarab UV response curve
spln.365 <- StavengaSpline(c(300, 700), 365) # an a1 opsin with a 365nm max absorption
whole.365 <- predict(spln.365, x = wln)$y
spln.520 <- StavengaSpline(c(300, 700), 520) # an a1 opsin with a 365nm max absorption
whole.520 <- predict(spln.520, x = wln)$y


dev.new(width=7, height = 5)
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(wln, whole.365, type = 'l', col = 'purple', lwd = 3, xlab = 'Wavelength (nm)', ylab = 'Relative Sensitivity', main = 'Stavenga Templates 365 nm & 520 peak')
polygon(c(350, 400, 400, 350), c(0,0,1, 1), col = rgb(1,0,1,0.1), border = NA)
lines(wln, predict(StavengaSpline(c(300, 700), 520), x = wln)$y, col = 'seagreen', lwd = 3)
PDFsave(Directory = paste0(ltp,spc), PlotName = paste('Stavenga template lambda max 365 and 520'))

#Weight by Scarab UV response curve
for(sn in snm){
  assign( paste0('rel.photon.',sn), get(paste0('photon.',sn))*whole.365 )
}#for(sn in snm)
assign( paste0('rel.photon.','fullmoon'), get(paste0('photon.','fullmoon'))*whole.365  )
#	Post-transform plot											#
up.photon <- AItoFlux(upAI, wln[which(get(paste0(sn)) == max( get(paste0(sn)) )) ] )
dev.new(width=7, height = 5)
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(NULL, xlim = c(300,700), ylim = c(0, up.photon), ann = F, type = 'l', col = 'red4')
for(sn in snm){
  i <- which(snm == sn)
  lines(wln, get(paste0('photon.',sn)), col = clz[i] )
}#for(sn in snm)
title(xlab = 'Wavelength (nm)')
title(ylab = expression('Photons cm'^{'-2'}*'s'^{'-1'}*'nm'^{'-1'}))
title(main = paste(mnm, 'as measured at 100 mm'))
legend(600, up.photon, rev(snm), col = rev(clz[1:length(snm)]), lty = 1, lwd = 2)
polygon(c(350, 400, 400, 350), c(0,0, up.photon, up.photon), col = rgb(1,0,1,0.1), border = NA)
PDFsave(Directory = paste0(ltp,spc), PlotName = paste(mnm, 'Photon Illumination', '4Lana'))

dev.new(width=7, height = 5)
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(NULL, xlim = c(300,700), ylim = c(8, log10(up.photon)), ann = F, type = 'l', col = 'red4')
for(sn in snm){
  i <- which(snm == sn)
  lines(wln, log10( get(paste0('photon.',sn)) ), col = clz[i] )
}#for(sn in snm)
title(xlab = 'Wavelength (nm)')
title(ylab = expression('log'[10]~'Photons cm'^{'-2'}*'s'^{'-1'}*'nm'^{'-1'}))
title(main = paste(mnm, 'as measured at 100 mm'))
legend(600, log10(up.photon), rev(snm), col = rev(clz[1:length(snm)]), lty = 1, lwd = 2)
PDFsave(Directory = paste0(ltp,spc), PlotName = paste(mnm, 'log10 Photon Illumination', '4Lana', 'scaled'))
lines(wln, log10(photon.fullmoon), col = 'darkgreen', lwd = 5)
legend(600, 9.5, 'Full Moon \nfrom Johnsen et al. 2008', col = 'darkgreen', lty = 1, lwd = 5, bty = 'n')
polygon(c(350, 400, 400, 350), c(0,0, log10(up.photon), log10(up.photon)), col = rgb(1,0,1,0.1), border = NA)
PDFsave(Directory = paste0(ltp,spc), PlotName = paste(mnm, 'Full moon and', 'log10 Photon Illumination', '4Lana', 'scaled'))

dev.new(width=7, height = 5)
par(mai = c(0.5,0.5,0.5,0.2), cex = 0.55)
plot(NULL, xlim = c(300,700), ylim = c(6, log10(up.photon)), ann = F, type = 'l', col = 'red4')
for(sn in snm){
  i <- which(snm == sn)
  lines(wln, log10( get(paste0('rel.photon.',sn)) ), col = clz[i] )
}#for(sn in snm)
title(xlab = 'Wavelength (nm)')
title(ylab = expression('log'[10]~'UV photopigment relative absorbance'~'•'~'Photons cm'^{'-2'}*'s'^{'-1'}*'nm'^{'-1'}))
title(main = paste(mnm, 'as measured at 100 mm'))
legend(600, log10(up.photon), rev(snm), col = rev(clz[1:length(snm)]), lty = 1, lwd = 2)
PDFsave(Directory = paste0(ltp,spc), PlotName = paste(mnm, 'log10 Relative Photon Illumination', '4Lana', 'scaled'))
lines(wln, log10(rel.photon.fullmoon), col = 'darkgreen', lwd = 5)
legend(600, 8.5, 'Full Moon \nfrom Johnsen et al. 2008', col = 'darkgreen', lty = 1, lwd = 5, bty = 'n')
polygon(c(350, 400, 400, 350), c(0,0, log10(up.photon), log10(up.photon)), col = rgb(1,0,1,0.1), border = NA)
PDFsave(Directory = paste0(ltp,spc), PlotName = paste(mnm, 'relative Full moon and ', 'log10 Photon Illumination', '4Lana', 'scaled'))


# Summed Photon Flux										 --------------------------------------------


#integrate under the spectral irradiance curve across the least noisy range (i.e. not the UV and far red where measured irradiance is probably noise).
#this gives total number of photons, regardless of wavelength
#needs integrate.xy() from 'sfsmisc' package, which performs a spline interpolated integration
awl <- c(350,400)#c(340,400)#;c(350,400)#Let's use the spline and guess values 325--400#too messy try from 340nm#now smoothed, try again

#Take integral in preferred range
for(sn in snm){
  assign( paste0('sm.photon.',sn), integrate.xy(wln, get( paste0('photon.',sn)), a= min(awl), b = max(awl) ) )
}#for(sn in snm)
assign(paste0('sm.photon.','fullmoon'), integrate.xy(wln, get(paste0('photon.','fullmoon')), a= min(awl), b = max(awl) ) ) 

#Take integral in preferred range
for(sn in snm){
  assign( paste0('sm.rel.photon','.',sn), integrate.xy(wln, get( paste0('rel.photon.',sn)), a= min(awl), b = max(awl) ) )
}#for(sn in snm)
assign(paste0('sm.rel.photon.','fullmoon'), integrate.xy(wln, get(paste0('rel.photon.','fullmoon')), a= min(awl), b = max(awl) ) ) 


#not as close as I would like
moonflux <- round( log10( get(paste0('sm.photon.', 'fullmoon' )) ), 2)
unpolflux <- round( log10(apply(cbind(paste0('sm.photon.', snm )), 1, get)), 2)[1:4]
polflux <- round( log10(apply(cbind(paste0('sm.photon.', snm )), 1, get)), 2)[-1:-4]

estimates <- matrix( c(wln,
                       rev(c(apply(cbind(paste0('photon.', snm )), 1, get),    
                             `photon.fullmoon`        )) ), ncol = length(snm)+1 +1, dimnames = list(c(wln), c('wavelength_nm', 'spline Full Moon Johnsen 2008', rev(snm)) ) ) 

rel.estimates <- matrix( c(wln,
                           rev(c(apply(cbind(paste0('rel.photon.', snm )), 1, get),    
                                 `rel.photon.fullmoon`        )) ), ncol = length(snm)+1 +1, dimnames = list(c(wln), c('wavelength_nm', 'spline Full Moon Johnsen 2008', rev(snm)) ) ) 
fluxes <- matrix( rev(c(apply(cbind(paste0('sm.photon.', snm )), 1, get),    
                        `sm.photon.fullmoon`        )), ncol = 1, dimnames = list(c('spline Full Moon Johnsen 2008', rev(c(snm))), paste('photons per cm squared per second', 'from', paste0(awl[1],'-', awl[2]) ) ) )

#what was the measurement for full moon?
print(signif(`sm.photon.fullmoon`, 3))
print(signif(fluxes, 3))

rel.fluxes <- matrix( rev(c(apply(cbind(paste0('sm.rel.photon.', snm )), 1, get),    
                            `sm.rel.photon.fullmoon`        )), ncol = 1, dimnames = list(c('spline Full Moon Johnsen 2008', rev(c(snm))), paste('relative photon absorption rates per cm squared per second', 'from', paste0(awl[1],'-', awl[2]) ) ) )

print(signif(rel.fluxes, 3))

dev.new(width=14, height = 5)
barplot(log10(t(fluxes)), col = c(rev(clz)), beside = T, xlab = 'condition', ylab = expression('log'[10]~'Photons cm'^{'-2'}*'s'^{'-1'}), main = paste0(awl[1],'-', awl[2],'nm'), ylim = c(0, 14), cex.names = .33, las = 2)
abline(h = log10(`sm.photon.fullmoon`), col = 'blue')		
PDFsave(Directory = paste0(ltp,spc), PlotName = paste('log10 Photon Sum', mnm, '4Lana'))

dev.new(width=14, height = 5)
barplot(log10(t(rel.fluxes)), col = c(rev(clz)), beside = T, xlab = 'condition', ylab = expression('log'[10]~'Relative Photons cm'^{'-2'}*'s'^{'-1'}), main = paste0(awl[1],'-', awl[2],'nm'), ylim = c(0, 14), cex.names = .33, las = 2)
abline(h = log10(`sm.rel.photon.fullmoon`), col = 'blue')		
PDFsave(Directory = paste0(ltp,spc), PlotName = paste('log10 Relative Photon Sum', mnm, '4Lana'))

write.csv(estimates, file = paste0(ltp,spc,paste('measured spectra--',mnm, '4Lana'), '.csv'))
write.csv(fluxes, file = paste0(ltp,spc,paste('summed photons',mnm, '4Lana'), '.xlsx'))
write.csv(rel.estimates, file = paste0(ltp,spc,paste('rel measured spectra--',mnm, '4Lana'), '.xlsx'))
write.csv(rel.fluxes, file = paste0(ltp,spc,paste('rel summed photons',mnm, '4Lana'), '.xlsx'))
