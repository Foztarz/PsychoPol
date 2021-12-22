rm(list = ls())
graphics.off()
#R versions <4.0.0 convert strings to factors, specify default behaviour
formals(data.frame)$stringsAsFactors <- FALSE
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2020 11 26
#     MODIFIED:	James Foster              DATE: 2021 12 17
#
#  DESCRIPTION: Adapted from "???.R"
#               Loads text files in counts/nm and calculates polarization
#               parameters across the UV visible spectrum.
#               
#       INPUTS: Path to a master folder. Each subfolder should contain a set of 
#               measurements of the same stimulus through a polarizer at 
#               different angles. Each subfolder may also contain a set of dark 
#               measurements, which will be averaged and subtracted.
#               
#      OUTPUTS: Plots of spectral polarization (.pdf).
#
#	   CHANGES:   - Plot saving 
#               - Spectrasuite compatibility
#               - International Light compatibility
#               - Attempted SpectrILight compatibility (file format unclear)
#               - Conversion to counts/second by reading header integration time
#               - Better handling of values below dark baseline
#
#   REFERENCES: Johnsen, S., (2012) The Optics of Life: A Biologist’s Guide to
#               Light in Nature (Princeton University Press)
#               http://books.google.com/books?id=Q8zWqiKA7JMC&pgis=1.
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
#- Neaten up    +
#- Save plots   + 
#- Counts/s     +
#- Summary calculations   +
#- Generalise calculations   +
#- Save results   +

# Input Variables ----------------------------------------------------------

#  .  User input -----------------------------------------------------------

mang <- 4#  number of measurement angles
labchar <-  0# 3#number of characters used in angle label: 0 if not used
lmx <- 344#nm Lambda max of photoreceptor class of interest
awl <- c(300,450)#wavelength range
AvFUN <- median#select an averaging function
specType <- 'OO' #'IL'#  Are measurements from an Ocean Optics or International Light spectrometer 

#check the operating system and assign a logical flag (TRUE or FALSE)
sys_win <- Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  ltp <- gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp <- Sys.getenv('HOME')#Life was easier on Mac
}
# Useful functions --------------------------------------------------------

#Open file with default program on any OS
# https://stackoverflow.com/a/35044209/3745353
shell.exec.OS  <- function(x){
  # replacement for shell.exec (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
  {return(base::shell.exec(x))}else
  {comm <- paste0('open "',x,'"')
  return(system(comm))}
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


#For curve integral
require(sfsmisc)

#  .  Select files ---------------------------------------------------------

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
#  .  Derive variables	----------------------------------------------

#folders containing measurements
setnm <- list.dirs(spc, recursive = F)
names(setnm) <- basename(setnm)
#basic sorting, works for 20201024 data
setnm <- setnm[order(nchar(names(setnm)))]
#Ignore extra stuff
setnm <- setnm[!(names(setnm) %in% c('zExtra'))]
#file names (minus ".txt")
stm <- lapply(setnm, dir, pattern = '.txt')
# stm <- lapply(setnm, dir, pattern = '.txt|.ilt')#quickfix for International Light spectrometers
names(stm) <- names(setnm)
    # #replace undesirable characters in names
    # snm <-  sub(subst_st,  repl_st, basename(setnm))
snm <- basename(setnm)
#repalce minus with a space
snm <- lapply(strsplit(snm, '-'), paste, collapse = ' ')
names(snm) <- names(setnm)

# Separate dark measurements ----------------------------------------------

# . Find dark measurements ------------------------------------------------
#check for dark measurement in folder
darkFUN <- function(x){grepl('dark',x)}
#label dark measurements
dark_label <- lapply(stm, 
                    sapply,
                    darkFUN
)
#collect names of dark measurements
dark_stm <- lapply(names(stm), 
                   function(i)
                     { stm[[i]][ dark_label[[i]] ] }
                  )
#retain folder names
names(dark_stm) <- names(stm)

#remove dark from main set
stm <- lapply(names(stm), 
                   function(i){ stm[[i]][ !dark_label[[i]] ] }
                  )
#retain folder names
names(stm) <- names(dark_stm)

# Read in spectra ---------------------------------------------------------

# . construct paths to each file ------------------------------------------
file_path <- lapply(names(setnm), function(i){file.path(setnm[[i]], stm[[i]])})
dark_path <- lapply(names(setnm), function(i){file.path(setnm[[i]], dark_stm[[i]])})
#label them with the correct folder names
names(file_path) <- names(setnm)
names(dark_path) <- names(setnm)

# . Load each file --------------------------------------------------------
#Load each file starting at Ocean Optics' ">>>Begin" flag
oo_begin <- '>>>>Begin'
oo_endold <- '>>>>End'#older versions end with this
#Set up a file reading function for batch reading
#N.B. oo_begin and oo_end are loaded from global environment (.GlobalEnv) within lapply() 
OOread <- function(fl)
            {
              read.table(
                file = fl,#current file
                header = F,#exclude header
                sep = '\t',#tab separated
                skip = grep(oo_begin,readLines(fl)),#begins at '>>>Begin'
                nrows = ifelse( any(grepl(oo_endold,readLines(fl))), #if '>>>End' exists
                                grep(oo_endold,readLines(fl)) -
                                  grep(oo_begin,readLines(fl)) - 1, #read only lines between 'Begin' and 'End'
                                -1#Otherwise read all except header
                              )
              )
            }
#Load each file starting at International Lights' "SN	" flag
il_begin <- 'SN	'
#Set up a file reading function for batch reading
#N.B. il_begin is loaded from global environment (.GlobalEnv) within lapply() 
ILread <- function(fl)
            {
              read.table(
                file = fl,#current file
                header = F,#exclude header
                sep = '\t',#tab separated
                skip = grep(il_begin,readLines(fl)),#begins at '>>>Begin'
                nrows = -1 #read everything except the start flag
              )
            }
#light files
raw_files <- lapply(file_path, 
                    function(s)
                    {
                      lapply(s,
                             switch(specType,
                                    OO = OOread,
                                    IL = ILread
                                    )
                            )
                    }
)
#dark files
dark_files <- lapply(dark_path, 
                    function(s)
                    {
                      lapply(s,
                             switch(specType,
                                    OO = OOread,
                                    IL = ILread
                             )
                            )
                    }
)


# Crop to UV-Vis range ----------------------------------------------------
RangeFUN <- function(x,v = 'V2'){subset(x, V1 >299 & V1 <701)[,v]}

#Make a wavelength vector
    # wln <- RangeFUN(raw_files[[1]][[1]], 'V1')#get(paste0(snm[i],'.',nn))$V1
#These could come from different spectrometers (e.g. 20210215)
#Make a separate wavelength vector for each measurement set
wln <- lapply(raw_files,
              function(xx)
              {RangeFUN(xx[[1]], 'V1')}#assume first file in folder has same wavelength binning
              )
names(wln) <- names(raw_files)

#crop measurements and discard wavelength vectors
raw_files <- lapply(raw_files,
                    sapply,
                    RangeFUN
)
dark_files <- lapply(dark_files,
                    sapply,
                    RangeFUN
)


# . Label with original file names ----------------------------------------
raw_files <- lapply(1:length(raw_files),
                    function(i)
                    {
                      xx <- raw_files[[i]]
                      colnames(xx) <- sub('.txt','', stm[[i]])#remove '.txt'
                      return(xx)
                    }
                    )
dark_files <- lapply(1:length(dark_files),
                    function(i)
                    {
                      if(length(dark_files[[i]]))#could be empty
                      {
                      xx <- dark_files[[i]]
                      colnames(xx) <- sub('.txt','', dark_stm[[i]])#remove '.txt'
                      return(xx)
                      }
                    }
                    )
names(raw_files) <- names(setnm)
names(dark_files) <- names(setnm)


# . Get angle labels ------------------------------------------------------
#If number of labelled characters is unspecified, try to guess it
if(is.null(labchar)|is.na(labchar))
{#if the filename starts with '000-' or contains '-000' it is in 3 character format
  if(any(sapply(sapply(stm, grepl, pattern = '^000-|-000'), any)))
  {labchar <- 3}else
  {labchar <- 0}#otherwise there is no angle label, assume they were recorded in order
}


# Process spectra ---------------------------------------------------------

# . Set up polarization calculations --------------------------------------
#Angle of polarization
apolFUN <- function(x, nang)
  {if(nang == 4)
    {atan2(x[2]-x[4], x[1]-x[3])*180/pi}else
    {warning('sorry, angle of polarization not implimented for ',nang, ' angles')}
}
#Degree of polarization
dpolFUN <- function(x, nang)
  {if(nang == 4)
    {
    if(any(x<0))#lower than reference baseline, set to 0
    return(0)
    xx <- sqrt((x[2]-x[4])^2 + (x[1]-x[3])^2)/(sum(x)/2)
    xx <- ifelse(test = xx<1,
                 yes = ifelse(test = xx>0,
                              yes = xx,
                              no = 0),
                 no = 1) 
    return(xx)
    }else
    {warning('sorry, degree of polarization not implimented for ',nang, ' angles')}
  }
#Unpolarized intensity
i0FUN <-  function(x)
  {if(length(x)==4){sum(x)/2}else
   {warning('sorry, I0 not implimented for ',length(x), ' angles')}
 }
# . Convert to counts/second ------------------------------------------------


#Fetch integration time in seconds WIP!
raw_time <- lapply(file_path, 
                   function(s){
                     lapply(s, 
                            function(fl)
                            {
                              tb <- read.table(
                                file = fl,#current file
                                header = F,#exclude header
                                # sep = '\t',#tab separated 
                                skip = grep('Integration' ,readLines(fl))-1, 
                                nrows = 1
                              )
                              return( tb[,length(tb[1,])] )
                            }
                     )
                   }
)
names(raw_time) <- names(raw_files)
raw_time <- lapply(names(raw_time), 
                   function(ii)
                   {xx <- do.call(cbind,raw_time[[ii]])
                   colnames(xx) <- colnames(raw_files[[ii]])
                   return(xx)
                   }
)
names(raw_time) <- names(raw_files)

#Fetch integration time in seconds for dark files WIP!
dark_time <- lapply(dark_path, 
                    function(s){
                      lapply(s, 
                             function(fl)
                             {
                               tb <- read.table(
                                 file = fl,#current file
                                 header = F,#exclude header
                                 # sep = '\t',#tab separated 
                                 skip = grep('Integration' ,readLines(fl))-1, 
                                 nrows = 1
                               )
                               return( tb[,length(tb[1,])] )
                             }
                      )
                    }
)
names(dark_time) <- names(dark_files)
dark_time <- lapply(names(dark_time), 
                    function(ii)
                    {xx <- do.call(cbind,dark_time[[ii]])
                    colnames(xx) <- colnames(dark_files[[ii]])
                    return(xx)
                    }
)
names(dark_time) <- names(dark_files)

#Count/second conversion
raw_cps <- lapply(names(raw_files),
                  function(ii)
                  {sapply(colnames(raw_files[[ii]]),
                          function(jj)
                          { raw_files[[ii]][,jj]/raw_time[[ii]][,jj] }
                  )
                  }
)
dark_cps <- lapply(names(dark_files),
                   function(ii)
                   {sapply(colnames(dark_files[[ii]]),
                           function(jj)
                           { dark_files[[ii]][,jj]/dark_time[[ii]][,jj] }
                   )
                   }
)
names(raw_cps) <- names(raw_files)
names(dark_cps) <- names(dark_files)


# . Average across repeats ------------------------------------------------

# . . Dark repeats --------------------------------------------------------
#Take median across dark repeats
# dark_found <- !sapply(dark_files, is.null)
dark_found <- !sapply(dark_cps, is.null)
#prepare vector
dark_averages <- list()
if(any(dark_found))
{
  dark_averages[!dark_found] <- 0
  # dark_averages[dark_found] <- lapply(dark_files[dark_found],
  dark_averages[dark_found] <- lapply(dark_cps[dark_found],
                                     apply,
                                     1,
                                     AvFUN
                                    )
}else{
  # dark_averages <- sapply(dark_files, function(x){0})
  dark_averages <- sapply(dark_cps, function(x){0})
  warning('No dark files found, raw traces used.')
}
# names(dark_averages) <- names(dark_files)
names(dark_averages) <- names(dark_cps)
#Subtract dark
# light_files <- lapply(names(raw_files), 
light_files <- lapply(names(raw_cps), 
                      function(i)
                        {
                        # xx <- raw_files[[i]] - dark_averages[[i]]
                        xx <- raw_cps[[i]] - dark_averages[[i]]
                        colnames(xx) <- sub('.txt','', stm[[i]])
                        return(xx)
                        }
                      )
#retain folder name
# names(light_files) <- names(raw_files)
names(light_files) <- names(raw_cps)

# . . Light recordings ----------------------------------------------------
#set up labels
anglab <- sapply(1:mang, function(i){sprintf('%03g',(i-1)*180/mang)})

light_averages <- lapply(light_files, 
                        function(xx)#for each subfolder
                        {
                          if(labchar)#if polarizer angles are labelled
                          {
                            cn <- colnames(xx)#extract the column names
                            sapply(anglab,#loop through angle labels
                                  function(i){#average across columns with these labels
                                     apply(xx[,grepl(paste0('^',i,'-|-',i),cn)],
                                           1,
                                           AvFUN #should be no NAs, could still set na.rm=T
                                           )
                                  }
                            )
                          }else#if polarizer angles are not labelled
                          {
                            sapply(anglab,#loop through appropriate labels
                                   function(i){
                                     mang_modulus <- 1:(dim(xx)[2])%%mang #derive column labels from modulus
                                     ind <- ifelse(mang_modulus,mang_modulus,mang)#replace mod = 0 with the number of angles
                                     if(sum(ind %in% match(i, anglab))>1)
                                     {
                                       apply(xx[, ind %in% match(i,anglab)],#average across columns for each angle
                                             1,
                                             AvFUN
                                             )
                                     }else#if there is only one, no averaging is necessary
                                     {xx[, ind %in% match(i,anglab)]}
                                   }
                                  )
                          }
                        }
                      )


# . Stokes parameter calculations -----------------------------------------


#Calculate unpolarized intensity 
light_I0s <- lapply(light_averages,
                       apply,
                       1,
                      i0FUN
                    )
#Calculate angle of polarization
aops <- lapply(light_averages,
                     apply,
                     1,
               apolFUN,
               mang
)
#Calculate degree of polarization
dops <- lapply(light_averages,
               apply,
               1,
               dpolFUN,
               mang
)

# Plot spectra ------------------------------------------------------------
#Set up plot colours
clz <- sapply(
  length(light_files),
  colorRampPalette(
    c('red','blue','cyan4','magenta4','orange')#on Rstudio these are distinct
  )
)
clz <- sample(clz)#Randomise order?



# . Set up plot area ------------------------------------------------------
plot_file <- file.path(spc, 'PolarizationPlot.pdf')
if(file.exists(plot_file))
{
  message('A polarization plot already exists in this folder.')
  nnm <- readline(prompt = 'New plot name: '
          )
  plot_file <-  file.path(spc,paste0(ifelse(nchar(nnm),nnm,'PolarizationPlot'),'.pdf'))
}
pdf(file = plot_file,
    paper = 'a4',
    height = 10,
    bg = 'white',
    useDingbats = F
    )
par(mar = c(5,5,1,0.5),
    mfrow = c(3,1)
    )
# . Unpolarized intensity -------------------------------------------------
#Set up plot area
# par(mar = c(4,4,0,0.5))
plot(NULL,
     # xlim = range(wln),
     xlim = awl,
     ylim = range(unlist(light_I0s)),
     xlab = 'Wavelength (nm)',
     # ylab = 'Intensity (counts)')
     ylab = 'Intensity (counts/second)')
#Add limits
abline(h = 0, lwd = 0.25, col = 'gray')
#Plot each measurement in a different colour
lapply(names(light_I0s),
       function(i)
       {
         lines(wln[[i]],
                light_I0s[[i]],
                col = clz[which(names(light_I0s) %in% i)],
               lwd = 2
         )
         }
       )
#Label the lines
legend('topright',
       names(light_I0s),
       col = clz,
       lty = 1,
       lwd = 2,
       cex = 1
       )

# . Angle of polarization -------------------------------------------------
#Set up plot area
# par(mar = c(4,4,0,0.5))
plot(NULL,
     # xlim = range(wln),
     xlim = awl,
     ylim = c(-180,180),
     xlab = 'Wavelength (nm)',
     ylab = 'Angle of Polarization (°)', 
     axes = F)
axis(1)
axis(2,
     at = seq(-180, 180, 90),
     labels = paste0(seq(-180, 180, 90),'°'),
     cex.axis = 0.7
     )
#Add limits
abline(h = seq(-180, 180, 90), lwd = 0.25, col = 'gray')
#Plot each measurement as dots of different colours
lapply(names(aops),
       function(i)
       {
         points(wln[[i]],
               aops[[i]],
               col = clz[which(names(light_I0s) %in% i)],
               cex = 0.5,
               pch = 19
         )
       }
)
#label each measurement
legend('topright',
       names(aops),
       col = clz,
       pch = 19,
       cex = 1
)

# . Degree of polarization ------------------------------------------------
# par(mar = c(4,4,0,0.5))
plot(NULL,
     # xlim = range(wln),
     xlim = awl,
     ylim = c(0,1),
     xlab = 'Wavelength (nm)',
     ylab = 'Degree of Linear Polarization'
     )
#Plot measurements as different coloured lines
lapply(names(dops),
       function(i)
       {
         lines(wln[[i]],
               dops[[i]],
               col = clz[which(names(dops) %in% i)],
               lwd = 2
         )
       }
)
#Add limits
abline(h = c(0,1))
#Label each measurement
legend('topright',
       names(dops),
       col = clz,
       lty = 1,
       lwd = 2,
       cex = 1
)

# . Save plot -------------------------------------------------------------
dev.off()
shell.exec.OS(plot_file)





#  Correct to effective sensitivity ---------------------------------------

#make a honeybee UV response curve (Menzel & Blakers, 1976)
spln.lmx <- StavengaSpline(c(300, 700), lmx) # an a1 opsin with a 344nm max absorption
wln.lmx <- lapply(wln,
                  function(nm)
                  {predict(spln.lmx, x = nm)$y}
                  )

# . Conversion to relative photon counts ----------------------------------
effective_averages <- lapply(names(light_averages),
                             function(ii)
                               { light_averages[[ii]]*wln.lmx[[ii]] }
                             )
names(effective_averages) <- names(light_averages)

sm.effective_averages <- t(
                            sapply(names(effective_averages),
                             function(ii)
                             {
                               apply(effective_averages[[ii]],
                                     2,
                                     function(xx)
                                       {
                                       xx[xx < 0] = 0#negative values subtract from integral
                                       sfsmisc::integrate.xy(
                                                              x = wln[[ii]],
                                                              fx = xx,
                                                              a = min(awl),
                                                              b = max(awl)
                                                             )
                                       }
                                     )
                             }
                            )
                          )


# . Process effective counts ----------------------------------------------

#Calculate unpolarized intensity 
effective_I0s <- apply(sm.effective_averages,
                        1,
                        i0FUN
                      )

#Calculate angle of polarization
effective_aops <- apply(sm.effective_averages,
                         1,
                         apolFUN,
                         mang
                        )

#Calculate degree of polarization
effective_dops <- apply(sm.effective_averages,
                         1,
                         dpolFUN,
                         mang
                        )

# Plot effective polarization ---------------------------------------------
# . Set up plot area ------------------------------------------------------
eff_plot_file <- file.path(spc, 'EffPolarizationPlot.pdf')
if(file.exists(eff_plot_file))
{
  message('An effective polarization plot already exists in this folder.')
  nnm <- readline(prompt = 'New plot name: '
  )
  eff_plot_file <-  file.path(spc,paste0(ifelse(nchar(nnm),nnm,'EffPolarizationPlot'),'.pdf'))
}
pdf(file = eff_plot_file,
    paper = 'a4',
    height = 10,
    bg = 'white',
    useDingbats = F
)
par(mar = c(5,5,1,0.5),
    mfrow = c(3,1)
)

#  . Unpolarized intensity ------------------------------------------------
spa = 0.2
wdt = 1.0
plot(NULL,
     xlim = c(1,length(effective_I0s))+c(-1,3)*1.5,
     ylim = c(0, log10(max(effective_I0s))),
     xlab = '',
     ylab = expression('log'[10]~'counts s'^{'-1'}),
     axes = F
     )
abline(h = seq(0, log10(max(effective_I0s)), 1), lwd = 0.25, col = 'gray')
barplot(log10(effective_I0s),
        space = spa,
        width = wdt,
        col = clz,
        add = T,
        las = 3
        )

# . Angle of Polarization -------------------------------------------------

plot(NULL,
     # xlim = pr1[c(1,2)],
     xlim = c(1,length(effective_aops))+c(-1,3)*1.5,
     ylim = c(-180, 180),
     xlab = '',
     ylab = 'Angle of Polarization',
     axes = F
     )
axis(1,
     at = seq(from = spa+1-wdt/2,
              to = length(effective_dops)*(1+spa)-wdt/2,
              length.out = length(effective_I0s) ),
     labels = names(effective_I0s),
     cex.axis = 0.7
)
axis(2,
     at = seq(-180, 180, 90),
     labels = paste0(seq(-180, 180, 90),'°'),
     cex.axis = 0.7
)
#Add limits
abline(h = seq(-180, 180, 90), lwd = 0.25, col = 'gray')
points(x = seq(from = spa+1-wdt/2,
               to = length(effective_dops)*(1+spa)-wdt/2,
               length.out = length(effective_aops) ),
       y  = effective_aops,
       pch = 3,
       lwd = 3,
       col = clz
       )

#  . Degree of polarization -----------------------------------------------
plot(NULL,
     xlim = c(1,length(effective_dops))+c(-1,3)*1.5,
     ylim = c(0, 1),
     xlab = '',
     ylab = 'Degree of Linear Polarization',
     axes = F
)
#Add limits
abline(h = seq(0, 1, 0.10), lwd = 0.25, col = 'gray')
barplot(effective_dops,
        col = clz,
        add = T,
        las = 3
        )
text(x = seq(from = spa+1-wdt/2,
             to = length(effective_dops)*(1+spa)-wdt/2,
             length.out = length(effective_dops) ),
     y = effective_dops+0.03,
     labels = round(effective_dops,3),
     col = adjustcolor(col = clz, 
                       red.f = 0.7,
                       green.f = 0.7,
                       blue.f = 0.7
                       )
     )
#Add limits
abline(h = c(0,1))
#Label each measurement
legend('topright',
       names(dops),
       col = clz,
       lty = 1,
       lwd = 2,
       cex = 1
)

# . Save plot -------------------------------------------------------------
dev.off()
shell.exec.OS(eff_plot_file)
# Save everything ---------------------------------------------------------
# fnm <- file.path(spc, mnm)
# 
# summed <- data.frame(total.flux = sm.photon_medians,
#                      rel.flux = sm.rel.photon_medians
# )
# Calculated at each wavelength
#only save wavelength data together if all measurements use the same scale
if(!diff(range(sapply(wln, length))))
{
  write.csv(x = cbind(wavelength = wln[[1]], do.call(cbind, light_I0s)),
            file = file.path(spc, paste0('-I0','.csv')),
  )
  write.csv(x = cbind(wavelength = wln[[1]], do.call(cbind, aops)),
            file = file.path(spc, paste0('-aop','.csv')),
  )
  write.csv(x = cbind(wavelength = wln[[1]], do.call(cbind, dops)),
            file = file.path(spc, paste0('-dop','.csv')),
  )
  
}else{}#TODO handle case when wavelength scales differ

# Average across range
write.csv(x = data.frame(counts.per.sec = effective_I0s),
          file = file.path(spc, paste0('-summed.countspersec_',awl[1],'-', awl[2],'nm','.csv')),
)
write.csv(x = data.frame(angle.of.polarization = effective_aops),
          file = file.path(spc, paste0('-aop_',awl[1],'-', awl[2],'nm','.csv')),
)
write.csv(x = data.frame(degree.of.polarization = effective_dops),
          file = file.path(spc, paste0('-dop_',awl[1],'-', awl[2],'nm','.csv')),
)

# WIP! I GOT THIS FAR -----------------------------------------------------