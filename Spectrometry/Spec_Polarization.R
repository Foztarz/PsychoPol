rm(list = ls())
graphics.off()
#R versions <4.0.0 convert strings to factors, specify default behaviour
formals(data.frame)$stringsAsFactors <- FALSE
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2020 11 26
<<<<<<< HEAD
#     MODIFIED:	James Foster              DATE: 2021 03 22
=======
#     MODIFIED:	James Foster              DATE: 2021 03 18
>>>>>>> 74c6ebb7a6d921ee30bc6dcd6eb8ac705d97a0bf
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
<<<<<<< HEAD
#               - International Light compatibility
=======
#               - Attempted SpectrILight compatibility (file format unclear)
>>>>>>> 74c6ebb7a6d921ee30bc6dcd6eb8ac705d97a0bf
#               
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
#- Summary calculations   
#- Generalise calculations   

# Input Variables ----------------------------------------------------------

#  .  User input -----------------------------------------------------------

mang <- 4#  number of measurement angles
labchar <- 3# 0# number of characters used in angle label: 0 if not used
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
    xx <- sqrt((x[2]-x[4])^2 + (x[1]-x[3])^2)/(sum(x)/2)
    xx <- ifelse(xx<1,ifelse(xx>0,xx,0),1) 
    }else
    {warning('sorry, degree of polarization not implimented for ',nang, ' angles')}
  }
#Unpolarized intensity
i0FUN <-  function(x)
  {if(length(x)==4){sum(x)/2}else
   {warning('sorry, I0 not implimented for ',length(x), ' angles')}
 }

# . Average across repeats ------------------------------------------------

# . . Dark repeats --------------------------------------------------------
#Take median across dark repeats
dark_found <- !sapply(dark_files, is.null)
#prepare vector
dark_averages <- list()
if(any(dark_found))
{
  dark_averages[!dark_found] <- 0
  dark_averages[dark_found] <- lapply(dark_files[dark_found],
                                     apply,
                                     1,
                                     AvFUN
                                    )
}else{
  dark_averages <- sapply(dark_files, function(x){0})
  warning('No dark files found, raw traces used.')
}
names(dark_averages) <- names(dark_files)
#Subtract dark
light_files <- lapply(names(raw_files), 
                      function(i)
                        {
                        xx <- raw_files[[i]] - dark_averages[[i]]
                        colnames(xx) <- sub('.txt','', stm[[i]])
                        return(xx)
                        }
                      )
#retain folder name
names(light_files) <- names(raw_files)


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
    c('red','blue','cyan4','magenta4')#on Rstudio these are distinct
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
     ylab = 'Intensity (counts)')
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




# WIP! I GOT THIS FAR -----------------------------------------------------