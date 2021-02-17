rm(list = ls())
graphics.off()
#R versions <4.0.0 convert strings to factors, specify default behaviour
formals(data.frame)$stringsAsFactors <- FALSE
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2020 11 26
#     MODIFIED:	James Foster              DATE: 2021 02 17
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
#	   CHANGES: 
#             
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
#- Save plots    
#- Summary calculations   

# Input Variables ----------------------------------------------------------

#  .  User input -----------------------------------------------------------

num <- 1#5	#number of repeats for each measurement
mang <- 4#  number of measurement angles

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
#wavelength range
awl <- c(320,400)

#folders containing measurements
setnm <- list.dirs(spc, recursive = F)
names(setnm) <- basename(setnm)
#basic sorting, works for 20201024 data
setnm <- setnm[order(nchar(names(setnm)))]
#Ignore extra stuff
setnm <- setnm[-which(names(setnm) %in% c('zExtra'))]
#file names (minus ".txt")
stm <- lapply(setnm, dir, pattern = '.txt')
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
#light files
raw_files <- lapply(file_path, 
                    function(s)
                    {
                      lapply(s,
                             function(fl)
                             {
                               read.table(
                                 file = fl, header = F, sep = '\t',
                                 skip = grep(readLines(fl),pattern = oo_begin)#,
                               )
                             }
                      )
                    }
)
#dark files
dark_files <- lapply(dark_path, 
                    function(s)
                    {
                      lapply(s,
                             function(fl)
                             {
                               read.table(
                                 file = fl, header = F, sep = '\t',
                                 skip = grep(readLines(fl),pattern = oo_begin)#,
                               )
                             }
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
              {RangeFUN(xx[[1]], 'V1')}
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

# Process spectra ---------------------------------------------------------
#Polarization calculations
#Angle of polarization
apolFUN <- function(x, nang)
  {if(nang == 4)
    {atan2(x[2]-x[4], x[1]-x[3])*180/pi}else
    {warning('sorry, angle of polarization not implimented for ',nang, ' angles')}
}
#Degree of polarization
dpolFUN <- function(x, nang)
  {if(nang == 4)
    {sqrt((x[2]-x[4])^2 + (x[1]-x[3])^2)/(sum(x)/2)}else
    {warning('sorry, degree of polarization not implimented for ',nang, ' angles')}
  }
#Unpolarized intensity
i0FUN <-  function(x)
  {if(length(x)==4){sum(x)/2}else
   {warning('sorry, I0 not implimented for ',length(x), ' angles')}
 }
#Take median across dark repeats
dark_medians <- sapply(dark_files,
                     apply,
                     1,
                     median
)
#Subtract dark
light_files <- lapply(names(raw_files), 
                      function(i){raw_files[[i]] - dark_medians[[i]]}
                      )
#retain folder name
names(light_files) <- names(raw_files)
#Calculate unpolarized intensity 
light_I0s <- sapply(light_files,
                       apply,
                       1,
                      i0FUN
                    )
#Calculate angle of polarization
aops <- sapply(light_files,
                     apply,
                     1,
               apolFUN,
               mang
)
#Calculate degree of polarization
dops <- sapply(light_files,
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


# . Unpolarized intensity -------------------------------------------------
#Set up plot area
par(mar = c(4,4,0,0.5))
plot(NULL,
     # xlim = range(wln),
     xlim = awl + c(0,100),
     ylim = range(unlist(light_I0s)),
     xlab = 'Wavelength (nm)',
     ylab = 'Intensity (counts)')
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
       cex = 0.5
       )

# . Angle of polarization -------------------------------------------------
#Set up plot area
par(mar = c(4,4,0,0.5))
plot(NULL,
     # xlim = range(wln),
     xlim = awl+c(-20,100),
     ylim = c(-180,180),
     xlab = 'Wavelength (nm)',
     ylab = 'Angle of Polarization (°)', 
     axes = F)
axis(1, pretty(range(wln)))
axis(2,
     at = seq(-180, 180, 90),
     labels = paste0(seq(-180, 180, 90),'°'),
     cex.axis = 0.7
     )
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
#Add limits
abline(h = c(-1,0,1)*180)
#label each measurement
legend('topright',
       names(aops),
       col = clz,
       pch = 19,
       cex = 0.5
)

# . Degree of polarization ------------------------------------------------
par(mar = c(4,4,0,0.5))
plot(NULL,
     # xlim = range(wln),
     xlim = awl,
     # ylim = c(0,1),
     ylim = c(0,0.2),
     xlab = 'Wavelength (nm)',
     # ylab = 'Degree of Linear Polarization')
     ylab = 'False DoP' # 20210215 measurements were *false* DoP from spectrometer PS
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
       cex = 0.5
)


# WIP! I GOT THIS FAR -----------------------------------------------------

