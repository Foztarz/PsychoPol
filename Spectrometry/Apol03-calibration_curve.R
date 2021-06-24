graphics.off()
#R versions <4.0.0 convert strings to factors, specify default behaviour
formals(data.frame)$stringsAsFactors <- FALSE
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 06 24
#     MODIFIED:	James Foster              DATE: 2021 06 24
#
#  DESCRIPTION: Adapted from "Irradiance APol0321210623.R"
#               Loads degrees of polarization and photon irradiances calculated
#               by 'Spec_Polarization.R' and (for example)
#               'Irradiance APol03-20210623.R'.
#               
#      OUTPUTS: Figures (PDF), look up table (csv).
#
#	     CHANGES: - Works on JMU Würzburg computers 
#             
#             
#
#   REFERENCES: Foster, J.J., Kirwan, J.D., el Jundi, B., Smolka, J., Khaldy, L.,
#               Baird, E., Byrne, M.J., Nilsson, D.-E., Johnsen, S., Dacke, M.
#               (2019). Orienting to polarized light at night – 
#               matching lunar skylight to performance in a nocturnal beetle.
#               The Journal of Experimental Biology 222, jeb188532
#               https://doi.org/10.1242/jeb.188532
#
#    EXAMPLES: 
#
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data 
#- Extract controller settings
#- Predict intensities
#- Predict DoP  
#- Export graphs  
#- Export calibration  

# Useful packages ---------------------------------------------------------
require(stringr)

# Input Variables ----------------------------------------------------------

#  .  User input -----------------------------------------------------------


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
  message('\n\nPlease select the degree of polarization .csv\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  dop_path  <- choose.files(
    default = file.path(ltp,'Documents'),#For some reason this is not possible in the "root" user
    caption = 'Please select the degree of polarization .csv'
  )
  message('\n\nPlease select the intensity .csv\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  int_path  <- choose.files(
    default = file.path(ltp,'Documents'),#For some reason this is not possible in the "root" user
    caption = 'Please select the intensity .csv'
  )
}else{
  message('\n\nPlease select the degree of polarization .csv\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  dop_path <- file.choose(new=F)
  message('\n\nPlease select the intensity .csv\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  int_path <- file.choose(new=F)
}
print(dop_path)
print(int_path)

# Read in data ------------------------------------------------------------
#Degree of polarization
dop <- read.csv(file = dop_path,
                header = T
                )
#Intensity
int <- read.csv(file = int_path,
                header = T
                )


# Extract controller settings ---------------------------------------------

Extract.sett <- function(x)
{lab = stringr::str_extract(string=x, pattern = '^p*...')
  pu = unlist( strsplit(x = lab, split = 'u')   )
  pp = unlist( strsplit(x = pu[1], split = 'p') )
  return(c(p = pp[2], u = pu[2]))
}

dop <- cbind(dop, t( apply(dop[1],1, Extract.sett) ) )
names(dop)[1:2] <- c('label', 'dop')
int <- cbind(int, t( apply(int[1],1, Extract.sett) ) )
names(int)[1:2] <- c('label')#, 'intensity')

which(dop$p > 0)
