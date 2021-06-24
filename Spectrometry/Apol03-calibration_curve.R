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
names(int)[1] <- c('label')#, 'intensity')


# Find single light conditions --------------------------------------------

dop_uonly <- dop[ which(! dop$p > 0), ]
dop_ponly <- dop[ which(! dop$u > 0), ]
int_uonly <- int[ which(! int$p > 0), ]
int_ponly <- int[ which(! int$u > 0), ]

with(dop_uonly,
  plot(x = u,
       y = dop,
       ylim = c(0,1),
       xlim = c(0,10)
       )
)
with(dop_ponly,
     plot(x = p,
          y = dop,
          ylim = c(0,1),
          xlim = c(0,10)
     )
)

with(int_uonly,
  plot(x = u,
       y = total.flux,
       ylim =range(int$total.flux),
       xlim = c(0,10),
       pch = 21,
       lwd = 1.5,
       col = 'blue',
       bg = 'gray'
       )
)
with(int_ponly,
     points(x = p,
          y = total.flux,
          pch = 21,
          lwd = 1.5,
          col = 'purple',
          bg = 'gray'
     )
)

lmu <- lm(formula = total.flux~as.numeric(u),
          data = int_uonly[int_uonly$u > 0, ]
          )
lmp <- lm(formula = total.flux~as.numeric(p),
          data = int_ponly[int_ponly$p > 0, ]
          )
abline(lmu, col = 'cyan')
abline(lmp, col = 'magenta')

prd.u_int <- predict(lmu, int)
prd.p_int <- predict(lmp, int)
prd.u_int[is.na(prd.u_int)] <- 0
prd.p_int[is.na(prd.p_int)] <- 0
#correction for reduction in polarized component?
int_upol_correction <- 0.5
int.correct_pol <- ifelse(!(int$u > 0),#if there is no unpol
                      1,#pol accounts for 100%
                      int_upol_correction#if there is unpol, it appears to be more like 50%
                      )
prd.intensity <- prd.u_int + int.correct_pol  *prd.p_int
colcode <- ifelse(!int$u > 0,
                  'purple',
                  ifelse(!int$p >0,
                         'blue',
                         'gray'
                         )
                    )
int$col <- colcode
plot(x = prd.intensity, 
     y = int$total.flux, 
     xlim =(range(int$total.flux)),
     ylim =(range(int$total.flux)),
     xlab = 'Predicted intensity',
     ylab = 'Measured intensity',
     pch = 21,
     lwd = 1.5,
     col = int$col,
     bg = 'gray90'
     )
lmint <- lm(int$total.flux~prd.intensity)
abline(a = 0, b = 1)
abline(lmint, col = 'red')
text(x = prd.intensity, 
     y = int$total.flux, 
     labels = int$label,
     pos = 4,
     cex = 0.5
)

plot(x = log10(prd.intensity), 
     y = log10(int$total.flux), 
     xlim =log10(range(int$total.flux)),
     ylim =log10(range(int$total.flux)),
     xlab = 'Predicted intensity',
     ylab = 'Measured intensity',
     pch = 21,
     lwd = 1.5,
     col = int$col,
     bg = 'gray80'
     )
abline(a = 0, b = 1)
text(x = log10(prd.intensity), 
     y = log10(int$total.flux), 
     labels = int$label,
     pos = 3,
     cex = 0.5
)



# Predict DoLP ------------------------------------------------------------

prd.u_dop <- mean(dop_uonly$dop)
prd.p_dop <- mean(dop_ponly$dop)

prd.u_pol_int <- predict(lmu, dop)
prd.p_pol_int <- predict(lmp, dop)
prd.u_pol_int[is.na(prd.u_pol_int)] <- 0
prd.p_pol_int[is.na(prd.p_pol_int)] <- 0
#correction for reduction in polarized component?
dop_upol_correction <- 1.50
dop.correct_pol <- ifelse(!(dop$u > 0),#if there is no unpol
                      1,#pol accounts for 100%
                      dop_upol_correction#if there is unpol, it appears to be more polarized!
)
# Works poorly

prd.dop <- (prd.u_dop*prd.u_pol_int + dop.correct_pol*prd.p_dop*prd.p_pol_int)/
            (prd.u_pol_int+dop.correct_pol*prd.p_pol_int)
dop$col <- ifelse(!dop$u > 0,
                  'purple',
                  ifelse(!dop$p >0,
                         'blue',
                         'gray'
                  )
)
plot(x = prd.dop,
     y = dop$dop,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = 'Predicted DoLP',
     ylab = 'Measured DoLP',
     pch = 21,
     lwd = 1.5,
     col = dop$col,
     bg = 'gray80'
)
abline(h = c(0,1), v = c(0,1))
abline(a = 0, b = 1)
text(x = prd.dop, 
     y = dop$dop, 
     labels = with(dop, paste0('p',p,',','u',u)),
     pos = 3,
     cex = 0.5
)
