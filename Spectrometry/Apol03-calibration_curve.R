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

# Predict Intensity -------------------------------------------------------

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


# Choose conditions -------------------------------------------------------
Pred.stim <- function(
                      p = 0,#controller box setting pol layer
                      u = 0,#controller box setting unpol layer
                      c.p = -4.975e12,#intercept for polarized layer
                      m.p =  9.406e12,#slope for polarized layer
                      c.u = -2.375e13,#intercept for unpolarized layer
                      m.u =  4.109e13,#slope for unpolarized layer
                      dop.p = 0.5377186,#DoP of polarized layer
                      dop.u = 0.04282066,#DoP of unpolarized layer
                      int_pol_ctn = 1.0,#polarized-added intensity correction
                      int_upol_ctn = 0.5,#unpolarized-added intensity correction
                      dop_pol_ctn = 1.0,#polarized-added DoP correction
                      dop_upol_ctn = 1.5#unpolarized-added DoP correction
)
{
  #check input
  numcheck <- function(x){suppressWarnings(!is.na(as.numeric(x)))}
  u <- ifelse(test = sapply(u, numcheck), yes = u, no = 0)#check for numeric values
  p <- ifelse(test = sapply(p, numcheck), yes = p, no = 0)#check for numeric values
  #linear intensity prediction
  u_int <- c.u+m.u*u
  p_int <- c.p+m.p*p
  u_int[u_int<0 | is.na(u_int)] <- 0#predict a non-negative photon flux
  p_int[p_int<0 | is.na(p_int)] <- 0#predict a non-negative photon flux
  #check for mixed input stimuli
  int.ct_pol <- rep(1, length(u))
  int.ct_pol[u>0 & !is.na(u)] <- int_upol_ctn#if there is unpol, it appears to be more like 50%
  int.ct_upol <- rep(1, length(p))
  int.ct_upol[p>0 & !is.na(p)] <- int_pol_ctn#if there is pol
  dop.ct_pol <- rep(1,length(u))
  dop.ct_pol[u>0 & !is.na(u)] <- dop_upol_ctn#if there is unpol, it appears to be more polarized!
  dop.ct_upol <- rep(1,length(p))
  dop.ct_upol[p>0 & !is.na(p)] <- dop_pol_ctn#if there is pol
  #estimate intensity
  est_intensity <- u_int*int.ct_upol + p_int*int.ct_pol
  #estimate degree of polarization
  est_dop <- (dop.ct_upol*dop.u * u_int*int.ct_upol + #DoP of unpol layer multiplied by its intensity
            dop.ct_pol*dop.p * p_int*int.ct_pol)/ #DoP of pol layer multiplied by its intensity
          (dop.ct_upol * u_int*int.ct_upol+ #divided by intensity of unpolarized layer plus
             dop.ct_pol * p_int*int.ct_pol) #divided by intensity of polarized layer
  return(cbind(p = p, u = u, int = est_intensity, dop = est_dop))
}

levs <- seq(from = 0,
            to = 7,
            by = 0.25
            )
conds <- expand.grid(u = levs, p = levs)
cond.pred <- as.data.frame(with(conds, Pred.stim(u=u,p=p)))
# cond.pred <- as.data.frame(cbind(conds, preds))

# Plot predictions --------------------------------------------------------

i_bins <- seq(from = 12, to = 14, by  = 0.5)
d_bins <- seq(from = 0, to  = 1, by = 0.1)

# . Set up plot area ------------------------------------------------------
plot_file <- file.path(ltp, 'Apol03-intensity.pdf')
if(file.exists(plot_file))
{
  message('A plot called "', basename(plot_file), '" already exists in this folder.')
  nnm <- readline(prompt = 'New plot name: '
  )
  plot_file <-  file.path(ltp,paste0(ifelse(nchar(nnm),nnm,'Apol03-intensity'),'.pdf'))
}
pdf(file = plot_file,
    paper = 'a4',
    height = 10,
    bg = 'white',
    useDingbats = F
)
par(mar = c(5,5,1,0.5),
    mfrow = c(length(i_bins)-1,1)
)


#  . Loop through intensity bins ------------------------------------------

for(i in (length(i_bins) - 1):1)
{
with(subset(cond.pred, u >0 & p >0),
     {
       plot(x = dop,
            y = log10(int),
            pch = 20,
            col = rgb(0,0,0,0.5),
            xlim = c(0,0.6),
            ylim = i_bins[i+c(0,1)]
            )
       abline(v = d_bins, lwd = 0.25)
       text(x = dop,
            y = log10(int),
            labels = paste0('p',p,',','u',u),
            adj = c(0,1),
            cex = 0.5
            )
     }
     )
with(subset(cond.pred, u == 0 | p == 0),
     {
       points(x = dop,
            y = log10(int),
            pch = 3,
            col = rgb(u>0,0,p>0,0.5)
            )
       text(x = dop,
            y = log10(int),
            labels = paste0('p',p,',','u',u),
            adj = c(1,0),
            cex = 0.5,
            col = rgb(u>0,0,p>0,1)
            )
     }
     )
}
# . Save plot -------------------------------------------------------------
dev.off()
shell.exec.OS(plot_file)


# Plot chosen conditions --------------------------------------------------


# . 20210901 --------------------------------------------------------------

#aimed at 1-3*10613
cond20210901 <- as.data.frame(
                  Pred.stim(p = c(0,1,1.5,2.5),
                            u = c(1,1,1,0)
                            )
                  )
with(subset(cond20210901, u >0 & p >0),
     {
       plot(x = dop,
            y = log10(int),
            pch = 20,
            col = rgb(0,0,0,0.5),
            xlim = c(0,0.6),
            ylim = c(13, 13.5),
            xlab = 'Degree of Polarization',
            ylab = '10^13 photons/cm2/s',
            main = 'Conditions 01.09.2021',
            axes = F
            
       )
       axis(1)
       axis(2, 
            at = log10(c(1,2,3,5)*10^13), 
            labels = c(1,2,3,5),
            cex = 0.5
            )
       abline(v = d_bins, lwd = 0.25)
       text(x = dop,
            y = log10(int),
            labels = paste0('p',p,',','u',u),
            adj = c(0,1),
            cex = 0.5
       )
     }
)
with(subset(cond20210901, u == 0 | p == 0),
     {
       points(x = dop,
              y = log10(int),
              pch = 3,
              col = rgb(u>0,0,p>0,0.5),
              cex = 0.5
       )
       text(x = dop,
            y = log10(int),
            labels = paste0('p',p,',','u',u),
            adj = c(1,0),
            cex = 0.5,
            col = rgb(u>0,0,p>0,1)
       )
     }
)


# . 20210902 --------------------------------------------------------------

#aimed at 1-3*10613
cond20210902 <- as.data.frame(
                  Pred.stim(p = c(0,1,1.5,5.5,5.5),
                            u = c(1.5,1.5,1.5,1,0)
                            )
                  )
with(subset(cond20210902, u >0 & p >0),
     {
       plot(x = dop,
            y = log10(int),
            pch = 20,
            col = rgb(0,0,0,0.5),
            xlim = c(0,0.6),
            ylim = c(13.5, 14),
            xlab = 'Degree of Polarization',
            ylab = '10^13 photons/cm2/s',
            main = 'Conditions 02.09.2021',
            axes = F
       )
       axis(1)
       axis(2, 
            at = log10(c(3,4,5,7,10)*10^13), 
            labels = c(3,4,5,7,10),
            cex.axis = 0.7
       )
       abline(v = d_bins, lwd = 0.25)
       text(x = dop,
            y = log10(int),
            labels = paste0('p',p,',','u',u),
            adj = c(0,1),
            cex = 0.5
       )
     }
)
with(subset(cond20210902, u == 0 | p == 0),
     {
       points(x = dop,
              y = log10(int),
              pch = 3,
              col = rgb(u>0,0,p>0,0.5),
              cex = 0.5
       )
       text(x = dop,
            y = log10(int),
            labels = paste0('p',p,',','u',u),
            adj = c(1,0),
            cex = 0.5,
            col = rgb(u>0,0,p>0,1)
       )
     }
)

