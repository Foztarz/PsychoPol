# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2026 03 01
#     MODIFIED:	James Foster              DATE: 2026 03 02
#
#  DESCRIPTION: A plotter for example dances.
#               Loads a text file and plots dance angles for each stimulus phase.
#               
#       INPUTS: A ".csv" table with columns for experiment phase ("stimulus") and
#               angle ("angle").
#               
#      OUTPUTS: Results table (.csv).
#
#	   CHANGES: - 
#
#   REFERENCES: Batschelet E (1981).
#               Graphical presentation, Chap 1.2, p. 4-6
#               Chapter 1: Measures of Location
#               In: Circular Statistics in Biology
#               Academic Press (London)
#
#    EXAMPLES:  Fill out user input (lines 80-87), then press ctrl+shift+s to run
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data   +
#- Plot angles    +
#- Neat plot  +
#- Save results + 
#- Perspective correction +
#- Bimodal mean vector +
#- Separate weird dances


# Find relevant functions -----------------------------------------------
fun_file = "DanceAnalysis_Functions.R" #Name of that functions file
fun_path = tryCatch(expr = #Search in the folder containing this script
                      {file.path(dirname(sys.frame(1)$ofile), fun_file)},
                    error = function(e){fun_file}
)
if(!file.exists(fun_path))#If not found, ask the user
{
  msg = paste0('Please select "',fun_file,'"')
  # ask user to find data
  if( Sys.info()[['sysname']] == 'Windows' ){#choose.files is only available on Windows
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    fun_path = choose.files(
      default = file.path(gsub(pattern = '\\\\',
                               replacement = '/', 
                               x = Sys.getenv('USERPROFILE')),#user
                          'Documents'),#For some reason this is not possible in the "root" user
      caption = msg
    )
  }else
  { #on OS where "choose.files" is not available
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    fun_path = file.choose(new=FALSE)
  }
}
#read in relevant functions
source(file = fun_path, 
       encoding = 'UTF-8')




# Input Variables ----------------------------------------------------------

#  .  User input -----------------------------------------------------------
csv_sep = ','#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
group_factor = "stimulus" #The title of the column; NO SPACES PLEASE
# angle_name = "angle" #The title of the column with angles; NO SPACES PLEASE
angle_unit = "degrees" # "degrees" or "radians"
angle_rot = 'clock' # counter' # 'counter' for anticlockwise (imageJ) 'clock' for clockwise
angle_zero = pi/2 # 0 # angle start point: _0_ for right along x axis (imageJ) _pi/2_ for up along y axis (e.g. geographic North)
point_col = 'darkblue' #colour for plot points
speedup_data.table = TRUE #data.table handles Excel's CSV export issues better 

# . Load packages ----------------------------------------------------------
#needs installing before first use (in Rstudio, see automatic message)
suppressMessages(#these are disturbing users unnecessarily
  {
    require(circular)#package for handling cirular data
    require(CircMLE)#package for circular hypothesis tests
    require(parallel)#package for parallel processing
  }
)

#Check the operating system and assign a logical flag (T or F)
sys_win = Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp = Sys.getenv('HOME')#Life was easier on Mac
}


# . Select files ---------------------------------------------------------


# . . Select measured dance angles ---------------------------------------

# set path to files
path_file = DA_select_file()#expects a ".csv" file


# . . Select measured distortion field -----------------------------------
tilt_list = list.files(path = dirname(path_file),
                       pattern = 'Tilt.*csv$'
)
if(length(tilt_list) == 1)
{ #if there is exactly one tilt file in the same directory, use that one
  tilt_file = file.path(dirname(path_file),
                        tilt_list[1]
  )
}else
{ #otherwise ask the user to find one
  message('\n\nPlease select the distortion ".csv" file\n\n')
  suppressMessages(
    {tilt_file = DA_select_file()}#expects a ".csv" file
  )
}
#show the user the path they have selected
if(!length(tilt_file))
{stop('No distortion file selected.')}else
{print(tilt_file)}

# Read in data ------------------------------------------------------------
if(speedup_data.table)
{ #N.B. Excel creates CSV files that can be difficult to read, fread works best
  adata = data.table::fread(file = path_file,#read from user-selected file
                            header = T,#read the file header to use for variable names
                            sep = csv_sep#,#values are separated by the user-specified character
                            #other parameters can be added here for troubleshooting
  )
}else
{ #TODO 20220215 find out why this is not working
  adata = read.table(file = path_file,#read from user-selected file
                     header = T,#read the file header to use for variable names
                     sep = csv_sep#,#values are separated by the user-specified character
                     #other parameters can be added here for troubleshooting
  )
}
#Distortion data
ddata = read.table(file = tilt_file,#read from user-selected file
                   header = T,#read the file header to use for variable names
                   sep = csv_sep#,#values are separated by the user-specified character
                   #other parameters can be added here for troubleshooting
)
ddata = within(ddata,
               {
                 raw_angle = 90-Angle
                 ground_truth = seq(from = 0,
                                    by = 45,
                                    length.out = length(Angle))
               }
)
#Excel makes empty rows, trim them
adata = subset(x = adata, 
               subset = !(is.na(Bearing)) # angle is an empty number, i.e. no data
)
#Convert date to character
adata = within(adata,
               {
                 date = as.character(date_time)
               }
)

# View(adata)#show the user the data that was

# Basic plot --------------------------------------------------------------
shrink_val = sqrt(dim(adata)[1])/4
par(mar = c(0,0,0,0),
    pty = 's')
with(adata, 
     plot.circular(x = Cformat(Bearing),
                   stack = T,
                   sep = 0.1,
                   col = point_col,
                   pch = 19,
                   shrink = shrink_val,
                   bins = 360/5-1,
                   axes = FALSE
     )
)

# Correct for distortion --------------------------------------------------
with(ddata,
     {
       points(x = shrink_val*sin(rad(90-Angle)),
              y = shrink_val*cos(rad(90-Angle)),
              col = 2,
              pch = 3
       )
       text(x = 1.1*shrink_val*sin(rad(90-Angle)),
            y = 1.1*shrink_val*cos(rad(90-Angle)),
            labels = ground_truth,
            cex = 0.7,
            col = 2)
     }
)
#suggested tilt angle
#TODO derive the tilt angle correctly
#360 - ddata$Angle[8] + ddata$Angle[6]
#ddata$Angle[4] - ddata$Angle[2] 
# tilt_ang = 90-median(range(abs(ddata$Angle)))#mean(diff(ddata$Angle))
tilt_rot = with(ddata,
                PhiTheta_AlphaDeltaStretch(phi = rad(raw_angle),
                                           theta = rad(ground_truth)
                )
)
# tilt_rot = with(ddata,
#                 DeltaStretch(phi = rad(raw_angle),
#                               theta = rad(ground_truth)
#                               )
#                 )
message(round(tilt_rot['sd'], 1))
#perform correction
adata = within(adata,
               {
                 raw_angle = Bearing
                 angle = deg(
                   Theta(phi = 
                           UnStre( phi = rad(raw_angle),
                                   stc = tilt_rot['stc'] ),
                         alpha = tilt_rot['alpha']) #+ tilt_rot['delta']
                 )
               }
)

ddata = within(ddata,
               {
                 angle = deg(
                   Theta(phi = 
                           UnStre( phi = rad(raw_angle),
                                   stc = tilt_rot['stc']),
                         alpha = tilt_rot['alpha']
                   ) + tilt_rot['delta']
                 )
               }
)

with(adata, 
     plot.circular(x = Cformat(angle),
                   stack = T,
                   sep = 0.1,
                   col = point_col,
                   pch = 19,
                   shrink = shrink_val,
                   bins = 360/5-1,
                   axes = FALSE
     )
)
abline(a = 0,
       b = 1,
       lty = 3,
       col = 'cyan'
)
abline(a = 0,
       b = -1,
       lty = 3,
       col = 'cyan'
)
abline(h = 0,
       v = 0,
       lty = 3,
       col = 'cyan'
)
with(ddata,
     {
       points(x = shrink_val*sin(rad(raw_angle)),
              y = shrink_val*cos(rad(raw_angle)),
              col = 2,
              pch = paste(1:8)#paste(1:length(raw_angle))
       )
       points(x = shrink_val*sin(rad(angle)),
              y = shrink_val*cos(rad(angle)),
              col = 3,
              pch = paste(1:8)#paste(1:length(angle))
       )
     }
)
legend(x = 'topright',
       legend = c('Measured compass points',
                  'Corrected compass points',
                  'All corrected waggle dances'),
       col = c(2,3,point_col),
       pch = c(19,19,19),
       cex = 0.7
)


# Plot for each phase -----------------------------------------------------

# . Set up plot file ------------------------------------------------------
#file to save to in the same location as the original
savepath = paste0(path_file, '-byBDSOD-corrected-MLE.pdf')
#open the file
pdf(file = savepath,
    paper = 'a4r',
    bg = 'white',
    useDingbats = FALSE,
    onefile = TRUE
)
# . Remove short dances ---------------------------------------------------
dance_length = aggregate(`Waggle run`~date_time,
                         data = adata,
                         FUN = max)
with(dance_length, hist(`Waggle run`, breaks = 100))
with(dance_length, quantile(`Waggle run`, 0.05))
#what we actually want are short condition combinations
cond_length = aggregate(`Waggle run`~Intensity*DoLP*AoP*date_time,
                         data = adata,
                         FUN = length)
with(cond_length, hist(`Waggle run`, breaks = 100))
with(cond_length, quantile(`Waggle run`, 0.05))

long_cond = with(subset(cond_length, `Waggle run` >5),
                 paste(date_time,Intensity,DoLP,AoP))
length(unique(long_cond))
#43
adata = within(adata,
              { IDA = paste(date_time, Intensity,DoLP,AoP) }
              )

edata = subset(adata, 
               IDA %in% long_cond)

# . Set up plot parameters ------------------------------------------------
#make a set of angles for each combination of stimulus, orientation, dance and bee
df_lst = aggregate(x = angle~Intensity*DoLP*AoP*date_time,
                   data = edata,
                   FUN = list
)
dim(df_lst)
#43 tests longer than 5 runs
#convert to data.frame list
dt_lst = lapply(X = long_cond,
                FUN = function(i)
                {
                  subset(edata, IDA %in% i)
                }
)
names(dt_lst) = long_cond

# . . Set up parallel processing ------------------------------------------

#circ_mle is painfully slow
#parallel takes <40s
avail.cores = parallel::detectCores() - 1
clt = makeCluster(avail.cores,# run as many as possible
                  type=if(sys_win){"SOCK"}else{"FORK"})
clusterExport(cl = clt,#the cluster needs some variables&functions outside parLapply
              list('dt_lst',
                   'DA_MLpars',
                   'Cformat',
                   'circ_mle',
                   'circular',
                   'deg',
                   'rad'),
              environment()#needs to be reminded to use function environment, NOT global environment
)

system.time({
ml_par =   parLapply(cl = clt,
                    X = dt_lst,
                    fun = DA_MLpars,
                    exclude = c('M1', # uniform
                                'M2B', #k2 uniform
                                'M2C' #k2 uniform
                                ) #attempt non-uniform for kappa comparison
                    )
})
stopCluster(clt)

names(ml_par) = long_cond


DA_BimodPlot_example = function(dat, mlmod = NA)
{
  with(dat,
       {
         plot.circular(
           x = Cformat(  angle ),
           bins = 360/5-1,
           stack = T,
           sep = 0.07,
           col = point_col,
           pch = 19,
           shrink = shrk
         )
         mtext(text = paste0(as.character(unique(date_time)),
                             '\n',
                             unique(Intensity),
                             'µW ',
                             unique(DoLP),
                             ', ',
                             unique(AoP),
                             '°'),
               line = -1.5,
               cex = 1.5/sq_cond,#3/sq_cond,
               col = switch (as.character(unique(AoP)),
                             `0` = 'red',
                             `90` = 'cyan3',
                             'gray'
               )
         )
       }
      )
         with(mlmod,
              {
               arrows(x0 = 0 ,
                      y0 = 0,
                      x1 = sin(rad(m1))*A1(k1),
                      y1 = cos(rad(m1))*A1(k1),
                      col = 2,
                      lwd = lb1/0.3,
                      length = 0.05
               )
               arrows(x0 = 0 ,
                      y0 = 0,
                      x1 = sin(rad(m2))*A1(k2),
                      y1 = cos(rad(m2))*A1(k2),
                      col = 2,
                      lwd = lb2/0.3,
                      length = 0.05
               )
              }
         )
}

nms = names(df_lst)
ucond = dim(df_lst)[1]#prod(lul)
sq_cond = min( c( ceiling(sqrt(ucond)), 5) )
shrk = 1+sqrt(dim(adata)[1])/ucond

# Plot for each phase -----------------------------------------------------

# . Set up plot file ------------------------------------------------------
#file to save to in the same location as the original
savepath = paste0(path_file, '-byBDSOD-corrected-MLE.pdf')
#open the file
pdf(file = savepath,
    paper = 'a4r',
    bg = 'white',
    useDingbats = FALSE,
    onefile = TRUE
)

# . Set up plot p

par(mfrow = c(sq_cond, sq_cond),
    mar = c(0,0,0,0),
    pty = 's'
)
invisible(
  mapply(dat = dt_lst,
         mlmod = ml_par,
        FUN = DA_BimodPlot_example
  )
)
#save plot
dev.off()
#show plot
shell.exec.OS(savepath)


# Save data with models ---------------------------------------------------
bound_dt = within(df_lst,
                  {
                    angle = sapply(angle, paste, collapse = ', ')
                  })
ml_par_df = do.call(what = rbind, args = ml_par)
bound_dt = cbind(bound_dt, ml_par_df)
res_path = paste0(path_file, '-MLE_dances.csv')
write.csv(x = bound_dt,
          file = res_path,
          row.names = FALSE)
shell.exec.OS(res_path)

# Extract parameters ------------------------------------------------------
par_dt = do.call(what = rbind,
                 args = ml_par)

mle_data = data.frame(cbind(df_lst[-5], par_dt))

#for unimodal, set k2 to 0
mle_data = within(mle_data,
                  {
                    k2 = ifelse(test = lb2 >0, 
                                yes = k2,
                                no = NA )
                  }
                  )

par(mfrow = c(2,2),
    mar = c(0,0,0,0),
    pty = 's')
with(mle_data,
     {
       plot.circular(x = Cformat(m1[AoP == 0]),
                     stack = T,
                     sep = 0.1,
                     col = point_col,
                     pch = 19,
                     shrink = 2,
                     bins = 360/5-1,
                     axes = FALSE
       )
       text(x = 0, y = 0,
            labels = 'Primary mean\n Stimulus: 0°')
       plot.circular(x = Cformat(m1[AoP == 90]),
                     stack = T,
                     sep = 0.1,
                     col = 'darkred',
                     pch = 19,
                     shrink = 2,
                     bins = 360/5-1,
                     axes = FALSE
       ) 
       text(x = 0, y = 0,
            labels = 'Primary mean\n Stimulus: 90°')
       plot.circular(x = Cformat(m2[AoP == 0]),
                     stack = T,
                     sep = 0.1,
                     col = point_col,
                     pch = 19,
                     shrink = 2,
                     bins = 360/5-1,
                     axes = FALSE
       )
       text(x = 0, y = 0,
            labels = 'Secondary mean\n Stimulus: 0°')
       plot.circular(x = Cformat(m2[AoP == 90]),
                     stack = T,
                     sep = 0.1,
                     col = 'darkred',
                     pch = 19,
                     shrink = 2,
                     bins = 360/5-1,
                     axes = FALSE
       )
       text(x = 0, y = 0,
            labels = 'Secondary mean\n Stimulus: 90°')
     }
)

par(mfrow = c(1,2),
    mar = c(4,4,2.7,2.7))
with(subset(mle_data,
            Intensity ==101),
     {
stripchart(x = A1(kappa = k1)~DoLP,
           
           xlab = 'stimulus',
           ylab = 'MLE rho',
           ylim = c(0,1),
           main = 'Primary mean',
           vertical  = TRUE,
           method = 'stack',
           pch = 19,
           col= adjustcolor(point_col, alpha.f = 0.5),
           # cex.axis = 0.3,
           las = 2)
stripchart(x = A1(kappa = k1)~DoLP,
          data = subset(mle_data, Intensity == 11),
          xlab = 'stimulus',
          ylab = 'MLE rho',
           ylim = c(0,1),
          main = 'Primary mean, High Int',
          vertical  = TRUE,
          method = 'stack',
          pch = 19,
          col= adjustcolor(2, alpha.f = 0.5),
          add = TRUE,
          las = 2)       
abline(h = c(0,1))
stripchart(x = A1(kappa = k2)~DoLP,
           xlab = 'stimulus',
           ylab = 'MLE rho',
           main = 'Secondary mean',
           vertical  = TRUE,
           method = 'stack',
           pch = 19,
           col= adjustcolor(point_col, alpha.f = 0.5),
           # cex.axis = 0.3,
           las = 2)
stripchart(x = A1(kappa = k2)~DoLP,
           data = subset(mle_data, Intensity == 11),
           xlab = 'stimulus',
           ylab = 'MLE rho',
           main = 'Primary mean, High Int',
           vertical  = TRUE,
           method = 'stack',
           pch = 19,
           col= adjustcolor(2, alpha.f = 0.5),
           add = TRUE,
           las = 2)
abline(h = c(0,1))
     }
)

baseline = summary(subset(mle_data,
                          Intensity ==0))


glm_101_k1 = glm(formula = A1(k1)~log10(DoLP), 
                 data = subset(mle_data, Intensity == 101),
                 family = quasibinomial())

glm_11_k1 = glm(formula = A1(k1)~log10(DoLP), 
                 data = subset(mle_data, Intensity == 11),
                 family = quasibinomial())


par(mfrow = c(1,2),
    mar = c(4,4,2.7,2.7))
with(subset(mle_data,
            Intensity ==101),
     {
       plot(x = DoLP,
            y = A1(kappa = k1),
            xlab = 'DoLP',
            ylab = 'MLE rho',
            main = 'Primary mean, High Int',
            pch = 19,
            col= adjustcolor(point_col, alpha.f = 0.5),
            ylim = c(0,1),
            xlim = c(0.02,0.4),
            log = 'x',
            las = 2)
       points(x = DoLP[lb2>0],
              y = A1(kappa = k2[lb2>0]),
              pch = 19,
              col= adjustcolor(point_col, alpha.f = 0.5),)
     }
)

xx = seq(from = 0.02, to  = 0.4, length.out = 1e3)
lines(x = xx,
      y = plogis(predict(glm_101_k1, newdata = data.frame( DoLP = xx )) ),
      col = 'blue')

with(subset(mle_data,
            Intensity ==11),
     {
       plot(x = DoLP,
            y = A1(kappa = k1),
            xlab = 'DoLP',
            ylab = 'MLE rho',
            main = 'Primary mean, Low Int',
            pch = 19,
            col= adjustcolor(2, alpha.f = 0.5),
            ylim = c(0,1),
            xlim = c(0.02,0.4),
            log = 'x',
            las = 2)
       points(x = DoLP[lb2>0],
              y = A1(kappa = k2[lb2>0]),
              pch = 19,
              col= adjustcolor(2, alpha.f = 0.5),)
     }
)

lines(x = xx,
      y = plogis(predict(glm_11_k1, newdata = data.frame( DoLP = xx )) ),
      col = 'red')


# Just bimodal conditions -------------------------------------------------
bim_dances = subset(mle_data, lb2>0)
bim_data = rbind(
            within(bim_dances, 
                   {
                     k12 = k1
                     rm(k1)
                     rm(k2)
                     }),
            within(bim_dances, 
                   {
                     k12 = k2
                     rm(k1)
                     rm(k2)
                     })
          )

#fit models
glm_101_k12 = glm(formula = A1(k12)~log10(DoLP), 
                 data = subset(bim_data, Intensity == 101),
                 family = quasibinomial())

glm_11_k12 = glm(formula = A1(k12)~log10(DoLP), 
                data = subset(bim_data, Intensity == 11),
                family = quasibinomial())


par(mfrow = c(1,2),
    mar = c(4,4,2.7,2.7))
with(subset(bim_data,
            Intensity ==101),
     {
       plot(x = DoLP,
            y = A1(kappa = k12),
            xlab = 'DoLP',
            ylab = 'MLE rho',
            main = 'Bimodal, High Int',
            pch = 19,
            col= adjustcolor(point_col, alpha.f = 0.5),
            ylim = c(0,1),
            xlim = c(0.02,0.4),
            log = 'x',
            las = 2)
     }
)

lines(x = xx,
      y = plogis(predict(glm_101_k12, newdata = data.frame( DoLP = xx )) ),
      col = 'blue')

with(subset(bim_data,
            Intensity ==11),
     {
       plot(x = DoLP,
            y = A1(kappa = k12),
            xlab = 'DoLP',
            ylab = 'MLE rho',
            main = 'Bimodal, Low Int',
            pch = 19,
            col= adjustcolor(2, alpha.f = 0.5),
            ylim = c(0,1),
            xlim = c(0.02,0.4),
            log = 'x',
            las = 2)
     }
)

lines(x = xx,
      y = plogis(predict(glm_11_k12, newdata = data.frame( DoLP = xx )) ),
      col = 'red')


# Nonlinear models --------------------------------------------------------
#primary and secondary mean (not used)
k12_data = rbind(
  within(mle_data, 
         {
           k12 = k1
           rm(k1)
           rm(k2)
         }),
  within(bim_dances, 
         {
           k12 = k2
           rm(k1)
           rm(k2)
         })
)

#anomalous trials for intensity 11, DoLP <0.05
model_data = subset(k12_data, 
                    !(Intensity == 11 & DoLP <0.05))


Psyfun = function(prm, #starting parameters
                  dt, #data
                  pri = list(c(-1.0,0.3),#priors
                             c(-1,1.0),#the range of log10 DoLP is log(1.3) = 0.26 wide, max plausible
                             c(-1.5,1.0),
                             c(-3,1.0)),
                  kname = 'k1'
                )
  {
  thresh = prm[1]
  lwidth = prm[2]
  lbase = prm[3]
  llapse = prm[4]
  
  ll= sum(
    with(dt,
       dlogis(x = qlogis(A1(get(kname)+.Machine$double.eps)), #make sure kappa >0
              location = qlogis(
                plogis(lbase) + #baseline (logit scaled)
                (1 - plogis(llapse) - plogis(lbase)) * #curve height (above baseline)
                                   plogis( 2*log(2/(1- 0.8) -1)* # width scaling factor
                                                (log10(DoLP) - thresh) / #effect of inflection point
                                                exp(lwidth) ) #curve width (log scaled))
                ),
              scale = 1,
              log = TRUE)
          ),
    na.rm = TRUE
        )
  ll = ll +
      dnorm(thresh, mean =  pri[[1]][1],sd =  pri[[1]][2], log = TRUE) +
      dnorm(lwidth, mean =  pri[[2]][1],sd =  pri[[2]][2], log = TRUE) +
      dnorm(lbase, mean =  pri[[3]][1],sd =  pri[[3]][2], log = TRUE) +
      dnorm(llapse, mean =  pri[[4]][1],sd =  pri[[4]][2], log = TRUE)
  
  return(-ll)
}
#repeated runs with SANN to get CI
avail.cores = parallel::detectCores() - 1
clt = makeCluster(avail.cores,# run as many as possible
                  type=if(sys_win){"SOCK"}else{"FORK"})
clusterExport(cl = clt,#the cluster needs some variables&functions outside parLapply
              list('model_data',
                   'Psyfun',
                   'A1'),
              environment()#needs to be reminded to use function environment, NOT global environment
)

system.time({
nlm_11 = data.frame(
        t(
          parSapply(cl = clt,
                    X = 1:1000,
                    FUN = function(i)
                      {
                      optim(
                       par = rnorm(4),
                       fn = Psyfun,
                       dt = subset(model_data, Intensity == 11),
                       kname = 'k12',           
                       pri = list(c(-1,0.1),#informative prior for higher threshold
                                  c(-2,1.0),
                                  c(-1.5,1.0),
                                  c(-3,1.0)),
                       method = 'SANN',
                       control= list(maxit = 1000)
                       )$par
                    }
                    )
          )
          )
nlm_101 = data.frame(
        t(
          parSapply(cl = clt,
                    X = 1:1000,
                    FUN = function(i)
                    {
          optim(par = rnorm(4),
           fn = Psyfun,
           dt = subset(model_data, Intensity == 101),
                       kname = 'k12',

           method = 'SANN',
           control= list(maxit = 1000))$par
                    }
          )
          )
          )
})
stopCluster(clt)

names(nlm_11) = c('thresh', 'lwidth', 'lbase', 'llapse')
names(nlm_101) = c('thresh', 'lwidth', 'lbase', 'llapse')
#check params
summary(nlm_11)
summary(nlm_101)
#check contrast
summary(nlm_11 - nlm_101)

par(mfrow = c(1,1)); hist(100*(10**nlm_11$thresh - 10**nlm_101$thresh), breaks = 100)

prm_nlm_11 = apply(nlm_11, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.5, 0.975))
prm_nlm_101 = apply(nlm_101, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.5, 0.975))

PredPsych = function(x, thresh, lwidth, lbase, llapse)
{
    plogis(lbase) + #baseline (logit scaled)
      (1 - plogis(llapse) - plogis(lbase)) * #curve height (above baseline)
      plogis( 2*log(2/(1- 0.8) -1)* # width scaling factor
                (log10(x) - thresh) / #effect of inflection point
                exp(lwidth) ) #curve width (log scaled))
}


par(mfrow = c(1,2),
    mar = c(4,4,2.7,2.7))
with(subset(model_data,
            Intensity ==101),
     {
       plot(x = DoLP,
            y = A1(kappa = k12),
            xlab = 'DoLP',
            ylab = 'MLE rho',
            main = 'Both mean, High Int',
            pch = 19,
            col= adjustcolor(point_col, alpha.f = 0.5),
            ylim = c(0,1),
            xlim = c(0.02,0.4),
            log = 'x',
            las = 2)
     }
)

with(data.frame(prm_nlm_101),
     {
lines(x = xx,
      y = PredPsych(xx, thresh[2], lwidth[2], lbase[2], llapse[2]),
      col = 'blue',
      lwd = 3)
 abline(v = 10^thresh, lty = c(3,1,3), col = 'blue')
    }
)
with(subset(model_data,
            Intensity ==11),
     {
       plot(x = DoLP,
            y = A1(kappa = k12),
            xlab = 'DoLP',
            ylab = 'MLE rho',
            main = 'Both mean, Low Int',
            pch = 19,
            col= adjustcolor(2, alpha.f = 0.5),
            ylim = c(0,1),
            xlim = c(0.02,0.4),
            log = 'x',
            las = 2)
     }
)

with(data.frame(prm_nlm_11),
     {
       lines(x = xx,
             y = PredPsych(xx, thresh[2], lwidth[2], lbase[2], llapse[2]),
             col = 'red',
             lwd = 3)
       abline(v = 10^thresh, lty = c(3,1,3), col = 'red')
     }
)



# Final plot --------------------------------------------------------------



par(mfrow = c(1,1),
    mar = c(4,4,2.7,2.7))
with(subset(model_data,
            Intensity ==101),
     {
       plot(x = DoLP,
            y = A1(kappa = k12),
            xlab = 'DoLP',
            ylab = 'MLE rho',
            main = 'Both means',
            pch = 19,
            col= adjustcolor(point_col, alpha.f = 0.5),
            ylim = c(0,1),
            xlim = c(0.02,0.4),
            log = 'x',
            las = 2)
     }
)

with(subset(model_data,
            Intensity ==11),
     {
       points(x = DoLP,
            y = A1(kappa = k12),
            pch = 19,
            col= adjustcolor(2, alpha.f = 0.5))
     }
)

with(data.frame(prm_nlm_101),
     {
       lines(x = xx,
             y = PredPsych(xx, thresh[2], lwidth[2], lbase[2], llapse[2]),
             col = 'blue',
             lwd = 3)
       abline(v = 10^thresh, lty = c(3,1,3), col = 'blue')
     }
)
with(data.frame(prm_nlm_11),
     {
       lines(x = xx,
             y = PredPsych(xx, thresh[2], lwidth[2], lbase[2], llapse[2]),
             col = 'red',
             lwd = 3)
       abline(v = 10^thresh, lty = c(3,1,3), col = 'red')
     }
)

# 
# #logscale
# par(mfrow = c(1,2),
#     mar = c(4,4,2.7,2.7))
# with(subset(mle_data,
#             Intensity ==101),
#      {
#        plot(x = DoLP,
#             y = A1(kappa = k1),
#             xlab = 'DoLP',
#             ylab = 'MLE rho',
#             main = 'Both means, High Int',
#             pch = 19,
#             col= adjustcolor(point_col, alpha.f = 0.5),
#             ylim = c(0.5,1),
#             xlim = c(0.02,0.4),
#             log = 'xy',
#             las = 2)
#        points(x = DoLP[lb2>0],
#               y = A1(kappa = k2[lb2>0]),
#               pch = 19,
#               col= adjustcolor(point_col, alpha.f = 0.5),)
#      }
# )
# 
# with(data.frame(prm_nlm_101),
#      {
# lines(x = xx,
#       y = PredPsych(xx, thresh[2], lwidth[2], lbase[2], llapse[2]),
#       col = 'blue',
#       lwd = 3)
# abline(v = 10^thresh, lty = c(3,1,3), col = 'blue')
#     }
# )
# 
# with(subset(mle_data,
#             Intensity ==11),
#      {
#        plot(x = DoLP,
#             y = A1(kappa = k1),
#             xlab = 'DoLP',
#             ylab = 'MLE rho',
#             main = 'Both means, Low Int',
#             pch = 19,
#             col= adjustcolor(2, alpha.f = 0.5),
#             ylim = c(0.7,1),
#             xlim = c(0.02,0.4),
#             log = 'xy',
#             las = 2)
#      }
# )
# 
# with(data.frame(prm_nlm_11),
#      {
#        lines(x = xx,
#              y = PredPsych(xx, thresh[2], lwidth[2], lbase[2], llapse[2]),
#              col = 'red',
#              lwd = 3)
#        abline(v = 10^thresh, lty = c(3,1,3), col = 'red')
#      }
# )
# 
# 
# # Kappa version --
# 
# 
# par(mfrow = c(1,2),
#     mar = c(4,4,2.7,2.7))
# with(subset(mle_data,
#             Intensity ==101),
#      {
#        plot(x = DoLP,
#             y = k1,
#             xlab = 'DoLP',
#             ylab = 'accuracy (kappa)',
#             main = 'Primary mean, High Int',
#             pch = 19,
#             col= adjustcolor(point_col, alpha.f = 0.5),
#             ylim = c(1, 250),
#             xlim = c(0.02,0.4),
#             log = 'xy',
#             las = 2)
#      }
# )
# 
# with(data.frame(prm_nlm_101),
#      {
#        lines(x = xx,
#              y = A1inv(PredPsych(xx, thresh[2], lwidth[2], lbase[2], llapse[2]) ),
#              col = 'blue',
#              lwd = 3)
#      }
# )
# with(subset(mle_data,
#             Intensity ==11),
#      {
#        plot(x = DoLP,
#             y = k1,
#             xlab = 'DoLP',
#             ylab = 'accuracy (kappa)',
#             main = 'Primary mean, Low Int',
#             pch = 19,
#             col= adjustcolor(2, alpha.f = 0.5),
#             ylim = c(1, 130),
#             xlim = c(0.02,0.4),
#             log = 'xy',
#             las = 2)
#      }
# )
# 
# with(data.frame(prm_nlm_11),
#      {
#        lines(x = xx,
#              y = A1inv(PredPsych(xx, thresh[2], lwidth[2], lbase[2], llapse[2]) ),
#              col = 'red',
#              lwd = 3)
#      }
# )
# 
