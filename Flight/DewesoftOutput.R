#FOR A 'CLEAN' RUN, RESTART Rstudio
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 11 11
#     MODIFIED:	James Foster              DATE: 2021 11 19
#
#  DESCRIPTION: Loads a Matlab file and wraps angles to a circle.
#               
#       INPUTS: A ".mat" table with a column of time stamps and column of angles ("angle").
#               User should specify processing details (line 60).
#               
#      OUTPUTS: Plot (.pdf or .png). Data table (.csv).
#
#	   CHANGES: - PNG option
#             - CSV saving
#             - Acceleration calculation
#
#   REFERENCES: Batschelet E (1981).
#               In: Circular Statistics in Biology
#               Academic Press (London)
#
#    EXAMPLES:  Fill out user input (lines 60-65), then press ctrl+shift+s to run
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data +
#- Convert to circular  +   
#- Plot +
#- Save plot  +
#- Save data as csv +
#- Test on multiple machines +
#- Test on Mac
#- Comment

# Useful functions --------------------------------------------------------
# . Load package ----------------------------------------------------------
#needs installing before first use (in Rstudio, see automatic message)
suppressMessages(#these are disturbing users unnecessarily
  {
    require(circular)#package for handling circular data
    require(CircStats)#package for circular hypothesis tests
    require(R.matlab)
  }
)

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
csv_sep = ','#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
av_window = 10#number of seconds to smooth over for averaging
angle_name = "angles" #The title of the column with angles; NO SPACES PLEASE
angle_unit = "degrees" # "degrees" or "radians"
point_col = "darkblue" # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
save_type ="png"# "pdf"# 

#Check the operating system and assign a logical flag (T or F)
sys_win <- Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  ltp <- gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp <- Sys.getenv('HOME')#Life was easier on Mac
}


# . Select files ---------------------------------------------------------

# set path to files
if(sys_win){#choose.files is only available on Windows
  message('\n\nPlease select the ".mat" file\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file  <- choose.files(
    default = file.path(ltp,'Documents', "*.mat"),#For some reason this is not possible in the "root" user
    caption = 'Please select the ".mat" file'
  )
}else{
  message('\n\nPlease select the ".mat" file\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file <- file.choose(new=F)
}
#show the user the path they have selected
if(is.null(path_file))
{stop('No file selected.')}else
{print(path_file)}


# Read in file ------------------------------------------------------------
message('Reading in ', basename(path_file), '\nplease be patient...')
tryCatch(#Perform no further analysis if the file doesn't load
  {
adata = R.matlab::readMat(con = path_file)#,#read from user-selected file
  },
error = function(e)
            {
            stop(
              paste0('User selected file "', 
                     basename(path_file), 
                     '" could not be loaded!\n',
                     e)
            )
            }
)
#Inform the user that the file has loaded
message(basename(path_file), ' loaded successfully')
mat_names = names(adata)#find variables from Matlab file
desired_names = grepl(x = mat_names, #search for the time and angle
                      pattern = 'Data1.time.DI|Data1.CNT.A.1.Winkel')
adata = data.frame(adata[mat_names[desired_names]])#extract only those variables
View(adata)#show the user the data that was selected

# . Clean up data ---------------------------------------------------------
adata = within(adata,
               { # rename time and angle, wrap angles to circle
               time = Data1.time.DI.1 
               experimental_time = Data1.time.DI.1-min(Data1.time.DI.1, na.rm=T)
               unwrap_angle = Data1.CNT.A.1.Winkel
               angle = circular(Data1.CNT.A.1.Winkel,
                                type = 'angles',
                                unit = 'degrees',
                                template = 'geographics',
                                modulo = '2pi',
                                zero = pi/2,
                                rotation = 'clock'
               )
               rm(Data1.time.DI.1, Data1.CNT.A.1.Winkel)
               }
               )
#calculate average sample rate
sample_rate = with(adata, 1/mean(diff(time)))#Hz

# . Process data ----------------------------------------------------------
message('Processing data...')
#Write a function for calculating the local mean angle
MAmeanang <- function(i, #index
                      dta, #vector of data
                      window = 1, # time window in seconds
                      hz = 100) # sampling rate
{
  n = window*hz # number of samples to use
  winmin = i-round(n/2)+1 #index of window start
  winmax = i+round(n/2)-1 #index of window end
  if(winmin<1){return(NA)}#if the window starts outside data, don't calculate
  if(winmax>length(dta)){return(NA)}#if the window ends outside data, don't calculate
  # dd = dta[winmin:winmax] *pi/180 #avoid repeating indexing for calculation
  return(
    mean.circular(#use the method for an object of class "circular"
      x = circular(dta[winmin:winmax],#data in the window, if the window is in the data
                   type = 'angles',
                   unit = 'degrees',
                   template = 'geographics',
                   modulo = '2pi'
                                )
      )
    #conversion to class circular is actually faster
    # atan2(
    #   y = mean(sin(dd)),
    #   x = mean(cos(dd))
    #   )*180/pi
    )
}
#Write a function for calculating the local mean vector length
MAmeanvec <- function(i, #index
                      dta, #vector of data
                      window = 1, # time window in seconds
                      hz = 100) # sampling rate
{
  n = window*hz # number of samples to use
  winmin = i-round(n/2)+1 #index of window start
  winmax = i+round(n/2)-1 # index of window end
  if(winmin<1){return(NA)}#if window starts outside data, don't calculate
  if(winmax>length(dta)){return(NA)}#if window ends outside data, don't calculate
  return(
    rho.circular(#use the method for an object of class "circular"
      x = circular(dta[winmin:winmax],#data in the window, if the window is in the data
                   type = 'angles',
                   unit = 'degrees',
                   template = 'geographics',
                   modulo = '2pi'
                                )
      )
    #conversion to class circular is actually faster
    # sqrt(
    #  mean(sin(dta[winmin:winmax]*pi/180))^2 +
    #  mean(cos(dta[winmin:winmax]*pi/180))^2
    #   )
    )
}
#Calculate average turning speed (1st derivative of angle)
#When input data is speed rather than angle
# returns angular acceleration (2nd derivative of angle)
MAturnspeed <- function(i, #index
                      dta, #vector of data
                      window = 1, # time window in seconds
                      hz = 100) # sampling rate
{
  n = window*hz # number of samples to use
  winmin = i-round(n/2)+1 #index of window start 
  winmax = i+round(n/2)-1 #index of window end
  if(winmin<1){return(NA)}#if window starts outside data, don't calculate
  if(winmax>length(dta)){return(NA)}#if window ends outside data, don't calculate
  return(
    mean( # default method for mean, no missing values allowed
      diff( # sequential differences within a vector of data
        x = dta[winmin:winmax], #data in the window
          )
      )*hz #number of data collected per second, units returned are deg/s
    )
}
#Apply these functions to the data using "simplify-list-apply" (sapply)
adata = within(adata, 
               {
               ma_angle = sapply(X = 1:length(angle),#all indices in angle
                                 FUN = MAmeanang,#the mean angle function
                                 dta = angle,#all angles observed
                                 window = av_window,#window size (s)
                                 hz = sample_rate #sample rate (Hz)
                                 )
               ma_rho = sapply(X = 1:length(angle),#all indices in angle
                                 FUN = MAmeanvec,#the mean vector function
                                 dta = angle,#all angles observed
                                 window = av_window,#window size (s)
                                 hz = sample_rate #sample rate (Hz)
                                 )
               ma_turn = sapply(X = 1:length(angle),#all indices in angle
                                 FUN = MAturnspeed, #the mean speed function
                                 dta = unwrap_angle,#raw angles observed
                                 window = av_window,#window size (s)
                                 hz = sample_rate #sample rate (Hz)
                                 )
               smooth_turn = predict( # fit a spline and predict its values across all times
                             smooth.spline(x = experimental_time[!is.na(ma_turn)], #use only times when speed was calculated
                                           y = ma_turn[!is.na(ma_turn)]), #use only speeds where speed was calculated
                             x = experimental_time # predict for all times
                             )$y
               ma_accel = sapply(X = 1:length(smooth_turn),#all indices in angle
                                FUN = MAturnspeed, #the mean speed function, here converts speed to acceleration
                                dta = smooth_turn, #use smoothed speeds
                                window = av_window,#window size (s)
                                hz = sample_rate #sample rate (Hz)
                               )
               }#end of expression
               )#close data.frame and execute expression
#  Save data --------------------------------------------------------------
message('Saving data')
csv_file <- file.path(dirname(path_file),
                      paste0(basename(path_file),
                             '_proc',
                             '.', 
                             '.csv')
)
write.csv(x = adata,
          file = csv_file,
          row.names = FALSE
)

# Plot data ---------------------------------------------------------------
message('Plotting data')

# . Set up plot area ------------------------------------------------------
plot_file <- file.path(dirname(path_file), paste0(basename(path_file),'_','.', save_type))
if(file.exists(plot_file))
{
  message('A plot called "', basename(plot_file), '" already exists in this folder.')
  nnm <- readline(prompt = 'New plot name: '
  )
  
  plot_file <-  file.path(dirname(path_file),paste0(ifelse(nchar(nnm),nnm,basename(path_file)),'_','.', save_type))
}
switch(save_type,
       pdf = 
         pdf(file = plot_file,
             paper = 'a4',
             height = 10,
             bg = 'white',
             useDingbats = F
         ),
       png = png(file = plot_file,
                 res = 150,
                 width = 210*10,
                 height = 297*10,
                 bg = 'white'
       ),
       jpeg(file = paste0(plot_file,'.jpeg'),
            quality = 100,
            width = 210*10,
            height = 297*10,
            bg = 'white'
       )
)
switch(save_type,
       png = par(mfrow = c(4,1),
                 mar = c(3,5,0,3),
                 oma = c(1.5,0,1.5,0),
                 cex = 1.5
       ),
       par(mfrow = c(4,1),
           mar = c(3,5,0,3),
           oma = c(1.5,0,1.5,0))
)

# . Plot raw data ---------------------------------------------------------
with(adata,
     {
      plot(x = experimental_time,
            y = unwrap_angle,
           type = 'l',
           col = point_col,
           axes = F
           )
       axis(side = 1,
            at = 60*(0:(max(experimental_time)/60)),
            labels = 1*(0:(max(experimental_time)/60))
       )
       axis(side = 2,
            at = 360*(round(min(unwrap_angle)/360):round(max(unwrap_angle)/360)),
            labels = paste0(360*(round(min(unwrap_angle)/360):round(max(unwrap_angle)/360)),
                            '°')
           )
       abline(h = 360*(round(min(unwrap_angle)/360):round(max(unwrap_angle)/360)),
              col = rgb(0,0,0,0.1)
              )
            
     }
    )
with(adata,
     {
      plot(x = experimental_time,
            y = angle,
           type = 'p',
           pch = 19,
           cex = 0.1,
           col = adjustcolor(point_col, alpha.f = 20/256),
           axes = F
           )
       axis(side = 1,
            at = 60*(0:(max(experimental_time)/60)),
            labels = 1*(0:(max(experimental_time)/60))
       )
       axis(side = 2,
            at = 90*(round(min(angle)/90):round(max(angle)/90)),
            labels = paste0(90*(round(min(angle)/90):round(max(angle)/90)),
                            '°')
           )
        abline(h = 90*(round(min(angle)/90):round(max(angle)/90)),
              col = rgb(0,0,0,0.1)
       )
            
     }
    )

# . Plot moving averages --------------------------------------------------
with(adata,
     {
       lines(x = experimental_time,
            y = ma_angle,
            type = 'p',
            pch = 19,
            cex = 0.01,
            col = rgb(0,0.5,0,0.3),
       )
     }
)
with(adata,
     {
      plot(x = NULL,
           xlim = range(experimental_time, na.rm = T),
           ylim = range(ma_turn, na.rm = T),
           xlab = 'time (s)',
           ylab = paste0('mean turning speed (°/s: ',av_window,'s)'),
           axes = F
           )
       axis(side = 1,
            at = 60*(0:(max(experimental_time)/60)),
            labels = 1*(0:(max(experimental_time)/60))
       )
       axis(side = 2,
            at = 90*(round(min(ma_turn, na.rm = T)/90):round(max(ma_turn, na.rm = T)/90))
           )
      points(x = experimental_time,
            y = ma_turn,
           col = adjustcolor(point_col, alpha.f = 20/256),
           cex = 0.1,
           pch = 19
           )
      lines(x = experimental_time,
            y = smooth_turn,
           col = 'darkgreen'
           )
        abline(h = c(-15, 0, 15),
              col = 'black',
              lwd = 0.25
               )
     }
    )
with(adata,
     {
      lines(x = experimental_time,
            y = ma_accel*5,
           col = adjustcolor('darkred', alpha.f = 200/256)
           )
      axis(side = 4,
            line = 0,
            at = 5*10*
              (round(min(ma_accel, na.rm = T)/10):
                 round(max(ma_accel, na.rm = T)/10)),
            labels = 10*
              (round(min(ma_accel, na.rm = T)/10):
                 round(max(ma_accel, na.rm = T)/10)/
                 5),
            col = 'darkred'
      )
      mtext(text = paste0('mean acceleration (°/s^2: ',av_window,'s)'),
            side = 4,
            cex = par('cex.main'),
            line = 2
            )
     }
    )
with(adata,
     {
      plot(x = NULL,
           xlim = range(experimental_time, na.rm = T),
           ylim = c(0,1),
           xlab = 'time (s)',
           ylab = paste0('mean vector length (',av_window,'s)'),
           axes = F
           )
       axis(side = 1,
            at = 60*(0:(max(experimental_time)/60)),
            labels = 1*(0:(max(experimental_time)/60))
       )
       axis(side = 2,
            at = 0:5/5
           )
      lines(x = experimental_time,
            y = ma_rho,
           col = point_col
           )
        abline(h = c(0,1),
              col = 'black',
              lwd = 0.25
               )
        abline(h = sqrt(-log(c(0.05, 0.01)))/(av_window*sample_rate),#Mean vector Rayleigh test p
              col = 'red',
              lty = c(3,2),
              lwd = 0.25
       )
            
     }
    )
mtext(text = basename(path_file),
      outer = T, 
      side = 3
      )
mtext(text = 'Time (min)',
      outer = T,
      side = 1
      )
# . Save plot -------------------------------------------------------------
dev.off()
shell.exec.OS(plot_file)
# 
# plot.circular(x = circular(x = adata1$angle, 
#                            type = 'angles',
#                            unit = 'degrees',
#                            template = 'geographics',
#                            modulo = '2pi',
#                            zero = 'pi',
#                            rotation = 'clock'
# ),
# stack = TRUE,
# bins = 360/5,
# sep = 0.05,
# col = point_col
# )
# dev.off()