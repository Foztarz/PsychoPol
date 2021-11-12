#FOR A 'CLEAN' RUN, RESTART Rstudio
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 11 11
#     MODIFIED:	James Foster              DATE: 2021 11 12
#
#  DESCRIPTION: Loads a matlab file and wraps angles to a circle.
#               
#       INPUTS: A ".mat" table with a column of time stamps and column of angles ("angle").
#               User should specify test details (line 50).
#               
#      OUTPUTS: Plot (.pdf). Data table (.csv).
#
#	   CHANGES: - 
#             - 
#             - 
#
#   REFERENCES: Batschelet E (1981).
#               In: Circular Statistics in Biology
#               Academic Press (London)
#
#    EXAMPLES:  Fill out user input (lines 50-55), then press ctrl+shift+s to run
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data   +
#- Convert to circular     
#- Plot 
#- Save data as csv 
#- Save plot
#- Test on multiple machines
#- Test on Mac

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
message(basename(path_file), ' loaded successfully')
mat_names = names(adata)
desired_names = grepl(x = mat_names, 
                      pattern = 'Data1.time.DI|Data1.CNT.A.1.Winkel')
adata = data.frame(adata[mat_names[desired_names]])
View(adata)#show the user the data that 

# . Clean up data ---------------------------------------------------------
adata = within(adata,
               {
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
sample_rate = with(adata, 1/mean(diff(time)))#Hz

# . Process data ----------------------------------------------------------
message('Processing data...')
MAmeanang <- function(i, #index
                      dta, #vector
                      window = 1, # time window in seconds
                      hz = 100) # sampling rate
{
  n = window*hz
  winmin = i-round(n/2)+1
  winmax = i+round(n/2)-1
  if(winmin<1){return(NA)}#{winmin = 1}
  if(winmax>length(dta)){return(NA)}#{winmax = length(x)}
  return(
    mean.circular(
      x = circular(dta[winmin:winmax],
                   type = 'angles',
                   unit = 'degrees',
                   template = 'geographics',
                   modulo = '2pi'
                                )
      )
    )
}
MAmeanvec <- function(i, #index
                      dta, #vector
                      window = 1, # time window in seconds
                      hz = 100) # sampling rate
{
  n = window*hz
  winmin = i-round(n/2)+1
  winmax = i+round(n/2)-1
  if(winmin<1){return(NA)}#{winmin = 1}
  if(winmax>length(dta)){return(NA)}#{winmax = length(x)}
  return(
    rho.circular(
      x = circular(dta[winmin:winmax],
                   type = 'angles',
                   unit = 'degrees',
                   template = 'geographics',
                   modulo = '2pi'
                                )
      )
    )
}
MAturnspeed <- function(i, #index
                      dta, #vector
                      window = 1, # time window in seconds
                      hz = 100) # sampling rate
{
  n = window*hz
  winmin = i-round(n/2)+1
  winmax = i+round(n/2)-1
  if(winmin<1){return(NA)}#{winmin = 1}
  if(winmax>length(dta)){return(NA)}#{winmax = length(x)}
  return(
    mean(
      diff(
        x = dta[winmin:winmax],
          )
      )/hz
    )
}
adata = within(adata, 
               {
               ma_angle = sapply(X = 1:length(angle),
                                 FUN = MAmeanang,
                                 dta = angle,
                                 window = av_window,
                                 hz = sample_rate
                                 )
               ma_rho = sapply(X = 1:length(angle),
                                 FUN = MAmeanvec,
                                 dta = angle,
                                 window = av_window,
                                 hz = sample_rate
                                 )
               ma_turn = sapply(X = 1:length(angle),
                                 FUN = MAturnspeed,
                                 dta = unwrap_angle,
                                 window = av_window,
                                 hz = sample_rate
                                 )
               smooth_turn = predict(
                             smooth.spline(x = experimental_time[!is.na(ma_turn)], 
                                           y = ma_turn[!is.na(ma_turn)]),
                             x = experimental_time
                             )$y
               }
               )

# Plot data ---------------------------------------------------------------
message('Plotting data')

# . Set up plot area ------------------------------------------------------
plot_file <- file.path(dirname(path_file), paste0(basename(path_file),'_','.pdf'))
if(file.exists(plot_file))
{
  message('A plot called "', basename(plot_file), '" already exists in this folder.')
  nnm <- readline(prompt = 'New plot name: '
  )
  plot_file <-  file.path(dirname(path_file),paste0(ifelse(nchar(nnm),nnm,basename(path_file)),'_','.pdf'))
}
pdf(file = plot_file,
    paper = 'a4',
    height = 10,
    bg = 'white',
    useDingbats = F
)
par(mfrow = c(4,1),
    mar = c(3,5,0,0),
    oma = c(1.5,0,1.5,0))

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
       axis(side = 2#,
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
        abline(h = c(-90, 0, 90),
              col = 'black',
              lwd = 0.25
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