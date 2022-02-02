#FOR A 'CLEAN' RUN, RESTART Rstudio (ctrl+shift+F10)
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 01 12
#     MODIFIED:	James Foster              DATE: 2022 01 24
#
#  DESCRIPTION: Loads a ".dat" file saved by fictrac and organises data into
#               speed and angle.
#               
#       INPUTS: A ".dat" csv file with 23 columns specified in : 
#               https://github.com/rjdmoore/fictrac/blob/master/doc/data_header.txt
#               User should specify analysis details (line 190).
#               
#      OUTPUTS: Results table (.csv) saved (.gz compression) in original folder.
#
#	   CHANGES: - Compression
#             - Legends
#
#   REFERENCES: Moore RJD, Taylor GJ, Paulk AC, Pearson T, van Swinderen B, 
#               Srinivasan MV (2014). 
#               FicTrac: a visual method for tracking spherical motion and 
#               generating fictive animal paths. 
#              Journal of neuroscience methods 225, 106–19. 
#              https://doi.org/10.1016/j.jneumeth.2014.01.010
#              https://github.com/rjdmoore/fictrac
# 
#       USAGE:  Fill out user input (lines 190-200), then press ctrl+shift+s to run
#TODO   ---------------------------------------------
#TODO   
#- Read in data   +
#- Extract speed   +
#- Extract heading  +
#- Choose scale mean speed  +
#- Choose scale acceleration  +
#- Save results +
#- Figure legend +
#- Fast version ?
#- Ground truth turn speed with "z_ball"

# Load packages ----------------------------------------------------------
require(circular)#for handing angles

# Useful functions --------------------------------------------------------
#Convert any angle in degrees to (-180,180)
Mod360.180 = function(x)
{
  deg(
    atan2(y = sin(rad(x)),
          x = cos(rad(x))
    )
  )
}

#Open file with default program on any OS
# https://stackoverflow.com/a/35044209/3745353
shell.exec.OS  <- function(x){
  # replacement for shell.exec (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
  {return(base::shell.exec(x))}else
  {comm <- paste0('open "',x,'"')
  return(system(comm))}
}
# . Averaging functions ---------------------------------------------------
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
  return(
    mean.circular(#use the method for an object of class "circular"
      x = circular(dta[winmin:winmax],#data in the window, if the window is in the data
                   type = 'angles',
                   unit = 'degrees',
                   template = 'geographics',
                   modulo = '2pi'
      )
    )
    #N.B. conversion to class circular faster than calculating from 1st principles in R
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
    #N.B. conversion to class circular faster than calculating from 1st principles in R
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
  dff = diff( # sequential differences within a vector of data
    x = dta[winmin:winmax], #data in the window
  )
  Mod360.180 = function(x)
  {#use atan2 to convert any angle to the range (-180,180)
    deg(
      atan2(y = sin(rad(x)),
            x = cos(rad(x))
      )
    )
  }
  dff = sapply(X = dff,#convert all angles to (-180,180)
               FUN = Mod360.180
  ) 
  return(
    # mean( # default method for mean, no missing values allowed
    median( # default method for median, no missing values allowed
      dff
    )*hz #number of data collected per second, units returned are deg/s
  )
}
#Calculate average speed from a vector of distances
MAspeed <- function(i, #index
                    dta_x, #vector of data
                    dta_y, #vector of data
                    window = 1, # time window in seconds
                    hz = 100,  # sampling rate
                    method = 'median' # mean between frames or distance across interval
)
{
  n = window*hz # number of samples to use
  winmin = i-round(n/2)+1 #index of window start 
  winmax = i+round(n/2)-1 #index of window end
  if(winmin<1){return(NA)}#if window starts outside data, don't calculate
  if(winmax>length(dta_x)){return(NA)}#if window ends outside data, don't calculate
  return(
    switch(EXPR = method,
           `total distance` = sqrt( # maximum distance travelled over time interval (not necessarily from start to end)
             diff(dta_x[c(winmin,winmax)])^2 + 
               diff(dta_y[c(winmin,winmax)])^2  
           )/window, #window size in seconds, units returned are mm/s
           mean =   mean(
             x = sqrt(
               diff(dta_x[winmin:winmax])^2 +
                 diff(dta_y[winmin:winmax])^2
             ),
             na.rm = T
           )*hz, #window size in seconds, units returned are mm/s
           median =   median(
             x = sqrt(
               diff(dta_x[winmin:winmax])^2 +
                 diff(dta_y[winmin:winmax])^2
             ),
             na.rm = T
           )*hz #window size in seconds, units returned are mm/s
    )
  )
}

# Input Variables ----------------------------------------------------------
#  .  User input -----------------------------------------------------------
ball_radius = 25#mm
av_window = 5.0#number of seconds to smooth over for averaging
angle_unit = "degrees" # "degrees" or "radians"
point_col = "darkblue" # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
trend_col = "darkgreen" # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
save_type = "pdf"# "png"#
csv_sep_load = ','#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
csv_sep_write = ','#Results in a comma separated or semicolon separated csv? For tab sep, use "\t"
compress_csv = TRUE #Compress to ".gz" to save space?
speedup_parallel = TRUE #Use the parallel package to speed up calculations

#Check the operating system and assign a logical flag (TRUE or FALSE)
sys_win = Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp = Sys.getenv('HOME')#Easier on Mac
}


# . Select files ---------------------------------------------------------
msg = paste('Please select the',#message to display
            '".dat"',
            'file')
here_path = tryCatch(expr = #look in the folder containing this file: sys.frame(1)$ofile
                       {file.path(dirname(sys.frame(1)$ofile))},
                     error = function(e)
                     {#if that fails, try to find the "Documents" folder
                       file.path(ltp,'Documents', 
                                      '*.dat')
                     }
)
# set path to files
if(sys_win){#choose.files is only available on Windows
  message('\n\n',msg,'\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file  = choose.files(
    default = here_path,#look where the function is stored
    caption = msg
  )
}else{
  message('\n\n',msg,'\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file = file.choose(new=F)
}
#show the user the path they have selected
if(is.null(path_file))
{stop('No file selected.')}else
{print(path_file)}


# Read in file ------------------------------------------------------------
#Column headers should indicate:
# COL     PARAMETER                       DESCRIPTION
# 1       frame counter                   Corresponding video frame (starts at #1).
# 2-4     delta rotation vector (cam)     Change in orientation since last frame,
#                                         represented as rotation angle/axis (radians)
#                                         in camera coordinates (x right, y down, z
#                       forward).
# 5       delta rotation error score      Error score associated with rotation
#                                         estimate.
# 6-8     delta rotation vector (lab)     Change in orientation since last frame,
#                                         represented as rotation angle/axis (radians)
#                                         in laboratory coordinates (see
#                           *configImg.jpg).
# 9-11    absolute rotation vector (cam)  Absolute orientation of the sphere
#                                         represented as rotation angle/axis (radians)
#                                         in camera coordinates.
# 12-14   absolute rotation vector (lab)  Absolute orientation of the sphere
#                                         represented as rotation angle/axis (radians)
#                                         in laboratory coordinates.
# 15-16   integrated x/y position (lab)   Integrated x/y position (radians) in
#                                         laboratory coordinates. Scale by sphere
#                                         radius for true position.
# 17      integrated animal heading (lab) Integrated heading orientation (radians) of
#                                         the animal in laboratory coordinates. This
#                                         is the direction the animal is facing.
# 18      animal movement direction (lab) Instantaneous running direction (radians) of
#                                         the animal in laboratory coordinates. This is
#                                         the direction the animal is moving in the lab
#                                         frame (add to animal heading to get direction
#                                               in world).
# 19      animal movement speed           Instantaneous running speed (radians/frame)
#                                         of the animal. Scale by sphere radius for
#                                         true speed.
# 20-21   integrated forward/side motion  Integrated x/y position (radians) of the
#                                         sphere in laboratory coordinates neglecting
#                                         heading. Equivalent to the output from two
#                                         optic mice.
# 22      time_stamp                       Either position in video file (ms) or frame
#                                         capture time (ms since epoch).
# 23      sequence counter                Position in current frame sequence. Usually
#                                         corresponds directly to frame counter, but
#                                         can reset to 1 if tracking is reset.
# 24      delta time_stamp                 Time (ms) since last frame.
# 25      alt. time_stamp                  Frame capture time (ms since midnight).

#Check for Byte Order Marks, which can make a mess
if(grepl(x = readLines(path_file,
                       n = 1,
                       warn = F),#check the first line of the file
         pattern = "ï|ÿ|þ")#common BOM renderings, are there any others?
)
{utf8BOM = T}else{utf8BOM = F}

# . Set up names ----------------------------------------------------------
cnames = c('frame_counter', #1
                    'x_cam', #2  change radians, camera coords
                    'y_cam', #3  change radians, camera coords
                    'z_cam', #4  change radians, camera coords
                    'cam_error',#5
                    'x_ball', #6  change radians, lab coords
                    'y_ball', #7  change radians, lab coords
                    'z_ball', #8  change radians, lab coords
                    'x_cam_abs', #9   absolute orientation radians, camera coords
                    'y_cam_abs', #10  absolute orientation radians, camera coords
                    'z_cam_abs', #11  absolute orientation radians, camera coords
                    'x_ball_abs', #12  absolute orientation radians, lab coords
                    'y_ball_abs', #13  absolute orientation radians, lab coords
                    'z_ball_abs', #14  absolute orientation radians, lab coords
                    'x_int', #15 integrated x position (radians), lab coords
                    'y_int', #16 integrated y position (radians), lab coords
                    'heading_integrated', #17 integrated orientation (facing direction), lab coords
                    'heading_instantaneous', #18  integrated orientation (moving direction), lab coords
                    'speed_movement', #19 radian/frame
                    'x_speed_int', #20 integrated x position (radians) of the sphere, neglecting heading
                    'y_speed_int', #21 integrated y position (radians) of the sphere, neglecting heading
                    'time_stamp', #22
                    'frame_counter' #23 rest not available in 2.03
                    # 'delta_timestap', #24 change in time since last frame
                    # 'ms_since_midnight' #25 time of capture
)
#read in data
adata = 
  read.table(file = path_file,#read from user-selected file
             sep = csv_sep_load,#user specified comma separation
             header = FALSE,#Fictrac does not produce headers
             fileEncoding = ifelse(test = utf8BOM, #If the file contains Byte Order Markers
                                   yes = "UTF-8-BOM",#read in using the appropriate format
                                   no = ""), #if not, R can guess
             skip = 88,
             col.names = cnames # best to set these here
             #other parameters can be added here for troubleshooting
  )




# . Inspect ---------------------------------------------------------------
# summary(adata)
# View(adata)#show the user the data that was read in

# #ball movements in 3D
# with(adata,
#      {
#        scatter3D(x = x_ball,
#             y = y_ball,
#             z = z_ball,
#             colvar = frame_counter,
#             col = hcl.colors(n = length(frame_counter),
#                              palette = 'viridis',
#                              alpha = 0.3),
#             type = 'o',
#             pch = 19,
#             lty = 3
#             )
#      }
# )
# with(adata,
#      {
#        plot(x = y_int,
#             y = x_int,
#             xlim = c(-1,1)*max(abs(c(y_int, x_int)), na.rm = T),
#             ylim = c(-1,1)*max(abs(c(y_int, x_int)), na.rm = T),
#             col = hcl.colors(n = length(frame_counter),
#                              palette = 'viridis'),
#             type = 'o',
#             pch = 19
#             )
#      }
# )
par(mfrow = c(2,1), 
    mar = c(0,0,0,0)
    )
with(adata,
     {
       plot(x = y_int,
            y = x_int,
            xlim = c(-1,1)*max(abs(c(y_int, x_int)), na.rm = T),
            ylim = c(-1,1)*max(abs(c(y_int, x_int)), na.rm = T),
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis',
                             alpha = 0.5),
            type = 'o',
            pch = 19,
            axes = F
            )
       abline(h = pretty(c(-1,1)*max(abs(c(y_int, x_int)), na.rm = T)),
              v = pretty(c(-1,1)*max(abs(c(y_int, x_int)), na.rm = T)),
              col = gray(level = 50/255,
                         alpha = 0.25),
              lwd = 0.25)
       plot.circular(circular(NA,
                              type = 'angles',
                              unit = 'degrees',
                              rotation = 'clock',
                              zero = pi/2,
                              modulo = '2pi'
       ),
       labels = 0:3*90,
                     xlim = c(-1,1)*max(frame_counter),
                     ylim = c(-1,1)*max(frame_counter),
                     shrink = 1/max(frame_counter)
       )
       lines.circular(x = circular(deg(heading_integrated)-180,
                                   type = 'angles',
                                   unit = 'degrees',
                                   template = 'geographics',
                                   modulo = '2pi'
                                 ),
            y = frame_counter/max(frame_counter)-1,
            # zero = -pi/2,
            col = adjustcolor(point_col, alpha.f = 0.5),
            type = 'p',
            pch = 19,
            lty = 2,
            cex = 0.75
            )
     }
)

# Conversions -------------------------------------------------------------

# . Derive variables ------------------------------------------------------
fps = 1/mean(diff(adata$time_stamp))*1e3 # frames per second

# . Set up parallel processing, if used -----------------------------------
if(speedup_parallel)
{
  require(parallel)
  message('Using parallel processing...')
  #Benefits from some parallel processing, but setting up the cluster is slow
  avail.cores <- parallel::detectCores() - 1
  clt = makeCluster(avail.cores,# run as many as possible
                   type="SOCK")
  clusterExport(cl = clt,#the cluster needs some variables&functions outside parLapply
                list('adata',
                     'fps',
                     'MAmeanang',
                     'MAmeanvec',
                     'MAturnspeed',
                     'mean.circular',
                     'rho.circular',
                     'circular',
                     'deg',
                     'rad'),
                environment()#needs to be reminded to use function environment, NOT global environment
  )
}

# . Add variables ---------------------------------------------------------
adata = within(adata, 
               {
                 experimental_time = (time_stamp - min(time_stamp))/1e3 # seconds since start
                 angle = circular(deg(heading_integrated),#180 is forwards!
                                  type = 'angles',
                                  unit = 'degrees',
                                  template = 'geographics',
                                  modulo = '2pi',
                                  zero = pi/2,
                                  rotation = 'clock'
                                  )
                 angle_instantaneous = circular(deg(heading_instantaneous),#180 is forwards!
                                  type = 'angles',
                                  unit = 'degrees',
                                  template = 'geographics',
                                  modulo = '2pi',
                                  zero = pi/2,
                                  rotation = 'clock'
                                  )
                 speed = speed_movement*ball_radius*fps #mm/s
                 x_pos = x_int*ball_radius #mm
                 y_pos = y_int*ball_radius #mm
                 z_turn = deg(-z_ball)*fps #°/s
                 angle_speed = c(0,diff(deg(heading_integrated)))*fps #deg/s
                 ma_angle = if(speedup_parallel)
                             {
                               parSapply(cl = clt,
                                     X = 1:length(angle),#all indices in angle
                                     FUN = MAmeanang,#the mean angle function
                                     dta = angle,#all angles observed
                                     window = av_window,#window size (s)
                                     hz = fps #sample rate (Hz)
                             )
                             }else
                             {
                               sapply(X = 1:length(angle),#all indices in angle
                                     FUN = MAmeanang,#the mean angle function
                                     dta = angle,#all angles observed
                                     window = av_window,#window size (s)
                                     hz = fps #sample rate (Hz)
                                     )
                             }
                 ma_rho = if(speedup_parallel)
                             {
                             parSapply(cl = clt,
                                       X = 1:length(angle),#all indices in angle
                                           FUN = MAmeanvec,#the mean vector function
                                           dta = angle,#all angles observed
                                           window = av_window,#window size (s)
                                           hz = fps #sample rate (Hz)
                                   )
                             }else
                             {
                             sapply(X = 1:length(angle),#all indices in angle
                                           FUN = MAmeanvec,#the mean vector function
                                           dta = angle,#all angles observed
                                           window = av_window,#window size (s)
                                           hz = fps #sample rate (Hz)
                                   )
                             }
                 ma_turn = if(speedup_parallel)
                             {
                               parSapply(cl = clt,
                                         X = 1:length(angle),#all indices in angle
                                              FUN = MAturnspeed, #the mean speed function
                                              dta = deg(heading_integrated),#raw angles observed
                                              window = av_window,#window size (s)
                                              hz = fps #sample rate (Hz)
                             )
                             }else
                             {
                               sapply(X = 1:length(angle),#all indices in angle
                                              FUN = MAturnspeed, #the mean speed function
                                              dta = deg(heading_integrated),#raw angles observed
                                              window = av_window,#window size (s)
                                              hz = fps #sample rate (Hz)
                             )
                             }
                 smooth_turn = predict( # fit a spline and predict its values across all times
                   smooth.spline(x = (1:length(angle))[!is.na(ma_turn)], #use only times when speed was calculated
                                 y = ma_turn[!is.na(ma_turn)]), #use only speeds where speed was calculated
                   x = 1:length(angle) # predict for all times
                 )$y
                 ma_accel = if(speedup_parallel)
                             {
                               parSapply(cl = clt,
                                         X = 1:length(smooth_turn),#all indices in angle
                                         FUN = MAturnspeed, #the mean speed function, here converts speed to acceleration
                                         dta = smooth_turn, #use smoothed speeds
                                         window = av_window,#window size (s)
                                         hz = fps #sample rate (Hz)
                                         )
                             }else
                             {
                               sapply(X = 1:length(smooth_turn),#all indices in angle
                                               FUN = MAturnspeed, #the mean speed function, here converts speed to acceleration
                                               dta = smooth_turn, #use smoothed speeds
                                               window = av_window,#window size (s)
                                               hz = fps #sample rate (Hz)
                             )
                             }
                 x_speed = c(NA, diff(x_pos))*fps #forwards speed in mm/s
                 y_speed = c(NA, diff(y_pos))*fps #sideways speed in mm/s
                 ground_speed = sqrt(x_speed^2 + y_speed^2) #speed in mm/s
                 straight_distance = sqrt(x_pos^2 + y_pos^2) #straight-line distance in mm
                 ma_speed = if(speedup_parallel)
                             {
                             parSapply(cl = clt,
                                       X = 1:length(straight_distance),#all indices in angle
                                       FUN = MAspeed, #the mean speed function
                                       dta_x = x_pos,#x coordinate
                                       dta_y = y_pos,#y coordinate
                                       window = av_window,#window size (s)
                                       hz = fps, #sample rate (Hz)
                                       method = 'mean' #average between frame
                                     )
                             }else
                             {
                             sapply(X = 1:length(straight_distance),#all indices in angle
                                    FUN = MAspeed, #the mean speed function
                                    dta_x = x_pos,#x coordinate
                                    dta_y = y_pos,#y coordinate
                                    window = av_window,#window size (s)
                                    hz = fps, #sample rate (Hz)
                                    method = 'mean' #average between frame
                                   )
                             }
               }
               )
#close the parallel cluster
if(speedup_parallel){ stopCluster(clt) }
#  Save data --------------------------------------------------------------
message('Saving data')
csv_file = file.path(dirname(path_file),
                      paste0(basename(path_file),
                             '_proc',
                             '.csv',
                             ifelse(test = compress_csv,
                                    yes = '.gz',
                                    no = ''
                                    )
                             )
)
write.csv(x = adata,
          file = if(compress_csv)
                  {gzfile(description = csv_file)}else
                  {csv_file},
          row.names = FALSE
)

# Plot data ---------------------------------------------------------------
message('Plotting data')

# . Set up plot variables -------------------------------------------------
plt_speed_max = 200
plt_accel_scale = 5
plt_turn_ax = 45
plt_leg_pos = 'topleft'
plt_leg_inset = c(0,-0.01)
plt_leg_cex = 0.5

# . Set up plot area ------------------------------------------------------
plot_file = file.path(dirname(path_file), 
                      paste0(basename(path_file),'_','.', save_type))
if(file.exists(plot_file))
{
  message('A plot called "', basename(plot_file), '" already exists in this folder.')
  nnm = readline(prompt = 'New plot name: '
  )
  
  plot_file = file.path(dirname(path_file),
                          paste0(ifelse(nchar(nnm),nnm,basename(path_file)),'_','.', save_type))
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
       png = par(mfrow = c(2,1),
                 mar = c(3,5,0,3),
                 oma = c(1.5,0,1.5,0),
                 cex = 1.5
       ),
       par(mfrow = c(2,1),
           mar = c(3,5,0,3),
           oma = c(1.5,0,1.5,0))
)

# . Plot raw data ---------------------------------------------------------
par(pty = 's',
    mar = c(3,0,0,0)) # set plot area to square?
#trajectory
with(adata,
     {
       plot(x = y_pos,
            y = x_pos,
            xlim = c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T), 
            ylim = c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T), 
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis',
                             alpha = 0.5),
            xlab = 'lateral position (mm)',
            ylab = 'forwards position (mm)',
            type = 'o',
            pch = 19
       )
       points(x = y_pos[which.min(abs(experimental_time - 60))],
              y = x_pos[which.min(abs(experimental_time - 60))],
              pch = 3,
              col = 'darkred'
              )
       abline(h = pretty(c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T)), 
              v = pretty(c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T)), 
              col = adjustcolor('black', alpha = 0.5),
              lwd = 0.25
       )
       
     }
)
pretty_time = with(adata, paste0(pretty(range(experimental_time)), 's'))
legend(x = plt_leg_pos,
       inset= plt_leg_inset,
       xpd = TRUE,
       legend = c(rev(pretty_time), '1 min'),
       pch = c(rep(x = 15, times = length(pretty_time)),3),
       col = c(
               rev(
                 hcl.colors(n = length(pretty_time),
                            palette = 'viridis',
                            alpha = 0.5)
                 ),
               'darkred'
               ),
       pt.cex = 1.2,
       cex = plt_leg_cex
)
#Circular heading
with(adata,
     {
      plot.circular(circular(NA,
                             type = 'angles',
                             unit = 'degrees',
                             rotation = 'clock',
                             zero = pi/2,
                             modulo = '2pi'
      ),
      labels = 0:3*90,
      xlim = c(-1,1)*max(experimental_time),
      ylim = c(-1,1)*max(experimental_time),
      shrink = 1/max(experimental_time)
      )
      lines.circular(x = circular(angle-180,
                                  type = 'angles',
                                  unit = 'degrees',
                                  template = 'geographics',
                                  modulo = '2pi'
      ),
      y = experimental_time/max(experimental_time)-1,
      col = adjustcolor(point_col, alpha.f = 0.5),
      type = 'p',
      pch = 19,
      lty = 2,
      cex = 0.5
      )
      invisible(
        {
          lapply(X = 10*(0:(max(experimental_time)/10)),
                FUN =function(i)
                  {
                  lines.circular(
                    x = circular(x = seq(from = -pi, 
                                                      to = pi, 
                                                      length.out = 1e3),
                                              template = 'none'),
                    y = rep(x = i/max(experimental_time) - 1, 
                            times = 1e3),
                    col = gray(level = 0, alpha = 0.5)
                    )
                  }
                )
        }
      )
     }
)
lines.circular(
  x = circular(x = seq(from = -pi, 
                       to = pi, 
                       length.out = 1e3),
               template = 'none'),
  y = rep(x = 60/max(adata$experimental_time) - 1, 
          times = 1e3),
  col = adjustcolor('darkred', alpha.f = 0.5),
  lwd = 5
)
with(adata,
     {
axis(side = 1,
     at = 10*round(-(max(experimental_time/10)):(max(experimental_time/10)))/max(experimental_time),
     labels = abs(
                 10*round(-(max(experimental_time/10)):(max(experimental_time/10)))
                 ),
     cex.axis = plt_leg_cex/1.5
     )
     }
)
mtext(text = 'Time (sec)',
      outer = T,
      side = 1
)
# . Plot time series --------------------------------------------------

# . . Ground speed --------------------------------------------------------

par(pty = 'm',
    mfrow = c(4, 1),
    mar = c(3,5,0,3)
    )
#Raw data
with(adata,
     {
       plot(x = experimental_time,
            y = ground_speed,
            xlab = 'time since start',
            ylab = 'ground speed (mm/s)',
            ylim = c(0, max(x = c(ma_speed,plt_speed_max), na.rm = T)),
            type = 'p',
            pch = 19,
            cex = 0.1,
            col = adjustcolor(point_col, alpha.f = 20/256),
            axes = F
       )
       axis(side = 1,
            at = 10*(0:(max(experimental_time)/10)),
            labels = 10*(0:(max(experimental_time)/10))
       )
       axis(side = 2
       )
       abline(h = 10*(round(min(ground_speed, na.rm = T)/10):round(max(ground_speed, na.rm = T)/10)),
              v = c(0,60,120),
              col = rgb(0,0,0,0.1)
       )
       
     }
)
#Trendline
with(adata,
     {
       lines(x = experimental_time,
             y = ma_speed,
             cex = 0.1,
             col = trend_col,
       )
     }
)
legend(x = plt_leg_pos,
       inset= plt_leg_inset,
       xpd = TRUE,
       legend = c('instantaneous',
                  paste0('moving average (median: ',av_window,'s)')),
       lty = c(NA,1),
       pch = c(19,NA),
       col = c(point_col,
               trend_col),
       cex = plt_leg_cex
       )

# . . Fictive direction ---------------------------------------------------
#Raw data
with(adata,
     {
       plot(x = experimental_time,
            y = Mod360.180(angle),
            ylim = c(-180,180),
            xlab = 'time since start',
            ylab = 'fictive direction',
            type = 'p',
            pch = 19,
            cex = 0.1,
            col = adjustcolor(point_col, alpha.f = 20/256),
            axes = F
       )
       axis(side = 1,
            at = 10*(0:(max(experimental_time)/10)),
            labels = 10*(0:(max(experimental_time)/10))
       )
       axis(side = 2,
            at = 90*(round(-180/90):round(180/90)),
            labels = paste0(90*(round(-180/90):round(180/90)),
                            '°')
       )
       abline(h = 90*(round(-180/90):round(180/90)),
              v = c(0,60,120),
              col = rgb(0,0,0,0.1)
       )
       
     }
)
#Trendline
with(adata,
     {
       lines(x = experimental_time,
             y = Mod360.180(ma_angle),
             type = 'p',
             pch = 19,
             cex = 0.1,
             col = adjustcolor(col = trend_col, alpha.f = 0.3),
       )
     }
)
legend(x = plt_leg_pos,
       inset= plt_leg_inset,
       xpd = TRUE,
       legend = c('instantaneous',
                  paste0('moving average (mean angle: ', av_window,'s)')),
       lty = c(NA,1),
       pch = c(19,NA),
       col = c(point_col,
               trend_col),
       cex = plt_leg_cex
)


# . . Turning speed -------------------------------------------------------
par(pty = 'm',
    mar = c(3,5,0,3))
#Raw data
with(adata,
     {
       plot(x = NULL,
            xlim = range(experimental_time, na.rm = T),
            ylim = c(-1,1)*max(abs(range(z_turn, na.rm = T))),
            xlab = 'time (s)',
            ylab = paste0('median turning speed (°/s: ',av_window,'s)'),
            axes = F
       )
       axis(side = 1,
            at = 10*(0:(max(experimental_time)/10)),
            labels = 10*(0:(max(experimental_time)/10))
       )
       axis(side = 2,
            at = plt_turn_ax*(round(min(z_turn, na.rm = T)/plt_turn_ax):round(max(z_turn, na.rm = T)/15))
       )
       lines(x = experimental_time,
              y = z_turn,
              col = adjustcolor(point_col, alpha.f = 50/256),
              cex = 0.1,
              pch = 19
       )
       points(x = experimental_time,
              y = ma_turn,
              col = adjustcolor(trend_col, alpha.f = 10/256),
              cex = 0.5,
              pch = 19
       )
# Trendline
       lines(x = experimental_time,
             y = smooth_turn,
             col = trend_col
       )
       abline(h = c(-15, 0, 15),
              v = c(0,60,120),
              col = 'black',
              lwd = 0.25
       )
     }
)
#Acceleration
with(adata,
     {
       lines(x = experimental_time,
             y = ma_accel*plt_accel_scale,
             col = adjustcolor('darkred', alpha.f = 200/256)
       )
       axis(side = 4,
            line = -1,
            at = plt_accel_scale*plt_turn_ax*
              (round(min(ma_accel, na.rm = T)/plt_turn_ax):
                 round(max(ma_accel, na.rm = T)/plt_turn_ax)),
            labels = plt_accel_scale*plt_turn_ax*
              (round(min(ma_accel, na.rm = T)/plt_turn_ax):
                 round(max(ma_accel, na.rm = T)/plt_turn_ax)/
                 plt_accel_scale),
            col = 'darkred'
       )
       mtext(text = bquote(median ~ acceleration ~
                             '(°/s'^2 ~ 
                             .(av_window)*s ~ ")"),
             side = 4,
             cex = par('cex.lab')/1.5,
             line = 2
       )
     }
)
legend(x = plt_leg_pos,
       inset= plt_leg_inset,
       xpd = TRUE,
       legend = c('instantaneous',
                   paste0('median ', '(', av_window,'s)'),
                  'smoothing spline',
                  'acceleration'),
       lty = c(1,NA, 1, 1),
       pch = c(NA,19, NA, NA),
       col = c(point_col,
               trend_col,
               trend_col,
               'darkred'),
       cex = plt_leg_cex
)



# . . Mean vector length --------------------------------------------------
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
            at = 10*(0:(max(experimental_time)/10)),
            labels = 10*(0:(max(experimental_time)/10))
       )
       axis(side = 2,
            at = 0:5/5
       )
       lines(x = experimental_time,
             y = ma_rho,
             col = point_col
       )
       abline(h = c(0,1),
              v = c(0,60,120),
              col = 'black',
              lwd = 0.25
       )
       abline(h = sqrt(-log(c(0.05, 0.01)))/(av_window*fps),#Mean vector Rayleigh test p
              col = 'red',
              lty = c(3,2),
              lwd = 0.25
       )
       
     }
)
legend(x = plt_leg_pos,
       inset= plt_leg_inset,
       xpd = TRUE,
       legend = c(paste0('mean vector ', '(', av_window,'s)'),
                  'Rayleigh (p = 0.05)'),
       lty = c(1, 3),
       pch = c(NA, NA),
       col = c(point_col,
               'red'),
       cex = plt_leg_cex
)

# . . Outer labels --------------------------------------------------------

mtext(text = basename(path_file),
      outer = T, 
      side = 3
)
mtext(text = 'Time (sec)',
      outer = T,
      side = 1
)
# . Save plot -------------------------------------------------------------
dev.off()
shell.exec.OS(plot_file)


# # Plot summary ---
# 
# par(mfrow = c(3, 1))
# with(adata,
#      {
#        plot(x = y_pos,
#             y = x_pos,
#             xlim = c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T),
#             ylim = c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T),
#             col = hcl.colors(n = length(frame_counter),
#                              palette = 'viridis'),
#             type = 'o',
#             pch = 19
#        )
#        abline(h = pretty(c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T)), 
#               v = pretty(c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T)), 
#               col = adjustcolor('black', alpha = 0.7),
#               lwd = 0.25
#               )
#        plot(x = time_stamp,
#             y = angle,
#             col = hcl.colors(n = length(frame_counter),
#                              palette = 'viridis'),
#             type = 'p',
#             pch = 19
#        )
#        lines(x = time_stamp,
#              y = ma_angle,
#              col = 'magenta',
#              bg = 'white',
#              type = 'p',
#              pch = 21,
#              cex = 0.5
#        )
#        abline(h = 0, 
#               col = adjustcolor('black', alpha = 0.7),
#               lwd = 0.25
#               )
#        plot(x = time_stamp,
#             y = ma_turn,
#             col = hcl.colors(n = length(frame_counter),
#                              palette = 'viridis'),
#             pch = 19,
#             type = 'o',
#        )
#        lines(x = time_stamp,
#              y = smooth_turn,
#              col = 'magenta',
#              bg = 'white',
#              type = 'p',
#              pch = 21,
#              cex = 0.5
#        )
#        abline(h = 0, 
#               col = adjustcolor('black', alpha = 0.7),
#               lwd = 0.25
#               )
#      }
# )