#FOR A 'CLEAN' RUN, RESTART Rstudio (ctrl+shift+F10)
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 01 12
#     MODIFIED:	James Foster              DATE: 2022 01 13
#
#  DESCRIPTION: Loads a ".dat" file saved by fictrac and organises data into
#               speed and angle.
#               
#       INPUTS: A ".dat" csv file with 23 columns specified in : 
#               https://github.com/rjdmoore/fictrac/blob/master/doc/data_header.txt
#               User should specify analysis details (line 40).
#               
#      OUTPUTS: Results table (.csv) saved in the same place as the data.
#
#	   CHANGES: - 
#
#   REFERENCES: Moore RJD, Taylor GJ, Paulk AC, Pearson T, van Swinderen B, 
#               Srinivasan MV (2014). 
#               FicTrac: a visual method for tracking spherical motion and 
#               generating fictive animal paths. 
#              Journal of neuroscience methods 225, 106–19. 
#              https://doi.org/10.1016/j.jneumeth.2014.01.010
#              https://github.com/rjdmoore/fictrac
# 
#       USAGE:  Fill out user input (lines 40-45), then press ctrl+shift+s to run
#TODO   ---------------------------------------------
#TODO   
#- Read in data   
#- Extract speed   
#- Extract heading
#- Save results 





# Input Variables ----------------------------------------------------------
#  .  User input -----------------------------------------------------------
ball_radius = 25#mm
av_window = 10#number of seconds to smooth over for averaging
angle_unit = "degrees" # "degrees" or "radians"
point_col = "darkblue" # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
save_type ="png"# "pdf"# 
csv_sep_load = ','#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
csv_sep_write = ','#Results in a comma separated or semicolon separated csv? For tab sep, use "\t"

#Check the operating system and assign a logical flag (TRUE or FALSE)
sys_win <- Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp = Sys.getenv('HOME')#Easier on Mac
}
# . Load packages ----------------------------------------------------------
# require(plot3D)#in case anything needs to be plotted in 3D
require(circular)#for handing angles

# . Select files ---------------------------------------------------------
msg = paste('Please select the',
                   '".dat"',
            'file')
here_path = tryCatch(expr = 
                       {file.path(dirname(sys.frame(1)$ofile))},
                     error = function(e)
                     {
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
                       warn = F),#check the first line of the file, where "angle" should be written
         pattern = "ï|ÿ|þ")#common BOM renderings, are there any others?
)
{utf8BOM = T}else{utf8BOM = F}

#set names
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
             sep = csv_sep_load,
             header = FALSE,#Fictrac does not produce headers
             fileEncoding = ifelse(test = utf8BOM, #If the file contains Byte Order Markers
                                   yes = "UTF-8-BOM",#read in using the appropriate format
                                   no = ""), #if not, R can guess
             col.names = cnames # best to set these here
             #other parameters can be added here for troubleshooting
  )

# . Set up names ----------------------------------------------------------



# . Inspect ---------------------------------------------------------------
summary(adata)
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

with(adata,
     {
       plot.circular(circular(NA),
                     xlim = c(-1,1)*max(frame_counter),
                     ylim = c(-1,1)*max(frame_counter),
                     shrink = 1/max(frame_counter)
       )
       lines.circular(x = heading_integrated,
            y = frame_counter/max(frame_counter)-1,
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis'),
            type = 'p',
            pch = 19,
            lty = 3
            )
       # axis(1); axis(side = 2, at = 0:4 * pi/2, labels = 0:4*90 )
     }
)

# Conversions -------------------------------------------------------------

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


fps = mean(diff(adata$time_stamp))
adata = within(adata, 
               {
                 heading = circular(deg(heading_integrated),#deg
                                  type = 'angles',
                                  unit = 'degrees',
                                  template = 'geographics',
                                  modulo = '2pi',
                                  zero = pi/2,
                                  rotation = 'clock'
                                  )
                 angle = circular(deg(heading_instantaneous),#deg
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
                 angle_speed = c(0,diff(deg(adata$heading_instantaneous)))*fps #deg/s
                 ma_angle = sapply(X = 1:length(angle),#all indices in angle
                                   FUN = MAmeanang,#the mean angle function
                                   dta = angle,#all angles observed
                                   window = av_window,#window size (s)
                                   hz = fps #sample rate (Hz)
                 )
                 ma_rho = sapply(X = 1:length(angle),#all indices in angle
                                 FUN = MAmeanvec,#the mean vector function
                                 dta = angle,#all angles observed
                                 window = av_window,#window size (s)
                                 hz = fps #sample rate (Hz)
                 )
                 ma_turn = sapply(X = 1:length(angle),#all indices in angle
                                  FUN = MAturnspeed, #the mean speed function
                                  dta = deg(heading_instantaneous),#raw angles observed
                                  window = av_window,#window size (s)
                                  hz = fps #sample rate (Hz)
                 )
                 smooth_turn = predict( # fit a spline and predict its values across all times
                   smooth.spline(x = time_stamp[!is.na(ma_turn)], #use only times when speed was calculated
                                 y = ma_turn[!is.na(ma_turn)]), #use only speeds where speed was calculated
                   x = time_stamp # predict for all times
                 )$y
                 ma_accel = sapply(X = 1:length(smooth_turn),#all indices in angle
                                   FUN = MAturnspeed, #the mean speed function, here converts speed to acceleration
                                   dta = smooth_turn, #use smoothed speeds
                                   window = av_window,#window size (s)
                                   hz = fps #sample rate (Hz)
                 )
               }
               )

# Plot summary ------------------------------------------------------------

par(mfrow = c(3, 1))
with(adata,
     {
       plot(x = y_pos,
            y = x_pos,
            xlim = c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T),
            ylim = c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T),
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis'),
            type = 'o',
            pch = 19
       )
       abline(h = 0, 
              v = 0, 
              col = adjustcolor('black', alpha = 0.7),
              lwd = 0.25
              )
       plot(x = time_stamp,
            y = angle,
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis'),
            type = 'p',
            pch = 19
       )
       lines(x = time_stamp,
             y = ma_angle,
             col = 'magenta',
             bg = 'white',
             type = 'p',
             pch = 21,
             cex = 0.5
       )
       abline(h = 0, 
              col = adjustcolor('black', alpha = 0.7),
              lwd = 0.25
              )
       plot(x = time_stamp,
            y = ma_turn,
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis'),
            pch = 19,
            type = 'o',
       )
       lines(x = time_stamp,
             y = smooth_turn,
             col = 'magenta',
             bg = 'white',
             type = 'p',
             pch = 21,
             cex = 0.5
       )
       abline(h = 0, 
              col = adjustcolor('black', alpha = 0.7),
              lwd = 0.25
              )
     }
)
