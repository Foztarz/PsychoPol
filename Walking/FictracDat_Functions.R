# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 01 24
#     MODIFIED:	James Foster              DATE: 2022 01 25
#
#  DESCRIPTION: A set of functions to load a set of ".dat" files exported from 
#               fictrac,export speed and angle and their moving averages, 
#               save summary plots and compile a file for the whole dataset.
#               
#       INPUTS: 
#               
#      OUTPUTS: 
#
#	   CHANGES: - Combine within folder
#             - Find animal and experiment names
#
#   REFERENCES: Moore RJD, Taylor GJ, Paulk AC, Pearson T, van Swinderen B, 
#               Srinivasan MV (2014). 
#               FicTrac: a visual method for tracking spherical motion and 
#               generating fictive animal paths. 
#              Journal of neuroscience methods 225, 106–19. 
#              https://doi.org/10.1016/j.jneumeth.2014.01.010
#              https://github.com/rjdmoore/fictrac
# 
#       USAGE:  Source in another script or press ctrl+shift+s to add to workspace
#TODO   ---------------------------------------------
#TODO   
#- Read in data   +
#- Add variables  +
#- Save results +
#- Combine folder +
#- Combine day  +
#- Combine all  +
#- Speed up with data.table  ...
#- Externalise cluster  ...
#- Raster plot

# General use -------------------------------------------------------------
IsWin = function(...)
{
  #Check the operating system and assign a logical flag (TRUE or FALSE)
  sys_win = Sys.info()[['sysname']] == 'Windows'
  return(sys_win)
}

# . Select files ---------------------------------------------------------
FT_select_dat = function(sys_win = IsWin())
{
  #On computers set up by JMU Würzburg, use user profile instead of home directory
  if(sys_win){
    #get rid of all the backslashes
    ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))
  }else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
    ltp = Sys.getenv('HOME')#Easier on Mac
  }
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
  return(path_file)
}

FT_select_folder = function(file_type = "_proc.csv.gz",
                            sys_win = Sys.info()[['sysname']] == 'Windows'
                            )
{
  #On computers set up by JMU Würzburg, use user profile instead of home directory
  if(sys_win){
    #get rid of all the backslashes
    ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))
  }else
  {#Root directory should be the "HOME" directory on a Mac (or Linux?)
    ltp = Sys.getenv('HOME')#Easier on Mac
  }
  msg = if(sys_win)
    {
    paste0('Please a folder containing ',#message to display
              '"',file_type,'"',
              ' files')
    }else
    {
    paste('Please select any',
          '"',file_type,'"',
          'file in the correct folder')
    }
  here_path = tryCatch(expr = #look in the folder containing this file: sys.frame(1)$ofile
                         {file.path(dirname(sys.frame(1)$ofile))},
                       error = function(e)
                       {#if that fails, try to find the "Documents" folder
                         file.path(ltp,'Documents', 
                                   '*',file_type)
                       }
  )
  # set path to files
  if(sys_win){#choose.dir is only available on Windows
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    path_folder  = choose.dir(
      default = here_path,#look where the function is stored
      caption = msg
    )
  }else{
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    path_folder = dirname(file.choose(new=F))
  }
  #show the user the path they have selected
  if(is.null(path_folder))
  {stop('Nothing selected/ no folder found.')}else
  {print(path_folder)}
  return(path_folder)
}

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

# Averaging functions ---------------------------------------------------
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


# Read in, calculate, write -----------------------------------------------

FT_read_write = function(path_file = FT_select_dat(sys_win = IsWin()),#path to the ".dat" file
                         ball_radius = 25,#mm
                         av_window = 5.0,#number of seconds to smooth over for averaging
                         csv_sep_load = ',',#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
                         speedup_parallel = TRUE, #Use the parallel package to speed up calculations
                         speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
                         compress_csv = TRUE, #Compress to ".gz" to save space?
                         verbose = TRUE,
                         clust = if(speedup_parallel)
                           {makeCluster(parallel::detectCores() - 1,type="SOCK")}else
                           {null}
)
{

# . Required packages -----------------------------------------------------
 invisible(
   { # hide verbose loading messages
    library(circular, quietly = TRUE)
    if(speedup_parallel){library(parallel, quietly = TRUE)}
    }
 )
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
  adata = if(speedup_data.table)
  {
  data.table::fread(
    file = path_file,#read from user-selected file
    sep = csv_sep_load,#user specified comma separation
    header = FALSE,#Fictrac does not produce headers
    col.names = cnames # best to set these here
    #other parameters can be added here for troubleshooting
  )
  }else
  {
    read.table(file = path_file,#read from user-selected file
               sep = csv_sep_load,#user specified comma separation
               header = FALSE,#Fictrac does not produce headers
               fileEncoding = ifelse(test = utf8BOM, #If the file contains Byte Order Markers
                                     yes = "UTF-8-BOM",#read in using the appropriate format
                                     no = ""), #if not, R can guess
               col.names = cnames # best to set these here
               #other parameters can be added here for troubleshooting
    )
  }
  
  # Conversions -------------------------------------------------------------
  
  # . Derive variables ------------------------------------------------------
  fps = 1/mean(diff(adata$time_stamp))*1e3 # frames per second
  
  # . Add variables ---------------------------------------------------------
  # . . Set up parallel processing, if used -----------------------------------
  if(speedup_parallel)
  {
    if(verbose)
    {message('Using parallel processing...')}
    #Benefits from some parallel processing, but setting up the cluster is slow
    clt = clust
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

# . . Variable definitions ------------------------------------------------

  if(speedup_data.table)
  {
    #has a slightly different syntax
    #N.B. Cannot call variable until after it has been created
    #data.table method
    adata[, `:=`( #assign from list call
                     experimental_time = (time_stamp - min(time_stamp))/1e3, # seconds since start
                     angle = circular(deg(heading_integrated),#180 is forwards!
                                      type = 'angles',
                                      unit = 'degrees',
                                      template = 'geographics',
                                      modulo = '2pi',
                                      zero = pi/2,
                                      rotation = 'clock'
                                     )
                     )
          ]#,
    adata[, `:=`( #assign from list call
                     angle_instantaneous = circular(deg(heading_instantaneous),#180 is forwards!
                                                    type = 'angles',
                                                    unit = 'degrees',
                                                    template = 'geographics',
                                                    modulo = '2pi',
                                                    zero = pi/2,
                                                    rotation = 'clock'
                     ),
                     speed = speed_movement*ball_radius*fps, #mm/s
                     x_pos = x_int*ball_radius, #mm
                     y_pos = y_int*ball_radius, #mm
                     angle_speed = c(0,diff(deg(heading_integrated)))*fps, #deg/s
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
                     },
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
                     },
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
                     )
          ]#,
    adata[, `:=`( #assign from list call
                     smooth_turn = predict( # fit a spline and predict its values across all times
                       smooth.spline(x = (1:length(angle))[!is.na(ma_turn)], #use only times when speed was calculated
                                     y = ma_turn[!is.na(ma_turn)]), #use only speeds where speed was calculated
                       x = 1:length(angle) # predict for all times
                     )$y
                     )
          ]#,
    adata[, `:=`( #assign from list call
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
                     },
                     x_speed = c(NA, diff(x_pos))*fps, #forwards speed in mm/s
                     y_speed = c(NA, diff(y_pos))*fps #sideways speed in mm/s
    )
    ]
    adata[, `:=`( #assign from list call
                     ground_speed = sqrt(x_speed^2 + y_speed^2), #speed in mm/s
                     straight_distance = sqrt(x_pos^2 + y_pos^2) #straight-line distance in mm
    )
    ]
    adata[, `:=`( #assign from list call
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
    )
    ]#end of data.table
  }else
  {
  
  #data.frame method
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
  }
  #close the parallel cluster
  if(speedup_parallel){ if(missing(clust)){stopCluster(clt)} }#only close internal cluster
  #  Save data --------------------------------------------------------------
  new_name = paste0(basename(path_file),
                    '_proc',
                    '.csv',
                    ifelse(test = compress_csv,
                           yes = '.gz',
                           no = ''
                    )
  )
  if(verbose)
  {message('Saving data as: ', new_name)}
  csv_file = file.path(dirname(path_file),
                       new_name
                      )
  if(speedup_data.table)
  {
    data.table::fwrite(
            x = adata,
            file = csv_file,#N.B. data.table >= 1.12.4 can write to .gz automatically
            row.names = FALSE,
            sep = ','
    )
  }else
  {
    write.csv(
              x = adata,
              file = if(compress_csv)
              {gzfile(description = csv_file)}else
              {csv_file},
              row.names = FALSE
    )
  }
  return(csv_file)
}


# Combine files in one folder ---------------------------------------------
FT_combine_folder = function(path_folder = FT_select_folder(),
                             file_type = '_proc.csv.gz',#N.B. currently only for this type
                             compress_txt = TRUE,
                             verbose = TRUE
                             )
{
  file_regex = paste0(file_type, '$')
  # . Find files ---------------------------------------------------------------
  names_files = list.files(path = path_folder,
                           pattern = file_regex,
                           recursive = if(file_type %in% #these files are stored in the folder
                                          c('_proc.csv.gz', '_proc.txt.gz')
                                         )
                                       {FALSE}else{TRUE}#other types may be in subfolders
                          )
  #show the user the path they have selected
  if(is.null(names_files)|!length(names_files))
  {stop('No "',file_type,'" files found in \n', path_folder)}else
  {message('Files found:\n',paste0(names_files,'\n'),
           '\n------------------------------------------')}

  # . Read in files ------------------------------------------------------------
  if(verbose)
  {message('Reading in files from:\t', basename(path_folder), '\nplease be patient...')}
  tryCatch(#Perform no further analysis if the file doesn't load
    {
      adata = lapply(X = lapply(file.path(path_folder, names_files), gzfile),
                     FUN = read.table,
                     sep = switch(EXPR = file_type,
                                  `_proc.csv.gz` = ',', 
                                  `_proc.txt.gz` = ' ',# '\t',
                                  '\t'
                                  ),
                     header = TRUE
      )#,#read from user-selected file
    },
    error = function(e)
    {
      stop(
        paste0('One or more files in "',
               basename(path_folder),
               '" could not be loaded!\n',
               e)
      )
    }
  )
  if(verbose)
  {message('All files in "',basename(path_folder), '" loaded successfully')}
  names(adata) = names_files

  # . Organise data ---------------------------------------------------------
  if(file_type == '_proc.csv.gz')
  {
  #check sample rates for each
  sample_rates = lapply(X = adata,
                        FUN = function(x)
                        {with(x, 1/mean(diff(experimental_time)))}
  )#Hz
  if(verbose)
  {
    if(sum(diff(unlist(sample_rates))))
      {
        warning('Sample rates differ between recordings\n',
               'proceed with caution!')
      }else
      {
       message('All recordings have a sample rate of ',
               sample_rates[[1]],
               ' Hz')
      }
  }
  }
  #combine all data into a single data frame
  adata_frame = do.call(what = rbind,
                        args = adata)
  #find names for tracks
  TrackNamer = function(txt)
  {
    regmatches(m = regexpr(pattern = '^([^.]+)',
                           text = txt),
               x = txt)
  }
  naming_expression = switch(
    EXPR = file_type,
    `_proc.csv.gz` = 
      {
  adata_frame$track = TrackNamer(rownames(adata_frame))
  adata_frame = within(adata_frame,
                       {
                         bumblebee = sub(pattern = 'Bumblebee ',
                                         x = basename(dirname(path_folder)),
                                         replacement = ''
                                         )
                         condition = regmatches(
                                         m = regexpr(pattern = '[^ -][^-]*$',
                                                     text = basename(path_folder)
                                                    ),
                                         x = basename(path_folder)
                                         )
                         date = regmatches(
                                         m = regexpr(pattern = '[^ -][^-]*$',
                                                     text = basename(
                                                             dirname(dirname(path_folder))
                                                           )
                                                    ),
                                         x = basename(
                                           dirname(dirname(path_folder))
                                                     )
                                         )
                       }
                      )
      }, #if it is the csv, find track, bumblebee and condition
  `_proc.txt.gz` = 
    { #If it is the txt, then the date is the rowname
      # adata_frame$date = TrackNamer(rownames(adata_frame))
    },
  {}
  )
  # .  Save data --------------------------------------------------------------
  txt_file = file.path(dirname(path_folder),#store outside this folder
                        paste0(basename(path_folder),
                               '_proc',
                               '.txt',
                               if(compress_txt)
                                 {'.gz'}else
                                 {''}
                               )
  )
  if(verbose)
  {
    message('Saving data as:\n',
            gsub(pattern = '/',
                 x = txt_file,
                 replacement = '\n')
    )
  }
  write.table(x = adata_frame,
              file = if(compress_txt)
                        {gzfile(txt_file)}else
                        {txt_file},
              sep = '\t',
              row.names = FALSE
  )
  return(txt_file)
}

