# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 01 24
#     MODIFIED:	James Foster              DATE: 2022 04 08
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
#             - Raster plot
#             - Averaging across "phases"
#             - MAsmoothturn() added to avoid short .dat error 
#             - NA handling in acceleration axis labels
#             - jump handling and NA handling in experimental time
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
#- Speed up with data.table  +
#- Externalise cluster  +
#- Raster plot  +
#- Add labels in csv +
#- Recursive FT_select_folder +
#- Frequency analysis +
#- Fix alignment raster plot

# General use -------------------------------------------------------------

# . Select files ---------------------------------------------------------
FT_select_file = function(file_type = ".dat",
                          sys_win = Sys.info()[['sysname']] == 'Windows')
{
  #On computers set up by JMU Würzburg, use user profile instead of home directory
  if(sys_win){
    #get rid of all the backslashes
    ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))
  }else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
    ltp = Sys.getenv('HOME')#Easier on Mac
  }
  msg = paste('Please select the',#message to display
              '"', file_type,'"',
              'file')
  here_path = tryCatch(expr = #look in the folder containing this file: sys.frame(1)$ofile
                         {file.path(dirname(sys.frame(1)$ofile))},
                       error = function(e)
                       {#if that fails, try to find the "Documents" folder
                         file.path(ltp,'Documents', 
                                   paste0('*',file_type)
                                   )
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

FT_select_dat = function(sys_win = Sys.info()[['sysname']] == 'Windows')
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
    paste0('Please select a folder containing ',#message to display
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
      ), na.rm = T
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
      ), na.rm = T
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
      dff, na.rm = T
    )*hz #number of data collected per second, units returned are deg/s
  )
}

MAsmoothturn <- function(angle,
                         ma_turn)
{
       if(sum(!is.na(ma_turn)) > 10) # smoothing only works where there is some data
       {
        predict( # fit a spline and predict its values across all times
          smooth.spline(x = (1:length(angle))[!is.na(ma_turn)], #use only times when speed was calculated
                        y = ma_turn[!is.na(ma_turn)]), #use only speeds where speed was calculated
          x = 1:length(angle) # predict for all times
        )$y
       }else
       {
         rep(x = mean(x = ma_turn, 
                      na.rm = T
                      ),
             times = length(ma_turn)
             )
       }
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

MA_stop_flag = function(i, #index
                        dta, #vector of data
                        window = 1, # time window in seconds
                        hz = 100,  # sampling rate
                        method = 'median' # mean between frames or distance across interval
)
{
  n = window*hz # number of samples to use
  winmin = i-round(n/2)+1 #index of window start 
  winmax = i+round(n/2)-1 #index of window end
  if(winmin<1){return(NA)}#if window starts outside data, don't calculate
  if(winmax>length(dta)){return(NA)}#if window ends outside data, don't calculate
  return(
    all(
        x = dta[winmin:winmax],
        na.rm = FALSE # large NA regions at the start don't count
        )
    )
}


# Read in, calculate, write -----------------------------------------------

FT_read_write = function(path_file = FT_select_dat(),#path to the ".dat" file
                         ball_radius = 25,#mm
                         av_window = 5.0,#number of seconds to smooth over for averaging
                         stop_speed = 30,#mm/s minimum walking speed
                         stop_length = 0.5,#number of seconds to identify a stop
                         csv_sep_load = ',',#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
                         speedup_parallel = TRUE, #Use the parallel package to speed up calculations
                         speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
                         compress_csv = TRUE, #Compress to ".gz" to save space?
                         verbose = TRUE, #Tell the user what is going on
                         clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
                           {parallel::makeCluster(parallel::detectCores() - 1,type="SOCK")}else
                           {NULL},
                         folder_labels = TRUE, #read date, animal and experiment from the containing folder
                         sg_order = 3 #read date, animal and experiment from the containing folder
)
{

# . Required packages -----------------------------------------------------
 invisible(
   { # hide verbose loading messages
    library(circular, quietly = TRUE)
    library(signal, quietly = TRUE)
    if(speedup_data.table){library(data.table, quietly = TRUE)}
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
             'seq_counter' #23 rest not available in 2.03
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
  dff_time = diff(adata$time_stamp)
  fps = 1/mean(dff_time, na.rm = T)*1e3 # frames per second
  
  # . Add variables ---------------------------------------------------------
  # . . Set up parallel processing, if used -----------------------------------
  if(speedup_parallel)
  {
    if(verbose)
    {message('Using parallel processing...')}
    #Benefits from some parallel processing, but setting up the cluster is slow
    clt = clust
    clusterExport(cl = clt,#the cluster needs some variables&functions outside parLapply
                  varlist = list('adata',
                       'fps',
                       'MAmeanang',
                       'MAmeanvec',
                       'MAturnspeed',
                       'MAsmoothturn',
                       'mean.circular',
                       'rho.circular',
                       'circular',
                       'deg',
                       'rad'),
                  envir = environment()#needs to be reminded to use function environment, NOT global environment
    )
  }


# . . Check for jumps -------------------------------------------------------
  #check for jumps (happened in fictrac-20220316_134902.dat)
  jmp = abs(dff_time) > 1e3*3.0/(fps) | # jump of more than 3x typical interval
      dff_time < 0 #or at all negative
  if(any(jmp))
  {
    # if(any(dff_time<0))
    # {
    #   jmp_ind = which(dff_time<0)
    #   min_jmp_ind = min(jmp_ind)
    # }else
    # {
    jmp_ind = which(jmp)
    min_jmp_ind = min(jmp_ind)
    # }
    post_jump = (min_jmp_ind:length(dff_time))+1 #diff is OBOE from data
    if(speedup_data.table)
    {
      adata[, `:=` ( heading_integrated = c(heading_integrated[-(post_jump)],
                                            rep(x = NA, times = length(post_jump))
                                            ),
                      z_ball = c(z_ball[-(post_jump)],
                                   rep(x = NA, times = length(post_jump))
                                ),
                      y_int = c(y_int[-(post_jump)],
                                   rep(x = NA, times = length(post_jump))
                                ),
                      x_int = c(x_int[-(post_jump)],
                                   rep(x = NA, times = length(post_jump))
                                ),
                     time_stamp = c(time_stamp[-(post_jump)],
                                   rep(x = NA, times = length(post_jump))
                                )
                    )
            ]
    }else
    {
      adata = within(adata,
                     {
                     heading_integrated[post_jump] = NA
                     z_ball[post_jump] = NA
                     y_int[post_jump] = NA
                     x_int[post_jump] = NA
                     time_stamp[post_jump] = NA
                     }
                    )
    }
  }
  
# . . Variable definitions ------------------------------------------------

  if(speedup_data.table)
  {
    #has a slightly different syntax
    #N.B. Cannot call variable until after it has been created
    #data.table method
    adata[, `:=`( #assign from list call
                     experimental_time = (time_stamp - min(time_stamp, na.rm = T))/1e3, # seconds since start
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
                     z_turn = deg(-z_ball)*fps, #°/s
                     forward_speed = y_ball*ball_radius*fps, #mm/s
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
    adata[, `:=`( smooth_turn = MAsmoothturn(    #assign from list call
                                    angle = angle,
                                    ma_turn = ma_turn) 
                     # smooth_turn = predict( # fit a spline and predict its values across all times
                     #   smooth.spline(x = (1:length(angle))[!is.na(ma_turn)], #use only times when speed was calculated
                     #                 y = ma_turn[!is.na(ma_turn)]), #use only speeds where speed was calculated
                     #   x = 1:length(angle) # predict for all times
                     # )$y
                     ),
          ]#,
    adata[, `:=`( #assign from list call
                    inst_accel = c(NA, diff(deg(heading_integrated))*fps),
                    smooth_accel = c(NA, diff(smooth_turn)*fps),
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
                     y_speed = c(NA, diff(y_pos))*fps, #sideways speed in mm/s
                     z_turn = deg(-z_ball)*fps, #°/s,
                     forward_speed = y_ball*ball_radius*fps #mm/s
    )
    ]
    adata[, `:=`( #assign from list call
                     ground_speed = sqrt(x_speed^2 + y_speed^2), #speed in mm/s
                     straight_distance = sqrt(x_pos^2 + y_pos^2) #straight-line distance in mm
    )
    ]
    adata[, `:=`( #assign from list call
                     ma_speed = signal::sgolayfilt(
                       x = forward_speed,
                       p = sg_order,
                       n = if(round(av_window*fps) %% 2) #filter length must be odd
                       {round(av_window*fps)}else
                       {round(av_window*fps+1)}
                     )
                     #   if(speedup_parallel)
                     # {
                     #   parSapply(cl = clt,
                     #             X = 1:length(straight_distance),#all indices in angle
                     #             FUN = MAspeed, #the mean speed function
                     #             dta_x = x_pos,#x coordinate
                     #             dta_y = y_pos,#y coordinate
                     #             window = av_window,#window size (s)
                     #             hz = fps, #sample rate (Hz)
                     #             method = 'mean' #average between frame
                     #   )
                     # }else
                     # {
                     #   sapply(X = 1:length(straight_distance),#all indices in angle
                     #          FUN = MAspeed, #the mean speed function
                     #          dta_x = x_pos,#x coordinate
                     #          dta_y = y_pos,#y coordinate
                     #          window = av_window,#window size (s)
                     #          hz = fps, #sample rate (Hz)
                     #          method = 'mean' #average between frame
                     #   )
                     # }
    )
    ]#end of data.table
  }else
  {
  
  #data.frame method
  adata = within(adata, 
                 {
                   experimental_time = (time_stamp - min(time_stamp, na.rm = T))/1e3 # seconds since start
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
                   forward_speed = y_ball*ball_radius*fps #mm/s
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
                   smooth_turn = MAsmoothturn(    
                                   angle = angle,
                                   ma_turn = ma_turn
                                   ) 
                   inst_accel = c(NA, diff(deg(heading_integrated))*fps)
                   smooth_accel = c(NA, diff(smooth_turn)*fps)
                   #   predict( # fit a spline and predict its values across all times
                   #   smooth.spline(x = (1:length(angle))[!is.na(ma_turn)], #use only times when speed was calculated
                   #                 y = ma_turn[!is.na(ma_turn)]), #use only speeds where speed was calculated
                   #   x = 1:length(angle) # predict for all times
                   # )$y
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
                   ma_speed = signal::sgolayfilt(
                                 x = forward_speed,
                                 p = sg_order,
                                 n = if(round(av_window*fps) %% 2) #filter length must be odd
                                 {round(av_window*fps)}else
                                 {round(av_window*fps+1)}
                               )
                   #   if(speedup_parallel)
                   # {
                   #   parSapply(cl = clt,
                   #             X = 1:length(straight_distance),#all indices in angle
                   #             FUN = MAspeed, #the mean speed function
                   #             dta_x = x_pos,#x coordinate
                   #             dta_y = y_pos,#y coordinate
                   #             window = av_window,#window size (s)
                   #             hz = fps, #sample rate (Hz)
                   #             method = 'mean' #average between frame
                   #   )
                   # }else
                   # {
                   #   sapply(X = 1:length(straight_distance),#all indices in angle
                   #          FUN = MAspeed, #the mean speed function
                   #          dta_x = x_pos,#x coordinate
                   #          dta_y = y_pos,#y coordinate
                   #          window = av_window,#window size (s)
                   #          hz = fps, #sample rate (Hz)
                   #          method = 'mean' #average between frame
                   #   )
                   # }
                 }
  )
  }

  # . . . Add absolute ------------------------------------------------------
  if(speedup_data.table)
  {
    adata[, `:=`( #assign from list call
                  abs_turn = abs(ma_turn),
                  abs_accel = abs(ma_accel)
                )
          ]
  }else
  {
  adata = within(adata, 
                 {
                   abs_turn = abs(ma_turn)
                   abs_accel = abs(ma_accel)
                 }
                )
  }
  # . . . Find stops --------------------------------------------------------
  if(speedup_data.table)
  {
    adata[, `:=`( #assign from list call
                  low_forward = forward_speed < stop_speed
                )
          ] 
    adata[, `:=`( #assign from list call
                  stop_flag = sapply(X = 1:length(forward_speed),
                                     dta = low_forward,
                                     FUN = MA_stop_flag,
                                     window = stop_length)
                )
          ]
  }else
  {
  adata = within(adata, 
                 {
                   low_forward = forward_speed < stop_speed
                   stop_flag = sapply(X = 1:length(forward_speed),
                                      dta = low_forward,
                                      FUN = MA_stop_flag,
                                      window = stop_length)
                 }
                )
  }
  # . Neaten up acceleration ------------------------------------------------
  adata = within(adata,
                 {
                   smooth_accel[experimental_time < av_window/2] = NA
                   ma_accel[experimental_time < av_window] = NA
                 }
  )
  
  #close the parallel cluster
  if(speedup_parallel){ if(base::missing(clust)){parallel::stopCluster(clt)} }#only close internal cluster
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


# Combine all files in all folders ---------------------------------------------
FT_combine_folders = function(path_folder = FT_select_folder(),
                             file_type = '_proc.csv.gz',#N.B. currently only for this type
                             speedup_parallel = TRUE, #Use the parallel package to speed up calculations
                             speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
                             compress_txt = TRUE, #Compress to ".gz" to save space?
                             verbose = TRUE, #Tell the user what is going on
                             recursive = TRUE,   #Search in sub folders                          
                             clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
                             {parallel::makeCluster(parallel::detectCores() - 1,type="SOCK")}else
                             {NULL}
                             )
{
  # . Required packages -----------------------------------------------------
  invisible(
    { # hide verbose loading messages
      if(speedup_data.table){library(data.table, quietly = TRUE)}
      if(speedup_parallel){library(parallel, quietly = TRUE)}
    }
  )
  file_regex = paste0(file_type, '$')
  # . Find files ---------------------------------------------------------------
  names_files = list.files(path = path_folder,
                           pattern = file_regex,
                           recursive = recursive
                          )
  #show the user the path they have selected
  if(is.null(names_files)|!length(names_files))
  {stop('No "',file_type,'" files found in \n', path_folder)}else
  {message(length(names_files),' files found:\n',paste0(names_files,'\n'),
           '\n------------------------------------------')}

  # . Read in files ------------------------------------------------------------
  if(verbose)
  {message('Reading in files from:\t', basename(path_folder), '\nplease be patient...')}
  file_paths = file.path(path_folder, names_files)
  if(speedup_parallel)
  {
    clusterExport(cl = clust,#the cluster needs some variables&functions outside parLapply
                  varlist = 
                    list('file_paths',
                         'data.table',
                         'fread',
                         'file_type'),
                  envir = environment()#needs to be reminded to use function environment, NOT global environment
    )
  }
  tryCatch(#Perform no further analysis if the file doesn't load
    {
      adata = if(speedup_parallel)
        {
        parLapply(cl = clust,
                  X = if(speedup_data.table)
                          {file_paths}else
                          {lapply(file_paths, gzfile)},
                     fun = if(speedup_data.table) #N.B. parLapply uses "fun" not "FUN"
                             {data.table::fread}else
                             {read.table},
                     sep = switch(EXPR = file_type,
                                  `_proc.csv.gz` = ',', 
                                  `_proc.txt.gz` = '\t',
                                  ' '#default output for write.table
                                  ),
                     header = TRUE
                    )
        }else#
        {
      lapply(X = if(speedup_data.table)
                          {file_paths}else
                          {lapply(file_paths, gzfile)},
                     FUN = if(speedup_data.table)
                             {data.table::fread}else
                             {read.table},
                     sep = switch(EXPR = file_type,
                                  `_proc.csv.gz` = ',', 
                                  `_proc.txt.gz` = '\t',
                                  ' '#default output for write.table
                                  ),
                     header = TRUE
                    )#
        }
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
                        {with(x, 1/mean(diff(experimental_time), na.rm = T))}
  )#Hz
  if(verbose)
  {
    if(sum(diff(unlist(sample_rates)), na.rm = T))
      {
        warning('Sample rates differ between recordings\n',
               'proceed with caution!\n',
               paste(x = signif(range(sample_rates, na.rm = TRUE), 5),
                     collapse = ' - '),
               'Hz\n'
               )
      }else
      {
       message('All recordings have a sample rate of ',
               sample_rates[[1]],
               ' Hz\n')
      }
  }
  }
  
  # . . Find trial details ----------------------------------------------------
  FoldName = function(x,
                      type = 'Condition')
    {
    switch(EXPR = type,
            Condition = basename(dirname(x)), #condition is folder containing file
            Bumblebee = basename(dirname(dirname(x))), #bumblebee name is one folder containing folder
            Day = basename(dirname(dirname(dirname(x)))), #day is two folders above containing folder
            basename(x)
            )
    }
  FT_trial_details = function(i)
  {
    dt = adata[[i]]
    
  if(speedup_data.table)
    {#assume works the same with data.table (nothing defined and used in same expr)
    dt[, `:=` (track = regmatches(
                           m = regexpr(pattern = '[^ -][^-]*$',#Remove illegal characters
                                       text = basename(file_paths[[i]])
                           ),
                           x = basename(file_paths[[i]])
                         )
      )]
    dt[, `:=` (condition = regmatches(
                                 m = regexpr(pattern = '[^ -][^-]*$',#Remove illegal characters
                                             text = FoldName(x = file_paths[[i]], 
                                                             type = 'Condition')
                                            ),
                                 x = FoldName(x = file_paths[[i]], 
                                              type = 'Condition')
                                 )
      )]
    dt[, `:=` (bumblebee = sub(pattern = 'Bumblebee ',
                                 x = FoldName(file_paths[[i]], 'Bumblebee'),
                                 replacement = ''
                                 )
      )]
    dt[, `:=` (date = regmatches(
                                 m = regexpr(pattern = '[^ -][^-]*$',#Remove illegal characters
                                             text = FoldName(x = file_paths[[i]], 
                                                             type = 'Day')
                                            ),
                                 x = FoldName(x = file_paths[[i]], 
                                              type = 'Day')
                                 )
      )]
    }else
    {
    dt = within(dt,
               {
                 track = regmatches(
                           m = regexpr(pattern = '[^ -][^-]*$',#Remove illegal characters
                                       text = basename(file_paths[[i]])
                           ),
                           x = basename(file_paths[[i]])
                         )
                 condition = regmatches(
                               m = regexpr(pattern = '[^ -][^-]*$',#Remove illegal characters
                                           text = FoldName(x = file_paths[[i]], 
                                                           type = 'Condition')
                               ),
                               x = FoldName(x = file_paths[[i]], 
                                            type = 'Condition')
                             )
                 bumblebee = sub(pattern = 'Bumblebee ',
                                 x = FoldName(file_paths[[i]], 'Bumblebee'),
                                 replacement = ''
                 )
                 date = regmatches(
                         m = regexpr(pattern = '[^ -][^-]*$',#Remove illegal characters
                                     text = FoldName(x = file_paths[[i]], 
                                                     type = 'Day')
                         ),
                         x = FoldName(x = file_paths[[i]], 
                                      type = 'Day')
                       )
               }
              )
    }
    return(dt)
  }
  # . . Set up parallel processing, if used -----------------------------------
  if(speedup_parallel)
  {
    if(verbose)
    {message('Using parallel processing...')}
    #Benefits from some parallel processing, but setting up the cluster is slow
    clt = clust
    clusterExport(cl = clust,#the cluster needs some variables&functions outside parLapply
                  varlist = 
                    list('adata',
                       'file_paths',
                       'data.table',
                       'FT_trial_details',
                       'FoldName',
                       'speedup_data.table'),
                  envir = environment()#needs to be reminded to use function environment, NOT global environment
    )
  }
  adata = if(speedup_parallel)
  {
    parLapply(cl = clust,
           X = 1:length(adata),
           fun = FT_trial_details #N.B. parLapply uses "fun" not "FUN" like parSapply & lapply
           )
  }else
  {
    lapply(
            X = 1:length(adata),
            FUN = FT_trial_details
          )
  }
  names(adata) = basename(file_paths)
  #close the parallel cluster
  if(speedup_parallel){ if(base::missing(clust)){parallel::stopCluster(clust)} }#only close internal cluster
  
  # . combine all data into a single data frame -------------------------------
  adata_frame = do.call(what = rbind,#N.B. seems to work the same with data.table & data.frame
                        args = adata)
  # .  Save data --------------------------------------------------------------
  txt_file = file.path(#dirname(path_folder),#store outside this folder
                       path_folder,#store INSIDE this folder
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
  if(speedup_data.table)
  {
  data.table::fwrite(x = adata_frame,
              file = txt_file,#N.B. data.table >= 1.12.4 can write to .gz automatically
              sep = '\t',
              row.names = FALSE
  )
  }else
  {
  write.table(x = adata_frame,
              file = if(compress_txt)
                        {gzfile(txt_file)}else
                        {txt_file},
              sep = '\t',
              row.names = FALSE
  )
  }
  return(txt_file)
}


# Raster plot each condition ---------------------------------------------

FT_raster_condition = function(
                        txt_file = FT_select_file(file_type = '_proc.txt.gz'),
                        experiment_length = 2, #minutes
                        condition1_length = 1, #minutes
                        av_window = 5.0,#number of seconds to smooth over for averaging
                        point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
                        save_type = "pdf",# "png",#
                        quantls = c(0.025, 0.25, 0.5, 0.75, 0.975), # quantiles to use when summarising
                        plette = 'Plasma',#'YlGnBu'#  for options see http://colorspace.r-forge.r-project.org/articles/approximations.html
                        ranking = TRUE,
                        crossval = FALSE, # TRUE, randomise the data to check the analysis
                        sys_win = Sys.info()[['sysname']] == 'Windows',
                        speedup_parallel = TRUE, #Use the parallel package to speed up calculations
                        speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
                        verbose = TRUE, #Tell the user what is going on
                        show_plot = TRUE, #Tell the user what is going on
                        clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
                        {parallel::makeCluster(parallel::detectCores() - 1,type="SOCK")}else
                        {NULL}
                        )
  {
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp = Sys.getenv('HOME')#Easier on Mac
}


# . Read in files ------------------------------------------------------------
if(verbose){message('\n','Reading in "', basename(txt_file), '"\nplease be patient...')}
tryCatch(#Perform no further analysis if the file doesn't load
  {
    all_data_table = 
      if(speedup_data.table)
      {
      data.table::fread(
        file = txt_file, 
        sep = '\t',
        header = TRUE
      )#,#read from user-selected file
      }else
      {
        read.table(
        file = txt_file, 
        sep = '\t',
        header = TRUE
      )#,#read from user-selected file
      }
  },
  error = function(e)
  {
    stop(
      paste0('"',
             basename(txt_file), 
             '" could not be loaded!\n',
             e)
    )
  }
)
if(verbose){message('"',basename(txt_file), '" loaded successfully')}

# . Check that there is some data to summarise --------------------------
sample_rate = 1/mean(diff(
  subset(all_data_table, 
         subset = track == unique(track)[1] &
           date == unique(date)[1] 
  )$experimental_time
), na.rm = T)
#set up function to flag full length experiments #N.B. this currently looks quite slow
FlagExper = function(trackID,
                     dta,
                     experiment_length = 2)
{
  if(with(dta, length(experimental_time) != length(track)))
  {stop('timeID and trackID vectors must be the same length')}
 return( #TODO speed up with data.table
        with(subset(dta, track %in% trackID), #subset to single track
             {
                     rep(x = (max(experimental_time, na.rm = T)/60)>
                             experiment_length,#is the experiment longer than the minimum?
                         times = length(experimental_time)) #replicate logical
             }
                    )
   )
}
if(speedup_parallel)
{
  parallel::clusterExport(cl = clust,
                varlist = list('all_data_table',
                               'FlagExper',
                               'experiment_length'),
                envir = environment()
                )
}
#Search for full length experiments
all_data_table = within(all_data_table,
                        {
                          flag_exp = if(speedup_parallel)
                            {unlist(
                              parallel::parSapply(cl = clust,
                                       X = unique(track),
                                       FUN = FlagExper,
                                       dta = all_data_table,
                                       experiment_length = experiment_length
                                      )
                              )
                            }else
                            {unlist(
                              sapply(
                                      X = unique(track),
                                      FUN = FlagExper,
                                      dta = all_data_table,
                                      experiment_length = experiment_length
                                    )
                            )
                            }
                        }
                        )
#Tell the user
if( with(all_data_table, !any( flag_exp)) )
{stop('\n',
      "NONE OF THE FILES LOADED WERE ", experiment_length, " min LONG!",
      '\n', "Consider reducing the 'experiment length' input variable")
}else
{
  message( 
    round(
      with(all_data_table, sum(flag_exp))/ 
        (experiment_length*sample_rate*60)
    ),
    " files were ", experiment_length, " min long or longer"
  )
}

# Summarise data --------------------------------------------------------

# . Add a user friendly name ----------------------------------------------
all_data_table = within(all_data_table,
                        {#randomise each parameter of interest across the dataset
                          day_anim_cond = paste(bumblebee, '_',
                                                condition, 
                                                sub(x = track,
                                                    pattern = '(.(dat)).*', 
                                                    replacement = '_')
                                                )
                        }
                        )

# . Cross validation ------------------------------------------------------
if(crossval)
{
  #randomise data to check the method
  
  all_data_table = within(all_data_table,
                          {#randomise each parameter of interest across the dataset
                            forward_speed = sample(x = forward_speed,
                                            size = length(bumblebee),
                                            replace = FALSE)
                            ma_rho = sample(x = ma_rho,
                                            size = length(bumblebee),
                                            replace = FALSE)
                            ma_turn = sample(x = ma_turn,
                                              size = length(bumblebee),
                                              replace = FALSE)
                            abs_turn = abs(ma_turn)
                            abs_accel = abs(
                              sapply(X = 1:length(experimental_time),
                                     FUN = MAturnspeed,
                                     dta = predict(
                                       smooth.spline(
                                         x = (1:length(experimental_time))[!is.na(ma_turn)],
                                         y = ma_turn[!is.na(ma_turn)]),
                                       x = 1:length(experimental_time)
                                     )$y,
                                     window = av_window,
                                     hz = sample_rate
                              )
                            )
                          }
  )
}

# . Rank by first condition -----------------------------------------------
if(verbose){message('Ranking by median value across 1st condition...','\n')}
#summarise 
track_speed = aggregate(formula = forward_speed~track,
                       data = subset(all_data_table, 
                                     subset = experimental_time < condition1_length*60),
                       FUN = quantile,
                       p = quantls)
track_rho = aggregate(formula = ma_rho~track,
                       data = subset(all_data_table, 
                                     subset = experimental_time < condition1_length*60),
                       FUN = quantile,
                       p = quantls)
track_abs_turn = aggregate(formula = abs_turn~track,
                            data = subset(all_data_table, 
                                          subset = experimental_time < condition1_length*60),
                            FUN = quantile,
                            p = quantls)
track_abs_accel = aggregate(formula = abs_accel~track,
                             data = subset(all_data_table, 
                                           subset = experimental_time < condition1_length*60),
                             FUN = quantile,
                             p = quantls) 
##probably don't need to calculate the rest?
all_data_table = data.table::merge.data.table(x = #combine with the rest of the data, adding quantiles that can be used for sorting
                                                data.table::merge.data.table( #combine rho with turn & accel
                                                  x =data.table::data.table(track_speed),
                                                  y = data.table::merge.data.table( #combine abs turn & accel
                                                    x =data.table::data.table(track_abs_turn) ,
                                                    y =data.table::data.table(track_abs_accel) ),
                                                  by = c('track')
                                                ),
                                              y = subset(all_data_table, 
                                                         subset =  flag_exp &
                                                           experimental_time <= experiment_length*60),
                                              by = c('track')
)
#find the order of each variable across the dataset
all_data_table = within(all_data_table,
                        {
                          rank_speed = rank(forward_speed.50.)  
                          # rank_rho = rank(ma_rho.50.)  
                          rank_turn = rank(abs_turn.50.)  
                          rank_accel = rank(abs_accel.50.)  
                        }
)

# . Find full experiments -------------------------------------------------
full_expr = subset(all_data_table,
                   subset = flag_exp &
                     experimental_time <= experiment_length*60 &
                     frame_counter <= experiment_length*60*sample_rate
)

#Check for glitches
Glitch_catcher = function(x){min(diff(x), na.rm = TRUE)<0}
diff_frames = aggregate(formula = frame_counter~track,
                        data = full_expr,
                        FUN = Glitch_catcher
                        )
if(with(diff_frames, {any(frame_counter)}))
{
  glitch_tracks = with(subset(diff_frames, frame_counter),
                       track)
  if(verbose)
  {
    message('\nExcluding ', 
            length(glitch_tracks), 
            ' tracks with time glitches:\n ', 
            paste0(glitch_tracks,'\n ')
            )
  }
  full_expr = subset(full_expr,
                     !(track %in% glitch_tracks) )
}


n_tracks = length(unique(full_expr$track))
if(dim(full_expr)[1] %% n_tracks)
{stop('Dimension mismatch\ninput file suggests different expected experiment length')}

#close the parallel cluster
if(speedup_parallel){ if(base::missing(clust)){parallel::stopCluster(clust)} }#only close internal cluster
# Plot Summary ------------------------------------------------------------
if(verbose){message("Making raster plot")}

# . Set up plot functions -------------------------------------------------

#  . . Set up colour scale transforms -------------------------------------
TurnTrans = function(x)
{
  log(x = ifelse(
            test = is.na(x),
            yes = 1,
            no = x + 1
              )
    )
  }
TurnTransInv = function(x)
{exp(x) - 1}
AccelTrans = function(x)
{
  log(x = ifelse(
            test = is.na(x),
            yes = 1,
            no =   x + 1)
            )
}
AccelTransInv = function(x)
{exp(x) - 1}
RhoTrans = function(x)
{
  ifelse(
    test = is.na(x), 
    yes = 0,
    no = x^2
  )
}
RhoTransInv = function(x)
{sqrt(x)}
speedlim = 0#20
SpeedTrans = function(x)
{
  ifelse(
            test = x <= speedlim,
            yes = 0,
            no = x
        )
  # ifelse(
  #           test = x <= speedlim | is.na(x),
  #           yes = 0,
  #           no = suppressWarnings({log10(x-speedlim)})
  #       )
  }
SpeedTransInv = function(x)
{
  ifelse(test = x == SpeedTrans(speedlim),
         yes = speedlim,
         no = x
        )
  # ifelse(test = x == SpeedTrans(speedlim),
  #        yes = speedlim,
  #        no = 10^x +speedlim
  #       )
 }

# . . Main plot function --------------------------------------------------

FT_raster = function(cnd,#condition to subset by
                     dta,#data to use
                     ... #other plotting info? 
                     )
{
  
  # . Subset  just this condition -------------------------------------------
  cnd_expr = subset(dta, condition %in% cnd)
  n_tracks = with(cnd_expr, length(unique(track)))
  
  # . Make matrices of full experiments -------------------------------------
  
  
  #turning speed
  if(!ranking)
  {dt_turn = cnd_expr}else
  {
  dt_turn = data.table::setorderv(#reorder by median turning speed across experiment
    cnd_expr, #full_expr,#dataset of full experiments
    cols = 'rank_turn',#order by median turning speed
    order = -1)
  }
  
  exp_len = experiment_length * 60
  while(dim(dt_turn)[1] %% n_tracks)
  { #if not a square matrix reduce the time period by one sample
    exp_len = exp_len - 1/sample_rate
    dt_turn = subset(x = dt_turn, 
                     subset = experimental_time < exp_len)
  }  
  mtr_turn = with(dt_turn, #descending order
    matrix(data = abs_turn, # produce a matrix for plotting
           ncol = n_tracks # columns are individual tracks
    )
  )
  #acceleration
  if(!ranking)
  {dt_accel =     cnd_expr}else
  {
  dt_accel = data.table::setorderv(
    cnd_expr, #full_expr,#dataset of full experiments
    cols = 'rank_accel',#order by median acceleration
    order = -1)
  }
  exp_len = experiment_length * 60
  while(dim(dt_accel)[1] %% n_tracks)
  { #if not a square matrix reduce the time period by one sample
    exp_len = exp_len - 1/sample_rate
    dt_accel = subset(x = dt_accel, 
                     subset = experimental_time < exp_len)
  }
  mtr_accel = with(dt_accel, # descending order
    matrix(data = abs_accel, # produce a matrix for plotting
           ncol = n_tracks # columns are individual tracks
    )
  )
  # #mean vector
  # mtr_rho = with(data.table::setorderv(
  #   cnd_expr, #full_expr,#dataset of full experiments
  #   cols = 'rank_rho',#order by median mean vector length
  #   order = 1), # ascending order
  #   matrix(data = ma_rho, # produce a matrix for plotting
  #          ncol = n_tracks # columns are individual tracks
  #   )
  # )
  #forward speed
  if(!ranking)
  {dt_speed = cnd_expr}else
  {
  dt_speed = data.table::setorderv(
    cnd_expr, #full_expr,#dataset of full experiments
    cols = 'rank_speed',#order by median mean vector length
    order = 1)
  }
  exp_len = experiment_length * 60
  while(dim(dt_speed)[1] %% n_tracks)
  { #if not a square matrix reduce the time period by one sample
    exp_len = exp_len - 1/sample_rate
    dt_speed = subset(x = dt_speed, 
                      subset = experimental_time < exp_len)
  }
  mtr_speed = with(dt_speed, # ascending order
    matrix(data = forward_speed, # produce a matrix for plotting
           ncol = n_tracks # columns are individual tracks
    )
  )

  # . Plot data -------------------------------------------------------------

  # . . Turning speed -------------------------------------------------------
  with(dt_turn, #full_expr,#dataset of full experiments
       {
         image(x = TurnTrans(mtr_turn),
               useRaster = TRUE,
               zlim = TurnTrans(c(0,360/8)),
               xlim = c(0,1.1),
               # ylim = c(0,1.0),
               xlab = 'time (s)',
               ylab = ' ',
               main = paste0('absolute median turning speed (°/s: ',av_window,'s)'),
               axes = F,
               col = hcl.colors(n = 16,
                                palette = plette)
         )
         axis(side = 1,
              at = sample_rate*10*(0:(max(experimental_time, na.rm = T)/10))/dim(mtr_turn)[1],
              labels = 10*(0:(max(experimental_time, na.rm = T)/10))
         )
         axis(side = 2,
              at = seq(from = 0, to  = 1, length.out = dim(mtr_turn)[2]),
              labels = gsub(pattern = '_',
                            x = unique(
                              data.table::setorderv(
                                dt_turn,
                                cols = 'rank_turn',
                                order = 1)$day_anim_cond,
                            ),
                            replacement = '\n'
              ),
              las = 1,
              line = -0.5,
              cex.axis = 0.25
         )
         abline(v = sample_rate*60*c(condition1_length)/dim(mtr_turn)[1],
                col = adjustcolor('white',alpha.f = 200/255),#c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
                lwd = 0.5
         )
       }
  )
  legend(x = 'right',
         inset=c(0,0),
         legend = c(
           round( 
             TurnTransInv(
               seq( from = TurnTrans(0), 
                    to = TurnTrans(360/8), 
                    length.out = 5) ) 
           ),
           paste0('>',360/8)
         ),
         pch = 22,
         pt.bg = c(
           hcl.colors(n = 5,
                      palette = plette),
           'white'
         )
  )
  
  # . . Acceleration --------------------------------------------------------
  with(dt_accel, #full_expr,#dataset of full experiments
       {
         image(x = AccelTrans(mtr_accel),
               useRaster = TRUE,
               zlim = AccelTrans(c(0,10)),
               xlim = c(0,1.1),
               # ylim = c(1,n_flightdates),
               xlab = 'time (s)',
               ylab = ' ',
               main = paste0('absolute median acceleration (°/s^2: ',av_window,'s)'),
               axes = F,
               col = hcl.colors(n = 16,
                                palette = plette)
         )
         axis(side = 1,
              at = sample_rate*10*(0:(max(experimental_time, na.rm = T)/10))/dim(mtr_accel)[1],
              labels = 10*(0:(max(experimental_time, na.rm = T)/10))
         )
         axis(side = 2,
              at = seq(from = 0, to  = 1, length.out = dim(mtr_accel)[2]),
              labels = gsub(pattern = '_',
                            x = unique(
                              data.table::setorderv(
                                dt_accel,
                                cols = 'rank_accel',
                                order = 1)$day_anim_cond,
                            ),
                            replacement = '\n'
              ),
              las = 1,
              line = -0.5,
              cex.axis = 0.25
         )
         abline(v = sample_rate*60*c(condition1_length)/dim(mtr_accel)[1],
                col = adjustcolor('white',alpha.f = 200/255),#c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
                lwd = 0.5
         )
       }
  )
  legend(x = 'right',
         inset=c(0,0),
         legend = c(
           round( 
             AccelTransInv(
               seq( from = AccelTrans(0), 
                    to = AccelTrans(10), 
                    length.out = 5) )
           ),
           '>10'
         ),
         pch = 22,
         pt.bg = c(
           hcl.colors(n = 5,
                      palette = plette),
           'white'
         )
  )
  # 
  # # . . Mean vector length --------------------------------------------------
  # with(cnd_expr, #full_expr,#dataset of full experiments
  #      {
  #        image(x = RhoTrans(mtr_rho),
  #              useRaster = TRUE,
  #              zlim = RhoTrans(c(0,0.99)),
  #              xlim = c(0,1.1),
  #              xlab = 'time (s)',
  #              ylab = 'Test',
  #              main = paste0('mean vector length (',av_window,'s)'),
  #              axes = F,
  #              col = hcl.colors(n = 16,
  #                               palette = plette)
  #        )
  #        axis(side = 1,
  #             at = sample_rate*10*(0:(max(experimental_time, na.rm = T)/10))/dim(mtr_rho)[1],
  #             labels = 10*(0:(max(experimental_time, na.rm = T)/10))
  #        )
  #        axis(side = 2,
  #             at = seq(from = 0, to  = 1, length.out = dim(mtr_rho)[2]),
  #             labels = gsub(pattern = '_',
  #                           x = unique(
  #                             data.table::setorderv(
  #                               cnd_expr,
  #                               cols = 'rank_rho',
  #                               order = -1)$day_anim_cond,
  #                           ),
  #                           replacement = '\n'
  #             ),
  #             line = -0.5,
  #             las = 1,
  #             cex.axis = 0.25
  #        )
  #        abline(v = sample_rate*60*c(condition1_length)/dim(mtr_rho)[1],
  #               col = adjustcolor('white',alpha.f = 200/255),#c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
  #               lwd = 0.5
  #        )
  #      }
  # )
  # legend(x = 'right',
  #        inset=c(0,0),
  #        legend = c(
  #          round(
  #            x = RhoTransInv(seq( from = RhoTrans(0), 
  #                                 to = RhoTrans(0.99), 
  #                                 length.out = 5)),
  #            digits = 2
  #          ),
  #          '1.0'
  #        ),
  #        pch = 22,
  #        pt.bg = c(
  #          hcl.colors(n = 5,
  #                     palette = plette),
  #          'white'
  #        )
  # )
  # . . Forward speed ---------------------------------------------------------
  with(dt_speed, #full_expr,#dataset of full experiments
       {
         image(x = SpeedTrans(mtr_speed),
               useRaster = TRUE,
               zlim = SpeedTrans(c(speedlim,100)),
               xlim = c(0,1.1),
               xlab = 'time (s)',
               ylab = 'Test',
               main = paste0('Forward speed (','mm/s',': by frame)'),
               axes = F,
               col = hcl.colors(n = 16,
                                palette = plette)
         )
         axis(side = 1,
              at = sample_rate*10*(0:(max(experimental_time, na.rm = T)/10))/dim(mtr_speed)[1],
              labels = 10*(0:(max(experimental_time, na.rm = T)/10))
         )
         axis(side = 2,
              at = seq(from = 0, to  = 1, length.out = dim(mtr_speed)[2]),
              labels = gsub(pattern = '_',
                            x = unique(
                              data.table::setorderv(
                                dt_speed,
                                cols = 'rank_speed',
                                order = -1)$day_anim_cond,
                            ),
                            replacement = '\n'
              ),
              line = -0.5,
              las = 1,
              cex.axis = 0.15
         )
         abline(v = sample_rate*60*c(condition1_length)/dim(mtr_speed)[1],
                col = adjustcolor('white',alpha.f = 200/255),#c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
                lwd = 0.5
         )
       }
  )
  legend(x = 'right',
         inset=c(0,0),
         legend = c(
           round(
             x = SpeedTransInv(seq( from = SpeedTrans(speedlim), 
                                  to = SpeedTrans(100), 
                                  length.out = 5)),
             digits = 0
           ),
           '>100'
         ),
         pch = 22,
         pt.bg = c(
           hcl.colors(n = 5,
                      palette = plette),
           'white'
         )
  )
  mtext(text = 'time (sec)',
        side = 1,
        outer = T,
        cex = par('cex.main')
  )
  mtext(text = cnd,
        side = 4,
        line = -1.5,
        outer = T,
        cex = par('cex.main')
  )
  if(crossval)
  {
    mtext(text = 'Data were randomised for cross-validation',
          side = 3,
          outer = T,
          cex = par('cex.main')
    )
  }
}
# . Set up plot area ------------------------------------------------------
plot_file = file.path(dirname(txt_file),
                       paste0(basename(txt_file),
                              '_rast', ifelse(crossval, '-CROSSVAL',''),
                              ifelse(test = save_type %in% 'pdf', 
                                     yes = '', 
                                     no = paste0('-',with(full_expr, unique(condition))[1])),
                              '.', save_type)
)
if(file.exists(plot_file))
{
  message('A plot called "', basename(plot_file), '" already exists in this folder.')
  nnm = readline(prompt = 'New plot name: '
  )
  
  plot_file = file.path(dirname(txt_file),
                          paste0(
                            ifelse(test = nchar(nnm),
                                   yes = nnm,
                                   no = basename(txt_file)),
                            '_rast',ifelse(crossval, '-CROSSVAL',''),
                            ifelse(test = save_type %in% 'pdf', 
                                   yes = '', 
                                   no = paste0('-',with(full_expr, unique(condition))[1])),
                            '.', save_type)
  )
}

switch(save_type,
       pdf = 
         pdf(file = plot_file,
             paper = 'a4',
             height = 10,
             bg = 'white',
             useDingbats = F,
             onefile = TRUE
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
       png = par(mfrow = c(3,1),
                 mar = c(2,3,2,5),
                 oma = c(1.5,0,1.5,0),
                 cex = 1.5
       ),
       par(mfrow = c(3,1),
           mar = c(2,3,2,5),
           oma = c(1.5,0,1.5,0))
)

# . Plot summarys ---------------------------------------------------------
#Find unique conditions
u_cond = with(full_expr, unique(condition))
#Plot PDF as multiple pages in a single file
#Or PNG as multiple files with different names
if(save_type %in% 'pdf')
{
  invisible(
    lapply(X = u_cond,
           FUN = FT_raster,
           dta = full_expr
           )
  )
  dev.off()
}else
{
png_files = list()
for(cnd in u_cond)
  {
    # dev.off()
    plot_file = file.path(dirname(txt_file),
                          paste0(basename(txt_file),
                                 '_rast', ifelse(crossval, '-CROSSVAL',''),
                                 '-', cnd,
                                 '.', save_type)
    )
    switch(save_type,
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
    par(mfrow = c(3,1),
        mar = c(2,3,2,5),
        oma = c(1.5,0,1.5,0),
        cex = 1.5
    )
    FT_raster(cnd = cnd,
              dta = full_expr
            )
    dev.off()#close and save
    png_files[cnd] = plot_file # add file name to the list
  }
}
# . Save plot -------------------------------------------------------------
  if(show_plot){
    if(save_type == 'pdf')
      {shell.exec.OS(plot_file)}else
      {
       for(cnd in u_cond)
       {
         shell.exec.OS(
           file.path(dirname(txt_file),
                     paste0(basename(txt_file),
                            '_rast', ifelse(crossval, '-CROSSVAL',''),
                            cnd,
                            '.', save_type)
                     )
         )
       }
      }
  }
return(if(save_type == 'png'){png_files}else{plot_file})
}
# Calculate frequency spectrum -----------------------------------------------

FT_frequency_analysis = function(path_file = FT_select_file(file_type = '_proc.csv.gz'),#path to the ".dat" file
                         target_freq = 0.1, #Hz 
                         av_window = 4/target_freq,#number of seconds to use for Welch PSD window
                         csv_sep_load = ',',#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
                         speedup_parallel = FALSE, #Use the parallel package to speed up calculations
                         speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
                         compress_csv = TRUE, #Compress to ".gz" to save space?
                         verbose = TRUE, #Tell the user what is going on
                         resample = NULL, #Hz Resample to predetermined sampling base
                         res.method = if(!is.null(resample)){'MAturnspeed'}, #Hz Resample to predetermined sampling base
                         validation = FALSE, #Test with the target sine wave
                         clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
                         {parallel::makeCluster(parallel::detectCores() - 1,type="SOCK")}else
                         {NULL}
)
{
  
  # . Required packages -----------------------------------------------------
  invisible(
    { # hide verbose loading messages
      library(bspec, quietly = TRUE)#Bayesian Spectral Inference
      if(speedup_data.table){library(data.table, quietly = TRUE)}
      if(speedup_parallel){library(parallel, quietly = TRUE)}
      if(res.method == 'MAturnspeed'){library(circular, quietly = TRUE)}#actually always needed!
    }
  )
  
  # . Read in data ----------------------------------------------------------
  adata = if(speedup_data.table)
  {
    data.table::fread(
      file = path_file,#read from user-selected file
      sep = csv_sep_load,#user specified comma separation
      header = TRUE#These files have headers
      #other parameters can be added here for troubleshooting
    )
  }else
  {
    read.table(file = gzfile(path_file),#read from user-selected file
               sep = csv_sep_load,#user specified comma separation
               header = TRUE#These files have headers
               #other parameters can be added here for troubleshooting
    )
  }


  # Check data --------------------------------------------------------------
  if(with(adata, !exists('z_turn')))
  {
    message('z_turn missing from data', 
           '\n please re-run "FT_read_write" for this ".dat" file')
    }

  
  # Conversions -------------------------------------------------------------
  
  # . Derive variables ------------------------------------------------------
  fps = 1/mean(diff(adata$time_stamp), na.rm = T)*1e3 # frames per second
  
  # . Convert to time series --------------------------------------------------
  ts_data = with(adata, 
                 ts(data = z_turn[!is.na(z_turn)], 
                    frequency = fps,
                    start = which.min(experimental_time[!is.na(z_turn)])
                    # end = which.max(experimental_time[!is.na(z_turn)])
                    )
                )
  if(validation)
  {
  ts_data = ts(data = ts_data + (sd(ts_data))*cos(
                        seq(from = tsp(ts_data)[1],
                            to = tsp(ts_data)[2] * 2*pi/target_freq,
                            length.out = length(ts_data))
                        ),
               frequency = fps,
               start = tsp(ts_data)[1]
              )
  }            

  # . Resample if necessary ---------------------------------------------------
  if(!is.null(resample))
  {
  new_time = seq(from = tsp(ts_data)[1],
                 to = tsp(ts_data)[2],
                 length.out = tsp(ts_data)[2]*resample
                )
  # . . Linear interpolation ------------------------------------------------
    if(res.method %in% 'linear')
    { 
      res_data = approx(x = time(ts_data),
                   y = ts_data,
                   xout = new_time
                   )$y
    }

    

# . . Moving average method -----------------------------------------------
    if(res.method %in% 'MAturnspeed')
    { 
      if(speedup_data.table){
        adata[, `:=`( #assign from list call
                      res_turn = if(speedup_parallel)
                      {
                        parSapply(cl = clt,
                                  X = 1:length(angle),#all indices in angle
                                  FUN = MAturnspeed,#the mean angle function
                                  dta = angle,#all angles observed
                                  window = resample,#window size (s)
                                  hz = fps #original sample rate (Hz)
                        )
                      }else
                      {
                        sapply(X = 1:length(angle),#all indices in angle
                               FUN = MAturnspeed,#the mean angle function
                               dta = angle,#all angles observed
                               window = resample,#window size (s)
                               hz = fps #original sample rate (Hz)
                              )
                      }
                    )
        ]
      }else
      {
        adata = within(adata,
                       {
                        res_turn = if(speedup_parallel)
                                        {
                                          parSapply(cl = clt,
                                                    X = 1:length(angle),#all indices in angle
                                                    FUN = MAturnspeed,#the mean angle function
                                                    dta = angle,#all angles observed
                                                    window = resample,#window size (s)
                                                    hz = fps #original sample rate (Hz)
                                          )
                                        }else
                                        {
                                          sapply(X = 1:length(angle),#all indices in angle
                                                 FUN = MAturnspeed,#the mean angle function
                                                 dta = angle,#all angles observed
                                                 window = resample,#window size (s)
                                                 hz = fps #original sample rate (Hz)
                                          )
                                        }
                       }
                      )
      }  
      res_data = c(0,
                   subset(adata, round(experimental_time*fps/resample) %in% 
                                  round(new_time*fps/resample)
                           )$res_turn
                  )
    }
  }

  # . . Overwrite time series -----------------------------------------------
  ts_data = ts(data = res_data[!is.na(res_data)],
             start = with(adata, 
                          {
                            which.min(experimental_time[!is.na(z_turn)])
                            }
                          ),
             frequency = resample
             )
  
  # . Calculate Welch PSD ---------------------------------------------------
    emp_data = empiricalSpectrum(ts_data)
    psd_data = welchPSD(x = ts_data,
                        seglength = av_window,
                        windowfun = tukeywindow,
                        method = "median", 
                        windowingPsdCorrection = TRUE
                        )
  
  # Plot the outcome --------------------------------------------------------
 with(emp_data,
      {
      plot(frequency,
           sqrt(power),
           log = 'x',
           type = 'l',
           xlim = c(1/(fps*1.2), fps*1.2),
           ylim = sqrt( c(0, max(c(emp_data$power, psd_data$power), na.rm = T) ) ),
           col = adjustcolor('darkblue', alpha.f = 100/255),
           lwd = 2,
           main = if(validation){paste(av_window/4, 'sec sinusoid')}else
                   {sub(x = basename(path_file),
                      pattern = '(.(dat)).*',
                      replacement = '')
                   }
           )  
      }
      )
  abline(v = c(target_freq,target_freq/4),
         lty = c(1, 3),
         col = adjustcolor('gray', alpha.f = 150/255)
         )
    with(psd_data,
         {
           lines(frequency,
                 sqrt(power),
                 lwd = 2,
                 col = adjustcolor('darkred', alpha.f = 150/255)
                 )
         }
         )
    legend(x = 'topright',
           legend  = c('discrete FFT',
                       paste0("Welch's PSD\n",
                              av_window," sec window"),
                       paste(target_freq, 'Hz'),
                       paste(target_freq/4, 'Hz')
                       ),
           col = c('darkblue',
                   'darkred',
                   'gray',
                   'gray'
                   ),
           cex = 0.7,
           lty = c(1, 1, 1, 3),
           lwd = c(2,2,1,1)
            )
}#that's all for now
  
# Plot data for each track -------------------------------------------------

FT_plot_track = function(path_file = FT_select_file(file_type = '_proc.csv.gz'),#path to the ".dat" file,
                         show_plot = TRUE, #Show the user the completed plot
                         point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
                         trend_col = "darkgreen", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
                         av_window = 5.0, #number of seconds to smooth over for averaging
                         plt_leg_pos = 'topleft', #legend position within plot
                         plt_leg_inset = c(0,-0.01), # legend xy inset
                         plt_leg_cex = 0.5, # legend scaling factor relative to plot
                         plt_speed_max = 200, #y axis maximum for ground speed
                         plt_turn_ax = 45, #tick interval for turn speed axes
                         plt_accel_scale = 5, #rescale factor for accelaration to fit turn speed axes
                         csv_sep_load = ',',#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
                         save_type = 'pdf',#Save as PDF or PNG
                         speedup_parallel = FALSE, #Use the parallel package to speed up calculations
                         speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
                         verbose = TRUE, #Tell the user what is going on
                         clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
                         {parallel::makeCluster(parallel::detectCores() - 1,type="SOCK")}else
                         {NULL}
                         )
{
  # . TODO   ---------------------------------------------
  #TODO   
  #- Fast version ?
  #- Frequency plot +
  
  # . Required packages -----------------------------------------------------
  invisible(
    { # hide verbose loading messages
      library(circular, quietly = TRUE)#Circular
      library(bspec, quietly = TRUE)#Bayesian Spectral Inference
      if(speedup_data.table){library(data.table, quietly = TRUE)}
      if(speedup_parallel){library(parallel, quietly = TRUE)}
    }
  )
  
  # . Read in data ----------------------------------------------------------
  adata = if(speedup_data.table)
  {
    data.table::fread(
      file = path_file,#read from user-selected file
      sep = csv_sep_load,#user specified comma separation
      header = TRUE#These files have headers
      #other parameters can be added here for troubleshooting
    )
  }else
  {
    read.table(file = gzfile(path_file),#read from user-selected file
               sep = csv_sep_load,#user specified comma separation
               header = TRUE#These files have headers
               #other parameters can be added here for troubleshooting
    )
  }
  
  
  
  # Conversions -------------------------------------------------------------
  
  # . Derive variables ------------------------------------------------------
  dff_time = diff(adata$time_stamp)
  fps = if(any(!is.na(dff_time)))
          {1/mean(dff_time, na.rm = T)*1e3}else
            {60}# frames per second
  
  # . Conversions -------------------------------------------------------------
  
  # . . Convert to time series --------------------------------------------------
  ts_data = if(sum(!is.na(adata$z_turn))>3)
              {
                with(subset(adata,!is.na(z_turn)), 
                     ts(data = z_turn, 
                        frequency = fps,
                        start = which.min(experimental_time)
                        # end = which.max(experimental_time[!is.na(z_turn)])
                     )
                    )
              }else
              {
                ts(rep(1,3))
              }
  ind_1 = with(subset(adata,!is.na(z_turn)), experimental_time <60)
  ind_2 = with(subset(adata,!is.na(z_turn)), experimental_time > 60 &
                 experimental_time < 120
               )
  ts_1 = if(sum(ind_1, na.rm = TRUE)>2)
          {
            ts(data = ts_data[ind_1 & !is.na(ts_data)],
              frequency = fps)
          }else
          {ts(rep(1,3))}
  ts_2 = if(sum(ind_2, na.rm = TRUE)>2)
          {
            ts(data = ts_data[ind_2 & !is.na(ts_data)],
              frequency = fps)
          }else
          {ts(rep(1,3))}
  emp_data1 = empiricalSpectrum(ts_1)
  emp_data2 = empiricalSpectrum(ts_2)
  
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
  par(pty = 's',
      mar = c(3,0,0,0)) # set plot area to square?
  #trajectory
  with(adata,
       {
         plot(x = y_pos,
              y = x_pos,
              xlim = if(any(!is.na(y_pos)))
                      {c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T)}else
                        {c(-1,1)*2000}, 
              ylim =  if(any(!is.na(x_pos)))
                        {c(-1,1)*max(abs(c(y_pos, x_pos)), na.rm = T)}else
                        {c(-1,1)*2000}, 
              col = hcl.colors(n = sum(!is.na(frame_counter)),
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
  pretty_time = with(adata, paste0(pretty(range(experimental_time, na.rm = T)), 's'))
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
  
  
  #sphere movement
  par(pty = 'm',
      mar = c(3,5,0,3))
  with(adata,
       {
         plot(x = experimental_time,
              y = circular::deg(y_ball),
              pch = 19,
              type = 'o',
              col = adjustcolor('darkred', alpha.f = 0.7),
              xlim = range(experimental_time, na.rm = TRUE), 
              ylim = c(-1,1)*360/60 *1.1, 
              xlab = 'experimental time (s)',
              ylab = 'rotation per frame (°)',
              cex = 0.2
         )
         mtext(text = 'y rotation (forwards)',
               line = -1.5
         )
         abline(h = 0,
                col = adjustcolor('black', alpha = 0.9),
                lwd = 0.5
         )
         plot(x = experimental_time,
              y = circular::deg(z_ball),
              pch = 19,
              type = 'o',
              col = adjustcolor('darkblue', alpha.f = 0.7),
              xlim = range(experimental_time, na.rm = TRUE), 
              ylim = c(-1,1)*360/60 *1.1, 
              xlab = 'experimental time (s)',
              ylab = 'rotation per frame (°)',
              cex = 0.2
         )
         mtext(text = 'z rotation (turning)',
               line = -1.5
         )
         abline(h = 0,
                col = adjustcolor('black', alpha = 0.9),
                lwd = 0.5
         )
         plot(x = experimental_time,
              y = circular::deg(x_ball),
              pch = 19,
              type = 'o',
              col = adjustcolor('cyan4', alpha.f = 0.7),
              xlim = range(experimental_time, na.rm = TRUE), 
              ylim = c(-1,1)*360/60 *1.1, 
              xlab = 'experimental time (s)',
              ylab = 'rotation per frame (°)',
              cex = 0.2
         )
         mtext(text = 'x rotation (lateral)',
               line = -1.5
         )
         abline(h = 0,
                col = adjustcolor('black', alpha = 0.9),
                lwd = 0.5
         )
         
       }
  )
  mtext(text = 'time (s)',
        side = 1,
        cex = plt_leg_cex,
        line = 3)
  
  # #Circular heading
  # with(adata,
  #      {
  #        circular::plot.circular(
  #            circular::circular(NA,
  #                               type = 'angles',
  #                               unit = 'degrees',
  #                               rotation = 'clock',
  #                               zero = pi/2,
  #                               modulo = '2pi'
  #        ),
  #        labels = 0:3*90,
  #        xlim = if(any(!is.na(experimental_time)))
  #                {c(-1,1)*max(experimental_time, na.rm = T)}else
  #                  {c(-1,1)*120},
  #        ylim = if(any(!is.na(experimental_time)))
  #                {c(-1,1)*max(experimental_time, na.rm = T)}else
  #                {c(-1,1)*120},
  #        shrink = if(any(!is.na(experimental_time)))
  #                  {1/max(experimental_time, na.rm = T)}else
  #                  {1}
  #        )
  #        if(any(!is.na(angle)))
  #        {
  #        circular::lines.circular(x = 
  #                 circular::circular(x = angle-180,
  #                                    type = 'angles',
  #                                    unit = 'degrees',
  #                                    template = 'geographics',
  #                                    modulo = '2pi'
  #        ),
  #        y = if(any(!is.na(experimental_time)))
  #              {experimental_time/max(experimental_time, na.rm = T)-1}else
  #              {experimental_time/120 - 1},
  #        col = adjustcolor(point_col, alpha.f = 0.5),
  #        type = 'p',
  #        pch = 19,
  #        lty = 2,
  #        cex = 0.5
  #        )
  #        }
  #        invisible(
  #          {
  #            lapply(X = if(any(!is.na(experimental_time)))
  #                        {10*(0:(max(experimental_time, na.rm = T)/10))}else
  #                        {10*0:12},
  #                   FUN =function(i)
  #                   {
  #                     circular::lines.circular(
  #                       x = circular::circular(x = seq(from = -pi, 
  #                                            to = pi, 
  #                                            length.out = 1e3),
  #                                    template = 'none'),
  #                       y = rep(x = if(any(!is.na(experimental_time)))
  #                                     {i/max(experimental_time, na.rm = T) - 1}else
  #                                     {i/120 - 1}, 
  #                               times = 1e3),
  #                       col = gray(level = 0, alpha = 0.5)
  #                     )
  #                   }
  #            )
  #          }
  #        )
  #      }
  # )
  # circular::lines.circular(
  #   x = circular::circular(x = seq(from = -pi, 
  #                        to = pi, 
  #                        length.out = 1e3),
  #                template = 'none'),
  #   y = rep(x = 60/max(adata$experimental_time, na.rm = T) - 1, 
  #           times = 1e3),
  #   col = adjustcolor('darkred', alpha.f = 0.5),
  #   lwd = 5
  # )
  # if(any(!is.na(adata$experimental_time)))
  # {
  #   with(adata,
  #        {
  #          axis(side = 1,
  #               at = 10*round(-(max(experimental_time/10, na.rm = T)):(max(experimental_time/10, na.rm = T)))/max(experimental_time, na.rm = T),
  #               labels = abs(
  #                 10*round(-(max(experimental_time/10, na.rm = T)):(max(experimental_time/10, na.rm = T)))
  #               ),
  #               cex.axis = plt_leg_cex/1.5
  #          )
  #        }
  #   )
  # }
  # mtext(text = 'Time (sec)',
  #       outer = T,
  #       side = 1
  # )
  
  # . Plot time series --------------------------------------------------
  
  # . . Ground speed --------------------------------------------------------
  
  par(pty = 'm',
      mfrow = c(5, 1),
      mar = c(3,5,0,3)
  )
  #Raw data
  with(adata,
       {
         plot(x = NULL,
              xlab = 'time since start',
              ylab = 'forward speed (mm/s)',
              xlim = c(0, max(x = experimental_time, na.rm = T)),
              ylim = c(0, max(x = c(ma_speed,plt_speed_max), na.rm = T)),
              type = 'p',
              pch = 19,
              cex = 0.1,
              col = adjustcolor(point_col, alpha.f = 20/256),
              axes = F
         )
         lines(x = experimental_time,
               y = forward_speed,
               # col = adjustcolor('orange', alpha.f = 100/256),
               col = adjustcolor(point_col, alpha.f = 100/256),
               cex = 0.1,
               pch = 19)
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
         abline(h = 0,
                col = 1)
         
       }
  )
  #label stops
  with(adata,
       {
         abline(v = experimental_time[stop_flag],
                col = adjustcolor(col = 'red',
                                  alpha.f = 20/256),
                lwd = 1
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
                    # paste0('moving average (median: ',av_window,'s)'),
                    # 'forward speed',
                    paste0('Savitzky-Golay filtered'),
                    # 'forward speed',
                    paste0('stop (fwd speed <', stop_speed, ' mm/s for ', stop_length, ' s)')
         ),
         lty = c(1,1,NA),
         pch = c(NA,NA,15),
         col = c(point_col,
                 trend_col,
                 'red'),
         cex = plt_leg_cex
  )
  
  # . . Fictive direction ---------------------------------------------------
  #Raw data
  with(adata,
       {
         plot(x = experimental_time,
              y = Mod360.180(angle),
              ylim = c(-180,180),
              xlim = if(any(!is.na(experimental_time)))
                      {range(experimental_time, na.rm = T)}else
                      {c(0,120)},
              xlab = 'time since start',
              ylab = 'fictive direction',
              type = 'p',
              pch = 19,
              cex = 0.1,
              col = adjustcolor(point_col, alpha.f = 20/256),
              axes = F
         )
         if(any(!is.na(experimental_time)))
         {
           axis(side = 1,
                at = 10*(0:(max(experimental_time, na.rm = T)/10)),
                labels = 10*(0:(max(experimental_time, na.rm = T)/10))
           )
         }
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
              xlim = if(any(!is.na(experimental_time)))
                      {range(experimental_time, na.rm = T)}else
                      {c(0,120)},
              ylim = if(any(!is.na(z_turn)))
                      {
                       quantile(x = z_turn, 
                                probs = c(0,1)+c(1,-1)*0.05,
                                na.rm = T)
                      }else
                      {c(-45,45)},
              xlab = 'time (s)',
              ylab = paste0('turning speed (°/s: ',av_window,'s)'),
              axes = F
         )
         if(any(!is.na(experimental_time)))
         {
           axis(side = 1,
                at = 10*(0:(max(experimental_time, na.rm = T)/10)),
                labels = 10*(0:(max(experimental_time, na.rm = T)/10))
           )
           axis(side = 2,
                at = plt_turn_ax*(round(min(z_turn, na.rm = T)/plt_turn_ax):round(max(z_turn, na.rm = T)/15))
           )
         }
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
               col = adjustcolor(trend_col, offset = c(0.5,0.5,0.5,0))
         )
         abline(h = c(-15, 0, 15),
                v = c(0,60,120),
                col = 'black',
                lwd = 0.25
         )
       }
  )
  legend(x = plt_leg_pos,
          inset= plt_leg_inset,
          xpd = TRUE,
          legend = c('instantaneous',
                     paste0('median ', '(', av_window,'s)'),
                     'smoothing spline'),
          lty = c(1,NA, 1),
          pch = c(NA,19, NA),
          col = c(point_col,
                  trend_col,
                  adjustcolor(trend_col, offset = c(0.5,0.5,0.5,0))
          ),
          cex = plt_leg_cex
  )
  
  # . . Acceleration --------------------------------------------------------
  
  with(adata,
       {
         plot(x = experimental_time,
              y = inst_accel,
              col = adjustcolor(point_col, alpha.f = 50/256),
              xlim = if(any(!is.na(experimental_time)))
                      {range(experimental_time, na.rm = T)}else
                      {c(0,120)},
              ylim = if(any(!is.na(z_turn)))
                      {
                      quantile(x = inst_accel, 
                                      probs = c(0,1)+c(1,-1)*0.05,
                                      na.rm = T)
                      }else
                      {c(-45,45)},
              type = 'l',
              xlab = 'time (s)',
              ylab = bquote(acceleration ~
                              '(°/s'^2 ~ 
                              .(av_window)*s ~ ")"),
              axes = FALSE
         )
         axis(side = 1,
              at = 10*(0:(max(experimental_time)/10)),
              labels = 10*(0:(max(experimental_time)/10))
         )
         axis(side = 2,
              at = plt_turn_ax*(round(min(inst_accel, na.rm = T)/plt_turn_ax):round(max(inst_accel, na.rm = T)/15))
         )
         abline(h = c(-15, 0, 15)/plt_accel_scale,
                v = c(0,60,120),
                col = 'black',
                lwd = 0.25
         )
         lines(x = experimental_time,
               y = smooth_accel,
               col = adjustcolor(trend_col, alpha.f = 200/256)
         )
         lines(x = experimental_time,
               y = ma_accel,
               lwd = 3,
               col = adjustcolor(trend_col, offset = c(0.5,0.5,0.5,0)),
         )
       }
  )
  legend(x = plt_leg_pos,
         inset= plt_leg_inset,
         xpd = TRUE,
         legend = c('instantaneous',
                    'from smoothed speed',
                    paste0('median ', '(', av_window,'s)')
         ),
         lty = c(1,1, 1),
         lwd = c(1,1, 3),
         pch = c(NA, NA, NA),
         col = c(point_col,
                 trend_col,
                 adjustcolor(trend_col, offset = c(0.5,0.5,0.5,0))
         ),
         cex = plt_leg_cex
  )
  # 
  # # . . Mean vector length --------------------------------------------------
  # with(adata,
  #      {
  #        plot(x = NULL,
  #             xlim = range(experimental_time, na.rm = T),
  #             ylim = c(0,1),
  #             xlab = 'time (s)',
  #             ylab = paste0('mean vector length (',av_window,'s)'),
  #             axes = F
  #        )
  #        axis(side = 1,
  #             at = 10*(0:(max(experimental_time)/10)),
  #             labels = 10*(0:(max(experimental_time)/10))
  #        )
  #        axis(side = 2,
  #             at = 0:5/5
  #        )
  #        lines(x = experimental_time,
  #              y = ma_rho,
  #              col = point_col
  #        )
  #        abline(h = c(0,1),
  #               v = c(0,60,120),
  #               col = 'black',
  #               lwd = 0.25
  #        )
  #        abline(h = sqrt(-log(c(0.05, 0.01)))/(av_window*fps),#Mean vector Rayleigh test p
  #               col = 'red',
  #               lty = c(3,2),
  #               lwd = 0.25
  #        )
  #        
  #      }
  # )
  # legend(x = plt_leg_pos,
  #        inset= plt_leg_inset,
  #        xpd = TRUE,
  #        legend = c(paste0('mean vector ', '(', av_window,'s)'),
  #                   'Rayleigh (p = 0.05)'),
  #        lty = c(1, 3),
  #        pch = c(NA, NA),
  #        col = c(point_col,
  #                'red'),
  #        cex = plt_leg_cex
  # )

# . Plot frequency --------------------------------------------------------
  with(emp_data1,
       {
         plot(frequency,
              sqrt(power),
              log = 'x',
              type = 'l',
              xlim = c(1/(fps*1.2), fps*1.2),
              ylim = if( any(!is.na(power)) )
                        {sqrt( c(0, max(c(emp_data1$power, emp_data2$power)) ) )}else
                        {c(0,1)},
              col = adjustcolor('darkblue', alpha.f = 100/255),
              lwd = 2
              # main = 'Discrete FFT'
             )  
       }
  )
  mtext(text = 'Discrete FFT',
        side = 4,
        cex = plt_leg_cex)
  abline(v = c(0.1),
         lty = c(1),
         col = adjustcolor('gray', alpha.f = 150/255)
  )
  with(emp_data2,
       {
         lines(frequency,
               sqrt(power),
               lwd = 2,
               col = adjustcolor('darkred', alpha.f = 150/255)
         )
       }
  )
  legend(x = plt_leg_pos,
         legend  = c('0 to 60s',
                     '60 to 120s',
                     '0.1Hz (10s)'
                     ),
         col = c('darkblue',
                 'darkred',
                 'gray'
         ),
         lty = c(1, 1, 1, 3),
         lwd = c(2,2,1,1),
         xpd = TRUE,
         inset= plt_leg_inset,
         cex = plt_leg_cex
  )
  
  # . . Outer labels --------------------------------------------------------
  
  mtext(text = paste( basename(dirname(path_file)),
                     basename(path_file) ),
        outer = T, 
        side = 3
  )
  mtext(text = 'Time (sec)',
        outer = T,
        side = 1
  )
  # . Save plot -------------------------------------------------------------
  dev.off()
  if(show_plot){shell.exec.OS(plot_file)}#open the plot in the OS native program
  return(plot_file)
}

# Summarise data ----------------------------------------------------------

FT_summarise_all = function(
    comb_file = FT_select_file(file_type = '_proc.txt.gz'),
    experiment_length = 2, #minutes
    condition1_length = 1, #minutes
    av_window = 5.0,#number of seconds to smooth over for averaging
    point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
    save_type = "png",# "pdf",#
    quantls = c(0.025, 0.25, 0.5, 0.75, 0.975), # quantiles to use when summarising
    sys_win = Sys.info()[['sysname']] == 'Windows',
    speedup_parallel = TRUE, #Use the parallel package to speed up calculations
    speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
    verbose = TRUE, #Tell the user what is going on
    show_plot = TRUE, #Tell the user what is going on
    clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
    {parallel::makeCluster(parallel::detectCores() - 1,type="SOCK")}else
    {NULL},
    ... #passed to; I don't know yet
)
{
  # Details ---------------------------------------------------------------
  #       AUTHOR:	James Foster              DATE: 2021 11 18
  #     MODIFIED:	James Foster              DATE: 2022 03 08
  #
  #  DESCRIPTION: Loads "_proc.txt.gz" files saved by "FT_combine_folders()" from 
  #               "FictracDat_Functions.R" and compiles them into a single dataset 
  #                for the whole experiment.
  #               
  #       INPUTS: A "_proc.txt.gz" file with all relevant headers.
  #               User should specify processing details in function inputs.
  #               
  #      OUTPUTS: Plot (.pdf or .png). Data table (.csv).
  #
  #	   CHANGES: - 
  #             - 
  #             - 
  #
  #   REFERENCES: Batschelet E (1981).
  #               In: Circular Statistics in Biology
  #               Academic Press (London)
  #
  #    EXAMPLES:  FT_summarise_all()
  # 
  #TODO   ---------------------------------------------
  #TODO   
  #- Read in data +
  #- Save data as txt + 
  #- Looping  +
  #- Combine all to csv +
  #- Angular acceleration + 
  #- Switch to fread to speed up +
  #- Comment
  
  # Useful functions --------------------------------------------------------
  # . Load package ----------------------------------------------------------
  #needs installing before first use (in Rstudio, see automatic message)
  suppressMessages(#these are disturbing users unnecessarily
    {
      require(circular)#package for handling circular data
      if(speedup_data.table){require(data.table)}#package for reorganising large datasets
      if(speedup_parallel){require(parallel)}#package for reorganising large datasets
    }
  )
  
  # . Read in files ------------------------------------------------------------
  if(verbose){message('\n','Reading in "', basename(comb_file), '"\nplease be patient...')}
  tryCatch(#Perform no further analysis if the file doesn't load
    {
      time_data_table = 
        if(speedup_data.table)
        {
          data.table::fread(
            file = comb_file, 
            sep = '\t',
            header = TRUE
          )#,#read from user-selected file
        }else
        {
          read.table(
            file = gzfile(comb_file), 
            sep = '\t',
            header = TRUE
          )#,#read from user-selected file
        }
    },
    error = function(e)
    {
      stop(
        paste0('"',
               basename(comb_file), 
               '" could not be loaded!\n',
               e)
      )
    }
  )
  if(verbose){message('"',basename(comb_file), '" loaded successfully')}
  
  # 
  # # Load and combine --------------------------------------------------------
  # day_files = lapply(X = path_folders,
  #                    FUN = list.files,
  #                    pattern = '_proc.txt$'
  # )
  # day_file_indices = which(as.logical(sapply(day_files, length)))
  # day_files_paths = file.path(path_folders[day_file_indices], 
  #                             day_files[day_file_indices])
  # 
  # day_data = lapply(day_files_paths,
  #                   FUN = read.table,
  #                   header = TRUE
  # )
  # 
  # # . Combine datasets across dates -----------------------------------------
  # names(day_data) = basename(path_folders[day_file_indices])
  # day_data_frame = do.call(what = rbind,
  #                          args = day_data)
  # day_data_frame$date = regmatches(m = regexpr(pattern = '^([^.]+)',
  #                                              text = rownames(day_data_frame) ),
  #                                  x = rownames(day_data_frame) )
  
  # # . Extract features ------------------------------------------------------
  # ExperimentFull = function(experiment_times,#time since first trigger, seconds
  #                           exper_length)#length of experiment, minutes
  # {max(experiment_times/60, na.rm = TRUE) > exper_length}
  # full_length_experiments = aggregate(formula = experimental_time ~ date*bumblebee*trial,
  #                                     data = day_data_frame,
  #                                     FUN = ExperimentFull,
  #                                     exper_length = experiment_length)
  # full_length_conditions = aggregate(formula = experimental_time ~ date*bumblebee*trial,
  #                                    data = day_data_frame,
  #                                    FUN = ExperimentFull,
  #                                    exper_length = condition1_length)
  # full_length_experiments = within(full_length_experiments,
  #                                  {flag_exp = experimental_time; rm(experimental_time)}
  # )
  # full_length_conditions = within(full_length_conditions,
  #                                 {flag_cnd = experimental_time; rm(experimental_time)}
  # )
  # full_flags = merge(x = full_length_experiments,
  #                    y = full_length_conditions)
  # #merge data by shared variable names
  # #can also do this with merge.data.frame, but data.table method is much faster
  # all_data_table = data.table::merge.data.table(
  #   x = day_data_frame, 
  #   y = full_flags)
  # rm(day_data_frame)#one copy in memory is enough
  # # View(all_data_table)
  # all_data_table = within(all_data_table,
  #                         {
  #                           abs_turn = abs(ma_turn)  
  #                           abs_accel = abs(ma_accel)  
  #                         }
  # )
  # 
  # # . Save combined dataset in master folder --------------------------------
  # comb_file <- file.path(path_folder1,
  #                        paste0(basename(path_folder1),
  #                               '_all',
  #                               '.csv')
  # )
  # message('Saving data as:\n', 
  #         gsub(pattern = '/',
  #              x = comb_file,
  #              replacement = '\n')
  # )
  # message('May take some time...','\n')
  # write.csv(x = all_data_table,
  #           file = comb_file,
  #           row.names = FALSE
  # )
  # 
  # # . Check that there is still some data to summarise ----------------------
  # if( with(all_data_table, !any( flag_exp)) )
  # {stop('\n',
  #       "NONE OF THE FILES LOADED WERE ", experiment_length, " min LONG!",
  #       '\n', "Consider reducing the 'experiment length' input variable")
  # }else
  # {
  #   message( 
  #     round(
  #       with(all_data_table, sum(flag_exp))/ 
  #         (experiment_length*sample_rate*60)
  #     ),
  #     " files were ", experiment_length, " min long or longer"
  #   )
  # }
  
  # . Check that there is some data to summarise --------------------------
  sample_rate = 1/mean(diff(
    subset(all_data_table, 
           subset = track == unique(track)[1] &
             date == unique(date)[1] 
    )$experimental_time
  ), na.rm = T)
  #set up function to flag full length experiments #N.B. this currently looks quite slow
  FlagExper = function(trackID,
                       dta,
                       experiment_length = 2)
  {
    if(with(dta, length(experimental_time) != length(track)))
    {stop('timeID and trackID vectors must be the same length')}
    return( #TODO speed up with data.table
      with(subset(dta, track %in% trackID), #subset to single track
           {
             rep(x = (max(experimental_time)/60)>
                   experiment_length,#is the experiment longer than the minimum?
                 times = length(experimental_time)) #replicate logical
           }
      )
    )
  }
  if(speedup_parallel)
  {
    clusterExport(cl = clust,
                  varlist = list('all_data_table',
                                 'FlagExper',
                                 'experiment_length'),
                  envir = environment()
    )
  }
  #Search for full length experiments
  all_data_table = within(all_data_table,
                          {
                            flag_exp = if(speedup_parallel)
                            {unlist(
                              parSapply(cl = clust,
                                        X = unique(track),
                                        FUN = FlagExper,
                                        dta = all_data_table,
                                        experiment_length = experiment_length
                              )
                            )
                            }else
                            {unlist(
                              sapply(
                                X = unique(track),
                                FUN = FlagExper,
                                dta = all_data_table,
                                experiment_length = experiment_length
                              )
                            )
                            }
                          }
  )
  # . . Close the cluster if it is not needed anymore -----------------------
  if(speedup_parallel){ if(base::missing(clust)){parallel::stopCluster(clust)} }#only close internal cluster
  
  # . . Tell the user how many were correct length --------------------------
  if( with(all_data_table, !any( flag_exp)) )
  {stop('\n',
        "NONE OF THE FILES LOADED WERE ", experiment_length, " min LONG!",
        '\n', "Consider reducing the 'experiment length' input variable")
  }else
  {
    if(verbose){
      message( 
        round(
          with(all_data_table, sum(flag_exp))/ 
            (experiment_length*sample_rate*60)
        ),
        " files were ", experiment_length, " min long or longer"
      )
    }
  }
  
  # . Remove stops ----------------------------------------------------------
  #where a stop is identified, set the summarised variables to NA 
  #to avoid biasing analysis
  all_data_table = within(all_data_table,
                          {
                            ma_rho[stop_flag] = NA
                            ma_turn[stop_flag] = NA
                            abs_turn[stop_flag] = NA
                            abs_accel[stop_flag] = NA
                            angle[stop_flag] = NA
                          }
  )
  
  
  # . Summarise data --------------------------------------------------------
  if(verbose){message('Summarising data from full experiments...','\n')}
  #summarise by in full experiments
  time_rho = aggregate(formula = ma_rho~experimental_time,
                       data = subset(all_data_table, flag_exp),
                       FUN = quantile,
                       p = quantls)
  time_turn = aggregate(formula = ma_turn~experimental_time,
                        data = subset(all_data_table, flag_exp),
                        FUN = quantile,
                        p = quantls)
  time_abs_turn = aggregate(formula = abs_turn~experimental_time,
                            data = subset(all_data_table, flag_exp),
                            FUN = quantile,
                            p = quantls)
  time_abs_accel = aggregate(formula = abs_accel~experimental_time,
                             data = subset(all_data_table, flag_exp),
                             FUN = quantile,
                             p = quantls)
  time_angle = aggregate(formula = angle ~ experimental_time,
                         data = subset(all_data_table, flag_exp),
                         FUN = function(a){
                           quantile.circular(
                             x = circular(a,
                                          type = 'angles',
                                          unit = 'degrees',
                                          template = 'geographics',
                                          modulo = '2pi',
                                          zero = pi/2,
                                          rotation = 'clock'),
                             p = quantls)
                         }
  )
  #combine data into one table of summary measures
  time_data_table = data.table::merge.data.table( # finally add time angle
    x = 
      data.table::merge.data.table( # then combine with rho using experimental_time
        x =data.table::data.table(time_rho),
        y = data.table::merge.data.table( # first combine absolute
          x =data.table::data.table(time_abs_turn) ,
          y =data.table::data.table(time_abs_accel) 
        ),
        by = 'experimental_time'
      ),
    y =data.table::data.table(time_angle)
  )
  
  # . Save combined dataset in master folder --------------------------------
  path_folder1 = dirname(comb_file)
  summ_file = file.path(path_folder1,
                        paste0(basename(comb_file),
                               '_summary',
                               '.csv')
  )
  if(verbose){
    message('Saving data summary as:\n', 
            gsub(pattern = '/',
                 x = summ_file,
                 replacement = '\n')
    )
    message('May take some time...','\n')
  }
  if(speedup_data.table)
  {
    fwrite(x = time_data_table,
           file = summ_file,
           row.names = FALSE
    )
  }else
  {
    write.csv(x = time_data_table,
              file = summ_file,
              row.names = FALSE
    )
  }
  if(show_plot)
  {FT_plot_summary(csv_file = summ_file)}
  return(summ_file)
}

FT_plot_summary = function(
    csv_file = FT_select_file(file_type = '_summary.csv'),
    experiment_length = 2, #minutes
    condition1_length = 1, #minutes
    av_window = 5.0,#number of seconds to smooth over for averaging
    point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
    save_type = "pdf",# "png",#
    quantls = c(0.025, 0.25, 0.5, 0.75, 0.975), # quantiles to use when summarising
    plette = 'Plasma',#'YlGnBu'#  for options see http://colorspace.r-forge.r-project.org/articles/approximations.html
    crossval = FALSE, # TRUE, randomise the data to check the analysis
    sys_win = Sys.info()[['sysname']] == 'Windows',
    speedup_parallel = TRUE, #Use the parallel package to speed up calculations
    speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
    verbose = TRUE, #Tell the user what is going on
    show_plot = TRUE, #Tell the user what is going on
    clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
    {parallel::makeCluster(parallel::detectCores() - 1,type="SOCK")}else
    {NULL}
)
{
  path_folder1 = dirname(csv_file)
  # Read in data ------------------------------------------------------------
  # . Read in files ------------------------------------------------------------
  if(verbose){message('\n','Reading in "', basename(csv_file), '"\nplease be patient...')}
  tryCatch(#Perform no further analysis if the file doesn't load
    {#try to load
      time_data_table = 
        if(speedup_data.table)
        {
          data.table::fread(
            file = csv_file, 
            sep = ',',
            header = TRUE
          )#,#read from user-selected file
        }else
        {
          read.table(
            file = csv_file, 
            sep = ',',
            header = TRUE
          )#,#read from user-selected file
        }
    },
    error = function(e)
    {#if it doesn't load
      stop(
        paste0('"',
               basename(csv_file), 
               '" could not be loaded!\n',
               e)
      )
    }
  )
  if(verbose){message('"',basename(csv_file), '" loaded successfully')}
  
  # . . Close the cluster if it is not needed anymore -----------------------
  if(speedup_parallel){ if(base::missing(clust)){parallel::stopCluster(clust)} }#only close internal cluster
  
  # Plot Summary ------------------------------------------------------------
  # . Set up plot area ------------------------------------------------------
  save_base = paste0('_summ','.', save_type)
  plot_file = file.path(path_folder1, 
                        paste0(basename(path_folder1),
                               save_base)
  )
  #if there is already a file with that name
  if(file.exists(plot_file))
  {
    message('A plot called "', basename(plot_file), '" already exists in this folder.')
    nnm = readline(prompt = 'New plot name: '
    )
    
    plot_file = file.path(dirname(path_folder1),
                          paste0(ifelse(test = nchar(nnm),
                                        yes = nnm,
                                        no = basename(path_folder1)
                          ),
                          save_base)
    )
  }
  switch(EXPR = save_type,
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
  switch(EXPR = save_type,
         png = par(mfrow = c(3,1),
                   mar = c(3,5,0,0),
                   oma = c(1.5,0,1.5,0),
                   cex = 1.5
         ),
         par(mfrow = c(3,1),
             mar = c(3,5,0,0),
             oma = c(1.5,0,1.5,0))
  )
  
  # . Plot summarys ---------------------------------------------------------
  
  # . . Turning speed -------------------------------------------------------
  with(time_data_table,
       {
         plot(x = NULL,
              xlim = range(experimental_time, na.rm = T),
              ylim = c(0,360)/2,#range(c(abs_turn.2.5.,abs_turn.97.5.), na.rm = T),
              xlab = 'time (s)',
              ylab = paste0('absolute mean turning speed (°/s: ',av_window,'s)'),
              axes = F
         )
         axis(side = 1,
              at = 60*(0:(max(experimental_time, na.rm = T)/60)),
              labels = 1*(0:(max(experimental_time, na.rm = T)/60))
         )
         axis(side = 2,
              at = 45*(0:360/45)
         )
         polygon(x = c(experimental_time,
                       rev(experimental_time) ),
                 y = c(abs_turn.97.5.,
                       rev(abs_turn.2.5.)),
                 col = 'gray',
                 border = NA
         )
         polygon(x = c(experimental_time,
                       rev(experimental_time) ),
                 y = c(abs_turn.25.,
                       rev(abs_turn.75.)),
                 col = 'lightblue',
                 border = NA
         )
         abline(#v = 60*c(2,4,6,8,10),
           v = 60*seq(from = 0,
                      to = experiment_length,
                      by = condition1_length),
           col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
           lwd = 2
         )
         lines(x = experimental_time,
               y = abs_turn.50.,
               col = point_col
         )
         abline(h = c(0, 15, 90),
                col = 'black',
                lwd = 0.25
         )
       }
  )
  
  # . . Acceleration --------------------------------------------------------
  with(time_data_table,
       {
         plot(x = NULL,
              xlim = range(experimental_time, na.rm = T),
              ylim = c(0,360)/20,#range(c(abs_turn.2.5.,abs_turn.97.5.), na.rm = T),
              xlab = 'time (s)',
              ylab = paste0('absolute mean acceleration (°/s^2 : ',av_window,'s)'),
              axes = F
         )
         axis(side = 1,
              at = 60*(0:(max(experimental_time, na.rm = T)/60)),
              labels = 1*(0:(max(experimental_time, na.rm = T)/60))
         )
         axis(side = 2,
              at = 15*(0:360/15)
         )
         polygon(x = c(experimental_time,
                       rev(experimental_time) ),
                 y = c(abs_accel.97.5.,
                       rev(abs_accel.2.5.)),
                 col = 'gray',
                 border = NA
         )
         polygon(x = c(experimental_time,
                       rev(experimental_time) ),
                 y = c(abs_accel.25.,
                       rev(abs_accel.75.)),
                 col = 'lightblue',
                 border = NA
         )
         abline(#v = 60*c(2,4,6,8,10),
           v = 60*seq(from = 0,
                      to = experiment_length,
                      by = condition1_length),
           col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
           lwd = 2
         )
         lines(x = experimental_time,
               y = abs_accel.50.,
               col = point_col
         )
         abline(h = c(0, 15, 90),
                col = 'black',
                lwd = 0.25
         )
       }
  )
  
  
  # . . Mean vector length --------------------------------------------------
  with(time_data_table,
       {
         plot(x = NULL,
              xlim = range(experimental_time, na.rm = T),
              ylim = c(0,1),
              xlab = 'time (s)',
              ylab = paste0('mean vector length (',av_window,'s)'),
              axes = F
         )
         axis(side = 1,
              at = 60*(0:(max(experimental_time, na.rm = T)/60)),
              labels = 1*(0:(max(experimental_time, na.rm = T)/60))
         )
         axis(side = 2,
              at = 0:5/5
         )
         polygon(x = c(experimental_time,
                       rev(experimental_time) ),
                 y = c(ma_rho.97.5.,
                       rev(ma_rho.2.5.)),
                 col = 'gray',
                 border = NA
         )
         polygon(x = c(experimental_time,
                       rev(experimental_time) ),
                 y = c(ma_rho.25.,
                       rev(ma_rho.75.)),
                 col = 'lightblue',
                 border = NA
         )
         abline(#v = 60*c(2,4,6,8,10),
           v = 60*seq(from = 0,
                      to = experiment_length,
                      by = condition1_length),
           col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
           lwd = 2
         )
         lines(x = experimental_time,
               y = ma_rho.50.,
               col = point_col
         )
         abline(h = c(0,1),
                col = 'black',
                lwd = 0.25
         )
       }
  )
  # . Save plot -------------------------------------------------------------
  dev.off()
  shell.exec.OS(plot_file)
}

FT_TimeAverage_all = function(
    path_file = FT_select_file(file_type = '_proc.txt.gz'),
    experiment_length = 2, #minutes
    condition1_length = 1, #minutes
    av_window = 5.0,#number of seconds to smooth over for averaging
    med_window = 10.0,#number of seconds phase length to average over
    point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
    save_type = "png",# "pdf",# 
    quantls = c(0.025, 0.25, 0.5, 0.75, 0.975), # quantiles to use when summarising
    plette = 'Plasma',#'YlGnBu'#  for options see http://colorspace.r-forge.r-project.org/articles/approximations.html
    crossval = FALSE, # TRUE, randomise the data to check the analysis
    sys_win = Sys.info()[['sysname']] == 'Windows',
    speedup_parallel = TRUE, #Use the parallel package to speed up calculations
    speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
    verbose = TRUE, #Tell the user what is going on
    show_plot = TRUE, #Tell the user what is going on
    show_csv = TRUE, #Open the output table after saving
    clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
    {parallel::makeCluster(parallel::detectCores() - 1,type="SOCK")}else
    {NULL}
)
{
  # Details ---------------------------------------------------------------
  #       AUTHOR:	James Foster              DATE: 2021 11 30
  #     MODIFIED:	James Foster              DATE: 2021 03 29
  #
  #  DESCRIPTION: Loads "_proc.txt.gz" files saved by "FT_combine_folders()" from 
  #               "FictracDat_Functions.R" and compiles them into a single dataset 
  #                for the whole experiment.
  #               
  #       INPUTS: A "_proc.txt.gz" table with a column of time stamps and column of angles ("angle").
  #               User should specify processing details in input.
  #               
  #      OUTPUTS: Data table (.csv). Plot (.pdf or .png). 
  #
  #	   CHANGES: - Became a function
  #             - Preserve condition
  #             - Include speed
  #
  #   REFERENCES: Batschelet E (1981).
  #               In: Circular Statistics in Biology
  #               Academic Press (London)
  #
  #    EXAMPLES:  FT_TimeAverage_all()
  # 
  #TODO   ---------------------------------------------
  #TODO   
  #- Read in data +
  #- Time averaging +
  #- Combine all to csv +
  #- Plot  +
  #- Test on multiple machines
  #- Comment
  
  # Useful functions --------------------------------------------------------
  # . Load package ----------------------------------------------------------
  #needs installing before first use (in Rstudio, see automatic message)
  suppressMessages(#these are disturbing users unnecessarily
    {
      require(circular)#package for handling circular data
      if(speedup_data.table){require(data.table)}#package for reorganising large datasets
      if(speedup_parallel){require(parallel)}#package for reorganising large datasets
    }
  )
  
  # Input Variables ----------------------------------------------------------
  
  
  # Read in files ------------------------------------------------------------
  message('\n','Reading in "', basename(path_file), '"\nplease be patient...')
  tryCatch(#Perform no further analysis if the file doesn't load
    {
      day_data_table = 
        if(speedup_data.table)
        {
          data.table::fread(
            file = path_file, 
            sep = '\t',
            header = TRUE
          )#,#read from user-selected file
        }else
        {
          read.table(
            file = gzfile(path_file), 
            sep = '\t',
            header = TRUE
          )#,#read from user-selected file
        }
    },
    error = function(e)
    {
      stop(
        paste0('"',
               basename(path_file), 
               '" could not be loaded!\n',
               e)
      )
    }
  )
  message('"',basename(path_file), '" loaded successfully')
  # View(day_data_table)
  
  # . Check that there is some data to summarise --------------------------
  sample_rate = 1/mean(diff(
    subset(day_data_table, 
           subset = track == unique(track)[1] &
             date == unique(date)[1] 
    )$experimental_time
  ))
  #set up function to flag full length experiments #N.B. this currently looks quite slow
  FlagExper = function(trackID,
                       dta,
                       experiment_length = 2)
  {
    if(with(dta, length(experimental_time) != length(track)))
    {stop('timeID and trackID vectors must be the same length')}
    return( #TODO speed up with data.table
      with(subset(dta, track %in% trackID), #subset to single track
           {
             rep(x = (max(experimental_time, na.rm = T)/60)>
                   experiment_length,#is the experiment longer than the minimum?
                 times = length(experimental_time)) #replicate logical
           }
      )
    )
  }
  if(speedup_parallel)
  {
    clusterExport(cl = clust,
                  varlist = list('day_data_table',
                                 'FlagExper',
                                 'experiment_length'),
                  envir = environment()
    )
  }
  #Search for full length experiments
  day_data_table = within(day_data_table,
                          {
                            flag_exp = if(speedup_parallel)
                            {unlist(
                              parSapply(cl = clust,
                                        X = unique(track),
                                        FUN = FlagExper,
                                        dta = day_data_table,
                                        experiment_length = experiment_length
                              )
                            )
                            }else
                            {unlist(
                              sapply(
                                X = unique(track),
                                FUN = FlagExper,
                                dta = day_data_table,
                                experiment_length = experiment_length
                              )
                            )
                            }
                          }
  )
  # . . Close the cluster if it is not needed anymore -----------------------
  if(speedup_parallel){ if(base::missing(clust)){parallel::stopCluster(clust)} }#only close internal cluster
  
  # . . Tell the user how many were correct length --------------------------
  if( with(day_data_table, !any( flag_exp)) )
  {stop('\n',
        "NONE OF THE FILES LOADED WERE ", experiment_length, " min LONG!",
        '\n', "Consider reducing the 'experiment length' input variable")
  }else
  {
    message( 
      round(
        with(day_data_table, sum(flag_exp))/ 
          (experiment_length*sample_rate*60)
      ),
      " files were ", experiment_length, " min long or longer"
    )
  }
  
  # . Remove stops ----------------------------------------------------------
  #where a stop is identified, set the summarised variables to NA 
  #to avoid biasing analysis
  day_data_table = within(day_data_table,
                          {
                            ma_rho[stop_flag] = NA
                            ma_turn[stop_flag] = NA
                            abs_turn[stop_flag] = NA
                            abs_accel[stop_flag] = NA
                            angle[stop_flag] = NA
                          }
  )
  
  # Summarise data --------------------------------------------------------
  # . Cross validation ------------------------------------------------------
  #randomise data to check the method
  if(crossval)
  {
    if(verbose){message('Randomising data to cross-validate...','\n')}
    day_data_table = within(day_data_table,
                            {#randomise each parameter of interest across the dataset
                              forward_speed = sample(x = forward_speed,
                                              size = length(bumblebee),
                                              replace = FALSE)
                              ma_rho = sample(x = ma_rho,
                                              size = length(bumblebee),
                                              replace = FALSE)
                              abs_turn = sample(x = abs_turn,
                                                size = length(bumblebee),
                                                replace = FALSE)
                              ## this one was smoothed
                              abs_accel = abs(
                                sapply(X = 1:length(experimental_time),
                                       FUN = MAturnspeed,
                                       dta = predict(
                                         smooth.spline(
                                           x = (1:length(experimental_time))[!is.na(abs_turn)],
                                           y = abs_turn[!is.na(abs_turn)]),
                                         x = 1:length(experimental_time)
                                       )$y,
                                       window = av_window,
                                       hz = sample_rate
                                )
                              )
                            }
    )
  }
  # . Rank by first condition -----------------------------------------------
  if(verbose){message('Calculating median across each phase...','\n')}
  #Add phases based on experimental time
  day_data_table = within(day_data_table,
                          {
                            phase = ceiling(#round up to nearest integer 
                              (experimental_time + .Machine$double.eps) / 
                                med_window
                            )
                          }
  )
  #summarise 
  track_speed = aggregate(formula = forward_speed~track*phase*condition,
                        data = subset(within(day_data_table, rm(experimental_time)), 
                                      subset = flag_exp),
                        FUN = quantile,
                        p = quantls)
  track_rho = aggregate(formula = ma_rho~track*phase*condition,
                        data = subset(within(day_data_table, rm(experimental_time)), 
                                      subset = flag_exp),
                        FUN = quantile,
                        p = quantls)
  track_abs_turn = aggregate(formula = abs_turn~track*phase*condition,
                             data = subset(within(day_data_table, rm(experimental_time)), 
                                           subset = flag_exp),
                             FUN = quantile,
                             p = quantls)
  track_abs_accel = aggregate(formula = abs_accel~track*phase*condition,
                              data = subset(within(day_data_table, rm(experimental_time)), 
                                            subset = flag_exp),
                              FUN = quantile,
                              p = quantls)
  
  # . . Combine summaries ---------------------------------------------------
  day_data_table = data.table::merge.data.table(
    x =data.table::data.table(track_speed),
    y = data.table::merge.data.table(
      x =data.table::data.table(track_abs_turn) ,
      y =data.table::data.table(track_abs_accel) ),
    by = c('track', 'phase', 'condition')
  )
  #give them more convenient names
  day_data_table = within(day_data_table,
                          {
                            speed_median = forward_speed.50.; rm(forward_speed.50.)  
                            # rho_median = ma_rho.50.; rm(ma_rho.50.)  
                            turn_median = abs_turn.50.; rm(abs_turn.50.)  
                            accel_median = abs_accel.50.; rm(abs_accel.50.)  
                          }
  )
  # . Save averaged dataset in master folder --------------------------------
  av_file = file.path(dirname(path_file),
                      paste0(basename(path_file),
                             '_average',
                             '.csv')
  )
  if(verbose)
  {
    message('Saving data summary as:\n', 
            gsub(pattern = '/',
                 x = av_file,
                 replacement = '\n')
    )
    message('May take some time...','\n')
  }
  if(speedup_data.table)
  {
    data.table::fwrite(x = day_data_table,
                       file = av_file,
                       row.names = FALSE
    )
  }else
  {
    write.table(x = day_data_table,
                file = av_file,
                sep = ',',
                row.names = FALSE
    )
  }
  if(show_csv){shell.exec.OS(av_file)}
  if(show_plot)
  {
    FT_plot_average(path_file = av_file,
                    show_plot = show_plot)
  }
  return(av_file)
}

FT_plot_average = function(path_file = FT_select_file('_average.csv'),
                           experiment_length = 2, #minutes
                           condition1_length = 1, #minutes
                           av_window = 5.0,#number of seconds to smooth over for averaging
                           med_window = 10.0,#number of seconds to smooth over for averaging
                           point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
                           save_type = "png",# "pdf",# 
                           plette = 'Plasma',#'YlGnBu'#  for options see http://colorspace.r-forge.r-project.org/articles/approximations.html
                           crossval = FALSE, # TRUE, data were randomised to check the analysis
                           sys_win = Sys.info()[['sysname']] == 'Windows',
                           speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
                           verbose = TRUE, #Tell the user what is going on
                           show_plot = TRUE #Tell the user what is going on
)
{
  path_folder1 = dirname(path_file)
  # Read in data ------------------------------------------------------------
  # . Read in files ------------------------------------------------------------
  if(verbose){message('\n','Reading in "', basename(path_file), '"\nplease be patient...')}
  tryCatch(#Perform no further analysis if the file doesn't load
    {#try to load
      day_data_table = 
        if(speedup_data.table)
        {
          data.table::fread(
            file = path_file, 
            sep = ',',
            header = TRUE
          )#,#read from user-selected file
        }else
        {
          read.table(
            file = path_file, 
            sep = ',',
            header = TRUE
          )#,#read from user-selected file
        }
    },
    error = function(e)
    {#if it doesn't load
      stop(
        paste0('"',
               basename(path_file), 
               '" could not be loaded!\n',
               e)
      )
    }
  )
  if(verbose){message('"',basename(path_file), '" loaded successfully')}
  

  # Derive variables --------------------------------------------------------
  cnds = with(day_data_table, unique(condition))
  #somehow the plotting is faster than the checking, slow down here
  if(!(save_type %in% 'pdf'))
  {
    plot_files = paste0(path_file,
                        '_average',
                        ifelse(test = crossval, 
                               yes = '-CROSSVAL',
                               no = ''),
                         '-',cnds,
                        '.', 
                        save_type)
    plot_exists = file.exists(plot_files)
    names(plot_exists) = plot_files
  }
  if(any(plot_exists))
  {
    message(sum(plot_exists), ' average plot(s) already exist(s) in this folder.',
            '\nPlease provide new name labels.')
    for(pf in plot_files[plot_exists])
    {
    nnm = readline(prompt = paste0(basename(pf),
                                   '\tNew plot label: ')
    )
    
    plot_files[which(plot_files %in% pf)] = 
                                            paste0(
                                                  sub(x = pf,
                                                      pattern = paste0('.',
                                                                       save_type,
                                                                       '$'),
                                                      replacement = ''
                                                      ),
                                                  '-',
                                                  ifelse(test = nchar(nnm),
                                                         yes = nnm,
                                                         no = ''),
                                                   '.',
                                                  save_type)
    }
    
  }
  
  # Plot Summary ------------------------------------------------------------
  # . Set up plot area ------------------------------------------------------
  save_base =  paste0('_average',
                      ifelse(test = crossval, 
                             yes = '-CROSSVAL',
                             no = ''),
                      ifelse(test = save_type %in% 'pdf',
                             yes = '',
                             no = paste0('-',
                                     with(day_data_table, unique(condition))[1])
                             ),
                      '.', 
                      save_type)
  if(save_type %in% 'pdf')
  {
    plot_file = file.path(dirname(path_file), 
                          paste0(basename(dirname(path_file)),
                                 save_base)
    )
    if(file.exists(plot_file))
    {
      message('A plot called "', basename(plot_file), '" already exists in this folder.')
      nnm = readline(prompt = 'New plot name: '
      )
      
      plot_file = file.path(dirname(path_file),
                            paste0(ifelse(test = nchar(nnm),
                                          yes = nnm,
                                          no = basename(dirname(path_file))),
                                   save_base)
      )
    }
  }
  
  switch(EXPR = save_type,
         pdf = 
           pdf(file = plot_file,
               paper = 'a4',
               height = 10,
               bg = 'white',
               onefile = TRUE,
               useDingbats = F
           ),
         png = png(file = plot_files[1],
                   res = 150,
                   width = 210*10,
                   height = 297*10,
                   bg = 'white'
         ),
         jpeg(file = paste0(plot_files[1],'.jpeg'),
              quality = 100,
              width = 210*10,
              height = 297*10,
              bg = 'white'
         )
  )
  
  switch(EXPR = save_type,
         png = par(mfrow = c(3,1),
                   mar = c(3,5,0,0),
                   oma = c(1.5,0,1.5,0),
                   cex = 1.5
         ),
         par(mfrow = c(3,1),
             mar = c(3,5,0,0),
             oma = c(1.5,0,1.5,0))
  )
  
  # . Plot summarys ---------------------------------------------------------
  # if(!(save_type %in% 'pdf'))
  # {png_files = list()}
  # . . Loop through experiments --------------------------------------------
  for(cnd in with(day_data_table, unique(condition)))
  {  
    cnd_i = which( with(day_data_table, unique(condition)) %in% cnd)
    # . . . Set up plot area ------------------------------------------------
    if( #except for the 1st condition, a new plot needs to be opened
      cnd_i>1 
      ) # open a new plot
       {
    plot_file = plot_files[cnd_i]
    switch(EXPR = save_type,
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
    switch(EXPR = save_type,
           png = par(mfrow = c(3,1),
                     mar = c(3,5,0,0),
                     oma = c(1.5,0,1.5,0),
                     cex = 1.5
           ),
           par(mfrow = c(3,1),
               mar = c(3,5,0,0),
               oma = c(1.5,0,1.5,0))
    )
    }
    
    #Plot each condition separately 
    if(dim(subset(day_data_table,
                  condition %in% cnd))[1])
    {
      # . . Turning speed -------------------------------------------------------
      with(subset(day_data_table,
                  condition %in% cnd),
           {
             boxplot(formula = turn_median ~ phase,
                     xlim = range(phase, na.rm = T),
                     ylim = c(0,360)/8,
                     xlab = 'phase end time (s)',
                     ylab = paste0('absolute median turning speed (°/s: ',av_window,'s)'),
                     axes = F
             )
             axis(side = 1,
                  at = unique(phase),
                  labels = round( (1:length(unique(phase)))*
                                    med_window
                  )#/60
             )
             axis(side = 2,
                  at = 15*(0:(360/15))
             )
             
             abline(v = 0.5+seq(from = 0,
                                to = experiment_length,
                                by = condition1_length)*60/med_window,
                    col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
                    lwd = 2
             )
             abline(h = c(0),
                    col = 'black',
                    lwd = 0.25
             )
           }
      )
      legend(x = 'topleft',
             legend = cnd,
             cex = 2)
      
      # . . Acceleration --------------------------------------------------------
      with(subset(day_data_table,
                  condition %in% cnd),
           {
             boxplot(formula = accel_median ~ phase,
                     xlim = range(phase, na.rm = T),
                     ylim = (c(0,360))/24,
                     xlab = 'phase end time (s)',
                     ylab = paste0('absolute median acceleration (°/s^2 : ',av_window,'s)'),
                     axes = F
             )
             axis(side = 1,
                  at = unique(phase),
                  labels = round( (1:length(unique(phase)))*
                                    med_window
                  )#/60
             )
             axis(side = 2,
                  at = 5*(0:(360/5))
             )
             
             abline(v = 0.5+seq(from = 0,
                                to = experiment_length,
                                by = condition1_length)*60/med_window,
                    col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
                    lwd = 2
             )
             abline(h = c(0),
                    col = 'black',
                    lwd = 0.25
             )
           }
      )
      # 
      # # . . Mean vector length --------------------------------------------------
      # with(subset(day_data_table,
      #             condition %in% cnd),
      #      {
      #        boxplot(formula = rho_median ~ phase,
      #                xlim = range(phase, na.rm = T),
      #                ylim = c(0,1),
      #                xlab = 'phase end time (s)',
      #                ylab = paste0('mean vector length (',av_window,'s)'),
      #                axes = F
      #        )
      #        axis(side = 1,
      #             at = unique(phase),
      #             labels = round( (1:length(unique(phase)))*
      #                               med_window
      #             )#/60
      #        )
      #        axis(side = 2,
      #             at = (0:5)/5
      #        )
      #        abline(v = 0.5+seq(from = 0,
      #                           to = experiment_length,
      #                           by = condition1_length)*60/med_window,
      #               col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
      #               lwd = 2
      #        )
      #        abline(h = c(0,1),
      #               col = 'black',
      #               lwd = 0.25
      #        )
      #      }
      # )
      # mtext(text = c(paste('Medians across', med_window, 's phases from each track'),
      #                'phase end time (min)'),
      #       side = c(3,1),
      #       outer = T)
      # . . Forward speed ----------------------------------------------------
      speedlim = 20
      with(subset(day_data_table,
                  condition %in% cnd),
           {
             boxplot(formula = speed_median ~ phase,
                     xlim = range(phase, na.rm = T),
                     ylim = c(0,10)*speedlim,
                     xlab = 'phase end time (s)',
                     ylab = paste0('Forward speed (','mm/s',': by frame)'),
                     axes = F
             )
             axis(side = 1,
                  at = unique(phase),
                  labels = round( (1:length(unique(phase)))*
                                    med_window
                  )#/60
             )
             axis(side = 2,
                  at = 10*(0:20)
             )
             abline(v = 0.5+seq(from = 0,
                                to = experiment_length,
                                by = condition1_length)*60/med_window,
                    col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
                    lwd = 2
             )
             abline(h = c(0,1,2)*speedlim,
                    col = 'black',
                    lwd = 0.25
             )
           }
      )
      mtext(text = c(paste('Medians across', med_window, 's phases from each track'),
                     'phase end time (min)'),
            side = c(3,1),
            outer = T)
    }else
    {
      plot(x = NULL,
           xlim = c(0,1),
           ylim = c(0,1),
           xlab = '',
           ylab = '',
           axes = F
      )
      legend(x = 'center',
             legend = paste0(cnd,'\n','(no data)'),
             cex = 2)
    }
    
    # . . Open next page ------------------------------------------------------
    if(
      !(save_type %in% 'pdf') &
      (cnd_i + 1) < length(cnds)
    )
    {
      # png_files[cnd] = plot_file
      dev.off()
    }
  }#loop through conditions
  # . Save plot -------------------------------------------------------------
  tryCatch(
            expr = dev.off(),
            error = function(e)
            {'TODO: Fix dev.off() error'}
          )
  if(show_plot)
  {
    if(save_type %in% 'pdf')
    {
      shell.exec.OS(plot_file)
    }else
    {
      for(fl in plot_files )
      {
        shell.exec.OS(fl)
      }
    }
  }
  return(
    if(save_type %in% 'pdf'){plot_file}else
    {
      plot_files
    }
  )
}

# Maintenance -------------------------------------------------------------

#Delete pre-existing processed files in the data folder
FT_delete_processed = function(path_folder = FT_select_folder())
{
  #Find processed files compressed to '.gz'
  pre_gzs = list.files(path = path_folder,
                       pattern = '.gz$',#ends in GZ
                       recursive = TRUE)
  #Find figures saved to '.png'
  pre_pngs = list.files(path = path_folder,
                        pattern = '.png$',#ends in PNG
                        recursive = TRUE)
  #Find figures saved to '.pdf'
  pre_pdfs = list.files(path = path_folder,
                        pattern = '.pdf$',#ends in PDF
                        recursive = TRUE)
  delete_list = file.path(path_folder,
                          c(pre_gzs,
                            pre_pngs,
                            pre_pdfs)
  )
  dlt = lapply(X = delete_list,
               FUN = file.remove # delete each file
  )
  names(dlt) = basename(delete_list)
  message('DELETED:\n',paste0(names(dlt[unlist(dlt)]),'\n') )#TRUE indicates these files were deleted
}

FT_troubleshoot_function = function(fn, ...)
{ #turn formal arguments into an expression
  #in the function body
  body(fn) = quote(expr = lapply(formals(), 
                                 FUN = function(x)
                                   {
                                   try(eval(x), TRUE)
                                   }
                                 )
                  )
  ex = fn(...) #execute the function and extract the arguments
  nm = names(ex) #find the argument names
  AssignGE = #in a loop, assign to global env with the name
    function(i)
      {
        assign(x = nm[i],
               value = ex[[i]],
               envir = .GlobalEnv,
               pos = -1
        )
        }
  invisible( # loop through without echoing
    {
  lapply(X = 1:length(ex),
         FUN = AssignGE
         )
    }
  )
}
