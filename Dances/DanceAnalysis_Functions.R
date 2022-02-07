# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 02 07
#     MODIFIED:	James Foster              DATE: 2022 02 07
#
#  DESCRIPTION: A set of functions for analysing honeybee waggle dances.
#               
#               
#       INPUTS: 
#               
#      OUTPUTS: 
#
#	   CHANGES: - 
#             - 
#             - 
#
#   REFERENCES: Batschelet E (1981).
#               Graphical presentation, Chap 1.2, p. 4-6
#               Chapter 1: Measures of Location
#               In: Circular Statistics in Biology
#               Academic Press (London)
#
#    EXAMPLES:  Source in another script or press ctrl+shift+s to add to workspace
#
# 
#TODO   ---------------------------------------------
#TODO   
#- Bimodal mean vector 

# General use -------------------------------------------------------------

# . Select files ---------------------------------------------------------
DA_select_file = function(file_type = ".csv",
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


DA_select_folder = function(file_type = ".csv.gz",
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


#Open file with default program on any OS
# https://stackoverflow.com/a/35044209/3745353
shell.exec.OS  <- function(x){
  # replacement for shell.exec (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
  {return(base::shell.exec(x))}else
  {comm <- paste0('open "',x,'"')
  return(system(comm))}
}

# Camera correction -------------------------------------------------------

#convert from angle to trapezoid-distorted angle
Phi = function(theta,#original angle, radians
               alpha)#angle between trapezoid and vertical, radians
{
  return(
    atan2(y = sin(theta)*(1-cos(alpha)*cos(theta)),
          x = cos(theta)*sin(alpha) )
  )
}
#estimate inverse of trapezoid distortion to get original angles
Theta = function(phi,#distorted angle, radians
                 alpha)#angle between trapezoid and vertical, radians
{
  aa = seq(from = -pi,
           to = pi,
           length.out = 1e4)
  ss = smooth.spline(x = Phi(theta = aa, alpha = alpha),
                     y = aa)
  return(predict(ss, x = phi)$y)
}

# Circular data -----------------------------------------------------------


#Convert any angle in degrees to (-180,180)
Mod360.180 = function(x)
{
  deg(
    atan2(y = sin(rad(x)),
          x = cos(rad(x))
    )
  )
}

Cformat = function(angles,
                    angle_unit = "degrees", # "degrees" or "radians"
                    angle_rot = 'clock', # counter' # 'counter' for anticlockwise (imageJ) 'clock' for clockwise
                    angle_zero = pi/2 # 0 # angle start point: _0_ for right along x axis (imageJ) _pi/2_ for up along y axis (e.g. geographic North)
                    )
{
  circular(x = angles,
           units = angle_unit,
           rotation = angle_rot,
           zero = angle_zero
          )
}

