#FOR A 'CLEAN' RUN, RESTART Rstudio (ctrl+shift+F10)
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 01 12
#     MODIFIED:	James Foster              DATE: 2022 01 12
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
ball_radius = 20#mm
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
require(plot3D)#in case anything needs to be plotted in 3D
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
                    'timestamp', #22
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
# 22      timestamp                       Either position in video file (ms) or frame
#                                         capture time (ms since epoch).
# 23      sequence counter                Position in current frame sequence. Usually
#                                         corresponds directly to frame counter, but
#                                         can reset to 1 if tracking is reset.
# 24      delta timestamp                 Time (ms) since last frame.
# 25      alt. timestamp                  Frame capture time (ms since midnight).



# . Inspect ---------------------------------------------------------------
summary(adata)
# View(adata)#show the user the data that was read in

with(adata,
     {
       scatter3D(x = x_ball,
            y = y_ball,
            z = z_ball,
            colvar = frame_counter,
            type = 'l'
            )
     }
)
with(adata,
     {
       plot(x = x_int,
            y = y_int,
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis'),
            type = 'o',
            pch = 19
            )
     }
)
with(adata,
     {
       plot(x = frame_counter,
            y = heading_integrated,
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis'),
            type = 'p',
            pch = 19,
            axes = F
            )
       axis(1); axis(side = 2, at = 0:4 * pi/2, labels = 0:4*90 )
     }
)

# Conversions -------------------------------------------------------------
fps = mean(diff(adata$timestamp))
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
               }
               )
adata$angle_speed = c(0,diff(deg(adata$heading_instantaneous)))*fps #deg/s

# Plot summary ------------------------------------------------------------

par(mfrow = c(3, 1))
with(adata,
     {
       plot(x = x_pos,
            y = y_pos,
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis'),
            type = 'o',
            pch = 19
       )
       plot(x = timestamp,
            y = angle,
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis'),
            type = 'p',
            pch = 19
       )
       plot(x = timestamp,
            y = angle_speed,
            col = hcl.colors(n = length(frame_counter),
                             palette = 'viridis'),
            type = 'o',
            pch = 19
       )
     }
)
