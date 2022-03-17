
# Print LED arena PNGs ----------------------------------------------------

# Useful functions --------------------------------------------------------

#any sequence on a circle symmetric around a middle point
CircSeq = function(from,
                   to,
                   middle,
                   maxim = 360,
                   by = 1)
{
  if(from<middle & from<to)# no special action sequence runs in line
  {
    seq(from = from,
        to = to,
        by = by)
  }else
  { #if the start is the biggest value,
    c( #  run from start to 360
    seq(from = from,
        to = maxim,
        by = by),
    seq(from = by, # then from 360 + step to end
        to = to,
        by = by)
    )
  }
}
#Horizontal indices of a bar, slightly larger than chosen visual angle
HorizBar = function(pos, #position in LEDs
                    width, #with in degrees
                    x_range = 64,#number of x direction pixels
                    deg_px = x_range/360#pixels per degree
)
{
  degrad = pi/180 # ratio of degrees to radians
  pos_rad = pos/deg_px*degrad # bar centre in radians
  rad_edge = c(left = -1,
               right = 1)*0.5* # plus & minus half the bar width
                width*degrad #bar width in radians
  pos_edges = pos_rad+rad_edge
  mod_edges = atan2(y = sin(pos_edges), #atan2 always in (-pi,pi)
                    x = cos(pos_edges)
                    )
  mod_edges[mod_edges <0] = 2*pi + mod_edges[mod_edges <0 ] #keep negative in (0,2*pi)
  bar_edges = deg_px*mod_edges/degrad #convert to pixels
  bar = CircSeq(from = floor(bar_edges['left']), # whole number of pixels
             to = ceiling(bar_edges['right']), # whole number of pixels
             middle = pos,
             maxim = x_range,
             by = 1)
  return(bar)
}

#save an array as a PNG with the same proportions
SavePNG = function(im_array,
                   folder,
                   fname)
{
  impath = file.path(folder,
                     paste0(fname,'.png')
  )
  dm = dim(im_array)
  if(!(dm[3] %in% c(1,3))){stop('image must be greyscale or RGB')}
  png(filename = impath,
      height = dm[1],
      width = dm[2],
      units = 'px',
      res = 72*max(dm),# at least big enough to save each pixel separately
      bg =  'black'
  )
  par(mar = c(0,0,0,0))
  plot(as.raster(x = im_array/
                   ifelse(test = max(im_array)>1,
                          yes = 255,
                          no = 1)
                 ),
       interpolate = FALSE)
  dev.off()
  return(impath)
}

#Select a folder
FT_select_folder_save = function(file_type = ".png",
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
    paste0('Please select a folder to save ',#message to display
           '"',file_type,'"',
           ' files in')
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
shell.exec.OS = function(x){
  # replacement for shell.exec (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
  {return(base::shell.exec(x))}else
  {comm = paste0('open "',x,'"')
  return(system(comm))}
}

# Load packages -----------------------------------------------------------
require(png)#load the PNG package


# Set up arena array ------------------------------------------------------
led_arena = array(data = 255, # white
                  dim = c(16, # elevation
                          64, # azimuth
                          3), # RGB
                  dimnames = list(
                                  elevation = rep(x = NULL,
                                                  times = 16),
                                  azimuth = rep(x = NULL,
                                                times = 64),
                                  rgb = c('red',
                                          'green',
                                          'blue')
                                  )
                  )

# Set up positions -------------------------------------------------------
degled = 64/360 # 64 leds encompass 360 degrees horizontally
anterior =  0.5 #halfway between last and 1st pixel#
#might also be 16.5# (between 1st and 2nd panel)
width1 = 15#deg, target angle (Seelig & Jayaraman, 2015)
width2 = 20#deg, slightly bigger target angle

bar1pos = HorizBar(pos = anterior,
                   width = width1)#degrees

bar2pos = HorizBar(pos = anterior,
                   width = width2)#degrees
# Make different arena PNGs -----------------------------------------------

#  . All red --------------------------------------------------------------
red_arena = led_arena
red_arena[ , , c('green','blue')] = 0 # set green & blue to black
#view
par(mar = c(bottom = 0, # set margins
            left = 0.1,
            top = 0,
            right = 0.1),
    bg = 'gray')#set background
plot(as.raster(red_arena/255))

# . Red with black horizon ------------------------------------------------
redblack_horizon = red_arena
redblack_horizon[-(1:8) , , ] = 0 # set all except top 1:8 to black
plot(as.raster(redblack_horizon/255),
     interpolate = FALSE)


#  . Blue with red horizon ------------------------------------------------
bluered_horizon = led_arena
bluered_horizon[-(1:8) , , c('green','blue')] = 0 # set all except top 1:8 to red (no green or blue)
bluered_horizon[1:8 , , c('red','green')] = 0 # set top 1:8 to blue (no red or green)
plot(as.raster(bluered_horizon/255),
     interpolate = FALSE)


# . Red-black horizon with 11.25deg bar anterior[?] -----------------------
redblack_horizon_bar1 = redblack_horizon
redblack_horizon_bar1[ , bar1pos , ] = 0 # set 1st
plot(as.raster(redblack_horizon_bar1/255),
     interpolate = FALSE)


# . Blue-red horizon with 11.25deg bar anterior[?] -----------------------
bluered_horizon_bar1 = bluered_horizon
bluered_horizon_bar1[ , bar1pos , ] = 0 # set 1st
plot(as.raster(bluered_horizon_bar1/255),
     interpolate = FALSE)

# . Red-black horizon with 22.50deg bar anterior[?] -----------------------
redblack_horizon_bar2 = redblack_horizon
redblack_horizon_bar2[ , bar2pos , ] = 0 # set 1st
plot(as.raster(redblack_horizon_bar2/255),
     interpolate = FALSE)

# . Blue-red horizon with 22.50deg bar at North[?] -----------------------
bluered_horizon_bar2 = bluered_horizon
bluered_horizon_bar2[ , bar2pos , ] = 0 # set 1st
plot(as.raster(bluered_horizon_bar2/255),
     interpolate = FALSE)

im_to_save = list(redblack_horizon = redblack_horizon,
                  bluered_horizon = bluered_horizon,
                  redblack_horizon_bar1 = redblack_horizon_bar1,
                  bluered_horizon_bar1 = bluered_horizon_bar1,
                  redblack_horizon_bar2 = redblack_horizon_bar2,
                  bluered_horizon_bar2 = bluered_horizon_bar2
                  )

# Save the images ---------------------------------------------------------
fld = FT_select_folder_save()

im_paths =  list()
  for(im in 1:length(im_to_save))
{
 im_paths[[im]] = 
   SavePNG(im_array = im_to_save[[im]],
          folder = fld,
          fname = names(im_to_save)[im]
          )
}

invisible({lapply(X = im_paths, FUN = shell.exec.OS)})