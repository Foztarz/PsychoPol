#FOR A 'CLEAN' RUN, RESTART Rstudio
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 08 12
#     MODIFIED:	James Foster              DATE: 2022 02 17
#
#  DESCRIPTION: Loads a text file and plots dance angles for each stimulus phase
#               .
#               
#       INPUTS: A ".csv" table with columns for experiment phase ("stimulus") and
#               angle ("angle").
#               User should specify test details (line 80).
#               
#      OUTPUTS: Results table (.csv).
#
#	   CHANGES: - Suppressed package loading messages (upsetting users)
#             - Use aggregate to sort and plot
#             - stimulus orientation label: "orientation" -> "stim_ori"
#             - correct for tilt and rotation
#
#   REFERENCES: Batschelet E (1981).
#               Graphical presentation, Chap 1.2, p. 4-6
#               Chapter 1: Measures of Location
#               In: Circular Statistics in Biology
#               Academic Press (London)
#
#    EXAMPLES:  Fill out user input (lines 80-87), then press ctrl+shift+s to run
#
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data   +
#- Plot angles    +
#- Subset by bee & day  +  
#- Neat plot  +
#- Save results + 
#- Reorganise functions +
#- Perspective correction +
#- Include dates in organisation
#- Separate weird dances
#- Bimodal mean vector 

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
    require(CircStats)#package for circular hypothesis tests
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
               subset = !(is.na(angle)) # angle is an empty number, i.e. no data
)

View(adata)#show the user the data that was

# Basic plot --------------------------------------------------------------
shrink_val = sqrt(dim(adata)[1])/4
par(mar = c(0,0,0,0))
with(adata, 
  plot.circular(x = Cformat(angle),
                 stack = T,
                sep = 0.1,
                col = point_col,
                pch = 19,
                shrink = shrink_val,
                bins = 360/5-1
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
                PhiTheta_AlphaDelta(phi = rad(raw_angle),
                              theta = rad(ground_truth)
                              )
                )
#perform correction
adata = within(adata,
               {
               raw_angle = angle
               angle = deg(
                         Theta(phi = rad(raw_angle),
                               alpha = tilt_rot['alpha']) + tilt_rot['delta']
                           )
               }
)

ddata = within(ddata,
               {
               angle = deg(
                         Theta(phi = rad(raw_angle),
                               alpha = tilt_rot['alpha']) + tilt_rot['delta']
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
                   bins = 360/5-1
     )
)
with(ddata,
     {
     points(x = shrink_val*sin(rad(raw_angle)),
            y = shrink_val*cos(rad(raw_angle)),
            col = 2,
            pch = paste(1:length(raw_angle))
     )
     points(x = shrink_val*sin(rad(angle)),
            y = shrink_val*cos(rad(angle)),
            col = 3,
            pch = paste(1:length(angle))
     )
     }
)

# Plot for each phase -----------------------------------------------------

# . Set up plot file ------------------------------------------------------
#file to save to in the same location as the original
savepath = paste0(path_file, '-byBDSOD-corrected.pdf')
#open the file
pdf(file = savepath,
    paper = 'a4r',
    bg = 'white', 
    useDingbats = FALSE
    )

# . Set up plot parameters ------------------------------------------------
#make a set of angles for each combinatino of stimulus, orientation, dance and bee
df_lst = aggregate(formula = angle~stim_ori*stimulus*dance*bee*date,
          data = adata, 
          FUN = list
          )
dim(df_lst)
nms = names(df_lst)
ucond = dim(df_lst)[1]#prod(lul)
sq_cond = ceiling(sqrt(ucond))
shrk = 1+sqrt(length(adata[,angle_name]))/ucond
par(mfrow = c(sq_cond, sq_cond),
    mar = c(0,0,0,0)
    )
invisible(
  apply(X = df_lst,
        MARGIN = 1,
       FUN = function(dat)
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
            mtext(text = paste0('bee ',
                               bee,
                               ', dance ',
                               dance, 
                               ', ',
                               stimulus,
                               ', ',
                               stim_ori,
                               '°'),
                  line = -1.5,
                  cex = 3/sq_cond
                  )
           arrows.circular(x = mean.circular(Cformat( angle )),
                           shrink = rho.circular(Cformat( angle )),
                           col = 2,
                           length = 0.05
                           )
              }
         )
       }
  )
)
#save plot
dev.off()
#show plot
shell.exec.OS(savepath)
