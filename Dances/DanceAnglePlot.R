#FOR A 'CLEAN' RUN, RESTART Rstudio
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 08 12
#     MODIFIED:	James Foster              DATE: 2021 08 12
#
#  DESCRIPTION: Loads a text file and plots dance angles for each stimulus phase
#               .
#               
#       INPUTS: A ".csv" table with columns for experiment phase ("stimulus") and
#               angle ("angle").
#               User should specify test details (line 60).
#               
#      OUTPUTS: Results table (.csv).
#
#	   CHANGES: - Suppressed package loading messages (upsetting users)
#             - 
#             - 
#
#   REFERENCES: Batschelet E (1981).
#               Graphical presentation, Chap 1.2, p. 4-6
#               Chapter 1: Measures of Location
#               In: Circular Statistics in Biology
#               Academic Press (London)
#
#    EXAMPLES:  Fill out user input (lines 60-56), then press ctrl+shift+s to run
#
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data   +
#- Plot angles    
#- Neat plot 
#- Save results 

# Useful functions --------------------------------------------------------
# . Load package ----------------------------------------------------------
#needs installing before first use (in Rstudio, see automatic message)
suppressMessages(#these are disturbing users unnecessarily
  {
    require(circular)#package for handling cirular data
    require(CircStats)#package for circular hypothesis tests
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
group_factor = "stimulus" #The title of the column; NO SPACES PLEASE
angle_name = "angle" #The title of the column with angles; NO SPACES PLEASE
angle_unit = "degrees" # "degrees" or "radians"
angle_rot = 'clock' # counter' # 'counter' for anticlockwise (imageJ) 'clock' for clockwise
angle_zero = pi/2 # 0 # angle start point: _0_ for right along x axis (imageJ) _pi/2_ for up along y axis (e.g. geographic North)

#Check the operating system and assign a logical flag (T or F)
sys_win <- Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  ltp <- gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp <- Sys.getenv('HOME')#Life was easier on Mac
}

# Simulate data (not used) ------------------------------------------------
# n_phases = 3
# n_angles = 10
# angles_sim = sapply(sort(rep(0:(n_phases - 1)*pi/2, n_angles)),
#                     function(mm)
#                     {suppressWarnings(rvonmises(n = 1,mu = mm, kappa = 3))}
#                     )
# sim = data.frame(stimulus = sort(rep(1:n_phases, n_angles)),
#                  angle = round(angles_sim*180/pi)
#                  )
# write.table(x = sim,
#             file = file.path(ltp,'Documents', "simulated_angles.csv"),
#             sep = csv_sep,
#             row.names = FALSE
#             )

# . Select files ---------------------------------------------------------

# set path to files
if(sys_win){#choose.files is only available on Windows
  message('\n\nPlease select the ".csv" file\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file  <- choose.files(
    default = file.path(ltp,'Documents', "*.csv"),#For some reason this is not possible in the "root" user
    caption = 'Please select the ".csv" file'
  )
}else{
  message('\n\nPlease select the ".csv" file\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file <- file.choose(new=F)
}
#show the user the path they have selected
if(is.null(path_file))
{stop('No file selected.')}else
{print(path_file)}


# Read in file ------------------------------------------------------------
adata = read.table(file = path_file,#read from user-selected file
                   header = T,#read the file header to use for variable names
                   sep = csv_sep#,#values are separated by the user-specified character
                   #other parameters can be added here for troubleshooting
)

View(adata)#show the user the data that was
Cformat <- function(angles)
{
  circular(x = angles,
          units = angle_unit,
          zero = angle_zero,
          rotation = angle_rot,
          )
}

# Basic plot --------------------------------------------------------------
par(mar = c(0,0,0,0))
plot.circular(x = Cformat(adata[,angle_name]),
                # circular(x = adata[,angle_name],
                #            units = angle_unit,
                #            zero = angle_zero,
                #            rotation = angle_rot,
                #            ),
               stack = T,
              sep = 0.1,
              # col = as.numeric(factor(adata$stimulus)),
              pch = 19,
              shrink = sqrt(length(adata[,angle_name]))/4,
              bins = 360/5-1
)

# Plot for each phase -----------------------------------------------------
ustim <- unique(adata$stimulus)
savepath <- paste0(path_file, '-byStimulus.pdf')
pdf(file = savepath,
    paper = 'a4r',
    bg = 'white', 
    useDingbats = FALSE
    )
par(mfrow = c(ceiling(sqrt(length(ustim))), ceiling(sqrt(length(ustim)))),
    mar = c(0,0,0,0)
    )
invisible(
  lapply(ustim,
       function(i)
         {
         with(subset(adata, stimulus %in% i),
              {
          plot.circular(
            x = Cformat( eval(str2lang(angle_name)) ),
          bins = 360/5-1,
          stack = T,
          sep = 0.1,
          # col = as.numeric(factor(adata$stimulus)),
          pch = 19,
          shrink = sqrt(length(adata[,angle_name]))/length(ustim)
          )
          mtext(text = i,
                line = -2
                )
         arrows.circular(x = mean.circular(Cformat( eval(str2lang(angle_name)) )),
                         shrink = rho.circular(Cformat( eval(str2lang(angle_name)) )),
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
