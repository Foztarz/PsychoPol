#FOR A 'CLEAN' RUN, RESTART Rstudio
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 11 16
#     MODIFIED:	James Foster              DATE: 2021 11 16
#
#  DESCRIPTION: Loads ".csv" files saved by "DewesoftOutput.R" and compiles them
#               into a single data frame for plotting and further analysis.
#               
#       INPUTS: A folder containing '.csv' files.
#               User should specify processing details (line 60).
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
#    EXAMPLES:  Fill out user input (lines 60-65), then press ctrl+shift+s to run
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data +
#- Convert to circular  +   
#- Plot +
#- Save plot  +
#- Save data as csv +
#- Test on multiple machines
#- Test on Mac

# Useful functions --------------------------------------------------------
# . Load package ----------------------------------------------------------
#needs installing before first use (in Rstudio, see automatic message)
suppressMessages(#these are disturbing users unnecessarily
  {
    require(circular)#package for handling circular data
    require(CircStats)#package for circular hypothesis tests
    # require(data.table)#package for reorganising large datasets
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
save_type ="png"# "pdf"# 

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
  message('\n\nPlease select the folder containing ".csv" files\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_folder  <- choose.dir(
    default = file.path(ltp,'Documents'),#For some reason this is not possible in the "root" user
    caption = 'Select the folder containing ".csv" files'
  )
}else{
  message('\n\nPlease select any ".csv" file in the correct folder\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file <- file.choose(new=F)
  if(is.null(path_file)){stop('No file selected.')}
  path_folder = dirname(path_file)
}
#show the user the path they have selected
if(is.null(path_folder) | is.na(path_folder))
{stop('No file selected.')}else
{print(path_folder)}

# Find files ---------------------------------------------------------------
names_files = list.files(path =path_folder,
                        pattern = '.csv$'
)
#show the user the path they have selected
if(is.null(names_files))
{stop('No ".csv" files found in', path_folder)}else
{message('Files found:\n',paste0(names_files,'\n'),
         '\n------------------------------------------')}

# Read in files ------------------------------------------------------------
message('Reading in files from', basename(path_folder), '\nplease be patient...')
tryCatch(#Perform no further analysis if the file doesn't load
  {
    adata = lapply(X = file.path(path_folder, names_files),
                   FUN = read.csv,
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
message('All files in "',basename(path_folder), ' loaded successfully')
names(adata) = names_files

View(adata)#show the user the data that 

# . Organise data ---------------------------------------------------------
#check sample rates for each
sample_rates = lapply(X = adata, 
                      FUN = function(x)
                        {with(x, 1/mean(diff(time)))}
                      )#Hz
if(sum(diff(unlist(sample_rates))))
{warning('Sample rates differ between recordings\n',
         'proceed with caution!')}else{
           message('All recordings have a sample rate of ',
                   sample_rates[[1]],
                   ' Hz')
         }
adata_frame = do.call(what = rbind,
                      args = adata)
FlightNamer = function(txt)
  {
  regmatches(m = regexpr(pattern = '^([^.]+)', 
                         text = txt),
             x = txt)
  }
adata_frame$flight = FlightNamer(rownames(adata_frame))
adata_frame = within(adata_frame,
                     {
                     moth = regmatches(m = regexpr(pattern = '^([^-]+)',
                                                    text = flight),
                                        x = flight)
                     trial = regmatches(m = regexpr(pattern = '[^ -][^-]*$',
                                                    text = flight),
                                        x = flight)
                     }
                     )
#  Save data --------------------------------------------------------------
txt_file <- file.path(path_folder,
                      paste0(basename(path_folder),
                             '_proc',
                             '.txt')
)
message('Saving data as:\n', 
        gsub(pattern = '/',
             x = txt_file,
             replacement = '\n')
        )
write.table(x = adata_frame,
          file = txt_file,
          row.names = FALSE
)
