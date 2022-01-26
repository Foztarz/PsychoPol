#FOR A 'CLEAN' RUN, RESTART Rstudio (ctrl+shift+F10)
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 01 12
#     MODIFIED:	James Foster              DATE: 2022 01 25
#
#  DESCRIPTION: Loads all ".dat" files saved by fictrac and organises data into
#               speed and angle. These are saved as compressed ".csv" files and 
#               which are then combined into a single ".txt" file.
#               
#       INPUTS: A ".dat" csv file with 23 columns specified in : 
#               https://github.com/rjdmoore/fictrac/blob/master/doc/data_header.txt
#               User should specify analysis details (line 40).
#               
#      OUTPUTS: Results table (.csv) saved in the same place as the data.
#
#	   CHANGES: - Load FictracDat_Functions.R
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
#- Read in ".dat" files
#- Read in ".txt" files
#- Combine all files





# Input Variables ----------------------------------------------------------
#  .  User input -----------------------------------------------------------
ball_radius = 25#mm
av_window = 5.0#number of seconds to smooth over for averaging
csv_sep_load = ','#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
use_par = TRUE # whether or not to use parallel processing

# . Load packages ----------------------------------------------------------
# require(plot3D)#in case anything needs to be plotted in 3D
require(circular)#for handing angles
require(parallel)#for parallel processing
require(data.table)#for fast data handling


# . Get functions ---------------------------------------------------------
fun_file = "FictracDat_Functions.R"
fun_path = tryCatch(expr = 
                      {file.path(dirname(sys.frame(1)$ofile), fun_file)},
                    error = function(e){fun_file}
)
if(!file.exists(fun_path))
{
  msg = paste0('Please select "',fun_file,'"')
  # ask user to find data
  if( Sys.info()[['sysname']] == 'Windows' ){#choose.files is only available on Windows
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    fun_path  <- choose.files(
      default = file.path(gsub('\\\\', '/', Sys.getenv('USERPROFILE')),#user
                          'Documents'),#For some reason this is not possible in the "root" user
      caption = msg
    )
  }else{
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    fun_path <- file.choose(new=F)
  }
}
#read in relevant functions
source(file = fun_path, 
       encoding = 'UTF-8')


# Find the files ----------------------------------------------------------
path_folder = FT_select_folder(file_type = 'Fictrac experiment')
path_files = list.files(path = path_folder,
                        pattern = '.dat$',
                        recursive = T
)
path_conditions = unique(dirname(path_files))
path_animals = unique(dirname(path_conditions))
path_days = unique(dirname(path_animals))

# Set up parallel processing ----------------------------------------------
clt = makeCluster(parallel::detectCores() - 1,type="SOCK")

# Process ".dat" files ----------------------------------------------------
message('Files found:\n', paste0(path_files,'\n'), '\nProcessing files.',
        '\n------------------------------------------')
invisible(
  {
  path_input = file.path(
                          path_folder,
                          path_files)
  path_csv = lapply(
                    X = path_input,
                    FUN = FT_read_write,
                    ball_radius = ball_radius,#mm
                    av_window = av_window,#number of seconds to smooth over for averaging
                    csv_sep_load = csv_sep_load,#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
                    speedup_parallel = use_par, #Use the parallel package to speed up calculations
                    compress_csv = TRUE, #Compress to ".gz" to save space?
                    verbose = FALSE, #tell the user what's going on?
                    clust = clt
                    )
  names(path_csv) = path_input
  }
  )
if(all(!is.na(path_csv)))
{
  message('\n','Files processed','\n')
}else
{
  warning(path_input[is.na(path_csv)], '\nnot processed!')
}

# Combine within conditions -----------------------------------------------
message('Conditions found:\n', paste0(path_conditions,'\n'), '\nCombining files within each.',
        '\n------------------------------------------')
invisible(
  {
    condition_path = file.path(
      path_folder,
      path_conditions)
    path_txt_conditions = lapply(
      X = condition_path,
      FUN = FT_combine_folder,
      file_type = '_proc.csv.gz',
      compress_txt = TRUE, #Compress to ".gz" to save space?
      verbose = FALSE #Tell the user what is going on?
    )
    names(path_txt_conditions) = condition_path
  }
)
if(all(!is.na(path_txt_conditions)))
{
  message('\n','Conditions combined','\n')
}else
{
  warning(condition_path[is.na(path_txt_conditions)], '\nnot included!')
}


# Combine within animals --------------------------------------------------
message('Animals found:\n', paste0(path_animals,'\n'), '\nCombining files within each.',
        '\n------------------------------------------')
invisible(
  {
    animal_path = file.path(
                            path_folder,
                            path_animals)
    path_txt_animals = lapply(
      X = animal_path,
      FUN = FT_combine_folder,
      file_type = '_proc.txt.gz',
      compress_txt = TRUE, #Compress to ".gz" to save space?
      verbose = FALSE #Tell the user what is going on?
    )
    names(path_txt_animals) = animal_path
  }
)
if(all(!is.na(path_txt_animals)))
{
  message('\n','Animals combined','\n')
}else
{
  warning(animal_path[is.na(path_txt_animals)], '\nnot included!')
}



# Combine within days --------------------------------------------------
message('Days found:\n', paste0(path_days,'\n'), '\nCombining files  within each.',
        '\n------------------------------------------')
invisible(
  {
    day_path = file.path(
      path_folder,
      path_days)
    path_txt_days = lapply(
      X = day_path,
      FUN = FT_combine_folder,
      file_type = '_proc.txt.gz',
      compress_txt = TRUE, #Compress to ".gz" to save space?
      verbose = FALSE #Tell the user what is going on?
    )
    names(path_txt_days) = day_path
  }
)
if(all(!is.na(path_txt_days)))
{
  message('\n','days combined','\n')
}else
{
  warning(day_path[is.na(path_txt_days)], '\nnot included!')
}
# Combine across days --------------------------------------------------
message('Combining files across days.',
        '\n------------------------------------------')
invisible(
  {
    path_txt_all = lapply(
      X = path_folder,
      FUN = FT_combine_folder,
      file_type = '_proc.txt.gz',
      compress_txt = TRUE, #Compress to ".gz" to save space?
      verbose = FALSE #Tell the user what is going on?
    )
  }
)
if(all(!is.na(path_txt_all)))
{
  message('\n','All combined in','\n', path_txt_all)
}else
{
  stop('Failed to combine all data in:\n', path_folder)
}
