#FOR A 'CLEAN' RUN, RESTART Rstudio
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 11 18
#     MODIFIED:	James Foster              DATE: 2021 11 19
#
#  DESCRIPTION: Loads ".csv" files saved by "DewesoftOutput.R" and compiles them
#               into a single data frame for each day. Each of these data frames
#               is then combined into a single dataset for the whole experiment.
#               
#       INPUTS: A ".mat" table with a column of time stamps and column of angles ("angle").
#               User should specify processing details (line 60).
#               
#      OUTPUTS: Plot (.pdf or .png). Data table (.csv).
#
#	   CHANGES: - Plot acceleration
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
#- Save data as txt + 
#- Looping  +
#- Combine all to csv +
#- Legend for plot  
#- Angular acceleration  
#- Test on multiple machines
#- Test on Mac
#- Comment

# Useful functions --------------------------------------------------------
# . Load package ----------------------------------------------------------
#needs installing before first use (in Rstudio, see automatic message)
suppressMessages(#these are disturbing users unnecessarily
  {
    require(circular)#package for handling circular data
    require(CircStats)#package for circular hypothesis tests
    require(data.table)#package for reorganising large datasets
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
av_window = 10#number of seconds to smooth over for averaging
experiment_length = 10#8 #minutes
condition1_length = 2 #minutes
point_col = "darkblue" # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
save_type ="png"# "pdf"# 
quantls = c(0.025, 0.25, 0.5, 0.75, 0.975) # quantiles to use when summarising

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
  message('\n\nPlease select the master folder containing folders with ".csv" files\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_folder1  <- choose.dir(
    default = file.path(ltp,'Documents'),#For some reason this is not possible in the "root" user
    caption = 'Please select the master folder'
  )
}else{
  message('\n\nPlease select any ".csv" file in the correct folder\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file <- file.choose(new=F)
  if(is.null(path_file)){stop('No file selected.')}
  path_folder1 = dirname(dirname(path_file))
}
#show the user the path they have selected
if(is.null(path_folder1) | is.na(path_folder1))
{stop('No file selected.')}else
{print(path_folder1)}

# Find files ---------------------------------------------------------------
path_folders = list.dirs(path = path_folder1,
                                   recursive = FALSE)
names_all_files = lapply(X = path_folders,
                     FUN = list.files,
                     pattern = '_proc.csv$'
                    )

# Open the loop -----------------------------------------------------------
#show the user the path they have selected
if(is.null(names_all_files))
{stop('No ".csv" files found in', path_folder1)}else
{
  message('Files found:\n',
         paste0(basename(path_folders),
                '\n',
                names_all_files,
                '\n\n'),
         'Starting loop.',
          '\n------------------------------------------')
  }

folder_indices = which(as.logical(
                                  sapply(X = names_all_files, FUN = length)
                                  ))
for(path_folder in path_folders[folder_indices])
{
# Read in files ------------------------------------------------------------
message('\n','Reading in files from "', basename(path_folder), '"\nplease be patient...')
tryCatch(#Perform no further analysis if the file doesn't load
  {
    names_files = list.files(path_folder,
                             pattern = '_proc.csv$')
    adata = lapply(X = file.path(path_folder, 
                                 names_files
                                ),
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
message('All files in "',basename(path_folder), '" loaded successfully')
names(adata) = names_files


# . Organise data ---------------------------------------------------------
#check sample rates for each
sample_rates = lapply(X = adata, 
                      FUN = function(x)
                      {with(x, 1/mean(diff(time)))}
)#Hz
if(sum(diff(unlist(sample_rates))))
  {warning('In "',
           basename(path_folder),
           '" sample rates differ between recordings\n',
           'proceed with caution!')}else{
             message('In "',
                     basename(path_folder),
                     '" all recordings have a sample rate of ',
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
message('\n', 'Saving combined data as:\n', 
        gsub(pattern = '/',
             x = txt_file,
             replacement = '\n')
)
write.table(x = adata_frame,
            file = txt_file,
            row.names = FALSE
)

# Close the loop ----------------------------------------------------------
}

# Load and combine --------------------------------------------------------
day_files = lapply(X = path_folders,
                   FUN = list.files,
                   pattern = '_proc.txt$'
                  )
day_file_indices = which(as.logical(sapply(day_files, length)))
day_files_paths = file.path(path_folders[day_file_indices], 
                            day_files[day_file_indices])

day_data = lapply(day_files_paths,
                  FUN = read.table,
                  header = TRUE
                  )

# . Combine datasets across dates -----------------------------------------
names(day_data) = basename(path_folders[day_file_indices])
day_data_frame = do.call(what = rbind,
                         args = day_data)
day_data_frame$date = regmatches(m = regexpr(pattern = '^([^.]+)',
                                             text = rownames(day_data_frame) ),
                                 x = rownames(day_data_frame) )

# . Extract features ------------------------------------------------------
ExperimentFull = function(experiment_times,#time since first trigger, seconds
                          exper_length)#length of experiment, minutes
                  {max(experiment_times/60, na.rm = TRUE) > exper_length}
full_length_experiments = aggregate(formula = experimental_time ~ date*moth*trial,
                                    data = day_data_frame,
                                    FUN = ExperimentFull,
                                    exper_length = experiment_length)
full_length_conditions = aggregate(formula = experimental_time ~ date*moth*trial,
                                    data = day_data_frame,
                                    FUN = ExperimentFull,
                                    exper_length = condition1_length)
full_length_experiments = within(full_length_experiments,
                                 {flag_exp = experimental_time; rm(experimental_time)}
                                )
full_length_conditions = within(full_length_conditions,
                                 {flag_cnd = experimental_time; rm(experimental_time)}
                                )
full_flags = merge(x = full_length_experiments,
                   y = full_length_conditions)
#merge data by shared variable names
#can also do this with merge.data.frame, but data.table method is much faster
day_data_table = data.table::merge.data.table(
                    x = day_data_frame, 
                    y = full_flags)
rm(day_data_frame)#one copy in memory is enough
# View(day_data_table)
day_data_table = within(day_data_table,
                        {
                        abs_turn = abs(ma_turn)  
                        abs_accel = abs(ma_accel)  
                        }
                        )

# . Save combined dataset in master folder --------------------------------
comb_file <- file.path(path_folder1,
                      paste0(basename(path_folder1),
                             '_all',
                             '.csv')
)
message('Saving data as:\n', 
        gsub(pattern = '/',
             x = comb_file,
             replacement = '\n')
)
message('May take some time...','\n')
write.csv(x = day_data_table,
            file = comb_file,
            row.names = FALSE
)



# . Summarise data --------------------------------------------------------
message('Summarising data from full experiments...','\n')
#summarise by in full experiments
time_rho = aggregate(formula = ma_rho~experimental_time,
                    data = subset(day_data_table, flag_exp),
                    FUN = quantile,
                    p = quantls)
time_turn = aggregate(formula = ma_turn~experimental_time,
                    data = subset(day_data_table, flag_exp),
                    FUN = quantile,
                    p = quantls)
time_abs_turn = aggregate(formula = abs_turn~experimental_time,
                    data = subset(day_data_table, flag_exp),
                    FUN = quantile,
                    p = quantls)
time_abs_accel = aggregate(formula = abs_accel~experimental_time,
                    data = subset(day_data_table, flag_exp),
                    FUN = quantile,
                    p = quantls)
time_angle = aggregate(formula = angle ~ experimental_time,
                    data = subset(day_data_table, flag_exp),
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
time_data_table = data.table::merge.data.table(x = 
                                                 data.table::merge.data.table(
                                                     x = data.table(time_rho),
                                                    y = data.table::merge.data.table(
                                                        x = data.table(time_abs_turn) ,
                                                        y = data.table(time_abs_accel) ),
                                                    by = 'experimental_time'
                                                   ),
                                               y = data.table(time_angle)
                                              )
# . Save combined dataset in master folder --------------------------------
summ_file <- file.path(path_folder1,
                       paste0(basename(path_folder1),
                              '_summary',
                              '.csv')
)
message('Saving data summary as:\n', 
        gsub(pattern = '/',
             x = summ_file,
             replacement = '\n')
)
message('May take some time...','\n')
write.csv(x = time_data_table,
          file = summ_file,
          row.names = FALSE
)


# Plot Summary ------------------------------------------------------------
# . Set up plot area ------------------------------------------------------
plot_file <- file.path(path_folder1, paste0(basename(path_folder1),'_summ','.', save_type))
if(file.exists(plot_file))
{
  message('A plot called "', basename(plot_file), '" already exists in this folder.')
  nnm <- readline(prompt = 'New plot name: '
  )
  
  plot_file <-  file.path(dirname(path_folder1),paste0(ifelse(nchar(nnm),nnm,basename(path_folder1)),'_','.', save_type))
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

# with(time_data_table,
#      {
#        plot(x = NULL,
#             y = NULL,
#             xlim = range(experimental_time, na.rm = T),
#             ylim = c(0,360),
#             xlab = 'Time (min)',
#             ylab = 'Angle',
#             type = 'p',
#             pch = 19,
#             cex = 0.1,
#             col = adjustcolor(point_col, alpha.f = 20/256),
#             axes = F
#        )
#        axis(side = 1,
#             at = 60*(0:(max(experimental_time)/60)),
#             labels = 1*(0:(max(experimental_time)/60))
#        )
#        axis(side = 2,
#             at = 90*(round(min(angle.2.5.)/90):round(max(angle.97.5.)/90)),
#             labels = paste0(90*(round(min(angle.2.5.)/90):round(max(angle.97.5.)/90)),
#                             '°')
#        )
#        abline(h = 90*(round(min(angle.2.5.)/90):round(max(angle.97.5.)/90)),
#               col = rgb(0,0,0,0.1)
#        )
#        
#      }
# )
# 
# with(time_data_table,
#      {
#        lines(x = experimental_time,
#              y = angle.50.,
#              type = 'p',
#              pch = 19,
#              cex = 0.01,
#              col = rgb(0,0.5,0,0.3),
#        )
#      }
# )
# 
# with(time_data_table,
#      {
#        plot(x = NULL,
#             xlim = range(experimental_time, na.rm = T),
#             ylim = range(c(ma_turn.2.5.,ma_turn.97.5.), na.rm = T),
#             xlab = 'time (s)',
#             ylab = paste0('mean turning speed (°/s: ',av_window,'s)'),
#             axes = F
#        )
#        axis(side = 1,
#             at = 60*(0:(max(experimental_time)/60)),
#             labels = 1*(0:(max(experimental_time)/60))
#        )
#        axis(side = 2#,
#        )
#        polygon(x = c(experimental_time,
#                      rev(experimental_time) ),
#                y = c(ma_turn.97.5.,
#                      rev(ma_turn.2.5.)),
#                col = 'gray',
#                border = NA
#        )
#        polygon(x = c(experimental_time,
#                      rev(experimental_time) ),
#                y = c(ma_turn.25.,
#                      rev(ma_turn.75.)),
#                col = 'lightblue',
#                border = NA
#        )
#        abline(v = 60*c(2,4,5,6,7),
#               col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
#               lwd = 2
#        )
#        lines(x = experimental_time,
#              y = ma_turn.50.,
#              col = point_col
#        )
#        abline(h = c(-90, 0, 90),
#               col = 'black',
#               lwd = 0.25
#        )
#      }
# )

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
            at = 60*(0:(max(experimental_time)/60)),
            labels = 1*(0:(max(experimental_time)/60))
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
       # abline(v = 60*c(2,4,5,6,7),
       #        col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
       #        lwd = 2
       # )
       abline(v = 60*c(2,4,6,8,10),
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
            at = 60*(0:(max(experimental_time)/60)),
            labels = 1*(0:(max(experimental_time)/60))
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
       # abline(v = 60*c(2,4,5,6,7),
       #        col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
       #        lwd = 2
       # )
       abline(v = 60*c(2,4,6,8,10),
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
            at = 60*(0:(max(experimental_time)/60)),
            labels = 1*(0:(max(experimental_time)/60))
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
       # abline(v = 60*c(2,4,5,6,7),
       #        col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
       #        lwd = 2
       # )
       abline(v = 60*c(2,4,6,8,10),
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
