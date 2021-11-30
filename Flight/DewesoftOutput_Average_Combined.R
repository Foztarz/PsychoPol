#FOR A 'CLEAN' RUN, RESTART Rstudio
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 11 30
#     MODIFIED:	James Foster              DATE: 2021 11 30
#
#  DESCRIPTION: Loads ".csv" files saved by "DewesoftOutput_Combine_All.R": 
#               a single dataset for the whole experiment. Data for each
#               flight is then averaged across a preselected time period.
#               
#       INPUTS: A "_all.csv" table with a column of time stamps and column of angles ("angle").
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
#- Read in data 
#- Time averaging
#- Combine all to csv 
#- Plot  
#- Test on multiple machines
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
med_window = 30#number of seconds to smooth over for averaging
experiment_length = 8# 10#  #minutes
condition1_length = 2 #minutes
point_col = "darkblue" # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
save_type ="png"# "pdf"# 
quantls = c(0.025, 0.25, 0.5, 0.75, 0.975) # quantiles to use when summarising
plette = 'Plasma'#'YlGnBu'#  for options see http://colorspace.r-forge.r-project.org/articles/approximations.html
crossval = FALSE # TRUE, randomise the data to check the analysis

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
msg = 'Please select the "_all.csv" file'
# set path to files
if(sys_win){#choose.files is only available on Windows
  message('\n\n',msg,'n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file  <- choose.files(
    default = file.path(ltp,'Documents','*_all.csv'),#For some reason this is not possible in the "root" user
    caption = msg
  )
}else{
  message('\n\n',msg,'\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  path_file <- file.choose(new=F)
  if(is.null(path_file)){stop('No file selected.')}
}
#show the user the path they have selected
if(is.null(path_file) | is.na(path_file))
{stop('No file selected.')}else
{print(path_file)}


# Read in files ------------------------------------------------------------
message('\n','Reading in "', basename(path_file), '"\nplease be patient...')
tryCatch(#Perform no further analysis if the file doesn't load
  {
    day_data_table = 
      # read.csv(
      data.table::fread(
        file = path_file, 
        sep = ',',
        header = TRUE
      )#,#read from user-selected file
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
View(day_data_table)

# . Check that there is some data to summarise --------------------------
sample_rate = 1/mean(diff(
  subset(day_data_table, 
         subset = flight == unique(flight)[1] &
           date == unique(date)[1] 
  )$experimental_time
))

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

# Summarise data --------------------------------------------------------
# . Cross validation ------------------------------------------------------
if(crossval)
{
  #function to recalc acceleration
  MAturnspeed <- function(i, #index
                          dta, #vector
                          window = 1, # time window in seconds
                          hz = 100) # sampling rate
  {
    n = window*hz
    winmin = i-round(n/2)+1
    winmax = i+round(n/2)-1
    if(winmin<1){return(NA)}#{winmin = 1}
    if(winmax>length(dta)){return(NA)}#{winmax = length(x)}
    return(
      mean(
        diff(
          x = dta[winmin:winmax],
        )
      )/hz
    )
  }
  #randomise data to check the method
  
  day_data_table = within(day_data_table,
                          {#randomise each parameter of interest across the dataset
                            ma_rho = sample(x = ma_rho,
                                            size = length(moth),
                                            replace = FALSE)
                            abs_turn = sample(x = abs_turn,
                                              size = length(moth),
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
                                     window = 10,
                                     hz = sample_rate
                              )
                            )
                          }
  )
}


# . Rank by first condition -----------------------------------------------
message('Calculating median across each phase...','\n')
#make a new ID, combination of flight and date
#Add phases based on experimental time
day_data_table = within(day_data_table,
                        {
                          flight_date = paste0(date, '_', flight)  
                          phase = ceiling(#round up to nearest integer 
                                    (experimental_time + .Machine$double.eps) / 
                                      med_window
                                    )
                        }
)
#summarise 
flight_rho = aggregate(formula = ma_rho~flight_date*phase,
                       data = subset(within(day_data_table, rm(experimental_time)), 
                                     subset = flag_exp),
                       FUN = quantile,
                       p = quantls)
flight_abs_turn = aggregate(formula = abs_turn~flight_date*phase,
                            data = subset(within(day_data_table, rm(experimental_time)), 
                                          subset = flag_exp),
                            FUN = quantile,
                            p = quantls)
flight_abs_accel = aggregate(formula = abs_accel~flight_date*phase,
                             data = subset(within(day_data_table, rm(experimental_time)), 
                                           subset = flag_exp),
                             FUN = quantile,
                             p = quantls)
day_data_table = data.table::merge.data.table(
                                              x = data.table(flight_rho),
                                              y = data.table::merge.data.table(
                                                x = data.table(flight_abs_turn) ,
                                                y = data.table(flight_abs_accel) ),
                                              by = c('flight_date', 'phase')
                                              )
day_data_table = within(day_data_table,
                        {
                          rho_median = ma_rho.50.; rm(ma_rho.50.)  
                          turn_median = abs_turn.50.; rm(abs_turn.50.)  
                          accel_median = abs_accel.50.; rm(abs_accel.50.)  
                        }
)
# . Save averaged dataset in master folder --------------------------------
av_file <- file.path(dirname(path_file),
                       paste0(basename(dirname(path_file)),
                              '_average',
                              '.csv')
)
message('Saving data summary as:\n', 
        gsub(pattern = '/',
             x = av_file,
             replacement = '\n')
)
message('May take some time...','\n')
data.table::fwrite(x = within(day_data_table, {rm()},
          file = av_file,
          row.names = FALSE
)

# Plot Summary ------------------------------------------------------------
# . Set up plot area ------------------------------------------------------
plot_file = file.path(dirname(path_file), paste0(basename(dirname(path_file)),'_average','.', save_type))
if(file.exists(plot_file))
{
  message('A plot called "', basename(plot_file), '" already exists in this folder.')
  nnm = readline(prompt = 'New plot name: '
  )
  
  plot_file = file.path(dirname(path_file),paste0(ifelse(nchar(nnm),nnm,basename(dirname(path_file))),'_','.', save_type))
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

# . . Turning speed -------------------------------------------------------
with(day_data_table,
     {
       boxplot(formula = turn_median ~ phase,
            xlim = range(phase, na.rm = T),
            ylim = c(0,360),
            xlab = 'phase end time (min)',
            ylab = paste0('absolute mean turning speed (°/s: ',av_window,'s)'),
            axes = F
       )
       axis(side = 1,
            at = unique(phase),
            labels = (1:max(phase))*med_window/60
       )
       axis(side = 2,
            at = 45*(0:(360/45))
       )
      
       abline(v = 0.5+c(2,4,6,8,10)*60/med_window,
              col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
              lwd = 2
       )
       abline(h = c(0),
              col = 'black',
              lwd = 0.25
       )
     }
)


# . . Acceleration --------------------------------------------------------
with(day_data_table,
     {
       boxplot(formula = accel_median ~ phase,
            xlim = range(phase, na.rm = T),
            ylim = c(0,360)/10,
            xlab = 'phase end time (min)',
            ylab = paste0('absolute mean acceleration (°/s^2 : ',av_window,'s)'),
            axes = F
       )
       axis(side = 1,
            at = unique(phase),
            labels = (1:max(phase))*med_window/60
       )
       axis(side = 2,
            at = 15*(0:(360/15))
       )
       
       abline(v = 0.5+c(2,4,6,8,10)*60/med_window,
              col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
              lwd = 2
       )
       abline(h = c(0),
              col = 'black',
              lwd = 0.25
       )
     }
)

# . . Mean vector length --------------------------------------------------
with(day_data_table,
     {
       boxplot(formula = rho_median ~ phase,
               xlim = range(phase, na.rm = T),
            ylim = c(0,1),
            xlab = 'phase end time (min)',
            ylab = paste0('mean vector length (',av_window,'s)'),
            axes = F
       )
       axis(side = 1,
            at = unique(phase),
            labels = (1:max(phase))*med_window/60
       )
       axis(side = 2,
            at = 0:5/5
       )
       abline(v = 0.5+c(2,4,6,8,10)*60/med_window,
              col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
              lwd = 2
       )
       abline(h = c(0,1),
              col = 'black',
              lwd = 0.25
       )
     }
)
mtext(text = c(paste('Medians across', med_window, 's phases from each flight'),
               'phase end time (min)'),
      side = c(3,1),
      outer = T)
# . Save plot -------------------------------------------------------------
dev.off()
shell.exec.OS(plot_file)
