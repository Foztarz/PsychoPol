#FOR A 'CLEAN' RUN, RESTART Rstudio
graphics.off()
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 11 25
#     MODIFIED:	James Foster              DATE: 2021 11 25
#
#  DESCRIPTION: Loads ".csv" files saved by "DewesoftOutput_Combine_All.R": 
#               a single dataset for the whole experiment. Data for each
#               flight is then plotted in a raster-plot.
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
experiment_length = 10# 8# #minutes
condition1_length = 2 #minutes
point_col = "darkblue" # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
save_type ="png"# "pdf"# 
quantls = c(0.025, 0.25, 0.5, 0.75, 0.975) # quantiles to use when summarising
plette = 'Plasma'#'YlGnBu'#  for options see http://colorspace.r-forge.r-project.org/articles/approximations.html
crossval = FALSE # TRUE, randomise the data to check the analysis

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
    # abs_accel = sample(x = abs_accel,
    #               size = length(moth),
    #               replace = FALSE)
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
message('Ranking by median value across 1st condition...','\n')
#make a new ID, combination of flight and date
day_data_table = within(day_data_table,
                        {
                        flight_date = paste0(date, '_', flight)  
                        }
                        )
#summarise 
flight_rho = aggregate(formula = ma_rho~flight_date,
                     data = subset(day_data_table, 
                                   subset = experimental_time < condition1_length*60),
                     FUN = quantile,
                     p = quantls)
flight_abs_turn = aggregate(formula = abs_turn~flight_date,
                          data = subset(day_data_table, 
                                        subset = experimental_time < condition1_length*60),
                          FUN = quantile,
                          p = quantls)
flight_abs_accel = aggregate(formula = abs_accel~flight_date,
                           data = subset(day_data_table, 
                                         subset = experimental_time < condition1_length*60),
                           FUN = quantile,
                           p = quantls)
##probably don't need to calculate the rest?
day_data_table = data.table::merge.data.table(x = 
                                                 data.table::merge.data.table(
                                                   x = data.table(flight_rho),
                                                   y = data.table::merge.data.table(
                                                     x = data.table(flight_abs_turn) ,
                                                     y = data.table(flight_abs_accel) ),
                                                   by = c('flight_date')
                                                 ),
                                               y = subset(day_data_table, 
                                                         subset = experimental_time < experiment_length*60),
                                              by = c('flight_date')
)
day_data_table = within(day_data_table,
                        {
                        rank_rho = rank(ma_rho.50.)  
                        rank_turn = rank(abs_turn.50.)  
                        rank_accel = rank(abs_accel.50.)  
                        }
                        )




# . Make matrices of full experiments -------------------------------------

full_expr = subset(day_data_table,
                   subset = flag_exp &
                     experimental_time < experiment_length*60
                   )
# sample_rate = 1/mean(diff(
#                           subset(full_expr, 
#                                  subset = flight_date == 
#                                            unique(flight_date)[1]
#                                  )$experimental_time
#                           ))
n_flightdates = length(unique(full_expr$flight_date))
#turning speed
mtr_turn = with(data.table::setorderv(
                          full_expr,
                          cols = 'rank_turn',
                          order = -1), 
                matrix(data = abs_turn,
                        ncol = n_flightdates
                       )
                )
#acceleration
mtr_accel = with(data.table::setorderv(
                          full_expr,
                          cols = 'rank_accel',
                          order = -1), 
                matrix(data = abs_accel,
                        ncol = n_flightdates
                       )
                )
#mean vector
mtr_rho = with(data.table::setorderv(
                          full_expr,
                          cols = 'rank_rho',
                          order = 1), 
                matrix(data = ma_rho,
                        ncol = n_flightdates
                       )
                )


# Plot Summary ------------------------------------------------------------
message("Making raster plot")
# . Set up plot area ------------------------------------------------------
plot_file <- file.path(dirname(path_file),
                       paste0(basename(path_file),
                              '_rast', ifelse(crossval, '-CROSSVAL',''),
                              '.', save_type)
                       )
if(file.exists(plot_file))
{
  message('A plot called "', basename(plot_file), '" already exists in this folder.')
  nnm <- readline(prompt = 'New plot name: '
  )
  
  plot_file <-  file.path(dirname(path_file),
                          paste0(
                                ifelse(test = nchar(nnm),
                                       yes = nnm,
                                       no = basename(path_file)),
                                '_rast',ifelse(crossval, '-CROSSVAL',''),'.', save_type)
                          )
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
                 mar = c(2,3,2,5),
                 oma = c(1.5,0,1.5,0),
                 cex = 1.5
       ),
       par(mfrow = c(3,1),
           mar = c(2,3,2,5),
           oma = c(1.5,0,1.5,0))
)

# . Plot summarys ---------------------------------------------------------

#  . . Set up colour scale transforms -------------------------------------
TurnTrans = function(x)
  {log(x + 1)}
TurnTransInv = function(x)
  {exp(x) - 1}
AccelTrans = function(x)
  {log(x + 1)}
AccelTransInv = function(x)
  {exp(x) - 1}
RhoTrans = function(x)
  {x^2}
  # {suppressWarnings({qlogis(x + 1e-3)})}
RhoTransInv = function(x)
  {sqrt(x)}
  # {plogis(x) - 1e-3}

# . . Turning speed -------------------------------------------------------
with(day_data_table,
     {
       image(x = TurnTrans(mtr_turn),
             useRaster = TRUE,
             zlim = TurnTrans(c(0,360)),
            xlim = c(0,1.1),
            # ylim = c(0,1.0),
            xlab = 'time (s)',
            ylab = ' ',
            main = paste0('absolute mean turning speed (°/s: ',av_window,'s)'),
            axes = F,
            col = hcl.colors(n = 16,
                             palette = plette)
       )
       axis(side = 1,
            at = sample_rate*60*(0:(max(experimental_time)/60))/dim(mtr_turn)[1],
            labels = 1*(0:(max(experimental_time)/60))
       )
       axis(side = 2,
            at = seq(from = 0, to  = 1, length.out = dim(mtr_turn)[2]),
            labels = gsub(pattern = '_',
                          x = unique(
                            data.table::setorderv(
                              full_expr,
                              cols = 'rank_turn',
                              order = 1)$flight_date,
                            ),
                            replacement = ' '
                         ),
            las = 1,
            cex.axis = 0.25
           )
       abline(v = sample_rate*60*c(2,4,6,8,10)/dim(mtr_turn)[1],
              col = adjustcolor('white',alpha.f = 100/255),#c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
              lwd = 2
       )
     }
)
legend(x = 'right',
       inset=c(0,0),
       legend = c(
         round( 
           TurnTransInv(
           seq( from = TurnTrans(0), 
                to = TurnTrans(360), 
                length.out = 5) ) 
           ),
         '>360'
         ),
       pch = 22,
       pt.bg = c(
               hcl.colors(n = 5,
                        palette = plette),
               'white'
               )
       )

# . . Acceleration --------------------------------------------------------
with(day_data_table,
     {
       image(x = AccelTrans(mtr_accel),
             useRaster = TRUE,
             zlim = AccelTrans(c(0,15)),
             xlim = c(0,1.1),
             # ylim = c(1,n_flightdates),
             xlab = 'time (s)',
             ylab = ' ',
             main = paste0('absolute mean acceleration (°/s^2: ',av_window,'s)'),
             axes = F,
             col = hcl.colors(n = 16,
                              palette = plette)
       )
       axis(side = 1,
            at = sample_rate*60*(0:(max(experimental_time)/60))/dim(mtr_accel)[1],
            labels = 1*(0:(max(experimental_time)/60))
       )
       axis(side = 2,
            at = seq(from = 0, to  = 1, length.out = dim(mtr_accel)[2]),
            labels = gsub(pattern = '_',
                          x = unique(
                            data.table::setorderv(
                              full_expr,
                              cols = 'rank_accel',
                              order = 1)$flight_date,
                          ),
                          replacement = ' '
            ),
            las = 1,
            cex.axis = 0.25
       )
       abline(v = sample_rate*60*c(2,4,6,8,10)/dim(mtr_accel)[1],
              col = adjustcolor('white',alpha.f = 100/255),#c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
              lwd = 2
       )
     }
)
legend(x = 'right',
       inset=c(0,0),
       legend = c(
         round( 
           AccelTransInv(
             seq( from = AccelTrans(0), 
                  to = AccelTrans(15), 
                  length.out = 5) )
           ),
         '>15'
       ),
       pch = 22,
       pt.bg = c(
         hcl.colors(n = 5,
                    palette = plette),
         'white'
       )
)

# . . Mean vector length --------------------------------------------------
with(day_data_table,
     {
       image(x = RhoTrans(mtr_rho),
             useRaster = TRUE,
             zlim = RhoTrans(c(0,0.99)),
             xlim = c(0,1.1),
             # ylim = c(1,n_flightdates),
             xlab = 'time (s)',
             ylab = 'Test',
             main = paste0('mean vector length (',av_window,'s)'),
             axes = F,
             col = hcl.colors(n = 16,
                              palette = plette)
       )
       axis(side = 1,
            at = sample_rate*60*(0:(max(experimental_time)/60))/dim(mtr_accel)[1],
            labels = 1*(0:(max(experimental_time)/60))
       )
       axis(side = 2,
            at = seq(from = 0, to  = 1, length.out = dim(mtr_rho)[2]),
            labels = gsub(pattern = '_',
                          x = unique(
                            data.table::setorderv(
                              full_expr,
                              cols = 'rank_rho',
                              order = -1)$flight_date,
                          ),
                          replacement = ' '
            ),
            las = 1,
            cex.axis = 0.25
       )
       abline(v = sample_rate*60*c(2,4,6,8,10)/dim(mtr_rho)[1],
              col = adjustcolor('white',alpha.f = 150/255),#c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
              lwd = 2
       )
     }
)
legend(x = 'right',
       inset=c(0,0),
       legend = c(
                  round(
                        x = RhoTransInv(seq( from = RhoTrans(0), 
                                 to = RhoTrans(0.99), 
                                 length.out = 5)),
                        digits = 2
                  ),
                  '1.0'
       ),
       pch = 22,
       pt.bg = c(
                 hcl.colors(n = 5,
                            palette = plette),
                 'white'
                 )
      )
mtext(text = 'time (min)',
      side = 1,
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
# . Save plot -------------------------------------------------------------
dev.off()
shell.exec.OS(plot_file)
