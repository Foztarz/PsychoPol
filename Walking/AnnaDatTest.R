FT_select_file = function(file_type = ".dat",
                          sys_win = Sys.info()[['sysname']] == 'Windows'
                          )
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

AnnounceTSP = function(tsr)
{
  tp = round(tsp(tsr),2)
  message(
    'start point : ',tp[1], ' seconds', '\n', 
    'end point : ',tp[2], ' seconds', '\n', 
    'frame rate : ',tp[3], ' Hz', '\n' 
  )
}

# Input parameters --------------------------------------------------------
target_freq = 0.1 #Hz 
av_window = 4/target_freq#number of seconds to use for Welch PSD window
validation = FALSE #TODO not implemented

# Select data -------------------------------------------------------------
path_file = FT_select_file()
nn = read.table(file = path_file,
                skip = 87,
                col.names = c("t", 
                             "frNr", 
                             "iHead", 
                             "driftX", 
                             "combinedX", 
                             "driftY", 
                             "dX", 
                             "dY", 
                             "dZ", 
                             "punish"),
                sep = ',')

# Plot example ------------------------------------------------------------
with(nn, plot(t, dZ, type = 'l'))

# Derive variables --------------------------------------------------------
fps = with(nn, 1/mean(diff(t)))*1e3


# Frequency analysis ------------------------------------------------------

# Load package
library(bspec, quietly = TRUE)#Bayesian Spectral Inference

# Convert to time series
ts_dZ = with(nn,
             {
              ts(data= dZ,#z rotation in degrees per frame(?)
                 start = min(t)/1e3,#time in seconds
                 frequency = fps
                 ) 
             }
             )

AnnounceTSP(ts_dZ)

# . Calculate Welch PSD ---------------------------------------------------
emp_data = empiricalSpectrum(ts_dZ)
psd_data = welchPSD(x = ts_dZ,
                    seglength = av_window,
                    windowfun = tukeywindow,
                    method = "median", 
                    windowingPsdCorrection = TRUE
)

# Plot the outcome --------------------------------------------------------
with(emp_data,
     {
       plot(x = frequency,
            y = sqrt(power),
            log = 'x',
            xlim = c(1/(fps*1.2), fps*1.2),
            ylim = sqrt( c(0, max(c(emp_data$power, psd_data$power)) ) ),
            type = 'l',
            col = adjustcolor('darkblue', alpha.f = 100/255),
            lwd = 2,
            main = if(validation){paste(1/target_freq, 'sec sinusoid')}else
            {sub(x = basename(path_file),
                 pattern = '(.(dat)).*',
                 replacement = '')
            }
       )  
     }
)
abline(v = c(target_freq,target_freq/4),
       lty = c(1, 3),
       col = adjustcolor('gray', alpha.f = 200/255)
)
with(psd_data,
     {
       lines(x = frequency,
             y = sqrt(power),
             col = adjustcolor('darkred', alpha.f = 150/255),
             lwd = 2
       )
     }
)
legend(x = 'topright',
       legend  = c('discrete FFT',
                   paste0("Welch's PSD\n",
                          av_window," sec window"),
                   paste(target_freq, 'Hz'),
                   paste(target_freq/4, 'Hz')
                   ),
       col = c('darkblue',
               'darkred',
               'gray',
               'gray'
       ),
       cex = 0.7,
       lty = c(1, 1, 1, 3),
       lwd = c(2, 2, 1, 1)
)

