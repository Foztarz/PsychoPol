FT_summarise_all = function(
    comb_file = FT_select_file(file_type = '_proc.txt.gz'),
    experiment_length = 2, #minutes
    condition1_length = 1, #minutes
    av_window = 5.0,#number of seconds to smooth over for averaging
    point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
    save_type = "png",# "pdf",#
    quantls = c(0.025, 0.25, 0.5, 0.75, 0.975), # quantiles to use when summarising
    sys_win = Sys.info()[['sysname']] == 'Windows',
    speedup_parallel = TRUE, #Use the parallel package to speed up calculations
    speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
    verbose = TRUE, #Tell the user what is going on
    show_plot = TRUE, #Tell the user what is going on
    clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
    {makeCluster(parallel::detectCores() - 1,type="SOCK")}else
    {NULL},
    ... #passed to; I don't know yet
  )
{
  # Details ---------------------------------------------------------------
  #       AUTHOR:	James Foster              DATE: 2021 11 18
  #     MODIFIED:	James Foster              DATE: 2022 03 08
  #
  #  DESCRIPTION: Loads "_proc.txt.gz" files saved by "FT_combine_folders()" from 
  #               "FictracDat_Functions.R" and compiles them into a single dataset 
  #                for the whole experiment.
  #               
  #       INPUTS: A "_proc.txt.gz" file with all relevant headers.
  #               User should specify processing details in function inputs.
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
  #    EXAMPLES:  FT_summarise_all()
  # 
  #TODO   ---------------------------------------------
  #TODO   
  #- Read in data +
  #- Save data as txt + 
  #- Looping  +
  #- Combine all to csv +
  #- Angular acceleration + 
  #- Switch to fread to speed up +
  #- Comment
  
  # Useful functions --------------------------------------------------------
  # . Load package ----------------------------------------------------------
  #needs installing before first use (in Rstudio, see automatic message)
  suppressMessages(#these are disturbing users unnecessarily
    {
      require(circular)#package for handling circular data
      require(CircStats)#package for circular hypothesis tests
      if(speedup_data.table){require(data.table)}#package for reorganising large datasets
      if(speedup_parallel){require(parallel)}#package for reorganising large datasets
    }
  )
  
    # . Read in files ------------------------------------------------------------
    if(verbose){message('\n','Reading in "', basename(comb_file), '"\nplease be patient...')}
    tryCatch(#Perform no further analysis if the file doesn't load
      {
        time_data_table = 
          if(speedup_data.table)
          {
            data.table::fread(
              file = comb_file, 
              sep = '\t',
              header = TRUE
            )#,#read from user-selected file
          }else
          {
            read.table(
              file = gzfile(comb_file), 
              sep = '\t',
              header = TRUE
            )#,#read from user-selected file
          }
      },
      error = function(e)
      {
        stop(
          paste0('"',
                 basename(comb_file), 
                 '" could not be loaded!\n',
                 e)
        )
      }
    )
    if(verbose){message('"',basename(comb_file), '" loaded successfully')}
    
  # 
  # # Load and combine --------------------------------------------------------
  # day_files = lapply(X = path_folders,
  #                    FUN = list.files,
  #                    pattern = '_proc.txt$'
  # )
  # day_file_indices = which(as.logical(sapply(day_files, length)))
  # day_files_paths = file.path(path_folders[day_file_indices], 
  #                             day_files[day_file_indices])
  # 
  # day_data = lapply(day_files_paths,
  #                   FUN = read.table,
  #                   header = TRUE
  # )
  # 
  # # . Combine datasets across dates -----------------------------------------
  # names(day_data) = basename(path_folders[day_file_indices])
  # day_data_frame = do.call(what = rbind,
  #                          args = day_data)
  # day_data_frame$date = regmatches(m = regexpr(pattern = '^([^.]+)',
  #                                              text = rownames(day_data_frame) ),
  #                                  x = rownames(day_data_frame) )
  
  # # . Extract features ------------------------------------------------------
  # ExperimentFull = function(experiment_times,#time since first trigger, seconds
  #                           exper_length)#length of experiment, minutes
  # {max(experiment_times/60, na.rm = TRUE) > exper_length}
  # full_length_experiments = aggregate(formula = experimental_time ~ date*bumblebee*trial,
  #                                     data = day_data_frame,
  #                                     FUN = ExperimentFull,
  #                                     exper_length = experiment_length)
  # full_length_conditions = aggregate(formula = experimental_time ~ date*bumblebee*trial,
  #                                    data = day_data_frame,
  #                                    FUN = ExperimentFull,
  #                                    exper_length = condition1_length)
  # full_length_experiments = within(full_length_experiments,
  #                                  {flag_exp = experimental_time; rm(experimental_time)}
  # )
  # full_length_conditions = within(full_length_conditions,
  #                                 {flag_cnd = experimental_time; rm(experimental_time)}
  # )
  # full_flags = merge(x = full_length_experiments,
  #                    y = full_length_conditions)
  # #merge data by shared variable names
  # #can also do this with merge.data.frame, but data.table method is much faster
  # all_data_table = data.table::merge.data.table(
  #   x = day_data_frame, 
  #   y = full_flags)
  # rm(day_data_frame)#one copy in memory is enough
  # # View(all_data_table)
  # all_data_table = within(all_data_table,
  #                         {
  #                           abs_turn = abs(ma_turn)  
  #                           abs_accel = abs(ma_accel)  
  #                         }
  # )
  # 
  # # . Save combined dataset in master folder --------------------------------
  # comb_file <- file.path(path_folder1,
  #                        paste0(basename(path_folder1),
  #                               '_all',
  #                               '.csv')
  # )
  # message('Saving data as:\n', 
  #         gsub(pattern = '/',
  #              x = comb_file,
  #              replacement = '\n')
  # )
  # message('May take some time...','\n')
  # write.csv(x = all_data_table,
  #           file = comb_file,
  #           row.names = FALSE
  # )
  # 
  # # . Check that there is still some data to summarise ----------------------
  # if( with(all_data_table, !any( flag_exp)) )
  # {stop('\n',
  #       "NONE OF THE FILES LOADED WERE ", experiment_length, " min LONG!",
  #       '\n', "Consider reducing the 'experiment length' input variable")
  # }else
  # {
  #   message( 
  #     round(
  #       with(all_data_table, sum(flag_exp))/ 
  #         (experiment_length*sample_rate*60)
  #     ),
  #     " files were ", experiment_length, " min long or longer"
  #   )
  # }
    
    # . Check that there is some data to summarise --------------------------
    sample_rate = 1/mean(diff(
      subset(all_data_table, 
             subset = track == unique(track)[1] &
               date == unique(date)[1] 
      )$experimental_time
    ))
    #set up function to flag full length experiments #N.B. this currently looks quite slow
    FlagExper = function(trackID,
                         dta,
                         experiment_length = 2)
    {
      if(with(dta, length(experimental_time) != length(track)))
      {stop('timeID and trackID vectors must be the same length')}
      return( #TODO speed up with data.table
        with(subset(dta, track %in% trackID), #subset to single track
             {
               rep(x = (max(experimental_time)/60)>
                     experiment_length,#is the experiment longer than the minimum?
                   times = length(experimental_time)) #replicate logical
             }
        )
      )
    }
    if(speedup_parallel)
    {
      clusterExport(cl = clust,
                    varlist = list('all_data_table',
                                   'FlagExper',
                                   'experiment_length'),
                    envir = environment()
      )
    }
    #Search for full length experiments
    all_data_table = within(all_data_table,
                            {
                              flag_exp = if(speedup_parallel)
                              {unlist(
                                parSapply(cl = clust,
                                          X = unique(track),
                                          FUN = FlagExper,
                                          dta = all_data_table,
                                          experiment_length = experiment_length
                                )
                              )
                              }else
                              {unlist(
                                sapply(
                                  X = unique(track),
                                  FUN = FlagExper,
                                  dta = all_data_table,
                                  experiment_length = experiment_length
                                )
                              )
                              }
                            }
    )
    # . . Close the cluster if it is not needed anymore -----------------------
    if(speedup_parallel){stopCluster(clust)}

    # . . Tell the user how many were correct length --------------------------
    if( with(all_data_table, !any( flag_exp)) )
    {stop('\n',
          "NONE OF THE FILES LOADED WERE ", experiment_length, " min LONG!",
          '\n', "Consider reducing the 'experiment length' input variable")
    }else
    {
      if(verbose){
        message( 
          round(
            with(all_data_table, sum(flag_exp))/ 
              (experiment_length*sample_rate*60)
          ),
          " files were ", experiment_length, " min long or longer"
        )
      }
    }
  
  # . Remove stops ----------------------------------------------------------
  #where a stop is identified, set the summarised variables to NA 
  #to avoid biasing analysis
  all_data_table = within(all_data_table,
                          {
                            ma_rho[stop_flag] = NA
                            ma_turn[stop_flag] = NA
                            abs_turn[stop_flag] = NA
                            abs_accel[stop_flag] = NA
                            angle[stop_flag] = NA
                          }
                          )
    
    
  # . Summarise data --------------------------------------------------------
  if(verbose){message('Summarising data from full experiments...','\n')}
  #summarise by in full experiments
  time_rho = aggregate(formula = ma_rho~experimental_time,
                       data = subset(all_data_table, flag_exp),
                       FUN = quantile,
                       p = quantls)
  time_turn = aggregate(formula = ma_turn~experimental_time,
                        data = subset(all_data_table, flag_exp),
                        FUN = quantile,
                        p = quantls)
  time_abs_turn = aggregate(formula = abs_turn~experimental_time,
                            data = subset(all_data_table, flag_exp),
                            FUN = quantile,
                            p = quantls)
  time_abs_accel = aggregate(formula = abs_accel~experimental_time,
                             data = subset(all_data_table, flag_exp),
                             FUN = quantile,
                             p = quantls)
  time_angle = aggregate(formula = angle ~ experimental_time,
                         data = subset(all_data_table, flag_exp),
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
  #combine data into one table of summary measures
  time_data_table = data.table::merge.data.table( # finally add time angle
    x = 
       data.table::merge.data.table( # then combine with rho using experimental_time
         x = data.table(time_rho),
         y = data.table::merge.data.table( # first combine absolute
           x = data.table(time_abs_turn) ,
           y = data.table(time_abs_accel) 
           ),
         by = 'experimental_time'
       ),
    y = data.table(time_angle)
  )
  
  # . Save combined dataset in master folder --------------------------------
  path_folder1 = dirname(comb_file)
  summ_file = file.path(path_folder1,
                         paste0(basename(comb_file),
                                '_summary',
                                '.csv')
  )
  if(verbose){
    message('Saving data summary as:\n', 
            gsub(pattern = '/',
                 x = summ_file,
                 replacement = '\n')
    )
    message('May take some time...','\n')
  }
  if(speedup_data.table)
  {
    fwrite(x = time_data_table,
          file = summ_file,
          row.names = FALSE
    )
  }else
  {
    write.csv(x = time_data_table,
              file = summ_file,
              row.names = FALSE
    )
  }
  if(show_plot)
  {FT_plot_summary(csv_file = summ_file)}
}

FT_plot_summary = function(
    csv_file = FT_select_file(file_type = '_summary.csv'),
    experiment_length = 2, #minutes
    condition1_length = 1, #minutes
    av_window = 5.0,#number of seconds to smooth over for averaging
    point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
    save_type = "pdf",# "png",#
    quantls = c(0.025, 0.25, 0.5, 0.75, 0.975), # quantiles to use when summarising
    plette = 'Plasma',#'YlGnBu'#  for options see http://colorspace.r-forge.r-project.org/articles/approximations.html
    crossval = FALSE, # TRUE, randomise the data to check the analysis
    sys_win = Sys.info()[['sysname']] == 'Windows',
    speedup_parallel = TRUE, #Use the parallel package to speed up calculations
    speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
    verbose = TRUE, #Tell the user what is going on
    show_plot = TRUE, #Tell the user what is going on
    clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
    {makeCluster(parallel::detectCores() - 1,type="SOCK")}else
    {NULL}
    )
{
  path_folder1 = dirname(csv_file)
  # Read in data ------------------------------------------------------------
  # . Read in files ------------------------------------------------------------
  if(verbose){message('\n','Reading in "', basename(csv_file), '"\nplease be patient...')}
  tryCatch(#Perform no further analysis if the file doesn't load
    {#try to load
      time_data_table = 
        if(speedup_data.table)
        {
          data.table::fread(
            file = csv_file, 
            sep = ',',
            header = TRUE
          )#,#read from user-selected file
        }else
        {
          read.table(
            file = csv_file, 
            sep = ',',
            header = TRUE
          )#,#read from user-selected file
        }
    },
    error = function(e)
    {#if it doesn't load
      stop(
        paste0('"',
               basename(csv_file), 
               '" could not be loaded!\n',
               e)
      )
    }
  )
  if(verbose){message('"',basename(csv_file), '" loaded successfully')}
  
  # . . Close the cluster if it is not needed anymore -----------------------
  if(speedup_parallel){stopCluster(clust)}
  
  # Plot Summary ------------------------------------------------------------
  # . Set up plot area ------------------------------------------------------
  save_base = paste0('_summ','.', save_type)
  plot_file = file.path(path_folder1, 
                        paste0(basename(path_folder1),
                                             save_base)
                        )
  #if there is already a file with that name
  if(file.exists(plot_file))
  {
    message('A plot called "', basename(plot_file), '" already exists in this folder.')
    nnm = readline(prompt = 'New plot name: '
    )
    
    plot_file = file.path(dirname(path_folder1),
                          paste0(ifelse(test = nchar(nnm),
                                        yes = nnm,
                                        no = basename(path_folder1)
                                        ),
                                 save_base)
                          )
  }
  switch(EXPR = save_type,
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
  switch(EXPR = save_type,
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
         abline(#v = 60*c(2,4,6,8,10),
                 v = 60*seq(from = 0,
                            to = experiment_length,
                            by = condition1_length),
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
         abline(#v = 60*c(2,4,6,8,10),
                 v = 60*seq(from = 0,
                            to = experiment_length,
                            by = condition1_length),
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
         abline(#v = 60*c(2,4,6,8,10),
                v = 60*seq(from = 0,
                           to = experiment_length,
                           by = condition1_length),
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
}

FT_TimeAverage_all = function(
    path_file = FT_select_file(file_type = '_proc.txt.gz'),
    experiment_length = 2, #minutes
    condition1_length = 1, #minutes
    av_window = 5.0,#number of seconds to smooth over for averaging
    med_window = 10.0,#number of seconds to smooth over for averaging
    point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
    save_type = "png",# "pdf",# 
    quantls = c(0.025, 0.25, 0.5, 0.75, 0.975), # quantiles to use when summarising
    plette = 'Plasma',#'YlGnBu'#  for options see http://colorspace.r-forge.r-project.org/articles/approximations.html
    crossval = FALSE, # TRUE, randomise the data to check the analysis
    sys_win = Sys.info()[['sysname']] == 'Windows',
    speedup_parallel = TRUE, #Use the parallel package to speed up calculations
    speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
    verbose = TRUE, #Tell the user what is going on
    show_plot = TRUE, #Tell the user what is going on
    show_csv = TRUE, #Open the output table after saving
    clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
    {makeCluster(parallel::detectCores() - 1,type="SOCK")}else
    {NULL}
)
{
  # Details ---------------------------------------------------------------
  #       AUTHOR:	James Foster              DATE: 2021 11 30
  #     MODIFIED:	James Foster              DATE: 2021 03 09
  #
  #  DESCRIPTION: Loads "_proc.txt.gz" files saved by "FT_combine_folders()" from 
  #               "FictracDat_Functions.R" and compiles them into a single dataset 
  #                for the whole experiment.
  #               
  #       INPUTS: A "_proc.txt.gz" table with a column of time stamps and column of angles ("angle").
  #               User should specify processing details in input.
  #               
  #      OUTPUTS: Data table (.csv). Plot (.pdf or .png). 
  #
  #	   CHANGES: - Became a function
  #             - Preserve condition
  #             - 
  #
  #   REFERENCES: Batschelet E (1981).
  #               In: Circular Statistics in Biology
  #               Academic Press (London)
  #
  #    EXAMPLES:  FT_TimeAverage_all()
  # 
  #TODO   ---------------------------------------------
  #TODO   
  #- Read in data +
  #- Time averaging +
  #- Combine all to csv +
  #- Plot  +
  #- Test on multiple machines
  #- Comment
  
  # Useful functions --------------------------------------------------------
  # . Load package ----------------------------------------------------------
  #needs installing before first use (in Rstudio, see automatic message)
  suppressMessages(#these are disturbing users unnecessarily
    {
      require(circular)#package for handling circular data
      require(CircStats)#package for circular hypothesis tests
      if(speedup_data.table){require(data.table)}#package for reorganising large datasets
      if(speedup_parallel){require(parallel)}#package for reorganising large datasets
    }
  )
  
  # Input Variables ----------------------------------------------------------
  
  
  # Read in files ------------------------------------------------------------
  message('\n','Reading in "', basename(path_file), '"\nplease be patient...')
  tryCatch(#Perform no further analysis if the file doesn't load
    {
      day_data_table = 
      if(speedup_data.table)
      {
        data.table::fread(
          file = path_file, 
          sep = '\t',
          header = TRUE
        )#,#read from user-selected file
      }else
      {
        read.table(
          file = gzfile(path_file), 
          sep = '\t',
          header = TRUE
        )#,#read from user-selected file
      }
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
  # View(day_data_table)
  
  # . Check that there is some data to summarise --------------------------
  sample_rate = 1/mean(diff(
    subset(day_data_table, 
           subset = track == unique(track)[1] &
             date == unique(date)[1] 
    )$experimental_time
  ))
  #set up function to flag full length experiments #N.B. this currently looks quite slow
  FlagExper = function(trackID,
                       dta,
                       experiment_length = 2)
  {
    if(with(dta, length(experimental_time) != length(track)))
    {stop('timeID and trackID vectors must be the same length')}
    return( #TODO speed up with data.table
      with(subset(dta, track %in% trackID), #subset to single track
           {
             rep(x = (max(experimental_time)/60)>
                   experiment_length,#is the experiment longer than the minimum?
                 times = length(experimental_time)) #replicate logical
           }
      )
    )
  }
  if(speedup_parallel)
  {
    clusterExport(cl = clust,
                  varlist = list('day_data_table',
                                 'FlagExper',
                                 'experiment_length'),
                  envir = environment()
    )
  }
  #Search for full length experiments
  day_data_table = within(day_data_table,
                          {
                            flag_exp = if(speedup_parallel)
                            {unlist(
                              parSapply(cl = clust,
                                        X = unique(track),
                                        FUN = FlagExper,
                                        dta = day_data_table,
                                        experiment_length = experiment_length
                              )
                            )
                            }else
                            {unlist(
                              sapply(
                                X = unique(track),
                                FUN = FlagExper,
                                dta = day_data_table,
                                experiment_length = experiment_length
                              )
                            )
                            }
                          }
  )
  # . . Close the cluster if it is not needed anymore -----------------------
  if(speedup_parallel){stopCluster(clust)}
  
  # . . Tell the user how many were correct length --------------------------
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
  
  # . Remove stops ----------------------------------------------------------
  #where a stop is identified, set the summarised variables to NA 
  #to avoid biasing analysis
  day_data_table = within(day_data_table,
                          {
                            ma_rho[stop_flag] = NA
                            ma_turn[stop_flag] = NA
                            abs_turn[stop_flag] = NA
                            abs_accel[stop_flag] = NA
                            angle[stop_flag] = NA
                          }
  )
  
  # Summarise data --------------------------------------------------------
  # . Cross validation ------------------------------------------------------
    #randomise data to check the method
    if(crossval)
    {
    if(verbose){message('Randomising data to cross-validate...','\n')}
    day_data_table = within(day_data_table,
                            {#randomise each parameter of interest across the dataset
                              ma_rho = sample(x = ma_rho,
                                              size = length(bumblebee),
                                              replace = FALSE)
                              abs_turn = sample(x = abs_turn,
                                                size = length(bumblebee),
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
                                       window = av_window,
                                       hz = sample_rate
                                )
                              )
                            }
    )
    }
  # . Rank by first condition -----------------------------------------------
  if(verbose){message('Calculating median across each phase...','\n')}
  #Add phases based on experimental time
  day_data_table = within(day_data_table,
                          {
                            phase = ceiling(#round up to nearest integer 
                              (experimental_time + .Machine$double.eps) / 
                                med_window
                            )
                          }
  )
  #summarise 
  track_rho = aggregate(formula = ma_rho~track*phase*condition,
                         data = subset(within(day_data_table, rm(experimental_time)), 
                                       subset = flag_exp),
                         FUN = quantile,
                         p = quantls)
  track_abs_turn = aggregate(formula = abs_turn~track*phase*condition,
                              data = subset(within(day_data_table, rm(experimental_time)), 
                                            subset = flag_exp),
                              FUN = quantile,
                              p = quantls)
  track_abs_accel = aggregate(formula = abs_accel~track*phase*condition,
                               data = subset(within(day_data_table, rm(experimental_time)), 
                                             subset = flag_exp),
                               FUN = quantile,
                               p = quantls)

  # . . Combine summaries ---------------------------------------------------
  day_data_table = data.table::merge.data.table(
    x = data.table(track_rho),
    y = data.table::merge.data.table(
      x = data.table(track_abs_turn) ,
      y = data.table(track_abs_accel) ),
    by = c('track', 'phase', 'condition')
  )
  #give them more convenient names
  day_data_table = within(day_data_table,
                          {
                            rho_median = ma_rho.50.; rm(ma_rho.50.)  
                            turn_median = abs_turn.50.; rm(abs_turn.50.)  
                            accel_median = abs_accel.50.; rm(abs_accel.50.)  
                          }
  )
  # . Save averaged dataset in master folder --------------------------------
  av_file = file.path(dirname(path_file),
                       paste0(basename(path_file),
                              '_average',
                              '.csv')
  )
  if(verbose)
  {
    message('Saving data summary as:\n', 
            gsub(pattern = '/',
                 x = av_file,
                 replacement = '\n')
    )
    message('May take some time...','\n')
  }
  if(speedup_data.table)
  {
    data.table::fwrite(x = day_data_table,
                       file = av_file,
                       row.names = FALSE
    )
  }else
  {
    write.table(x = day_data_table,
               file = av_file,
               sep = ',',
               row.names = FALSE
              )
  }
  if(show_csv){shell.exec.OS(av_file)}
  if(show_plot)
  {
    FT_plot_average(path_file = av_file,
                    show_plot = show_plot)
  }
}

FT_plot_average = function(path_file = FT_select_file('_average.csv'),
                           experiment_length = 2, #minutes
                           condition1_length = 1, #minutes
                           av_window = 5.0,#number of seconds to smooth over for averaging
                           med_window = 10.0,#number of seconds to smooth over for averaging
                           point_col = "darkblue", # try "red", "blue", "green" or any of these: https://htmlcolorcodes.com/color-names/
                           save_type = "png",# "pdf",# 
                           plette = 'Plasma',#'YlGnBu'#  for options see http://colorspace.r-forge.r-project.org/articles/approximations.html
                           crossval = FALSE, # TRUE, data were randomised to check the analysis
                           sys_win = Sys.info()[['sysname']] == 'Windows',
                           speedup_data.table = TRUE, #Use the data.table package to speed up reading and writing
                           verbose = TRUE, #Tell the user what is going on
                           show_plot = TRUE #Tell the user what is going on
                           )
{
  path_folder1 = dirname(path_file)
  # Read in data ------------------------------------------------------------
  # . Read in files ------------------------------------------------------------
  if(verbose){message('\n','Reading in "', basename(path_file), '"\nplease be patient...')}
  tryCatch(#Perform no further analysis if the file doesn't load
    {#try to load
      day_data_table = 
        if(speedup_data.table)
        {
          data.table::fread(
            file = path_file, 
            sep = ',',
            header = TRUE
          )#,#read from user-selected file
        }else
        {
          read.table(
            file = path_file, 
            sep = ',',
            header = TRUE
          )#,#read from user-selected file
        }
    },
    error = function(e)
    {#if it doesn't load
      stop(
        paste0('"',
               basename(path_file), 
               '" could not be loaded!\n',
               e)
      )
    }
  )
  if(verbose){message('"',basename(path_file), '" loaded successfully')}
    
  # Plot Summary ------------------------------------------------------------
  # . Set up plot area ------------------------------------------------------
  save_base =  paste0('_average',
                      ifelse(test = crossval, 
                             yes = '-CROSSVAL',
                             no = ''),
                      ifelse(test = save_type %in% 'pdf',
                             yes = '',
                             no = '-1'),
                      '.', 
                      save_type)
  plot_file = file.path(dirname(path_file), 
                        paste0(basename(dirname(path_file)),
                               save_base)
                        )
  if(file.exists(plot_file))
  {
    message('A plot called "', basename(plot_file), '" already exists in this folder.')
    nnm = readline(prompt = 'New plot name: '
    )
    
    plot_file = file.path(dirname(path_file),
                          paste0(ifelse(test = nchar(nnm),
                                        yes = nnm,
                                        no = basename(dirname(path_file))),
                                 save_base)
                          )
  }
  switch(EXPR = save_type,
         pdf = 
           pdf(file = plot_file,
               paper = 'a4',
               height = 10,
               bg = 'white',
               onefile = TRUE,
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
  switch(EXPR = save_type,
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

  # . . Loop through experiments --------------------------------------------
  for(cnd in with(day_data_table, unique(condition)))
  {  
    if(dim(subset(day_data_table,
                  condition %in% cnd))[1])
    {
    # . . Turning speed -------------------------------------------------------
    with(subset(day_data_table,
                condition %in% cnd),
         {
           boxplot(formula = turn_median ~ phase,
                   xlim = range(phase, na.rm = T),
                   ylim = c(0,360),
                   xlab = 'phase end time (s)',
                   ylab = paste0('absolute mean turning speed (°/s: ',av_window,'s)'),
                   axes = F
           )
           axis(side = 1,
                at = unique(phase),
                labels = round( (1:length(unique(phase)))*
                                  med_window
                                )#/60
           )
           axis(side = 2,
                at = 15*(0:(360/15))
           )
           
           abline(v = 0.5+seq(from = 0,
                             to = experiment_length,
                             by = condition1_length)*60/med_window,
                  col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
                  lwd = 2
           )
           abline(h = c(0),
                  col = 'black',
                  lwd = 0.25
           )
         }
    )
    legend(x = 'topleft',
           legend = cnd,
          cex = 2)
    
    # . . Acceleration --------------------------------------------------------
    with(subset(day_data_table,
                condition %in% cnd),
         {
           boxplot(formula = accel_median ~ phase,
                   xlim = range(phase, na.rm = T),
                   ylim = c(0,360)/10,
                   xlab = 'phase end time (s)',
                   ylab = paste0('absolute mean acceleration (°/s^2 : ',av_window,'s)'),
                   axes = F
           )
           axis(side = 1,
                at = unique(phase),
                labels = round( (1:length(unique(phase)))*
                                  med_window
                )#/60
           )
           axis(side = 2,
                at = 5*(0:(360/5))
           )
           
           abline(v = 0.5+seq(from = 0,
                              to = experiment_length,
                              by = condition1_length)*60/med_window,
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
    with(subset(day_data_table,
                condition %in% cnd),
         {
           boxplot(formula = rho_median ~ phase,
                   xlim = range(phase, na.rm = T),
                   ylim = c(0,1),
                   xlab = 'phase end time (s)',
                   ylab = paste0('mean vector length (',av_window,'s)'),
                   axes = F
           )
           axis(side = 1,
                at = unique(phase),
                labels = round( (1:length(unique(phase)))*
                                  med_window
                              )#/60
           )
           axis(side = 2,
                at = (0:5)/5
           )
           abline(v = 0.5+seq(from = 0,
                              to = experiment_length,
                              by = condition1_length)*60/med_window,
                  col = c('orange','seagreen','MediumAquamarine','MediumAquamarine','MediumAquamarine'),
                  lwd = 2
           )
           abline(h = c(0,1),
                  col = 'black',
                  lwd = 0.25
           )
         }
    )
    mtext(text = c(paste('Medians across', med_window, 's phases from each track'),
                   'phase end time (min)'),
          side = c(3,1),
          outer = T)
  }else
  {
    plot(x = NULL,
         xlim = c(0,1),
         ylim = c(0,1),
         xlab = '',
         ylab = '',
         axes = F
         )
    legend(x = 'center',
           legend = paste0(cnd,'\n','(no data)'),
           cex = 2)
  }

  # . . Open next page ------------------------------------------------------
  if(
    !(save_type %in% 'pdf') &
    with(day_data_table,  
          {
          which(cnd %in% unique(condition))+1 < 
              length(unique(condition))
          }
        )
    )
  {
    dev.off()
    # . . . Set up plot area ------------------------------------------------
    save_base =  paste0('_average',
                        ifelse(test = crossval, 
                               yes = '-CROSSVAL',
                               no = ''),
                        '-',
                        which(with(day_data_table, unique(condition)) %in% cnd)+1, # new page number
                        '.', 
                        save_type)
    plot_file = file.path(dirname(path_file), 
                          paste0(basename(dirname(path_file)),
                                 save_base)
    )
    if(file.exists(plot_file))
    {
      message('A plot called "', basename(plot_file), '" already exists in this folder.')
      nnm = readline(prompt = 'New plot name: '
      )
      
      plot_file = file.path(dirname(path_file),
                            paste0(ifelse(test = nchar(nnm),
                                          yes = nnm,
                                          no = basename(dirname(path_file))),
                                   save_base)
      )
    }
    switch(EXPR = save_type,
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
    switch(EXPR = save_type,
           png = par(mfrow = c(3,1),
                     mar = c(3,5,0,0),
                     oma = c(1.5,0,1.5,0),
                     cex = 1.5
           ),
           par(mfrow = c(3,1),
               mar = c(3,5,0,0),
               oma = c(1.5,0,1.5,0))
    )
    
  }
  }
  # . Save plot -------------------------------------------------------------
  dev.off()
  if(show_plot)
    {
      if(save_type %in% 'pdf')
      {
      shell.exec.OS(plot_file)
      }else
      {
        for(cnd in with(day_data_table, unique(condition)) )
        {
          open_plot = sub(pattern = paste0('..',save_type,'$'),
                          replacement = paste0(
                            which( 
                              with(day_data_table, unique(condition)) %in% cnd
                                                     ),
                                               '.',save_type),
                          x =  plot_file
                          )
          shell.exec.OS(open_plot)
        }
      }
    }
}