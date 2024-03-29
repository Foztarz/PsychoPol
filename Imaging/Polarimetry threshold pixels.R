# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 07 13
#     MODIFIED:	James Foster              DATE: 2022 07 13
#
#  DESCRIPTION: Adapted from "Irradiance APol03-20210623.R"
#               Loads text files called "DoLP histogram.csv" generated by
#               "processing_Polanalyser_HDR.py" and calculates the proportion
#               of above-threshold pixels for a range of values. This data is
#               then saved in another '.csv' file in the master folder.
#
#      OUTPUTS: Table of proportions for each image (csv).
#
#	     CHANGES: - strip whitespace out of thousands with ' ' separator
#               - loop through all subfolders with the right name
#
#
#   REFERENCES: Temple, S. E, J. E Mcgregor, C. Miles, L. Graham, J. Miller,
#               J. Buck, N. E. Scott-Samuel & N. W. Roberts. (2015)
#               Perceiving Polarization with the Naked Eye: Characterization of
#               Human Polarization Sensitivity’. Proc. R. Soc. B 282, 20150338
#               https://doi.org/10.1098/rspb.2015.0338.
#
#               Foster J.J.,  Kirwan J.D., el Jundi B., Smolka J., Khaldy L.,
#               Baird E., Byrne M.J., Nilsson D.-E., Johnsen S. & Dacke M.
#               Orienting to Polarized Light at Night – Matching Lunar Skylight
#               to Performance in a Nocturnal Beetle
#               J. Exp. Biol. 222, jeb188532
#               https://doi.org/10.1242/jeb.188532.
#
#    USAGE:     ctrl + shift + s, select top level folder containing 10 image folders
#
#
#TODO   ---------------------------------------------
#TODO
#- Read in data +
#- Calculate for each species  +
#- Save  +
#- Loop through 10image folders +

# Input Variables ----------------------------------------------------------

#  .  User input -----------------------------------------------------------

#_end_ of folder name
fld_end = '10times'

#Species thresholds
th = data.frame(
  Gryllus_campestris = 05,
  #Labhart, 1996
  Apis_mellifera = 10,
  #Edrich & von Helversen, 1987
  Schistocerca_gregaria = 30,
  #Pfeiffer _et al._, 2011
  Homo_sapiens = 56 # Temple _et al._, 2015
) / 100


#check the operating system and assign a logical flag (TRUE or FALSE)
sys_win <- Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if (sys_win) {
  #get rid of all the backslashes
  ltp <-
    gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
} else{
  #Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp <- Sys.getenv('HOME')#Life was easier on Mac
}

#  .  Select files ---------------------------------------------------------

#	directories														#
# ltp <- Sys.getenv('HOME') #Base level in environment
# set path to files
if (sys_win) {
  #choose.files is only available on Windows
  message('\n\nPlease select the top-level folder\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  fld  <- choose.dir(default = file.path(ltp, 'Documents'), #For some reason this is not possible in the "root" user
                     caption = 'Please select the top-level folder')
} else{
  message('\n\nPlease select any csv file in a subfolder of the top-level folder\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  fld <-
    dirname(dirname(dirname(file.choose(new = F)))) # guess the top folder is up 3
}
print(fld)


# Find files --------------------------------------------------------------
#list all folders in the selected folder
flds_all = list.dirs(path = fld,
                     recursive = TRUE,
                     #ALL subfolders
                     full.names = TRUE) #full paths to files
#among those, find the pattern "10images"
flds = grep(pattern = paste0(fld_end,'$'),#regular expression, '$' indicates then end
            x =  flds_all, #look at the subfolders listed
            value = TRUE) #when found, return those names
#set up a function to list all histogram files in those subfolders
ListHistograms = function(i)
{
  list.files(
    path = i,
    # this folder
    pattern = 'DoLP_histogram.csv',
    # histogram name
    recursive = TRUE,
    # could be in a subfolder
    full.names = TRUE
  ) # return the full file path
}
#loop through the whole list
fls = lapply(X = flds, #for each master folder...
             FUN = ListHistograms) # return all histogram files
#function to read in all files
ReadHistograms = function(i)
{
  lapply(X = i,
         read.table,
         sep = ',',
         header = FALSE)
}
#loop through the whole list
hsts = lapply(X = fls, # for each master folder..
              ReadHistograms) # read in all histograms
#check how many master folders there are
l_hists = 1:length(hsts)


# Calculate proportions ---------------------------------------------------


# . Set up calculation functions ------------------------------------------
#Function to calculate the proportion of above threshold pixels
PropThresh = function(xx, # the histogram data
                      tt = 0.10)
  #threshold (default honeybee ~10%)
{
  #check for white space in numbers
  if (any(sapply(xx[1:2, ], class) %in% "character"))
  {
    x1 = matrix(data = NA,
                nrow = 2,
                ncol = length(xx[1, ]))
    x1[1, ] = sapply(xx[1, ], function(x) {
      as.numeric(sub(
        pattern = ' ',
        replacement = '',
        x = x
      ))
    })
    x1[2, ] = sapply(xx[2, ], function(x) {
      as.numeric(sub(
        pattern = ' ',
        replacement = '',
        x = x
      ))
    })
    xx = x1
  }
  sum(xx[1, ][xx[2, ] > tt]) / #total above threshold pixels
    sum(xx[1, ]) # divided by total number of pixels
}

#Function to calculate proportions for a list of histograms
ListThresh = function(ll, #list of histograms
                      tt = 0.10)
  #threshold (default honeybee ~10%)
{
  do.call(what = rbind,
          #collapse into a row vector
          args = lapply(X = ll, # for each list element...
                        FUN = PropThresh, #calculate the proportion of above threshold px
                        tt = tt) )# threshold
}

FrameThresh = function(ll, #list of histograms
                       tvec)
  # vector of named thresholds
{
  lst = lapply(
    X = names(tvec),
    # for each threshold name...
    FUN = function(i)
    {
      data.frame(Species = i,
                 # make a dataframe with that species name
                 Detectable_Pixels = ListThresh(ll, tvec[[i]])) # apply the ListThresh function
    }
  )
  #collapse into rows
  return(do.call(what = rbind,
                 args = lst))
}



# . Calculate for all histograms and all thresholds -----------------------
#create an empty list
dtf = list()
#loop through each master folder and calculate
for (i in l_hists)
{
  dtf[[i]] = FrameThresh(ll = hsts[[i]], #list of histograms
                         tvec = th) # vector of named thresholds
}

# Save the data in the master folder --------------------------------------
#loop through each master folder and save
for (i in l_hists)
{
  save_name = file.path(flds[[i]], 'Threshold_Pixels.csv')
  write.table(
    x = dtf[[i]],
    # this data frame
    file = save_name,
    #
    sep = ',',
    row.names = FALSE
  )
}
