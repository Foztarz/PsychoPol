# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 02 07
#     MODIFIED:	James Foster              DATE: 2022 02 15
#
#  DESCRIPTION: Loads a text file and plots relative frequencies of different 
#               dance typdes.
#               
#       INPUTS: A ".csv" table with columns for experiment phase ("stimulus") and
#               angle ("angle").
#               User should specify test details (line 80).
#               
#      OUTPUTS: Results table (.csv).
#
#	   CHANGES: - Use data.table to open awkward CSVs
#             - 
#             - 
#
#   REFERENCES: Frisch, K. von (1967).
#               The dance language and orientation of bees. 
#               Harvard University Press, Cambridge, MA, US.
#
#    EXAMPLES:  Fill out user input (lines 70-75), then press ctrl+shift+s to run
#
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data   +
#- Summary plot    +
#- Calculate proportions  +
#- Plot by stimulus
#- Save results  
#- Estimate effect

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





# User input --------------------------------------------------------------
csv_sep = ','#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
point_col = 'darkblue' #colour for plot points
speedup_data.table = TRUE #some difficulty reading Excel csvs without data.table


# Load packages -----------------------------------------------------------
if(speedup_data.table){library(data.table, quietly = TRUE)}

# . Select data -------------------------------------------------------------
path_file = DA_select_file()#expects a ".csv" file

# Read in data ------------------------------------------------------------
if(speedup_data.table)
{
  adata = data.table::fread(file = path_file,#read from user-selected file
                   header = T,#read the file header to use for variable names
                   sep = csv_sep#,#values are separated by the user-specified character
                   #other parameters can be added here for troubleshooting
                  )
}else
{
  adata = read.table(file = path_file,#read from user-selected file
                   header = T,#read the file header to use for variable names
                   sep = csv_sep#,#values are separated by the user-specified character
                   #other parameters can be added here for troubleshooting
                  )
}
#Excel makes empty rows, trim them
adata = subset(x = adata, 
               subset = !(is.na(angle)) # angle is an empty number, i.e. no data
               )

# Summarise categories -----------------------------------------------------

# . Derive variables ------------------------------------------------------
u_stim = with(adata, unique(stimulus)) # unique stimulus categories
tru_stim = u_stim[!is.na(u_stim)] # NAs are ignored
adata = within(adata, #derive variables within dataset
               {
               weird_dances[weird_dances %in% ""] = 'normal' # all dances without comment are normal
               dance_cat = factor(x = weird_dances,# make them an unordered factor
                                  ordered = FALSE
                                  ) 
               dance_cat = relevel(x = dance_cat, # convert to ordered factor
                                   ref = 'normal' #with normal dances as the reference
                                  ) 
               # reorder the levels of stimlus
               stim_cat = factor(x = stimulus, #N.B. does not account for stimuli 
                                 levels = c( #other than the 13 specified here
                                             NA,        
                                             "dark",    
                                             "p0u0",
                                             "p0u1",
                                             "p0.5u1",
                                             "p0.75u1",
                                             "p1u1",
                                             "p5u5", 
                                             "p1.5u1",
                                             "p1u0",
                                             "p1.5u0",
                                             "p2.5u0",
                                             "p5u0",  
                                             "p7u0"
                                            )
                                 )
               }
               )
#Double-check that all non-NA stimuli are accounted for
if(
  any(
    with(adata, !(tru_stim %in% levels(stim_cat))) # stop if any are missing
    )
  )
{ #check which ones are missing
  forgotten_stim = with(adata, 
                        tru_stim[
                          !(tru_stim %in% levels(stim_cat))
                            ] # just the missing ones
                        )
  #inform the user
  stop('Stimulus levels\n', paste(forgotten_stim, '\n'), 'are missing')
}

# . Set up plot variables -------------------------------------------------
#Colours for the dance categories
cll = hcl.colors(n = length(levels(adata$dance_cat)),
                 palette = 'dark3'
                 )

# Plot numbers of weird dances --------------------------------------------
#Check how many of type of dance how many waggle runs
cat_agg = aggregate(formula = angle ~ dance_cat,
                    data = adata,
                    FUN = length)
#rename angle to number
cat_agg = within(cat_agg, {number = angle; rm(angle)})
#sort by number observed
cat_agg = cat_agg[rev(order(cat_agg$number)),]


# . Open bar plot ---------------------------------------------------------

with(cat_agg,
     {
      barplot(height = number ,
              names.arg = dance_cat,
              las = 3,
              col = cll,
              xlab = '',
              ylab = 'Number of waggle runs',
              main = paste0('All waggle runs\n', basename(path_file))
              )
     }
)


# Plot relative numbers for each stimulus type ----------------------------
#a function to count numbers of normal and weird dances
WeirdProp = function(x)
  {
  c(prop = 1 - mean(x == 'normal', na.rm = T), # proportion of weird waggle runs
    n_normal = sum(x == 'normal', na.rm = T), # number of normal waggle runs
    n_weird = sum(x != 'normal', na.rm = T), # number of weird waggle runs
    n_total = sum(!is.na(x)) # number of waggle runs
    )
}
#Calculate for each stimulus
stim_agg = aggregate(formula = dance_cat ~ stim_cat,
                     data = adata,
                     FUN = WeirdProp
                     )
#Check number of non-NA stimulus types
nlvl = sum(!is.na(levels(adata$stim_cat)))
#Set up the spacing to use for labels  
stim_seq = BarPlotSpacing(n = nlvl)
#Set up labels
QuoteFracDcat = function(i)
                  { 
                    bquote(
                      frac(.(stim_agg[i,]$dance_cat[,'n_weird']),
                           .(stim_agg[i,]$dance_cat[,'n_total'])
                      )
                    )
                  }
#list of labels the same length as the number of stimuli
lab_seq = lapply(X = 1:dim(stim_agg)[1],
                 FUN = QuoteFracDcat
                 )
#Set up colours for each stimulus type
stim_cll = hcl.colors(n = nlvl,
                      palette = 'dark3'
                      )
p_average = WeirdProp(adata$weird_dances)['prop']

# . Open bar plot ---------------------------------------------------------
barplot(formula = dance_cat[,'prop'] ~ stim_cat,
                data = stim_agg,
                las = 3,
                col = hcl.colors(n = nlvl,
                                 palette = 'dark3'
                                 ),
                xlab = '',
                ylab = 'Proportion of weird dances',
                ylim = c(0,1),
                main = paste0('By stimulus\n', basename(path_file))
                )
# add reference lines
for( ii in 1:nlvl)
{
  segments( x0 = stim_seq[ii] - 0.3,
            x1 = stim_seq[ii] + 0.3,
            y0 = qbinom(p = 0.025, #2.5%ile
                        size = stim_agg$dance_cat[ii,'n_total'],
                        prob = p_average
                        )/stim_agg$dance_cat[ii,'n_total'],
            col = 'gray',
            lty = 3
          )
  segments( x0 = stim_seq[ii] - 0.5,
            x1 = stim_seq[ii] + 0.5,
            y0 = p_average, #population average
            col = 'orange',
            lty = 3
          )
  segments( x0 = stim_seq[ii] - 0.3,
            x1 = stim_seq[ii] + 0.3,
            y0 = qbinom(p = 1 - 0.025, #97.5%ile
                        size = stim_agg$dance_cat[ii,'n_total'],
                        prob = p_average
                        )/stim_agg$dance_cat[ii,'n_total'],
            col = 'darkred',
            lty = 3
          )
}
#add labels
with(stim_agg,
     {
       invisible(
         {
           lapply(X = 1:(length(stim_seq)),
                  FUN = function(i)
                  { # interpret each label as an "expression"
                    text(x = stim_seq[i],
                         y = dance_cat[i,'prop']+0.20,#0.07 
                         labels = as.expression(lab_seq[i]),
                         cex = 0.5,
                         col = stim_cll[i]
                        )
                  }
                 )
         }
       )
     }
)
legend(x = 'topright',
       legend = c('average occurence',
                  'predicted 97.5%ile',
                  'predicted 2.5%ile'),
       col = c('orange',
               'darkred',
               'gray'),
       lty = 3,
       cex = 0.7,
       bty = 'n'
       )

# Plot for each bee and stimulus ------------------------------------------
#summarise dance category for each individual and stimulus
cat_stim_agg = aggregate( dance_cat ~ bee*stim_cat,
                          data = adata,
                          FUN = summary
                        )
# add grey as the reference colour to the vector of category colours
cat_cll = c('gray',
            hcl.colors(n = length(levels(adata$dance_cat))-1,
                       palette = 'dark3'
                      )
            )

# . Set up plot area and plot ---------------------------------------------
par(mar = c(5,5,3,0))
#make bar plot
with(cat_stim_agg,
     {
      barplot(height = t(dance_cat), # rotate to make stacked bars
              names.arg = paste(stim_cat,bee),
              xlim = c(0,length(bee))*1.4, # wider area to include legend
              las = 3,
              col = cat_cll,
              xlab = '',
              ylab = 'Number of waggle runs',
              main = paste0('By bee\n', basename(path_file)),
              cex.names = 0.3
      )
     }
)
# add legend
legend(x = 'topright',
       inset = c(0,0),
       legend = rev(levels(adata$dance_cat)),
       col = rev(cat_cll),
       pch = 15,
       cex = 0.6
       )
