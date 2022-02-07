# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 02 07
#     MODIFIED:	James Foster              DATE: 2022 02 07
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
#	   CHANGES: - 
#             - 
#             - 
#
#   REFERENCES: Batschelet E (1981).
#               Graphical presentation, Chap 1.2, p. 4-6
#               Chapter 1: Measures of Location
#               In: Circular Statistics in Biology
#               Academic Press (London)
#
#    EXAMPLES:  Fill out user input (lines 80-86), then press ctrl+shift+s to run
#
# 
#TODO   ---------------------------------------------
#TODO   
#- Read in data   
#- Summary plot    
#- Calculate proportions
#- Plot by stimulus
#- Save results  
#- Estimate effect

# Find relevation functions -----------------------------------------------

fun_file = "DanceAnalysis_Functions.R"
fun_path = tryCatch(expr = #Search in the folder containing this script
                      {file.path(dirname(sys.frame(1)$ofile), fun_file)},
                    error = function(e){fun_file}
)
if(!file.exists(fun_path))#If not foud, ask the user
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


# User input --------------------------------------------------------------
csv_sep = ','#Is the csv comma separated or semicolon separated? For tab sep, use "\t"
point_col = 'darkblue' #colour for plot points

# . Select data -------------------------------------------------------------
path_file = DA_select_file()#expects a ".csv" file

# Read in data ------------------------------------------------------------
adata = read.table(file = path_file,#read from user-selected file
                   header = T,#read the file header to use for variable names
                   sep = csv_sep#,#values are separated by the user-specified character
                   #other parameters can be added here for troubleshooting
                  )

# Summarise categories -----------------------------------------------------
adata = within(adata,
               {
               weird_dances[weird_dances %in% ""] = 'normal'
               dance_cat = factor(x = weird_dances,
                                  ordered = FALSE
                                  ) 
               dance_cat = relevel(x = dance_cat,
                                   ref = 'normal' 
                                  ) 
               stim_cat = factor(x = stimulus,
                                 levels = c('dark',
                                            'p0u1',
                                            'p1u1', 
                                            'p1u0', 
                                            'p2.5u0', 
                                            'p5u0', 
                                            'p7u0' 
                                            )
                                 )
               }
               )
cll = hcl.colors(n = length(levels(adata$dance_cat)),
                 palette = 'dark3'
                 )

cat_agg = aggregate(formula = angle ~ dance_cat,
                    data = adata,
                    FUN = length)
cat_agg = within(cat_agg, {number = angle; rm(angle)})
cat_agg = cat_agg[rev(order(cat_agg$number)),]
with(cat_agg,
     {
      barplot(height = number ,
              names.arg = dance_cat,
              las = 3,
              col = cll,
              xlab = '',
              main = paste0('All dances\n', basename(path_file))
              )
     }
)
# barplot(formula = number ~ dance_cat,
#         data = cat_agg,
#         las = 3,
#         col = cll,
#         xlab = '',
#         main = paste0('All dances\n', basename(path_file))
#         )

WeirdProp = function(x){1 - mean(x == 'normal')}
stim_agg = aggregate(formula = dance_cat ~ stim_cat,
                     data = adata,
                     FUN = WeirdProp)

barplot(formula = dance_cat ~ stim_cat,
                data = stim_agg,
                las = 3,
                col = hcl.colors(n = length(levels(adata$stim_cat)),
                                 palette = 'dark3'
                                 ),
                xlab = '',
                ylab = 'Proportion of wierd dances',
                ylim = c(0,1),
                main = paste0('By stimulus\n', basename(path_file))
                )

cat_stim_agg = aggregate( dance_cat ~ bee*stim_cat,
                          data = adata,
                          FUN = summary
                        )
cat_cll = c('gray',
            hcl.colors(n = length(levels(adata$dance_cat))-1,
                       palette = 'dark3'
                      )
            )
with(cat_stim_agg,
     {
      barplot(height = t(dance_cat),
              names.arg = paste(stim_cat,bee),
              las = 3,
              col = cat_cll,
              xlab = '',
              ylab = 'Number of waggle runs',
              main = paste0('By bee\n', basename(path_file)),
              cex.names = 0.5
      )
     }
)
legend(x = 'topright',
       legend = rev(levels(adata$dance_cat)),
       col = rev(cat_cll),
       pch = 15,
       cex = 0.5
       )
