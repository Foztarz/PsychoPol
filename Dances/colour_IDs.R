#make a vector of possible marking colours
mark_cols = c(
              'red',
              'green',
              'blue',
              'pink',
              'yellow',
              'brown',
              'orange',
              'teal'
            )
#decide on number per forager (2 recommended!)
marks_per_id = 2
#find all possible combinations
ids = combn(
            x = mark_cols,
            m = marks_per_id
            )
#rotate to vertical arrangement
ids = t(ids)
#name the columns
colnames(ids) = c('colour_1', 'colour_2')
#Display as spreadsheet
View(ids)
#Write out as a spreadsheet
write.csv(x = ids,
          file = file.path(gsub('\\\\',#on windows remove any incorrect slashes 
                                '/', #replace them with the ones the system actually uses
                                Sys.getenv('USERPROFILE')#find user default directory
                                ),
                           'Documents',#save in user's documents folder
                           paste0('Colour IDs','.csv')#file name plus extension
                          )
)
