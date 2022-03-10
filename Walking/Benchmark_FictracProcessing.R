library(knitr)
system.time(
  {
source(knitr::purl(
                  file.path(dirname(sys.frame(1)$ofile), 'FictracProcessing.Rmd'), 
                  output = tempfile()
                  )
       )
  }
)