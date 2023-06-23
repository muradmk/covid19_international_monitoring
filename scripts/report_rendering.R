rmarkdown::render('international_monitoring.Rmd',
                  output_file = paste0('international_monitoring_', format(Sys.Date(), "%d-%b-%Y"), 
                                      '.html', sep=''),
                  output_dir = "outputs/")
