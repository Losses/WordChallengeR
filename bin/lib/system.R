#--------------------------------------------------------
#  FUNCTION FOR SYSTEM ACTION
#  By Losses Don @ 2016-08-07
#--------------------------------------------------------

explorer <- function(dir = ''){
  if(.Platform$OS.type != 'windows') return(F)
  
  command <- (paste0('explorer ', getwd(), '/', dir) %>%
                strsplit('/'))[[1]] %>% paste(collapse = '\\')
  
  tryCatch(shell(command, intern=TRUE), warning = function(e){})
}
