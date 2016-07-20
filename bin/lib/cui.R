#--------------------------------------------------------
#  FUNCTIONS FOR CUI
#  By Losses Don @ 2016-07-07
#--------------------------------------------------------

cls <- function(){
  if (getIdentification() == 'R Console')
    cat('\f')                         # R Console
  else{
    if (Sys.info()[['sysname']] == 'windows')
      system('powershell clear-host') # Windows
    else
      system('clear')                 # *Unix
  }
}

yn <- function(hint){
  status <- readline(hint)
  if (!status %in% c(0, 1, 'q')) yn(hint)
  if (status == 'q') stop('You choose to quit.')
  as.numeric(status)
}

get_file_hint <- function(file = readline()){
  if (file.exists(sprintf('usr/list/%s', file))) file
  else get_file()
}

list_data_file <- function(){
  csv_list <- list.files('usr/list')
  csv_list <- csv_list[grep('\\s*.csv', csv_list)]
  csv_list[csv_list != 'temp.csv']
}

get_file <- function(){
  csv_list <- list_data_file()
  cat('Choose a word list from following files: \n')
  cat(paste(csv_list, collapse = '\t'))
  cat('\n')
  get_file_hint()
}
