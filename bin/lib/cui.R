#--------------------------------------------------------
#  FUNCTIONS FOR CUI
#  By Losses Don @ 2016-07-07
#--------------------------------------------------------

cls <- function(){
  if (getIdentification() == 'R Console')
    cat('\014')                       # R Console
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

new_status <- function(x){
  x_len <- strlength(x)
  terminal_width <- options('width')$width
  dots_count <- terminal_width - 2 - x_len - 14
  c(x,' ', rep('.', dots_count), ' ', '[ ', yellow('PROCESSING', ']')) %>%
    paste(., collapse = '') %>% cat
}

status.change <- function(){
  rep('\b', 14) %>%
    paste(., collapse = '') %>% cat
}

status.done <- function(){
  status.change()
  c('[    ', green('DONE', '   ]\n')) %>%
    paste(., collapse = '') %>% cat
}

status.fail <- function(){
  status.change()
  c('[   ', red('FAILED'), '   ]\n') %>%
    paste(., collapse = '') %>% cat
}
