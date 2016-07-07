#--------------------------------------------------------
#  FUNCTIONS FOR CUI
#  By Losses Don @ 2016-07-07
#--------------------------------------------------------

cls <- function(){
  cat('\014')
}

yn <- function(hint){
  status <- readline(hint)
  if (!status %in% c(0, 1, 'q')) yn(hint)
  if (status == 'q') stop('You choose to quit.')
  as.numeric(status)
}

get_file_hint <- function(file = readline()){
  if(file.exists(sprintf('usr/list/%s', file))) file
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


command_line <- function(command_list){
  command_list <- list(
    dictionary = 'bin/dictionary.R',
    quiz = 'bin/quiz.R',
    summary = 'bin/summary.R',
    get = 'bin/get.R'
  )
  
  command <- readline('$WC > ')
  LAST_COMMAND <<- strsplit(command, ' ')[[1]]
  program <- strsplit(command, ' ')[[1]][1]
  if (command == 'debug') return(F)
  if (command == 'q') q()
  if (!program %in% names(command_list)) {
    cat('Command not found!\n')
  } else {
    source(command_list[[program]], encoding = 'utf8')
  }
  command_line()
}

wcR <- function(){
  cls()
  cat('\n  Word Challange System  \n=========================\n\nAuthor: Losses Don\nVersion: 1.0\n')
  cat(sprintf('R Version: %s.%s\n', version$major, version$minor))
  cat('EC Dictionary API: Youdao Dict\nEE Dictionary API: Merriam-Webster\n\n')
  cat('This program is free software: you can redistribute it and/or modify it under the terms of the LGPLv3.\n\n')
  
  command_line(command_list)
}