if_command <- function(n, default){
  ifelse(is.na(LAST_COMMAND[n]), default, LAST_COMMAND[n])
}

command_line <- function(){
  command_list <- list(
    dictionary = 'bin/dictionary.R',
    quiz = 'bin/quiz.R',
    summary = 'bin/summary.R',
    get = 'bin/get.R',
    new = 'bin/new.R'
  )
  
  command <- readline('$WC > ' %>% bold %>% green)
  program <- strsplit(command, ' ')[[1]][1]
  if (command == 'debug') 
    rep('\b', 100) %>% paste(collapse = '') %>%
    sprintf('%s Entering debug mode...\n', .) %>% stop()
  if (command == 'reset') reset_env()
  if (command == 'q') q()
  
  LAST_COMMAND <<- strsplit(command, ' ')[[1]]
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
  
  command_line()
}