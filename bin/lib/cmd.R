if_command <- function(n, default, file_complete, extention = 'csv'){
  if(is.na(LAST_COMMAND[n])) return(default)

  ifelse(file_complete, auto_file_name(LAST_COMMAND[n], extention), LAST_COMMAND[n])
}

command_line <- function(){
  command_list <- list(
    dictionary = 'bin/dictionary.R',
    quiz = 'bin/quiz.R',
    summary = 'bin/summary.R',
    get = 'bin/get.R',
    new = 'bin/new.R',
    clear = 'bin/clear.R'
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
    command_line()
  } else {
    run.script(command_list[[program]], then = command_line, ignore.warning = T)
  }
  
  remove.files(DICTIONARY_NEED_DELETE)
  DICTIONARY_NEED_DELETE <- character(0)
}

wcR <- function(){
  cls()
  cat('\n  Word Challange System  \n=========================\n\nAuthor: Losses Don\nVersion: 1.0\n')
  cat(sprintf('R Version: %s.%s\n', version$major, version$minor))
  cat('EC Dictionary API: Youdao Dict\nEE Dictionary API: Merriam-Webster\n\n')
  cat('This program is free software: you can redistribute it and/or modify it under the terms of the LGPLv3.\n\n')
  
  command_line()
}