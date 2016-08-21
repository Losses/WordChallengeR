if_command <- function(n, default, file_complete = F, extention = 'csv'){
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
    clear = 'bin/clear.R',
    explorer = 'bin/explorer.R'
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
  
  template <- readLines('etc/template/welcome.txt')
  rversion <- paste0(version$major, '.', version$minor)
  
  cat('\n')
  for(.i in 1:7){
    .this_line <- strsplit(template[.i], '@|@')[[1]]
    cat(cyan(.this_line[1]))
    
    if(.i == 4)
      sprintf(.this_line[3], WCR_VERSION) %>% cat
    else if (.i == 5)
      sprintf(.this_line[3], rversion) %>% cat
    else
      cat(.this_line[3])
    
    cat('\n')
  }
  
  for(.i in 8:length(template)){
    cat(template[.i])
    cat('\n')
  }
  
  command_line()
}
