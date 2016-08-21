#--------------------------------------------------------
#  FUNCTIONS FOR CREATING WORD LIST
#  By Losses Don @ 2016-07-08
#--------------------------------------------------------

check_unit_title <- function(x) {
  class_reg <- '^[1-9A-Za-z]+$'
  na.omit(x) %>% grep(class_reg, .) %>% length != 0
}

invalid_msg <- function(x, auto.cat = T){
  if(auto.cat){
    cat(x)
    cat('\n')
  }
  
  list(
    valid  = F,
    msg = x
  )
}

suggest_list <- function(x){
  paste(1:length(x), '. ', x, sep = '', collapse = '\t')
}

require_suggest <- function(misspell, suggestion, choose_suggestion = readline()){
  if (choose_suggestion == '!') return(NA)
  
  choose_suggestion <- as.numeric(choose_suggestion)
  
  if (is.na(choose_suggestion) || choose_suggestion != ceiling(choose_suggestion)) {
    cat('Invalid input!\n')
    require_suggest(misspell, suggestion)
  } else if (choose_suggestion == 0) {
    return(misspell)
  } else if (choose_suggestion > length(suggestion)) {
      cat('The # you input is too large.\n')
      require_suggest(misspell, suggestion)
  } else {
      return(suggestion[choose_suggestion])
  }
}

suggest_cli <- function(misspell, suggestion, n = 3) {
  suggest_hint <- 'Misspell, here are some suggestions:\n%s\n0.%s[old]\t!.[cancel]\t\nChoose the correct word:'
  if (n > length(suggestion[[1]])) n <- length(suggestion[[1]])
  suggestion_limit <- suggestion[[1]] %>% head(n)
  suggestion_limit %>% suggest_list %>% sprintf(suggest_hint, ., misspell) %>% cat
  require_suggest(misspell, suggestion_limit)
}

format_list <- function(x){
  list_length <- lapply(x,length) %>% as.numeric
  blank_count <- max(list_length) - list_length
  for (.i in 1:length(list_length)) {
    x[[.i]] <- c(x[[.i]], rep('', blank_count[.i]))
  }
  do.call(rbind, x) %>% t
}

is.edit_command <- function(x){
  strsplit(x, ' ')[[1]][1] %in% c(':e', ':E')
}

read.edit_command <- function(x, result){
  command <- strsplit(x, ':e ')[[1]][-1] %>% 
    strsplit(',') %>% .[[1]] %>% remove_side_blank

  position <- list(col = NULL, row = NULL)
  
  for(.i in 1:2){
    num_str <- str_to_numeric(command[.i])
    if(num_str != F) position[[.i]] <- num_str
    else position[[.i]] <- command[.i]
  }
  
  if(!is.numeric(position$row))
    return(invalid_msg('Word number must be a number.'))
  
  if (is.character(position$col) && !command %in% names(list))
    return(invalid_msg('Unit name does not exists.'))
  
  if (is.numeric(position$col) %% position$col > names(list) %>% length)
    return(invalid_msg('Unit number out of bound.')) 
  
  if(position$row > result[[position$col]] %>% length)
    return(invalid_msg('Word number out of bound.')) 
  
  list(
    valid = T,
    action = 'edit',
    col = position$col,
    row = position$row,
    word = command[3]
  )
}

is.switch_command <- function(x){
  col_reg <- '^>([\\s\\S])*'
  
  if (grep(col_reg, x) %>% length > 0)
    gsub(col_reg, '\\1', x) 
  else
    F
}

read.switch_command <- function(x, result){
  col_str <- is.switch_command(x)
  if(is.numeric(x)){
    if(x[1] > names(result) %>% length) 
      return(invalid_msg('Unit number out of bound'))
  } else {
    if(!check_unit_title(col_str)) 
      return(invalid_msg('Invalid unit name'))
  }
  
  list(
    valid = T,
    action = 'switch',
    col = col_str
  )
}

new_list_promot_route <- function(x, result){
  if(is.switch_command(x) != F) return(read.switch_command(x, result))
  if(is.edit_command(x)) return(read.edit_command(x, result))
  if(tolower(x) == ':q') return(list(action = 'quit'))
  if(tolower(x) == ':fq') return(list(action = 'forceQuit'))
  if(tolower(x) == ':v') return(list(action = 'view'))
  else return(list(action = 'add', word = x))
}

new_list_get_position <- function(result, col_name = NULL, col_no = NULL, row_no = NULL){
  if (is.null(col_name) && is.null(col_no)) col_no <- names(result) %>% length
  if (!is.null(col_name)) col_no <- col_name

  
  if (is.null(row_no)) row_no <- ifelse((names(result) %>% length) != 0, length(result[[col_no]]) + 1, 0)
  
  list(
    col = col_no,
    row = row_no
  )
}

new_list_check_word <- function(x){
  if(is.pharse(x)) return (x)
  if (!hunspell_check(x)) {
    suggestion <- hunspell_suggest(x)
    word <- suggest_cli(x, suggestion)
  } else {
    word <- x
  }
  
  word
}

new_list_promot <- function(result = list(), current_unit = NULL, file.name = NULL) {
  repeat {
    promot_position <- new_list_get_position(result)
    
    input <- ifelse(!is.null(current_unit), current_unit, '?') %>% 
      sprintf('$new/%s (%s,%s) > ', ., promot_position$col, promot_position$row) %>%
      magenta %>% readline
    
    i <- new_list_promot_route(input, result)
    
    if(!is.null(i$valid) && i$valid == F) next
    
    if(i$action == 'add'){
      if (is.null(current_unit)) {
        cat('You must assign a unit name at first.\n')
        next
      }
      
      word <- new_list_check_word(i$word)
      
      if(is.na(word) || is.null(word) || word == '') next
      
      add_position <- new_list_get_position(result, col_name = current_unit)
      result[[add_position$col]][add_position$row] <- word
    }
    
    if(i$action == 'edit' && i$valid == T){
      word <- new_list_check_word(i$word)
      result[[i$col]][i$row] <- word
    }
    
    if(i$action == 'switch' && i$valid == T){
      if(is.null(result[[i$col]])) result[[i$col]] <- character(0)
      current_unit <- i$col
    }
    
    if(i$action == 'view')
      print(result, quote = F)
    
    if(i$action == 'forceQuit') {
      return(list(action = 'forceQuit'))
      break
    }
    
    if(i$action == 'quit') break
    
    save(result, current_unit, file.name, file = 'tmp/new.RData')
  }
  
  new_list_remove_tmp_file()
  result
}

continue_list <- function(tmpfile.name = 'tmp/new.RData') {
  if(file.exists(tmpfile.name)){
    load(tmpfile.name)
    start_list_editor(result, current_unit, file.name)
  } else F
}

new_list_remove_tmp_file <- function(){
  new_status('Removing temporary file')
  unlink('tmp/new.RData')
  status.done()
}

start_list_editor <- function(result = list(), current_unit = NULL, file.name = NULL, force = F){
  tmpfile.name <- 'tmp/new.RData'
  
  if(is.null(file.name) && file.exists(tmpfile.name)){
    if(yn('A uncompleted list found, continue edit it?(0 = no, 1 = yes)')){
      load(tmpfile.name)
      return(start_list_editor(result, current_unit, file.name))
    } else {
      if(!yn('Keep the uncompleted list file?(0 = REMOVE, 1 = keep)')){
        new_list_remove_tmp_file()
      }
    }
  }
  
  if(is.null(file.name)) file.name <- readline('List file name:')
  file_name <- sprintf('usr/list/%s.csv', file.name)
  if (file.exists(file_name) && !force) 
    stop('File already exist!')
  
  list_data <- new_list_promot(result, current_unit, file.name)
  
  if(!is.null(list_data$action) && list_data$action == 'forceQuit') 
    green('You have quited the editor safely.\n') %>% cat
  else {
    new_status('Writing your list')
    list_data %>% format_list %>% write.csv(., file = file_name, quote = F, row.names = F)
    status.done()
  }
}
