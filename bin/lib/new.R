#--------------------------------------------------------
#  FUNCTIONS FOR CREATING WORD LIST
#  By Losses Don @ 2016-07-08
#--------------------------------------------------------

check_unit_title <- function(x) {
  class_reg <- '^[1-9A-Za-z]+$'
  na.omit(x) %>% grep(class_reg, .) %>% length != 0
}

suggest_list <- function(x){
  paste(1:length(x), '. ', x, sep = '', collapse = '\t')
}

require_suggest <- function(misspell, suggestion, choose_suggestion = readline()){
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

new_list_cli <- function() {
  class_reg <- '^>([\\s\\S])*'
  result <- list()
  current_unit <- F
  repeat {
    input <- readline('$new > ' %>% bold %>% magenta)
    if (grep(class_reg, input) %>% length > 0) {
      unit_str <- gsub(class_reg, '\\1', input)
      
      if (!check_unit_title(unit_str)) {
        cat('Invalid unit name.')
        next
      } else {
        current_unit <- unit_str
      }
    } else if (input %in% c('!E', '!e', '!Q', '!q')) {
      break
    } else {
      if (current_unit == F) {
        cat('You must assign a unit name at first.')
        next
      } else {
        if (!hunspell_check(input)) {
          suggestion <- hunspell_suggest(input)
          word <- suggest_cli(input, suggestion)
        } else {
          word <- input
        }
        result[[current_unit]] <- c(result[[current_unit]], word)
      }
    }
  }
  
  result
}

new_list <- function(file.name) {
  file_name <- sprintf('usr/list/%s.csv', file.name)
  cat('\n')
  new_list_cli() %>% format_list %>% write.csv(., file = file_name, quote = F, row.names = F)
}