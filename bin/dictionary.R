FILE_NAME <- ifelse(is.na(LAST_COMMAND[2]), get_file(), LAST_COMMAND[2])
PROFILE <- ifelse(is.na(LAST_COMMAND[3]), 'default.csv', LAST_COMMAND[3])
SESSION <- as.numeric(Sys.time())

new_dictionary_list <- function(list.file = FILE_NAME){
  words <- read.list(list.file)
  unique <- unique(flatten_list(words))
  
  list(
    words = words,
    unique = unique,
    order = sort(unique),
    shuffle = sample(unique)
  )
}

fill_template <- function(x, template){
  sprintf(paste(readLines(template),collapse = ''), x)
}

unit_header <- function(x, html){
  if(html) return(html_tag(x, 'h2'))
  else return(paste(x, 
                    paste(rep('=', length(x)), collapse = ''),
                    sep = '\n')
              )
}

split_char <- function(x){
  strsplit(x, split = '')[[1]]
}

switch_list <- function(x, order){
  switch(order,
         order = x$order,
         shuffle = x$shuffle,
         x$unique)
}

generate_dictionary <- function(x, order, html = T){
  summary_str <- '%s words in total, %s vocabulary not within the scope of examination, %s key words.'
  words_list <- switch_list(x, order)
  
  words_dictionary <- ''
  
  for (.i in words_list){
    entry <- read.word(.i)
    
    if(html){
      words_dictionary <- paste(
        words_dictionary,
        as.dictionary(entry, html = T),
        collapse = ''
      )
    } else {
      words_dictionary <- pasteLines(
        words_dictionary,
        as.dictionary(entry, html = T)
      )
    }
  }
  
  words_dictionary <- pasteLines(
    words_dictionary,
    sprintf(summary_str,
            length(x$unique),
            sum(sapply(x$unique,need_italic)),
            sum(sapply(x$unique,need_bold)))
  )
  
  words_dictionary
}

generate_list_by_unit <- function(x, order, html = T){
  result <- ''
  warp <- ifelse(html, '<div class="unit">%s</div>', '%s')
  if (order == 'original') {
    words <- x$words
  } else if (order == 'order') {
    words <- list()
    for (.i in x$order) {
      .group <- split_char(.i)[1] %>% toupper
      words[[.group]] <- c(words[[.group]], .i)
    }
  } else return(F)
  
  for (.class in names(x$words)) {
    .class_original <- ''
    for (.word in x$words[[.class]]) {
      .class_original <- pasteLines(
        .class_original,
        .word, html = html
      )
    }

    .class_original <- unit_header(.class, html = html) %>%
      paste(. , .class_original) %>%
      sprintf(warp, .)
    
    result <- paste(
      c(result, .class_original),
      collapse = ifelse(html, '', '\n')
    )
  }
  
  result
}

generate_simple_list <- function(x, order, html = T){
  words_list <- switch_list(x, order)
  
  result <- ''
  
  for(.i in words_list){
    result <- pasteLines(result, .i, html = html)
  }
  
  length(words_list) %>% sprintf('%s words in total', .) %>%
    pasteLines(result, ., html = html)
}

generate_list <- function(x, order, style, html = T){
  if (style == 'unit' && order != 'shuffle')
    return(generate_list_by_unit(x, order, html))
  else
    return(generate_simple_list(x, order, html))
}

content_warpper <- function(x, profile){
  profile <- as.list(profile)
  if (profile$type == 'list')
    content <- generate_list(x, order = profile$order, style = profile$style)
  else if (profile$type == 'dictionary')
    content <- generate_dictionary(x, order = profile$order)
  else content <- ''
  
  sprintf('<div data-list-id="%s" class="%s">%s</div>',
          profile$id, profile$type, content)
}

generate_content <- function(x, profile){
  apply(profile, 1, content_warpper, x = x) %>% paste(collapse = '')
}

generate_list_list_element <- function(profile){
  profile <- as.list(profile)
  sprintf('<li data-to-list="%s">%s</li>', profile$id, profile$name)
}

generate_list_list <- function(profile){
  apply(profile, 1, generate_list_list_element) %>% paste(collapse = '')
}

generate_application <- function(list.file, profile.file = 'default.csv'){
  profile <- read.profile(profile.file)
  dict_list <- new_dictionary_list(list.file = list.file)
  
  dict_content <- generate_content(dict_list, profile)
  list_content <- generate_list_list(profile)
  
  readLines('etc/template/webApplication.html') %>% paste(collapse = '\n') %>%
    sub_utf(pattern = '\\{\\{DICT_CONTENT\\}\\}', replacement = dict_content, .) %>%
    sub_utf(pattern = '\\{\\{LIST_CONTENT\\}\\}', replacement = list_content, .)
}

o_name <- function(x, dir.name = SESSION){
  dir_name <- sprintf('usr/output/%s', dir.name)
  if (!dir.exists(dir_name)) dir.create(dir_name)
  sprintf('usr/output/%s/%s', dir.name, x)
}

write.application <- function(list.file, profile.file = 'default.csv', dir.name = SESSION){
  generate_application(list.file, profile.file) %>%
    writeLines(o_name('application.html'), useBytes = T)
}

write.application(FILE_NAME, PROFILE)
