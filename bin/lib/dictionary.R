#--------------------------------------------------------
#  FUNCTIONS FOR DICTIONARY GENERATING
#  By Losses Don @ 2016-07-07
#--------------------------------------------------------

EC_API <- '1512553241'
EE_API <- '84ca89b8-2ca9-4851-9ede-d93d7b866f39'
DICTIONARY_NEED_DELETE <- character(0)

read.profile <- function(profile.file){
  sprintf('usr/profile/%s', profile.file) %>% read.csv
}

read.word <- function(word, lang = 'EC', generate_sentence = F, ...){
  if (word == '') return(NA)
  if (generate_sentence == T) return(read.word.ee(word, generate_sentence = T, ...))
  if (lang == 'EE') return(read.word.ee(word, ...))
  read.word.ec(word, ...)
}

#-----------------------
# EC Dictionary
#-----------------------

read.word.ec <- function(word, ...){
  entry <- parse.remote(
    url = sprintf('http://fanyi.youdao.com/openapi.do?keyfrom=WordChallengeR&key=%s&type=data&doctype=json&only=dict&version=1.1&q=%s', EC_API, word),
    file.loc = sprintf('etc/dictionary/ec/%s.json', word),
    hint = sprintf('Getting EC "%s"', word),
    type = 'JSON', ...
  )
  
  if (is.logical(entry) && entry == F) return(F)
  
  list(
    word = word,
    phonetic = entry$basic$`us-phonetic`,
    explains = entry$basic$explains[!1:length(entry$basic$explains) %in% grep('人名',entry$basic$explains)]
  )
}

#-----------------------
# EE Dictionary
#-----------------------

value_node <- function(node, tag){
  set <- getNodeSet(node, tag)
  if (length(set) == 0) return(NA)
  xmlValue(set[[1]])
}

get_def <- function(node){
  text <- xmlChildren(node)$text
  if (!is.null(text)) return(xmlValue(text))
  xmlValue(xmlChildren(node)$un)
}

clear_def <- function(text){
  gsub('^:', '', text) %>%
    gsub(':$', '', .) %>%
    gsub(' :', ', ', ., perl = T) %>%
    gsub(' $', '', ., perl = T)
}

filter_def <- function(text){
  if(is.valid(text)) text
  else ''
}

read.entry_node.ee <- function(node){
  entry <- list()
  entry$fl <- value_node(node, 'fl')
  entry$gram <- value_node(node, 'def/gram')
  entry$phonetic <- value_node(node, 'pr')
  
  entry$def <- getNodeSet(node, 'def/dt') %>% sapply(get_def) %>%
    na.omit  %>% filter_def %>% clear_def %>% paste(., collapse = '; ')
  entry$sentence <- getNodeSet(node, 'def/dt/vi') %>%
    lapply(. ,xmlValue) %>% as.character %>% filter_def
  
  entry
}

get_entry_def.ee <- function(x){
  sprintf('%s. %s', x$fl, x$def)
}

get_entry_sentence.ee <- function(x, n = 1){
  na.omit(x$sentence[1:n])
}

invalid.ec <- function(word){
  DICTIONARY_NEED_DELETE <- c(DICTIONARY_NEED_DELETE,
                              'etc/dictionary/ec/%s.xml' %>% sprintf(word)
  )
  read.word.ec(word)
}

read.word.ee <- function(word, generate_sentence = F, ...){
  if(is.pharse(word)) return(read.word.ec(word))
  
  data <- parse.remote(
    url = sprintf('http://www.dictionaryapi.com/api/v1/references/learners/xml/%s?key=%s', word, EE_API),
    file.loc = sprintf('etc/dictionary/ee/%s.xml', word),
    hint = sprintf('Getting EE "%s"', word),
    type = 'XML', ...
  )
  
  if (is.logical(data) && data == F) {
    invalid.ec(word)
    return(F)
  }
  
  if (getNodeSet(data, '//entry_list/suggestion') %>% length) return(invalid.ec(word))
  
  path <- sprintf('//entry_list/entry[contains(@id, "[") or @id="%s"]', word)
  entry_list <- getNodeSet(data, path)
  
  if (length(entry_list) == 0) {
    path <- '//entry_list/entry'
    entry_list <- getNodeSet(data, path)
    if (length(entry_list) == 0) return(invalid.ec(word))
  }
  
  query <- lapply(entry_list, read.entry_node.ee) %>% do.call(rbind, .)
  phonetic <- read.word.ec(word)$phonetic
  #phonetic <- na.omit.safe(query[,'phonetic']$phonetic)[1]
  sentence <- apply(query, 1, get_entry_sentence.ee)
  
  if (!generate_sentence) {
    list(
      word = word,
      phonetic = phonetic,
      explains = apply(query, 1, get_entry_def.ee),
      sentence = sentence
    )
  } else {
    list(
      word = word,
      phonetic = phonetic,
      explains = sentence
    )
  }
}

#####################################################
# Dictionary List Functions
#####################################################

read.list <- function(list.file, order = T){
  words <- read.csv(sprintf('usr/list/%s', list.file), header = T)
  words <- lapply(words, remove_blank)
  
  if(!order) return(words)
  
  unique <- unique(flatten_list(words))
  
  list(
    words = words,
    unique = unique,
    order = sort(unique),
    shuffle = sample(unique)
  )
}

flatten_list <- function(list){
  words_character <- c()
  for (.i in names(list))
    words_character <- c(words_character, list[[.i]])
  words_character <- words_character[words_character != '']
  
  words_character
}

as.dictionary <- function(entry, lib_mark = T, html = F, force = F){
  if (is.logical(entry) && entry == F) {
    if (!force) stop('Var entry is invalid!')
    else {
      if (html) return('<p style="color:red">\\(X_X)/<br></p>')
      else return('\\(X_X)/')
    }
  }
  
  words_dictionary <- ''
  words_dictionary <- pasteLines(
    words_dictionary,
    sprintf('%s%s',
            ifelse(need_italic(entry$word),
                   ifelse(lib_mark,
                          ifelse(html,
                                 html_tag(entry$word, 'i'),
                                 sprintf('△ %s', entry$word)),
                          entry$word),
                   ifelse(html & need_bold(entry$word),
                          html_tag(entry$word, 'b'),
                          entry$word)
            ), 
            ifelse(is.null(entry$phonetic) || is.na(entry$phonetic),
                   '',
                   sprintf(' /%s/', strsplit(entry$phonetic, ';')[[1]][1])
            )
    )
    , html = html
  )
  
  words_dictionary <- pasteLines(
    words_dictionary,
    paste(entry$explains, 
          collapse = ifelse(html, '<br>', '\n'))
    , html = html
  )
  if (html) words_dictionary <- sprintf('<div class="word">%s</div>', words_dictionary)
  words_dictionary
}

is.pharse <- function(x){
  length(grep(' ', x)) > 0
}

word_in_cache <- function(word){
  nrow(WORD_CACHE[WORD_CACHE$word == word,])>0
}

need_bold <- function(word){
  word_in_cache(word) && WORD_CACHE[WORD_CACHE$word == word,'freq'] > 1
}

need_italic <- function(word){
  word_in_cache(word) && !WORD_CACHE[WORD_CACHE$word == word,'in_lib']
}

build_word_cache <- function(){
  word_cache <- list()
  all_words <- c()
  data_files <- list_data_file()
  
  if (length(data_files) == 0) {
    return(
      data.frame(
        word = NA, freq = NA, in_lib = NA
      )[-1, ]
    )
  }
  
  for (.file in data_files)
    all_words <- c(all_words, read.list(.file, F))
  
  word_cache <- as.data.frame(table(flatten_list(all_words)))
  colnames(word_cache) <- c('word', 'freq')
  word_cache$in_lib <- sapply(word_cache$word, is.pharse) | word_cache$word %in% STANDARD_WORD_LIB
  word_cache$word <- as.character(word_cache$word)
  
  word_cache
}

#####################################################
# Test Paper Functions
#####################################################

entry_to_df <- function(entry){
  if(is.null(entry$explains)){
    result <- data.frame(
      word = entry$word,
      explain = entry$word
    )
  } else {
    result <- data.frame(
      word = entry$word,
      explain = entry$explains
    )
  }
  
  result
}

generate_test_unit <- function(x, html = T){
  if(html){
    pattern <- '<li class="test_question"><label class="question">%s</label><input pattern="%s" title="Wrong answer" /></li>'
    result <- sprintf(pattern, x[['explain']], x[['word']])
  } else {
    result <- sprintf('%s __________', x[['explain']])
  }
  
  result
}

generate_test <- function(x, lang = 'EC', order = 'shuffle', html = T, ...){
  df <- data.frame(
    word = character(0),
    explain = character(0)
  )
  
  for(.i in x$unique) {
    .df <- read.word(.i, lang = lang) %>% entry_to_df
    df <- rbind(df, .df)
  }
  
  if(order == 'shuffle') df <- df[nrow(df) %>% sample,]
  
  question_str <- apply(df, 1, generate_test_unit, html = html) %>% 
    paste(collapse = '\n') %>% sprintf('<ol class="test_body">%s</ol>', .)
  
  if(html){
    answer_str <- df[['word']] %>% sprintf('<li>%s</li>', .) %>%
      paste(collapse = '\n') %>% sprintf('<ol class="answer">%s</ol>', .)
  } else {
    answer_str <- df[['word']] %>% paste(collapse = '\n') %>%
      sprintf(' Answer\n========\n', .)
  }
  
  paste(question_str, answer_str, sep = '\n')
}

#####################################################
# Web Application Generating Functions
#####################################################

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

generate_dictionary <- function(x, order, lang = 'EC',
                                generate_sentence = F, html = T, ...){
  summary_str <- '%s words in total, %s vocabulary not within the scope of examination, %s key words.'
  words_list <- switch_list(x, order)
  
  words_dictionary <- ''
  
  for (.i in words_list) {
    entry <- read.word(.i, lang = lang, generate_sentence = generate_sentence, ...)
    
    if (html) {
      words_dictionary <- paste(
        words_dictionary,
        as.dictionary(entry, html = T, ...),
        collapse = ''
      )
    } else {
      words_dictionary <- pasteLines(
        words_dictionary,
        as.dictionary(entry, html = F, ...)
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

dictionary_content_warpper <- function(x, profile, ...){
  profile <- as.list(profile)
  sprintf('Generate %s: %s', profile$type, profile$name) %>% new_status
  if (profile$type == 'list')
    content <- generate_list(x, order = profile$order, style = profile$style)
  else if (profile$type == 'sentence')
    content <- generate_dictionary(x, order = profile$order, generate_sentence = T, ...)
  else if (profile$type == 'dictionary')
    content <- generate_dictionary(x, order = profile$order, lang = toupper(profile$lang), ...)
  else if (profile$type == 'test')
    content <- generate_test(x, order = profile$order, lang = toupper(profile$lang), ...)
  else content <- ''
  
  status.done()
  
  sprintf('<div data-list-id="%s" class="%s page">%s</div>',
          profile$id, profile$type, content)
}

generate_dictionary_content <- function(x, profile, ...){
  apply(profile, 1, dictionary_content_warpper, x = x, ...) %>% paste(collapse = '')
}

generate_list_list_element <- function(profile, title_page = F){
  profile <- as.list(profile)
  if (!title_page)
    sprintf('<li data-to-list="%s">%s</li>', profile$id, profile$name)
  else
    sprintf('<li data-to-list="%s"><h1>%s</h1><p>%s %s</p></li>',
            profile$id, profile$name, profile$order, profile$type)
}

generate_list_list <- function(profile, title_page = F){
  apply(profile, 1, generate_list_list_element, title_page = title_page) %>% 
    paste(collapse = '')
}

generate_application <- function(dict_list, profile, ...){
  dict_content <- tryCatch(generate_dictionary_content(dict_list, profile, ...),
                           error = function(e) list(error = T, msg = e))
  if (is.list(dict_content) && is.logical(dict_content$error) && dict_content$error) {
    dict_content$msg %>% sprintf('%s\n', .) %>% red %>% cat
    return()
  }
  
  list_content <- generate_list_list(profile)
  title_list_content <- generate_list_list(profile, T)
  
  readLines('etc/template/webApplication.html') %>% paste(collapse = '\n') %>%
    sub_utf('\\{\\{DICT_CONTENT\\}\\}', dict_content, .) %>%
    sub_utf('\\{\\{LIST_CONTENT\\}\\}', list_content, .) %>%
    sub_utf('\\{\\{TITLE_LIST_CONTENT\\}\\}', title_list_content, .)
}

write.application <- function(dict_list, profile,
                              dir.name = as.numeric(Sys.time()),
                              auto.browse = F, ...){
  if(is.null(profile) || nrow(profile) == 0) return(F)
  
  generate_application(dict_list, profile, ...) %>%
    writeLines(o_name('application.html', dir.name), useBytes = T)
  
  if(auto.browse) sprintf('file:///%s/usr/output/%s/application.html', getwd(), dir.name) %>% browseURL
}

#####################################################
# PDF Generating Function
#####################################################

generate_pdf_content <- function(dict_list, profile){
  list(
    size = as.character(profile[1, 'size']),
    dict_content = generate_dictionary_content(dict_list, profile)
  )
}

generate_single_pdf <- function(dict_content_obj, dir.name){
  sprintf('Writing PDF file %s.pdf', dict_content_obj$size) %>% new_status
  
  page_direction <- ifelse(dict_content_obj$size %in% c('B4', 'A3'), 'landscape', 'portrait')
  page_size_style <- paste0(dict_content_obj$size, ' ', page_direction)
  
  pdf_source <<- readLines('etc/template/PDF.html') %>% paste(collapse = '\n') %>%
    sub_utf('\\{\\{DICT_CONTENT\\}\\}', dict_content_obj$dict_content, .) %>%
    sub_utf('\\{\\{PAGE_SIZE\\}\\}', dict_content_obj$size, .) %>%
    sub_utf('\\{\\{PAGE_SIZE_STYLE\\}\\}', page_size_style, .)
  
  temp_file <<- tempfile(fileext = '.html')
  writeLines(pdf_source, temp_file, useBytes = T)
  
  pdf_file_name <- sprintf('%s.pdf', dict_content_obj$size) %>% o_name(dir.name)
  pdf_result <- sprintf('%s --page-size=%s --page-margin=0 %s -o %s --input=html5',
                        PDF_GENERATOR_CALL, dict_content_obj$size,
                        temp_file, pdf_file_name) %>% system

  if(pdf_result == 0) status.done() else status.fail()
}

write.pdf <- function(dict_list, profile,
                      dir.name = as.numeric(Sys.time())){
  
  page_sizes <- profile[['size']] %>% levels %>% as.character %>% .[. != '']
  
  if((page_sizes %>% .[! . %in% c('A4', 'A3', 'B5', 'B4', 'letter')] %>% length) != 0)
    stop('Page size must be A4, A3, B5, B4, letter.')
  
  profile_by_size <- list()
  
  for(.size in page_sizes)
    profile_by_size[[.size]] <- profile[profile[, 'size'] == .size, ]
  
  lapply(profile_by_size, generate_pdf_content, dict_list = dict_list) %>%
    lapply(generate_single_pdf, dir.name = dir.name)
}

#####################################################
# Dictionary Generating Function
#####################################################

o_name <- function(x, dir.name = as.numeric(Sys.time())){
  dir_name <- sprintf('usr/output/%s', dir.name)
  if (!dir.exists(dir_name)) dir.create(dir_name)
  sprintf('usr/output/%s/%s', dir.name, x)
}

new_dictionary <- function(list.file, 
                           profile.file = 'default.csv',
                           dir.name = as.numeric(Sys.time()), ...){
  
  dict_list <- read.list(list.file = list.file)
  profile <- read.profile(profile.file)
  
  profile_list <- list(
    html = profile[profile[['format']] == 'html', ],
    pdf = profile[profile[['format']] == 'pdf', ]
  )
  
  write.application(dict_list, profile_list$html, dir.name)
  write.pdf(dict_list, profile_list$pdf, dir.name)
  paste0('usr/output/', dir.name) %>% explorer
}

