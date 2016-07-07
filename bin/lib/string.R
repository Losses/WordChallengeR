#--------------------------------------------------------
#  STRING PROCESSING FUNCTIONS 
#  By Losses Don @ 2016-07-07
#--------------------------------------------------------

pasteLines <- function(x, y = '', html = F){
  if (x == '') return(y)
  if (html) paste(x, y, sep = '</br>')
  else paste(x, y, sep = '\n')
}

remove_blank <- function(x){
  x <- as.character(x)
  x[x != '' & !is.na(x) & !is.null(x)]
}

html_tag <- function(content, tag){
  sprintf('<%s>%s</%s>', tag, content, tag)
}

sub_utf <- function(pattern, replacement, x){
  split <- strsplit(x, split = pattern)[[1]]
  c(split[1], replacement, split[2]) %>% paste(collapse = '')
}

na.omit.safe <- function(x){
  if (is.null(x)) return(NA)
  else return(na.omit(x))
}

is.valid.s <- function(x){
  !(is.null(x) || is.na(x))
}

is.valid <- function(x){
  if (length(x)[1] == 0) return(F)
  lapply(x, is.valid.s) %>% as.logical
}