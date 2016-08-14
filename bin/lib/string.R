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
  x <- x[1]
  if (length(x) == 0) return(F)
  lapply(x, is.valid.s) %>% as.logical
}

rand_string <- function(length = 10,
                        set = '1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
                        ){
  set_sep <- strsplit(set, split = '')[[1]]
  sample(set_sep, size = length) %>% as.character %>% paste(collapse = '')
}

check1Char <- function(x){
  if (!is.character(x)) stop('x must be a character')
  if (length(x) > 1) stop('the length of x must equal to 1')
}

strlength <- function(x){
  check1Char(x)
  str <- x %>% strsplit(x = .,split = '')
  length(str[[1]])
}

strsplit_by_len <- function(x, len = 46){
  check1Char(x)
  xlen <- strlength(x)
  split_point <- seq.int(1, xlen, len)
  
  if (split_point[length(split_point)] != xlen + 1)
    split_point <- c(split_point, xlen + 1)
  
  str <- (x %>% strsplit(x = .,split = ''))[[1]]
  .i <- 1
  result <- character(0)
  while (.i <= length(split_point) - 1) {
    l <- str[split_point[.i]:split_point[.i + 1] - 1]
    if (.i > 1) l <- l[-1]
    l <- paste(l, collapse = '')
    result <- c(result, l)
    .i <- .i + 1
  }
  
  result
}

str_to_numeric <- function(x){
  tryCatch(as.numeric(x), warning = function(e) F)
}