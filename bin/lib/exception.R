#--------------------------------------------------------
#  FUNCTIONS FOR EXCEPTION HANDLEING
#  By Losses Don @ 2016-07-17
#--------------------------------------------------------

cat_exception <- function(x, len = 48){
  if (x == '') x <- ' '
  x <- strsplit_by_len(x)
  
  for (.i in x) {
    llen <- strlength(.i)
    lblen <- 48 - llen - 2
    lb <- ifelse(lblen <= 0,
                 '',
                 rep(' ', lblen) %>% paste(collapse = ''))
    '| %s%s |\n' %>% sprintf(., .i, lb) %>% cat
  }
}

read.exception <- function(e, type, fn = NULL){
  if (is.null(fn)) {
    '+------------------------------------------------+\n' %>% cat
    cat_exception(type)
    '|------------------------------------------------|\n' %>% cat
    cat_exception('MESSAGE: ')
    cat_exception(e$message)
    cat_exception('')
    cat_exception('CALL: ')
    cat_exception(deparse(e$call))
    '+------------------------------------------------+\n' %>% cat
  }
  else fn(e)
}

run.script <- function(file, error = NULL, warning = NULL, then = NULL, ignore.warning = F){
  if (!file.exists(file))
    stop(sprintf('Script file "%s" does not exists!', file))
  
  for (.i in c('error', 'warning')) {
    .target <- eval(parse(text = .i))
    if (!is.null(.target) && !is.function(.target))
      stop(sprintf('%s isnt a function!', .i))
  }
  
  if (ignore.warning)
    tryCatch(eval(parse(file, encoding = 'UTF-8')),
             error = function(e) read.exception(e, type = 'Error', fn = error)
    )
  else
    tryCatch(eval(parse(file, encoding = 'UTF-8')),
             error = function(e) read.exception(e, type = 'Error', fn = error),
             warning = function(e) read.exception(e, type = 'Warning', fn = warning)
    )
  
  if (is.function(then)) then()
}

exception <- function(x, force){
  if (!force) stop(x)
  # else warning(x)
}
