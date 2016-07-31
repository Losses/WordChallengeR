#--------------------------------------------------------
#  FUNCTIONS FOR EXCEPTION HANDLEING
#  By Losses Don @ 2016-07-17
#--------------------------------------------------------

read.exception <- function(e, type, fn = NULL){
  if (is.null(fn)) {
    new_card(type, c('MESSAGE: ', e$message, '', 'CALL: ', deparse(e$call)))
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
