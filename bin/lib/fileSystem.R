#--------------------------------------------------------
#  FUNCTIONS FOR FILE SYSTEM
#  By Losses Don @ 2016-07-07
#--------------------------------------------------------

write.file <- function(x, file){
  con <- file(file, "a")
  tryCatch({
    cat(iconv(x, to = "UTF-8"), file = con, sep = "\n")
  },
  finally = {
    close(con)
  })
}

remove.files <- function(x){
  if (is.character(x) && length(x) > 0)
    for (.i in x) file.remove(x)
}

auto_file_name <- function(x, extention = 'csv'){
  pattern <- sprintf('.%s$', extention)
  exists <- (grep(pattern, x) %>% length) > 0
  ifelse(exists, x, sprintf('%s.%s', x, extention))
}

get.remote.try <- function(url){
  temp_location <- sprintf('%s/%s', tempdir(), rand_string())
  status <- tryCatch(download.file(url = url, destfile = temp_location, quiet = T),
               error = function(e) T,
               warning = function(e) F
             )
  
  if (status || file.size(temp_location) == 0) return(F)
  temp_location
}

get.remote <- function(url, location = getwd(),
                       hint = 'Getting file from remote server',
                       max.retry = 3,
                       force = T){
  
  if (hint != '') new_status(hint)
  try_count <- 1
  status <- T
  repeat {
    
    if (try_count > 3) {
      status <- F
      break()
    }
    
    file.loc <- get.remote.try(url)
    if (file.loc != F) break()
    try_count <- try_count + 1
  }
  
  if (status) file.rename(file.loc, location)
  
  if (hint != '')
    if (status) status.done() else status.fail()

  if (!status) sprintf('Cant download %s', url) %>% exception(force)
  
  status
}

clear_output <- function(){
  new_status('Clearning usr/output')
  list.dirs('usr/output')[-1] %>% lapply(unlink, recursive=TRUE)
  status.done()
}

parse.remote <- function(url, file.loc, hint, type, remote_bad = T, max.retry  = 3, force = T){
  if (!file.exists(file.loc)) {
    download_report <- get.remote(url, file.loc, hint = hint, max.retry, force = force)
    if (!download_report) return(F)
  }
  
  result <- F
  
  if (type == 'JSON') {
    result <- tryCatch(fromJSON(file = file.loc),
                       error = function(e) F)
  } else if (type == 'XML') {
    result <- tryCatch(xmlParse(file.loc, error = function(e) F),
                       error = function(e) F)
  } else {
    stop('Wrong dictionary file type!')
  }
  
  if (remote_bad && is.logical(result) && result == F) {
    file.remove(file.loc)
    exception('Dictionary source file format error.', force)
  }
  
  result
}