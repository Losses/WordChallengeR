#--------------------------------------------------------
#  FUNCTIONS FOR FILE SYSTEM
#  By Losses Don @ 2016-07-07
#--------------------------------------------------------

write.file <- function(x, file){
  con <- file(file, "a")
  tryCatch({
    cat(iconv(x, to="UTF-8"), file=con, sep="\n")
  },
  finally = {
    close(con)
  })
}