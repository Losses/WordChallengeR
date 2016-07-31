.packages <- c('utils', 'stats', 'XML', 'rjson', 'crayon', 'magrittr', 'hunspell')

for(.i in .packages){
  cat(sprintf('Loading Package: %s\n', .i))
  eval(parse(text = sprintf('library(%s)', .i)))
}

.First <- function() source('./bin/cmd.R')
