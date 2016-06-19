cat(
  as.dictionary(
    read.word(
      paste(strsplit(LAST_COMMAND, ' ')[[1]][-1], collapse = ' ')
      )
    )
  )
cat('\n')