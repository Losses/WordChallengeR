source('./script/function.R', encoding = 'utf8')

cat(sprintf('Summary\n-------\nTotal entries\t%s\nIn scope\t%s\nPharses     \t%s\nOut of scope\t%s\n',
            nrow(WORD_CACHE), sum(WORD_CACHE$in_lib) - sum(sapply(WORD_CACHE$word, is.pharse)),
            sum(sapply(WORD_CACHE$word, is.pharse)), sum(!WORD_CACHE$in_lib)))
