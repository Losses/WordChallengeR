rm(list = ls())
Sys.setlocale("LC_CTYPE","chinese")
Sys.setenv(LANG = "en")

for (.lib_file in list.files('bin/lib')) {
  cat(sprintf('Loading Library: %s\n', .lib_file))
  source(sprintf('bin/lib/%s',.lib_file), encoding = 'utf8')
}

standard_lib <- readLines('etc/standardLib.txt')
WORD_CACHE <- build_word_cache()

wdR()