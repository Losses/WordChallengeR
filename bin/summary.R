#--------------------------------------------------------
#  SUMMARY TOOL
#  By Losses Don @ 2016-07-31
#--------------------------------------------------------
#
# command:
#   summary
#

new_card(title = 'Summary', c(
  sprintf('Total Entries: %s', nrow(WORD_CACHE)),
  sprintf('In scope: %s', sum(WORD_CACHE$in_lib) - sum(sapply(WORD_CACHE$word, is.pharse))),
  sprintf('Pharse: %s', sum(sapply(WORD_CACHE$word, is.pharse))),
  sprintf('Not in scope: %s', sum(!WORD_CACHE$in_lib))
))
