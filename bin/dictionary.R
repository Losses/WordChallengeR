FILE_NAME <- ifelse(is.na(LAST_COMMAND[2]), get_file(), LAST_COMMAND[2])
PROFILE <- ifelse(is.na(LAST_COMMAND[3]), 'default.csv', LAST_COMMAND[3])
SESSION <- as.numeric(Sys.time())

write.application(FILE_NAME, PROFILE)
