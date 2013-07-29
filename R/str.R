str_first_upper <- function(string){
  string <- tolower(string)
  paste(toupper(substring(string, 1, 1)),substring(string, 2, nchar(string)), sep = "")
}

str_pattern <- function(string, pattern){
  require(stringr)
  string[str_detect(string, pattern)]
}

str_capitalize <- function(string){
  # http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string/6365349#6365349
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(string), perl=TRUE)
}

str_clean <- function(string,
                      remove.punct = TRUE,
                      remove.digit = TRUE,
                      remove.accent = TRUE,
                      trim = TRUE,
                      to.lower = TRUE){
  require(stringr)
  if(remove.punct)     string <- gsub("[[:punct:]]", "", string)
  if(remove.digit)     string <- gsub("[[:digit:]]", "", string)
  if(remove.accent)    string <- iconv(string, from="UTF-8", to="ASCII//TRANSLIT")  # https://stat.ethz.ch/pipermail/r-help/2010-September/251833.html
  if(to.lower)         string <- tolower(string)
  if(trim)             string <- str_trim(string)
  
  return(string)
}

str_is_email <- function(string){
  email_pattern <- "^([a-zA-Z0-9]+[a-zA-Z0-9._%-]*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,4})$"
  grepl(email_pattern, string)
}