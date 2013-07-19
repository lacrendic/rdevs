str_first_upper <- function(string){
  string <- tolower(string)
  paste(toupper(substring(string, 1, 1)),substring(string, 2, nchar(string)), sep = "")
}

str_pattern <- function(string, pattern){
  library(stringr)
  string[str_detect(string, pattern)]
}

str_capitalize <- function(strings){
  #   laply(strings, function(x){r
  #     x <- tolower(x)
  #     x <- str_split(x, " ")
  #     paste(laply(x, str_first_upper), collapse=" ")
  #   })
  # http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string/6365349#6365349
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(strings), perl=TRUE)
}




str_clean <- function(string,
                      remove.punct = TRUE,
                      remove.digits = TRUE,
                      remove.accent = TRUE,
                      trim = TRUE,
                      to.lower = TRUE){
  
  if(remove.punct)     string <- gsub("[[:punct:]]", "", string)
  if(remove.digits)    string <- gsub("[[:digit:]]", "", string)
  if(remove.accent)    string <- iconv(string, from="UTF-8", to="ASCII//TRANSLIT")  
  if(to.lower)         string <- tolower(string)
  if(trim)             string <- gsub('^+ | +$','', string)
  
  return(string)
}

str_is_email <- function(x){
  email_pattern <- "^([a-zA-Z0-9]+[a-zA-Z0-9._%-]*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,4})$"
  grepl(email_pattern, x)
}