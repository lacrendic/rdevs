readtable <- function(files, ...){
  require(plyr)
  # 20120314: Use of the function ldply to load more than 1 file
  # 20130409: Support pipe separated values
  # 20130720: Support json
  read.table.aux <- function(name, ...){
    ext <- tolower(unlist(strsplit(name, "\\."))[length(unlist(strsplit(name, "\\.")))])
    ext_support <- c("txt","csv","xlsx","xls","dbf","sas7bdat","sav","tsv","dat", "psv", "json")
    if(!ext %in% ext_support){
      stop("No posible format (extension)")
    }
    if(ext %in% c("txt","tsv","dat")) return(read.table(name, sep = "\t", header = TRUE, comment.char = "", quote = "",  ...))
    if(ext == "csv") return(read.table(name, sep = ";", header = TRUE,  comment.char = "", quote = "", dec=",", ...))
    if(ext == "psv") return(read.table(name, sep = "|", header = TRUE,  comment.char = "", quote = "", dec=",", ...))
    if(ext == "sav"){
      require(foreign)
      return(read.spss(name, to.data.frame = T))
    }
    if(ext == "json"){
      l <- fromJSON(file=name)
      d <- ldply(l, function(x){
        d <- data.frame(t(unlist(x)), stringsAsFactors=FALSE)
        names(d) <- names(x)
        d
      })
      
      for(c in seq(ncol(d))){
        if(!all(is.na(as.numeric(d[,c])))){
          d[,c] <- as.numeric(d[,c])
        } 
      }
      
      return(d)
      
    }
    if(ext == "sas7bdat"){ require(sas7bdat); return(read.sas7bdat(name))}
    if(ext %in% c("xlsx","xls")){
		require(xlsx)
		return(read.xlsx(name, sheetIndex=1, ...))
	}
  }
  # This function not necessary require all data with the same structure
  ldply(files, function(f) read.table.aux(f, ...))
}