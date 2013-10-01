plot_df_dist <- function(dataframe, facet_formula, file = "output_plot_dist_by_var.pdf", ...){
  library(ggplot2)
  pdf(file, ...)
  
  if(!missing(facet_formula)){
    facet_vars <- setdiff(as.character(facet_formula), c("~", "."))  
  }
  
  for(namevar in setdiff(names(dataframe), facet_vars)){
    message(sprintf("plotting %s", namevar))
    daux <- subset(dataframe, select = c(namevar, facet_vars))
    names(daux) <- c("namevar", facet_vars)
    p <- ggplot(daux) + geom_bar(aes(x=namevar)) + xlab(NULL) + ylab(NULL) +  ggtitle(str_capitalize(namevar))   
    if(!missing(facet_formula)){
      p <- p  + facet_grid(facet_formula, scales="free")
    }
    print(p)
  }
  dev.off()
}



plot_df_dist_2 <- function(dataframe,
                           facet_formula,
                           names.responses,
                           file = "output_plot_dist_by_var.pdf",
                           ...){
  library(ggplot2)
  library(scales)
  
  pdf(file, ...)
  colors <- c("#A61000", "#1B0773", "#007D1C", "#FFCE00")
  
  if(!missing(facet_formula)){
    facet_vars <- setdiff(as.character(facet_formula), c("~", "."))  
  }

  for(namevar in setdiff(names(dataframe), facet_vars)){
   
    message(sprintf("plotting %s", namevar))
    daux <- subset(dataframe, select = c(namevar, facet_vars, responses))
    names(daux)[1] <- "namevar"
    head(daux)
    p <- ggplot(daux) +  
      geom_bar(aes(x=namevar, y=(..count..)/sum(..count..))) +
      ylab(NULL) + xlab(NULL) +
      ggtitle(str_capitalize(namevar))
    if(!missing(names.responses)){
      for(response in names.responses){
        color <- colors[which(response == names.responses)]
        if(!is.numeric(daux[["namevar"]])){
          p <- p +
            stat_summary(aes_string(x="namevar", y=response), fun.y=mean, colour=color, geom="point") +
            stat_summary(aes_string(x="namevar", y=response, group = 1), fun.y=mean, colour=color, geom="line")
        }
      }
    }
    if(!missing(facet_formula)){
      p <- p  + facet_grid(facet_formula, scales="free")
    }
    print(p)
  }
  dev.off()
}
