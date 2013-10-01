plot_df_dist <- function(df, facet_formula, responses, file = "output_plot_dist_by_var.pdf", ...){
  library(ggplot2)
  library(scales)
  
  pdf(file, ...)
  colors <- c("#A61000", "#1B0773", "#007D1C", "#FFCE00")
  
  namevars <- names(df)
  if(!missing(facet_formula)){
    facet_vars <- setdiff(as.character(facet_formula), c("~", "."))
    namevars <- setdiff(namevars, facet_vars)
  }
  if(!missing(responses)){
    responses_var <- names(responses)
    namevars <- setdiff(namevars, responses_var)
  }
  
  for(namevar in namevars){
    message(sprintf("plotting %s", namevar))
    daux <- subset(df, select=namevar)
    
    if(!missing(facet_formula)){
      daux <- cbind(daux, subset(df, select=facet_vars))
    }
    if(!missing(responses)){
      daux <- cbind(daux, subset(df, select=names(responses)))
    }
    
    # Base plot
    p <- ggplot(daux, aes_string(x=namevar)) +
      geom_bar(aes(y=(..count..)/sum(..count..))) +
      xlab(NULL) + ylab(NULL) +  ggtitle(str_capitalize(namevar))
    
    if(!missing(responses)){
      for(response in names(responses)){
        color <- colors[which(response == names(responses))]
        if(is.numeric(daux[[namevar]])){
          p <- p + stat_smooth(data=daux, aes_string(x=namevar,y=response), colour = color)
        } else{
          p <- p +
            stat_summary(data=daux, aes_string(x=namevar, y=response), fun.y=mean, colour=color, geom="point") +
            stat_summary(data=daux, aes_string(x=namevar, y=response, group = 1), fun.y=mean, colour=color, geom="line")
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