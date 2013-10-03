plot_calendar_hm <- function(dates, values){
  #   date_seq <- seq.Date(as.Date(ymd("20120803")), as.Date(ymd("20141003")), by=1)
  #   dates <- sample(date_seq, size = round(length(date_seq)*.9))
  #   values <- rnorm(length(dates))
  #   plot_calendar_hm(dates, values)
  #   plot_calendar_hm(values, values)
  #   plot_calendar_hm(dates, values[-2])
  
  library(ggplot2)
  library(plyr)
  library(lubridate)
  library(zoo)
  if(!is.Date(dates)) stop("No dates")
  if(!length(dates)==length(values)) stop("Dates and values are not the same length")
  
  df <- data.frame(dates, values, row.names=NULL)
  df <- join(data.frame(dates = seq.Date(min(dates), max(dates), by=1)), df, by = "dates")
  df <- df[order(dates),]
  df <- transform(df,
                  year = year(dates),
                  month = month(dates),
                  monthf = month(dates, label=TRUE),
                  weekday = wday(dates),
                  weekdayf = wday(dates, label=TRUE),
                  yearmonth = as.yearmon(dates),
                  week = week(dates),
                  day = day(dates))
  df <- ddply(df,  .(yearmonth), transform, monthweek = 1 + week - min(week))
  #   df <- transform(df, monthweek = ifelse(weekday %in% c(1,2), monthweek+1, monthweek))
  
  p <- ggplot(df, aes(monthweek, weekdayf, fill = values)) + 
    geom_tile(colour = "white") +
    facet_grid(year ~ monthf) +
    theme(legend.position = "none") +
    geom_text(aes(label=day, size = 2), colour = "white") +
    xlab(NULL) + ylab(NULL)
  
  print(p)
  return(p)
}



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
    
    if(!is.numeric(daux[[namevar]])){
      daux[[namevar]] <- addNA(daux[[namevar]])
    }
    if(!missing(facet_formula)){
      daux <- cbind(daux, subset(df, select=facet_vars))
    }
    if(!missing(responses)){
      daux <- cbind(daux, subset(df, select=names(responses)))
    }
    
    # Base plot
    p <- ggplot(daux, aes_string(x=namevar)) +
      geom_bar(aes(y=(..count..)/sum(..count..))) +
      xlab(NULL) + ylab(NULL) +  ggtitle(str_capitalize(namevar)) +
      scale_y_continuous(labels = percent)
    
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