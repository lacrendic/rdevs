plot_calendar_hm <- function(dates, values){
  #   date_seq <- seq.Date(as.Date(ymd("20120101")), as.Date(ymd("20130101")), by=1)
  #   dates <- sample(date_seq, size = round(length(date_seq)*.9))
  #   values <- rnorm(length(dates))
  #   plot_calendar_hm(dates, values)
  #   plot_calendar_hm(values, values)
  #   plot_calendar_hm(dates, values[-2])
  
  require(ggplot2)
  require(plyr)
  require(lubridate)
  require(zoo)
#   if(!is.Date(dates)) stop("No dates")
  if(!length(dates)==length(values)) stop("Dates and values are not the same length")
  
  s <- ymd(paste(year(min(dates)), month(min(dates)), "1", sep = "-"))
  f <- ymd(paste(year(max(dates)), month(max(dates)), "1", sep = "-"))
  month(f) <- month(f) + 1
  f <- f - days(1)
    
  df <- join(data.frame(dates = seq.Date(as.Date(s), as.Date(f), by=1)),
             data.frame(dates, values, row.names=NULL),
             by = "dates")
  #   head(df); tail(df)
  
  df <- df[order(df$dates),]
  
  df <- transform(df,
                  year = year(dates),
                  month = month(dates),
                  monthf = month(dates, label=TRUE),
                  weekday = wday(dates),
                  weekdayf = wday(dates, label=TRUE),
                  yearmonth = as.yearmon(dates),
                  yearmonthf = factor(as.yearmon(dates)),
                  week = as.numeric(format(dates,"%W")), #week(dates),
                  day = day(dates),
                  monthday =  mday(dates),
                  monthweek = ceiling(mday(dates)/7))
  df <- ddply(df,  .(yearmonthf), transform, monthweek = 1 + week - min(week))
  df$weekdayf <- factor(as.character(df$weekdayf), levels=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))
  
  
  df <- df[complete.cases(df),]
  p <- ggplot(df, aes(monthweek, weekdayf, fill = values)) + 
    geom_tile(colour = "white") +
    facet_grid(year ~ monthf) +
    theme(legend.position = "none") +
    geom_text(aes(label=day, size = 2), colour = "white") +
    xlab(NULL) + ylab(NULL)
  
  print(p)
  return(p)
}

plot_complete_df <- function(df, facet_formula, responses,smooth_span=0.99, file = "output_plot_dist_by_var.pdf", ...){
  require(ggplot2)
  require(scales)
  
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
          p <- p + stat_smooth(data=daux, aes_string(x=namevar,y=response), colour = color,method="loess",span=smooth_span,sd=F)
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