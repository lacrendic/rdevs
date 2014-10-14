theme_hc <- function(){
  # Highcharts theme
  theme(
    text                = element_text(size = 10),
    title               = element_text(hjust=0), 
    axis.title.x        = element_text(hjust=.5),
    axis.title.y        = element_text(hjust=.5),
    panel.grid.major.y  = element_line(color='gray', size = .3),
    panel.grid.minor.y  = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.border        = element_blank(),
    panel.background    = element_blank(),
    legend.position     = "bottom",
    legend.title        = element_blank()
  )
}

theme_null <- function(){

  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()
        )
}


theme_pallet <- function(name = "default"){
  
  palettes <- list()
  
  # highcharts
  # https://github.com/highslide-software/highcharts.com/blob/master/js/highcharts.src.js#L1251
  palettes[["default"]] <- c("#7CB5EC", "#434348", "#90ED7D", "#F7A35C", "#8085E9", "#F15C80", 
                             "#E4D354", "#8085E8", "#8D4653", "#91E8E1")
  palettes[["dark_blue"]] <- c("#DDDF0D", "#55BF3B", "#DF5353", "#7798BF", "#AAEEEE", "#FF0066", 
                               "#EEAAEE", "#55BF3B", "#DF5353", "#7798BF", "#AAEEEE")
  palettes[["dark_green"]] <- c("#DDDF0D", "#55BF3B", "#DF5353", "#7798BF", "#AAEEEE", "#FF0066", 
                               "#EEAAEE", "#55BF3B", "#DF5353", "#7798BF", "#AAEEEE")
  palettes[["gray"]] <- c("#DDDF0D", "#7798BF", "#55BF3B", "#DF5353", "#AAEEEE", "#FF0066", 
                          "#EEAAEE", "#55BF3B", "#DF5353", "#7798BF", "#AAEEEE")
  
  # Others in web
  # http://www.mulinblog.com/a-color-palette-optimized-for-data-visualization/
  palettes[["smtn"]] <- c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0",
                          "#B2912F", "#B276B2", "#DECF3F", "#F15854")
  return(palettes[[name]])
}

colors_hc <- function(){
  c("#7CB5EC", "#313131", "#F7A35C", "#90EE7E", "#7798BF", "#AAEEEE",
               "#FF0066", "#EEAAEE", "#55BF3B", "#DF5353", "#7798BF", "#AAEEEE")
}