theme_hc <- function(){
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

colors_hc <- function(){
  c("#7CB5EC", "#313131", "#F7A35C", "#90EE7E", "#7798BF", "#AAEEEE",
               "#FF0066", "#EEAAEE", "#55BF3B", "#DF5353", "#7798BF", "#AAEEEE")
}