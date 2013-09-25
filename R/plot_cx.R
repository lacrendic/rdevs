plot_dist_by_var <- function(dataframe, split.var, file = "output_plot_dist_by_var.pdf", ...){
  library(ggplot2)
  pdf(file, ...)
  l_ply(setdiff(names(dataframe), split.var), function(namevar){
    message(sprintf("plotting %s", namevar))
    daux <- subset(dataframe, select = c(namevar, split.var))
    names(daux) <- c("namevar", "split.var")
    p <- ggplot(daux) + geom_bar(aes(x=namevar)) + facet_grid(split.var ~ .)  + ggtitle(namevar)
    print(p)
  })
  dev.off()
}