plot_df_dist_formula <- function(dataframe, facet_formula, file = "output_plot_dist_by_var.pdf", ...){
  library(ggplot2)
  pdf(file, ...)
  facet_vars <- setdiff(as.character(facet_formula), c("~", "."))
  
  l_ply(setdiff(names(dataframe), facet_vars), function(namevar){
    message(sprintf("plotting %s", namevar))
    daux <- subset(dataframe, select = c(namevar, facet_vars))
    names(daux) <- c("namevar", facet_vars)
    p <- ggplot(daux) +
      
      geom_bar(aes(x=namevar)) +
      facet_grid(facet_formula, scales="free") +
      ggtitle(str_capitalize(namevar))
    print(p)
  })
  dev.off()
}

