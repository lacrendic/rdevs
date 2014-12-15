
make_cluster_vars <- function(df_dir,n,plot){
  library(ClustOfVar)
  B <- 20
  
  df <- readtable(df_dir)
  df[is.na(df)] <- 0
  X <- as.matrix(df[,-1])
  
  tree <- hclustvar(X)
  
  if(plot){
    stab<-stability(tree, B = B , graph = F)
    
    pdf("plot_output.pdf", width = 8, height = 4)
    par(mfrow=c(1,2),cex=0.7)
    plot(tree)
    plot(stab, main="Stabilidad de los clusters")
    dev.off()
  }
  
  rs <- cutreevar(tree, k = n)
  
  cat(paste("n = ",n,
            "\nplot = ",plot,
            "\ndf = ", df_dir,
            "\n\nClusters:\n",paste(names(rs$cluster),collapse=" "),"\n", 
            paste(rs$cluster,collapse=" "), sep=""), file="output.txt")
}