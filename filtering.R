# returns FALSE if a variable is not significant for correlation or significance test
filtering <- function(data, method = "cor") {
  # correlation test
  if (method == "cor") {
    target_column           <- "Class"
    if (class(data[[target_column]]) == "factor") {
      data[[target_column]] <- as.numeric(data[[target_column]]) - 1
    }
    pval <- sapply(1:(ncol(data) - 1), 
                   function(i) cor.test(data[[target_column]], 
                                        data[[i]])$p.value)
    return(pval < 0.05)
  }
  
  # significance test
  if (method == "sig") {
    if (!any(levels(data$Class) == "0")) {
      data$Class <- as.factor(as.numeric(data$Class) - 1)
    }
    pval         <- sapply(1:(ncol(data) - 1), function(i) {
      wt         <- wilcox.test(data[data$Class == "0", i], 
                                data[data$Class == "1", i])
      wt$p.value
    })
    return(pval < 0.05)
  }
}


filtering(datasets[[1]], method = "cor")
filtering(datasets[[1]], method = "sig")

filtering(datasets[[2]], method = "cor")
filtering(datasets[[2]], method = "sig")

filtering(datasets[[3]], method = "cor")
filtering(datasets[[3]], method = "sig")

filtering(datasets[[4]], method = "cor")
filtering(datasets[[4]], method = "sig")

filtering(datasets[[5]], method = "cor")
filtering(datasets[[5]], method = "sig")

filtering(datasets[[6]], method = "cor")
filtering(datasets[[6]], method = "sig")

filtering(datasets[[7]], method = "cor")
filtering(datasets[[7]], method = "sig")

filtering(datasets[[8]], method = "cor")
filtering(datasets[[8]], method = "sig")

filtering(datasets[[9]], method = "cor")
filtering(datasets[[9]], method = "sig")

filtering(datasets[[10]], method = "cor")
filtering(datasets[[10]], method = "sig")

filtering(datasets[[11]], method = "cor")
filtering(datasets[[11]], method = "sig")

filtering(datasets[[12]], method = "cor")
filtering(datasets[[12]], method = "sig")

filtering(datasets[[13]], method = "cor")
filtering(datasets[[13]], method = "sig")

filtering(datasets[[14]], method = "cor")
filtering(datasets[[14]], method = "sig")

filtering(datasets[[15]], method = "cor")
filtering(datasets[[15]], method = "sig")

filtering(datasets[[16]], method = "cor")
filtering(datasets[[16]], method = "sig")

filtering(datasets[[17]], method = "cor")
filtering(datasets[[17]], method = "sig")

filtering(datasets[[18]], method = "cor")
filtering(datasets[[18]], method = "sig")

filtering(datasets[[19]], method = "cor")
filtering(datasets[[19]], method = "sig")

filtering(datasets[[20]], method = "cor")
filtering(datasets[[20]], method = "sig")

filtering(datasets[[21]], method = "cor")
filtering(datasets[[21]], method = "sig")

