SD <- function(DF) {
  # user define C1
  cat("Enter top concentration in μM : \n")
  C1 <- as.numeric(readline(prompt = ""))
  C2 <- signif(C1/DF, 3)
  C3 <- signif(C2/DF, 3)
  C4 <- signif(C3/DF, 3)
  C5 <- signif(C4/DF, 3)
  C6 <- signif(C5/DF, 3)
  C7 <- signif(C6/DF, 3)
  C8 <- signif(C7/DF, 3)
  C9 <- signif(C8/DF, 3)
  C10 <- signif(C9/DF, 3)
  
  results <- data.frame(Concentration = rep(c(C1, C2, C3, C4, C5, 
                                                   C6, C7, C8, C9, C10)))
  print(results)
  
  # plot
  results <- as.data.frame(results)
  library(ggplot2)
  library(ggrepel)
  ggplot(results, aes(x = 1:10, y = Concentration)) +
    geom_point() +
    geom_line(color = "#669933") +
    xlab("Dilution Level") +
    ylab("Concentration (μM)") +
    theme_linedraw() +
    scale_x_continuous(breaks = seq(1,10, by=1)) +
    geom_label_repel(aes(label = Concentration),
                     box.padding = 0.35,
                     point.padding = 0.5)
  
}