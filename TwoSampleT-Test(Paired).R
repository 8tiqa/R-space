
library("AzureML")
ws <- workspace()
gaulton <- download.datasets(ws, "GaltonFamilies.csv")

head(gaulton)

dim(gaulton)

install.packages('gridExtra')
hist.plot = function(df, col, bw, max, min){
    ggplot(df, aes_string(col)) +
      geom_histogram( binwidth = bw) + 
      xlim(min, max)
}
    
hist.family = function(df, col1, col2, num.bin = 30){
  require(ggplot2)
  require(gridExtra)
  
  ## Compute bin width
  max = max(max(df[, col1]), max(df[, col2]))
  min = min(min(df[, col2]), min(df[, col2]))
  bin.width = (max - min)/num.bin
  
  ## Create a first histogram
  p1 = hist.plot(df, col1, bin.width, max, min)
  p1 = p1 + geom_vline(xintercept = mean(df[, col1]),
                        color = 'red', size = 1)
  
  ## Create a second histogram
  p2 = hist.plot(df, col2, bin.width, max, min)
  p2 = p2 + geom_vline(xintercept = mean(df[, col2]),
                        color = 'red', size = 1)
  
  ## Now stack the plots
  grid.arrange(p1, p2, nrow = 2, ncol = 1)
}

sons = gaulton[gaulton$gender == 'male', ]
hist.family(sons, 'childHeight', 'mother')

daughters = gaulton[gaulton$gender == 'female', ]
hist.family(daughters, 'childHeight', 'mother')

families.test <- function(df, col1, col2, paired = TRUE){
  t.test(df[, col1], df[, col2], paired = paired)
}

hist.family.conf <- function(df, col1, col2, num.bin = 30, paired = FALSE){
  require(ggplot2)
  require(gridExtra)
  
  ## Compute bin width
  max <- max(max(df[, col1]), max(df[, col2]))
  min <- min(min(df[, col2]), min(df[, col2]))
  bin.width <- (max - min)/num.bin
  
  mean1 <- mean(df[, col1])
  mean2 <- mean(df[, col2])
  t <- t.test(df[, col1], df[, col2], paired = paired)
  pv1 <- mean2 + t$conf.int[1]
  pv2 <- mean2 + t$conf.int[2]
  
  ## Plot a histogram
  p1 <- hist.plot(df, col1, bin.width, max, min)
  p1 <- p1 + geom_vline(xintercept = mean1,
                        color = 'red', size = 1) + 
             geom_vline(xintercept = pv1,
                        color = 'red', size = 1, linetype = 2)  + 
             geom_vline(xintercept = pv2,
                        color = 'red', size = 1, linetype =2) 
  
  ## A simple boxplot
  p2 <-  hist.plot(df, col2, bin.width, max, min)
  p2 <- p2 + geom_vline(xintercept = mean2,
                        color = 'red', size = 1.5)
  
  ## Now stack the plots
  grid.arrange(p1, p2, nrow = 2)
  
  print(t)
}

hist.family.conf(sons, 'mother', 'childHeight')

hist.family.conf(daughters, 'mother', 'childHeight')

hist.family.conf(daughters, 'father', 'childHeight')


