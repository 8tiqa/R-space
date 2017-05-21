
library("AzureML")
ws = workspace()
auto.price = download.datasets(ws, "Automobile price data (Raw)")

## Coerce some character columns to numeric
cols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
auto.price[, cols] = lapply(auto.price[, cols], function(x) ifelse(x == '?', NA, x))
auto.price[, cols] = lapply(auto.price[, cols], as.numeric)

## remove rows with NAs 
auto.price = auto.price[complete.cases(auto.price), ]

## Add a log transformed column for price
auto.price$lnprice = log(auto.price$price)
  
## Consolidate the number of cylinders
auto.price$num.cylinders = 
    ifelse(auto.price$num.of.cylinders %in% c("four", "three"), "three-four",
        ifelse(auto.price$num.of.cylinders %in% c("five", "six"), "five-six", "eight-twelve"))
  
head(auto.price)

library(ggplot2)
library(repr)
options(repr.plot.width=8, repr.plot.height=8)

num.cols = c("wheel.base",
              "width",
              "height",
              "curb.weight",
              "engine.size",
              "bore",
              "compression.ratio",
              "city.mpg",
              "price",
              "lnprice")

pairs(~ ., data = auto.price[, num.cols])

## Function to plot conditioned histograms
install.packages('gridExtra')
options(repr.plot.width=6, repr.plot.height=3)
auto.hist = function(x) {
  library(ggplot2)
  library(gridExtra)
  ## Compute the bin width
  rg = range(auto.price[,x])
  bw = (rg[2] - rg[1])/30
  ## Define the title
  title <- paste("Histogram of", x, "conditioned on type of drive wheels")
  ## Create the histogram
  ggplot(auto.price, aes_string(x)) +
    geom_histogram(aes(y = ..count..), binwidth = bw) +
    facet_grid(. ~ drive.wheels) +
    ggtitle(title) 
}

## Create histograms for specified features.
plot.cols2 = c("length",
               "curb.weight",
               "engine.size",
               "city.mpg",
               "price")
lapply(plot.cols2, auto.hist)


## Function to create conditioned box plots
auto.box = function(x) {
  title = paste("Box plot of", x, "by type of drive wheels")
  ggplot(auto.price, aes_string('drive.wheels', x)) +
    geom_boxplot() +
    ggtitle(title)
}

lapply(plot.cols2, auto.box)

## Scatter plot using color to differentiate points
scatter.auto = function(x){
  require(ggplot2)
  title = paste("price vs.", x, 'with color by num.cylinders')
  ggplot(auto.price, aes_string(x, 'price')) +
    geom_point(aes(color = factor(num.cylinders))) +
    ggtitle(title)
}

## Define columns for making scatter plots
plot.cols3 = c("length",
                "curb.weight",
                "engine.size",
                "city.mpg")
lapply(plot.cols3, scatter.auto)

## Conditioned scatter plots
scatter.auto.cond = function(x){
  require(ggplot2)
  library(gridExtra)
  title = paste("price vs.", x, 'with color by num.cylinders and body style')
  ggplot(auto.price, aes_string(x, 'price')) +
    geom_point(aes(color = factor(fuel.type))) +
    facet_grid(body.style ~ num.cylinders) +
    ggtitle(title)
}

options(repr.plot.width=8, repr.plot.height=6)
lapply(plot.cols3, scatter.auto.cond)

library("AzureML")
ws = workspace()
Income = download.datasets(ws, "Adult Census Income Binary Classification dataset")
str(Income)

## Bar plot of categorical features
bar.income = function(x){
  library(ggplot2)
  if(!is.numeric(Income[,x])) {
    capture.output(
      plot( ggplot(Income, aes_string(x)) +
               geom_bar() + 
               facet_grid(. ~ income) + 
               theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
               ggtitle(paste("Counts of income level by",x))))
      }
    }

## Set up plot area
library(repr)
options(repr.plot.width=8, repr.plot.height=4)

## Features to plot
name.list = function(x) {
  names = names(x)
  len = length(names)
  names[-len]
}

feature.names = name.list(Income)
lapply(feature.names, bar.income)

## Create Box plot of numeric features 
box.income = function(x){
  library(ggplot2)
  if(is.numeric(Income[,x])) {
    capture.output(
      plot( ggplot(Income, aes_string('income', x)) +
              geom_boxplot() +
              ggtitle(paste("Counts of income level by",x))))
  }}

lapply(feature.names, box.income)
