
# The variable ‘Satellites’ describes a number of hopeful male horseshoe crabs that, in addition to the sexual partner, attach themselves to the females during mating. 
# We shall look at the event that the female horseshoe crab has a satellite. 
# Perform logistic regression of the probability of having a satellite on the width of the female crab


data(crabs)
head(crabs)
crab.data<-data.frame(satellite=1*(crabs$Satellites>0),width=crabs$Width)

my.analysis<-glm(satellite~width,family=binomial,data=crab.data)
my.analysis


# data frame my.linear.predictor that contains the linear predictors,
# the lower confidence limit (linear predictor minus 1.96 times the standard error) and the upper confidence limit. 

my.linear.predictor<-data.frame(
  prediction=predict(my.analysis,se.fit=TRUE)$fit,
  lower=predict(my.analysis,se.fit=TRUE)$fit-
        1.96*predict(my.analysis,se.fit=TRUE)$se.fit,
  upper=predict(my.analysis,se.fit=TRUE)$fit+
        1.96*predict(my.analysis,se.fit=TRUE)$se.fit)
        
        
 # Order the rows of my.linear.predictor according to crab.data$width, 
 my.linear.predictor<-my.linear.predictor[order(crab.data$width),]
 
 # transform my.linear.predictorwith the logistic function. 
 logistic<-function(x){exp(x)/(1+exp(x))}
 my.predictor<-logistic(my.linear.predictor)
 
 # plot the predicted values of the probability of a satellite as a function of width, 
 # with dashed lines indicating upper and lower confidence limits.
 plot(sort(crab.data$width),my.predictor$prediction,type="l",
     xlab='width',ylab='p(satellite)')
lines(sort(crab.data$width),my.predictor$upper,type="l",lty=2)
lines(sort(crab.data$width),my.predictor$lower,type="l",lty=2) 

# Use the cut() and tapply() functions to create 5 grouped means of satellite by width.
summary(crab.data$width)

my.cut<-cut(crab.data$width,breaks=20+(0:5)*3) 
my.means<-tapply(crab.data$satellite,my.cut,mean) 
lines(20+(0:4)*3+1.5,my.means,type="p",pch=16) 
