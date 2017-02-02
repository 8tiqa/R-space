x=c(3, 1, 4, 2, 1, 4, 5, 1,4)
length(x)
mean(x)
table(x)

data("InsectSprays")
dim(InsectSprays)
table(InsectSprays[,"spray"])
pie(table(InsectSprays[,"spray"]))
summary(InsectSprays[,"count"])

hist(InsectSprays[,"count"], xlab="Insect Counts", main="")

?plot
plot(count~spray, data = InsectSprays, xlab ="Types of Insecticides", ylab = "Counts of Insects")
plot(InsectSprays$count)
plot(InsectSprays$count,axes=F, ylim=c(0,150), typ='l', ann=F)
plot(InsectSprays$count,ylim=c(0,150), typ='l')

anova(lm(count~spray, data=InsectSprays))
