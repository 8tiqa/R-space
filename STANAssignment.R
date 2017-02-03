require(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#SIMULATE DATA
N <- 5000
# we assume 5000 members
female <- rbinom(N,1,0.6) 
# simulate 60% of female
fotography <- rbinom(N,1,0.5) 
# simulate 50% of customers who expressed interest for fotography
sculpture <- rbinom(N,1,0.3) 
# simulate 30% of customers who expressed interest for sculpture
digital_content <- rpois(N,0.5) 
# simulate digital content
months_since <- rpois(N,5) 
# simulate months since last visit, with a max of 12
months_since[months_since>12] <- 12

# simulate satisfaction with an average 0.3
satisfaction <- rpois(N,0.3) 


#simulate renewal behavior and prepare the data
beta0 <- 0.6
beta1 <- 0.9
beta2 <- 0.6
beta3 <- 0.01
beta4 <- -0.01
beta5 <- -0.2
beta6 <- 0.6
prob_simul <- pnorm(beta0 + beta1*female + beta2*fotography 
                    + beta3*sculpture + beta4*digital_content 
                    + beta5*months_since + beta6*satisfaction )
renewal <- rbinom(N,1,prob_simul) 

#Prepare data for Stan
#X matrix combines all the input variables
X <- cbind(rep(1,N),
           female,
           fotography,
           sculpture,
           digital_content,
           months_since,
           satisfaction)

# assign column names
colnames(X) <- c("intercept",
                 "female",
                 "fotography",
                 "sculpture",
                 "digital_content",
                 "months_since",
                 "satisfaction")
K<-dim(X)[2]

#Model for STAN
probit <- '
data{
int<lower=0> N; # number of observations
int<lower=0> K; # number of parameters

int<lower=0,upper=1> y[N];
vector[K] X[N];
}
parameters{
vector[K] beta;
}
model{
beta ~ cauchy(0,5);

for(j in 1:N)
y[j] ~ bernoulli(Phi_approx(dot_product(X[j],beta)));
}
'
#Run STAN
data_list <- list(N=N,K=K,y=renewal,X=X)
fit <- stan(model_code=probit,
            data=data_list,
            warmup=2000,
            iter=4000,
            chains=2)

#RESULTS
print(fit)
fitlist <- extract(fit)
#Convergence plots for coefficients
par(mfrow=c(3,3))
plot(fitlist$beta[,1],type="l",xlab="",ylab="Intercept")
plot(fitlist$beta[,2],type="l",xlab="",ylab="Gender Coefficient")
plot(fitlist$beta[,3],type="l",xlab="",ylab="Fotography Coefficient")
plot(fitlist$beta[,4],type="l",xlab="",ylab="Sculpture Coefficient")
plot(fitlist$beta[,5],type="l",xlab="",ylab="Digital content Coefficient")
plot(fitlist$beta[,6],type="l",xlab="",ylab="Months since login")
plot(fitlist$beta[,7],type="l",xlab="",ylab="Satisfaction Rating")
#Posterior distributions of model coefficients
par(mfrow=c(2,3))  
par(bg="white", fg="black", 
    col.lab="black", col.axis="black", 
    col.main="black", lwd=1)
#Gender
param <- fitlist$beta[,2]
min <- min(0,min(param))
max <- max(0,max(param))
hist(param,breaks=seq(min-0.05,max+0.05,0.05),main="Female",xlab="", ylab="",yaxt='n')
axis(1, lab=T , lwd=2)
abline(v=beta1, col=2)
#PHOTO
param <- fitlist$beta[,3]
min <- min(0,min(param))
max <- max(0,max(param))
hist(param,breaks=seq(min-0.04,max+0.04,0.04),main="Photography",xlab="", ylab="",yaxt='n')
axis(1, lab=T , lwd=2)
abline(v=beta2, col=2)
#SCULPTURE
param <- fitlist$beta[,4]
min <- min(0,min(param))
max <- max(0,max(param))
hist(param,breaks=seq(min-0.03,max+0.03,0.03),main="Sculpture",xlab="", ylab="",yaxt='n')
axis(1, lab=T , lwd=2)
abline(v=beta3, col=2)
#ONLINE CONTENT
param <- fitlist$beta[,5]
min <- min(0,min(param))
max <- max(0,max(param))
hist(param,breaks=seq(min-0.02,max+0.02,0.02),main="Downloads",xlab="", ylab="",yaxt='n')
axis(1, lab=T , lwd=2)
abline(v=beta4, col=2)
#VISIT
param <- fitlist$beta[,6]
min <- min(0,min(param))
max <- max(0,max(param))
hist(param,breaks=seq(min-0.01,max+0.01,0.01),main="Months since visit",xlab="", ylab="",yaxt='n')
axis(1, lab=T , lwd=2)
abline(v=beta5, col=2)
#VISIT
param <- fitlist$beta[,7]
min <- min(0,min(param))
max <- max(0,max(param))
hist(param,breaks=seq(min-0.01,max+0.01,0.01),main="Satisfaction Rating",xlab="", ylab="",yaxt='n')
axis(1, lab=T , lwd=2)
abline(v=beta6, col=2)