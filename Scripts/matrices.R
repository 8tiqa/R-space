#examples of creating matrices

seq1 <- seq(1:6)
mat1 <- matrix(seq1, 2) 
mat1

#filling the matrix by rows
mat2 <- matrix(seq1, 2, byrow=T)
mat2

#since we are using the ncol argument out of order
#(it is the third argument) we have to specify that 
#hence the need to specify ncol=2 and not just the value 2
mat3 <- matrix(seq1, ncol=2)
mat3

#same as writing which uses the nrow and ncol arguments in 
#the order they appear in the function
mat4 <- matrix(seq1, 3, 2)
mat4

#creating a matrix of 20 numbers from a standard normal dist.
mat5 <- matrix(rnorm(20), 4)
mat5

#appending matrices using the cbind and rbind function

#appending v1 to mat5
v1 <- c(1, 1, 2, 2)
mat6 <- cbind(mat5, v1)
mat6

v2 <- c(1:6)
mat7 <- rbind(mat6, v2)
mat7

#determining the dimensions of a matrix
dim(mat7)

#removing names of rows and columns
#the first NULL refers to all row names, the second to all column names 
dimnames(mat7) <- list(NULL, NULL)
mat7

#accessing rows, columns or elements in the matrix
#matrix_name[row#, col#]

mat7[1, 6]

#to access an entire row leave the column number blank
mat7[1, ]

#to access an entire column leave the row number blank
mat7[ ,6]

#matrix operations
mat8 <- matrix(1:6, 2)
mat8
mat9 <- matrix(c(rep(1, 3), rep(2, 3)), 2, byrow=T)
mat9

#addition
mat9+mat8
mat9+3

#subtraction
mat8 - mat9

#inverse
solve(mat8[, 2:3])

#transpose
t(mat9)

#multiplication
#we transpose mat8 since the dimension of the matrices have to match
#dim(2, 3) times dim(3, 2)
mat8%*%t(mat9)

#element-wise multiplication
mat8*mat9
mat8*4

#division
mat8/mat9
mat9/2

#using submatrices from the same matrix in computations
mat8[, 1:2]
mat8[, 2:3]
mat8[, 1:2] / mat8[, 2:3]

#linear regression example using the hsb2 data set
#regressing write on math, science, socst and female

#input the hsb2 data set
#hsb2 <- read.table("c:\\hsb.txt")

y <- matrix(hsb2$write, ncol=1)
y[1:10, 1]
x <- as.matrix( cbind(1, hsb2$math, hsb2$science, hsb2$socst, hsb2$female))
x[1:10, ]
n <- nrow(x)
p <- ncol(x)

#parameter estimates
beta.hat <- solve(t(x) %*% x) %*% t(x) %*% y
beta.hat

#obtain predicted values
y.hat <- x %*% beta.hat
y.hat[1:5, 1]
y[1:5, 1]

#obtaining the variance, residual standard error and df's
sigma2 <- sum( (y-y.hat)^2) / (n-p)
sqrt(sigma2)
n-p

#obtaining the standard errors, t-values and p-values for estimates
v <- solve(t(x)%*%x)*sigma2
sqrt(diag(v))
t.values <- beta.hat/sqrt(diag(v))
t.values
2*(1- pt(abs(t.values), n-p) )

#checking that we got the correct results
ex1 <- lm(write ~ math+science+socst+female, hsb2) 
summary(ex1)