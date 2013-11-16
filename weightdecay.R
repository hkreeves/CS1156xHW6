##
## EdX - Machine Learning
## HW6
##
## Author: Kai He
##

## Q2-Q6 Overfitting and Regularization With Weight Decay
# load in-sample and out-of-sample data, schema = (x1, x2, y)
din <- read.table("http://work.caltech.edu/data/in.dta", 
			col.names=c("x1","x2","y"))
dout <- read.table("http://work.caltech.edu/data/out.dta",
			col.names=c("x1","x2","y"))

# transform from (x1, x2) space to z space
# z = (1, x1, x2, x1^2, x2^2, x1*x2, |x1-x2|, |x1+x2|) 
nonlinTrans <- function(X)
{
	Z <- apply(X, 1, function(x) c(1, x[1], x[2], x[1]^2, x[2]^2, x[1]*x[2], abs(x[1]-x[2]),
		abs(x[1]+x[2])))
	t(Z)
}
	
Zin <- nonlinTrans(din[,1:2])
Yin <- din[,3]
Zout <- nonlinTrans(dout[,1:2])
Yout <- dout[,3]

## Traditional linear regression
linReg <- function(X, Y)
{
	w <- solve(t(X) %*% X) %*% t(X) %*% Y
	w
}

## linear regression with weight decay
wd.linReg <- function(X, Y, lambda)
{
	d <- dim(X)[2]
	w.reg <- solve(t(X) %*% X + lambda*diag(d)) %*% t(X) %*% Y 
	w.reg
}

## predict using weight w
predict <- function(X, w)
{
	sign(X %*% w)
}

testlinReg <- function(lambda=0, plot.on=F)
{
	# inspect the data
	if(plot.on)
	{
		par(mfrow=c(1,2))
		plot(din[,1:2], col=factor(din[,3]))
		plot(dout[,1:2], col=factor(dout[,3]))
		par(mfrow=c(1,1))
	}	

	# get weight from in-sample data
	if(lambda==0)
		w <- linReg(Zin, Yin)
	else
		w <- wd.linReg(Zin, Yin, lambda)

	# predict
	pYin <- predict(Zin, w)
	pYout <- predict(Zout, w)
	
	# evaluate
	ein <- mean(Yin!=pYin)
	eout <- mean(Yout!=pYout)

	c(ein, eout)
}
	