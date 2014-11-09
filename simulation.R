library(MASS)

#defination
n <- 20
p <- 8
rou <- 0.5
d <- 1
b <- matrix(c(3,1.5,0,0,2,0,0,0),ncol=1)
# b <- matrix(c(0.85,0.85,0.85,0.85,0.85,0.85,0.85,0.85),ncol=1)
# b <- matrix(c(5,0,0,0,0,0,0,0),ncol=1)
# b <- matrix(c(rep(0,10),sample(10:30,90,replace = TRUE)/10),ncol = 1)


set.seed(12345)
t <- 0
iterate <- 50
eps = .Machine$double.eps

# build parameter
coef <- matrix(0,nrow = iterate, ncol = p)
Cp <- RSS <- matrix(0, nrow = iterate,ncol = 1)
num <- c(1:p)
model <- list()
distinmodel <- list()
mean <- rep(0,p)
sigma <- diag(p)
# sigma <- matrix(0, nrow = p, ncol = p)
# for (i in 1:p)
# {
# 	for(j in i:p)
# 	{
# 		temp <- abs(i - j)
# 		sigma[j,i] <- sigma[i,j] <-rou^temp 
# 	}
# }
# for (i in 1:p)
# {
# 	sigma[i,i] <- 2
# 	for(j in i:p)
# 	{
# 		sigma[j,i] <- sigma[i,j] <-0.5 
# 	}
# }
# compute matrix coef,model,Cp,RSS
while(t < iterate)
{
	t <- t + 1
	#e <- 0
	e <- matrix(rnorm(n,mean = 0,sd = d), ncol = 1)
	#x <- mvrnorm(n,mean,sigma)
	x <- mvrnorm(n,mean,sigma)
    # z0 <- matrix(rnorm(p,mean = 0,sd = 1), nrow = 1)
    # x <- z + rep(1,n) %*% z0
	y <- x %*% b + e
	f <- lasso(x,y,trace = FALSE)
	
	minCp <- f$Cp  # f$Cp - F$len.act abs
	mintemp <- min(minCp) 
	logitemp <- minCp == mintemp
	coef[t,] <- f$beta[logitemp]
	zerotemp <- coef[t,] > eps
	model[[t]] <- num[zerotemp]
	Cp[t,] <- f$Cp[logitemp]
	RSS[t,] <- f$RSS[logitemp]
}

# compute model accurate proportion distinmodel, prop
distinmodel <- unique(model)
l <- length(distinmodel)
prop <- no.of0 <- rep(0,l)
for (i in 1:l)
{
	li <- length(distinmodel[[i]])
	logitemp <- rep(0,iterate)
	for(j in i:iterate)
	{
		lj <- length(model[[j]])
		if(lj == li)
		{
			if(sum(model[[j]] == distinmodel[[i]]) == li)
			{
				logitemp[j] <- 1
			}			
		}
	}
	prop[i] <- sum(logitemp)/iterate
	# no.of0[i] <- p - li
}
q <- seq(l)
maxprop <- max(prop)
coor <- q[prop == maxprop]
# aver0 <- no.of0 %*% prop

y1 <- y2 <- y3 <- y4 <- 0

for(i in 1:iterate)
{
	temp <- length(model[[i]])
	if(temp == 95) y1 <- y1 + 1
	else if(temp == 96) y2 <- y2 + 1
	else if(temp == 97) y3 <- y3 + 1
	else if(temp == 98) y4 <- y4 + 1
}

prop1 <- y1/iterate
prop2 <- y2/iterate
prop3 <- y3/iterate
prop4 <- y4/iterate