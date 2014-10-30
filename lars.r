library("matrixStats")

lars <- function() {
    rawdata <- read.table("diabetes.txt", header=TRUE)
    datmat <- data.matrix(rawdata)
    Ys <- as.matrix(datmat[,11])
    Xs <- as.matrix(datmat[,1:10])
    dimXs <- dim(Xs)  
    dimYs <- dim(Ys)
    N <- dimXs[1]
    P <- dimXs[2]
    meanXs <- colMeans(Xs)
    varXs <- colVars(Xs)
    regularXs = (Xs-meanXs)/(sqrt(N*varXs))
    meanYs <- colMeans(Ys)
    varYs <- colVars(Ys)
    regularYs <- (Ys-meanYs)/(1.0)
    AFullSet <- seq(1, P)
    ASet <- c()
    AcSet <- seq(1, P)
    UaHat <- matrix(0, N, 1)
    Beta = matrix(0, N, P)
    Xa = matrix(0, N, 0)
    sign = rep(1, P)

}
lars()
