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
    for ( step in 1:P ) {
        corr = t(Xs)%*%(regularYs-UaHat)
        absCorr = abs(corr)
        cHat = max(absCorr)
        maxCorrIdx = 1 
        maxCorr = -2
        for ( ci in AcSet ) {
            if ( absCorr[ci,] > maxCorr ) {
                maxCorr = absCorr[ci,]
                maxCorrIdx = ci
            }
        }
        ASet = union(ASet, c(maxCorrIdx))
        if ( corr[maxCorrIdx,] < 0 ) {
            sign[maxCorrIdx] = -1
        }
        Xa = cbind(Xa, sign[maxCorrIdx]*matrix(regularXs[,maxCorrIdx]))
        XaTXaInv = solve(t(Xa)%*%Xa)
        p = length(ASet)
        Ia = matrix(1, p, 1)
        Aa = (t(Ia)%*%XaTXaInv%*%Ia)[1,1]^(-0.5)
        OmigaA = Aa*XaTXaInv%*%Ia
        Ua = Aa*Xa%*%XaTXaInv%*%Ia
        AcSet = setdiff(AFullSet, ASet)
        a = t(regularXs)%*%Ua 
        GamaHat = 0
        minj = -1
        for ( j in AcSet ) {
            val = (cHat-corr[j])/(Aa-a[j,1])
            if ( val > 0 && (minj == -1 || val < GamaHat) ) {
                GamaHat = val
                minj = j
            }
            val = (cHat+corr[j])/(Aa+a[j,1])
            if ( val > 0 && (minj == -1 || val < GamaHat) ) {
                GamaHat = val
                minj = j
            }
        }
        UaHat = UaHat+GamaHat%*%Ua

    }
}
lars()
