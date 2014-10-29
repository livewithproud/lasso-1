import numpy as np
import numpy.linalg as alg

def input():
    rawdata = []
    xs = []
    ys = []
    with open("./diabetes.txt", "r") as dfile:
        next(dfile)
        for line in dfile:
            rawdata.append([float(d) for d in line.split("\t") ])
        idx = 0
        for rawd in rawdata:
            xs.append(rawd[0:len(rawd)-1])
            ys.append(rawd[len(rawd)-1])
            idx = idx + 1
#            if idx > 3:
#                break
    return np.asarray(xs), np.transpose(np.asarray([ys]))

def lars(xs, ys):
    P = len(xs[0])
    N = len(ys)
    print P, N

# Normal        
    meanXs = np.mean(xs, axis=0)
    varXs = np.var(xs, axis=0)
    regularXs = np.asmatrix(np.divide(np.subtract(xs, meanXs), np.sqrt(N*varXs)))
#    print np.mean(regularXs, axis=0)
#    print alg.norm(regularXs, axis=0)
    print regularXs.shape
    meanYs = np.mean(ys)
    varYs = np.var(ys)
    regularYs = np.asmatrix(np.divide(np.subtract(ys, meanYs), np.sqrt(N*varYs)))
#    print np.mean(regularYs, axis=0)
#    print alg.norm(regularYs, axis=0)
    print regularXs
    print regularYs
    print regularYs.shape
    UaHat = np.asmatrix([[0] for i in range(N)])
    corr = np.transpose(regularXs)*(regularYs-UaHat)
    absCorr = np.absolute(corr)
    print corr
    (maxCorrIdx,_) = np.unravel_index(absCorr.argmax(), absCorr.shape) 
    sj = 1
    if corr[maxCorrIdx] < 0:
        sj = -1
    Xa = np.asmatrix([]).reshape(N,0)
    Xa = np.append(Xa, sj*regularXs[:,maxCorrIdx], axis=1)
    XaTXaInv = alg.inv(np.transpose(Xa)*Xa)
    (_,p) = Xa.shape
    Ia = np.matrix([[1] for i in range(p)])
    Aa = np.power(Ia*XaTXaInv*Ia, -0.5)
    Ua = Xa*XaTXaInv*Aa*Ia

#xs, ys = input()
#lars(xs, ys)
lars(np.array([[101,157,4,87],[87,183,3,69],[93,156,4,85]]), np.array([[151],[75],[141]]))
