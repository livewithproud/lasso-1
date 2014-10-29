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
    print regularYs.shape
    AFullSet = set(range(0,P)) 
    ASet = set([]) 
    AcSet = set(range(0,P))
    UaHat = np.asmatrix([[0] for i in range(N)])
    Beta = [None for i in range(P)]
    for step in range(0,P):
        corr = np.transpose(regularXs)*(regularYs-UaHat)
        absCorr = np.absolute(corr)
        cHat = max(absCorr)
    #    print corr
        maxCorrIdx = 0
        maxCorr = -2.0
        for ci in AcSet:
            if absCorr[ci] > maxCorr:
                maxCorr = absCorr[ci]
                maxCorrIdx = ci
#        (maxCorrIdx,_) = np.unravel_index(absCorr.argmax(), absCorr.shape) 
        ASet.add(maxCorrIdx)
        print "add", maxCorrIdx +1
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
        AcSet = AFullSet - ASet
        a = np.transpose(regularXs)*Ua
        GamaHat = 0
        minj = None 
        for j in AcSet:
            val = (cHat-corr[j])/(Aa-a[j,0])
            if val > 0 and (minj == None or val < GamaHat):
                GamaHat = val[0,0]
                minj = j
            val = (cHat+corr[j])/(Aa+a[j,0])
            if val > 0 and (minj == None or val < GamaHat):
                GamaHat = val[0,0]
                minj = j
        UaHat = UaHat+GamaHat*Ua
        (Beta[step],_,_,_) = alg.lstsq(regularXs,UaHat)
        print Beta[step]
     
xs, ys = input()
lars(xs, ys)
#lars(np.array([[101,157,4,87],[87,183,3,69],[93,156,4,85]]), np.array([[151],[75],[141]]))
