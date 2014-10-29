import numpy as np

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
#    print np.linalg.norm(regularXs, axis=0)
    print regularXs.shape
    meanYs = np.mean(ys)
    varYs = np.var(ys)
    regularYs = np.asmatrix(np.divide(np.subtract(ys, meanYs), np.sqrt(N*varYs)))
#    print np.mean(regularYs, axis=0)
#    print np.linalg.norm(regularYs, axis=0)
    print regularYs.shape
    corr = np.transpose(regularXs)*regularYs
    absCorr = np.absolute(corr)
    print corr
    (maxCorrIdx,_) = np.unravel_index(absCorr.argmax(), absCorr.shape) 
    sj = 1
    if corr[maxCorrIdx] < 0:
        sj = -1
    Xa = regularXs[:,maxCorrIdx]
     

xs, ys = input()
lars(xs, ys)
#lars([[1,2,3],[4,5,6]], [1,1])
