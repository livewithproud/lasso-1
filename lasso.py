import numpy as np

def input():
    rawdata = []
    xs = []
    ys = []
    with open("./diabetes.txt", "r") as dfile:
        next(dfile)
        for line in dfile:
            rawdata.append([float(d) for d in line.split("\t") ])
        for rawd in rawdata:
            xs.append(rawd[0:len(rawd)-1])
            ys.append(rawd[len(rawd)-1])
    return xs, ys

def LASSO(xs, ys):
    dim = len(xs[0])

    meanXs = np.mean(xs, axis=0)
    varXs = np.var(xs, axis=0)
    sVarXs = np.sqrt(varXs)
    meanYs = np.mean(ys)
    varYs = np.var(ys)
    sVarYs = np.sqrt(varYs)

    normalXs = [ [(x[i]-meanXs[i])/sVarXs[i] for i in range(0, len(x)) ] for x in xs ]
    normalYs = [ y - meanYs for y in ys ]/sVarYs
    print np.var(normalYs)
    print np.mean(normalYs)
    print np.var(normalXs, axis=0)
    print np.mean(normalXs, axis=0)

    w = np.random.rand(dim)
    
    lamb = 0.3
    yita = 0.1
    deta = np.asarray([0.0 for i in range(0, dim)])
    for t in range(0,3):
        for nX in normalXs:
#            h = normalYs - nX*w
#            cost = sum(pow(h, 2))+lamb*sum(abs(w))
            for di in range(0, dim):
                deta[di] = (normalYs[di]-w[di]*nX[di])*(-nX[di])
                if w[di] > 0:
                    deta[di] = deta[di] + lamb 
                elif w[di] < 0:
                    deta[di] = deta[di] - lamb
                else:
                    deta[di] = 0
                w[di] = w[di] - deta[di]*yita
    print w
    for i in range(40):
        print sum(w*normalXs[i]),normalYs[i]

xs, ys = input()
        
LASSO(xs, ys)

