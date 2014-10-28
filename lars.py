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
            if idx > 3:
                break
    return xs, ys

def lars(xs, ys):
    P = len(xs[0])
    N = len(ys)

# Normal        
    print xs
    meanXs = np.mean(xs, axis=0)
    normXs = np.linalg.norm(xs, axis=0) 
    regularXs = np.divide(np.subtract(xs, meanXs), normXs)
    print np.mean(regularXs, axis=0)
    print np.linalg.norm(regularXs, axis=0)

xs, ys = input()
lars(xs, ys)

