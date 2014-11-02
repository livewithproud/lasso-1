lasso <- function(x , y, eps = .Machine$double.eps, max.steps)
{
  call <- match.call()
  ### 参数
  nm <- dim(x)
  n <- nm[1]
  m <- nm[2]
  one <- rep(1,n)
  im <- inactive <- seq(m)
  vn <- dimnames(x)[[2]]
  
  ### x的中心化，标准化   y 的中心化
  meanx <- drop(one %*% x)/n
  x <- scale(x, meanx, FALSE)
  normx <- sqrt(drop(one %*% (x^2)))
  # 无效变量
  nosignal<-normx/sqrt(n) < eps
  if(any(nosignal))# ignore variables with too small a variance
  {
    ignores<-im[nosignal]
    inactive<-im[-ignores]
    normx[nosignal]<-eps*sqrt(n)
    cat("LARS Step 0 :\t", sum(nosignal), "Variables with Variance < \ eps; dropped for good\n")  #
  }
  else ignores <- NULL
  
  names(normx) <- NULL
  x <- scale(x, FALSE, normx)
  mu <- mean(y)
  y <- drop(y-mu)
 
  
  ### 相关系数向量
  Cvec <- drop(t(y) %*% x)  
  # 参数
  residuals <- y
  if(missing(max.steps)) max.steps <- 8*min(m, n-1)
  beta <- matrix(0, max.steps+1, m)
  first.in <- integer(m) # m个0
  active <- NULL
  actions <- as.list(seq(max.steps))
  drops <- FALSE
  Sign <- NULL
  R <- NULL
  k <- 0
  ### 函数正文
  while((k < max.steps) & length(active) < min(m - length(ignores), n-1)) 
  {
    action <- NULL
    k <- k+1
    C <- Cvec[inactive]
    Cmax <- max(abs(C))
    if(!any(drops)){  # 如果drop==FALSE
      new <- abs(C) >= Cmax - eps
      C <- C[!new]
      #cat("corr",C,"/t/n")
      new <- inactive[new]
      for(inew in new) { # active Sign action
        R <- updateR(x[,inew], R, x[,active])
        if(attr(R, "rank") == length(active)) {
          nR <- seq(length(active)) 
          R <- R[nR, nR, drop=FALSE]
          attr(R, "rank") <- length(active)
          ignores <- c(ignores, inew)
          action <- c(action, -inew)
          cat("LASSO Steps", k, ":\t Variable", inew, "\tcollinear; dropped for good\n")
        }
        else{
          if(first.in[inew] == 0) first.in[inew] <- k
          active <- c(active, inew)
          Sign <- c(Sign, sign(Cvec[inew]))
          action <- c(action, inew)
          cat("\nLASSO Steps", k, ":\t Variable", inew, "\tadded\n")
        }      
      }
    }
    else action <- -dropid
    Gi1 <- backsolve(R, backsolve(R, Sign, ncol(R), transpose=TRUE))
    A <- 1/sqrt(sum(Gi1 * Sign))
    w <- A*Gi1  #  with direction
    u <- drop(x[,active, drop=FALSE] %*% w)
    if(length(active) >= min(n-1, m=length(ignores))) {
      gamhat <- Cmax/A
    }
    else {
      a <- drop(u %*% x[, -c(active, ignores), drop = FALSE])
      gam <- c((Cmax - C)/(A-a), (Cmax + C)/(A+a))
      gamhat <- min(gam[gam > eps], Cmax/A)
    }
    #cat(u,a,"\t\n")
    # 是否减去变量
    dropid <- NULL
    b1 <- beta[k, active]
    z1 <- -b1/w
    # cat("Sign",Sign,"\t\n")
    # cat("w",w,"\t\n")
    # cat("b1",b1,"\t\n")
    # cat("z1",z1,"\t\n")
    zmin <- min(z1[z1 > eps], gamhat)
    if(zmin < gamhat) {
      gamhat <- zmin
      drops <- z1 == zmin
    }
    else drops <- FALSE
    # 系数
    beta[k + 1, ] <- beta[k, ]
    beta[k + 1, active] <- beta[k + 1, active] + gamhat * w
    # residuals
    cat("residuals",residuals,)
    residuals <- residuals - gamhat * u
    cat("residuals",residuals,)
    Cvec <- drop(t(residuals) %*% x)
    if(any(drops)) {
      dropid <- seq(drops)[drops]
      for(id in rev(dropid)) {
        cat("Lasso Step", k+1, ":\t Variable", active[id], "\tdropped\n")
        R <- downdateR(R, id)
      }
      dropid <- active[drops]
      active <- active[!drops]
      Sign <- Sign[!drops]
    }
    if(!is.null(vn)) # 如果列名称不为空
      names(action) <- vn[abs(action)]
    actions[[k]] <- action
    inactive <- im[ - c(active,ignores)]
  }
  beta <- beta[seq(k + 1), ]
  dimnames(beta) <- list(paste(0:k), vn)
  # RSS, R2,Cp
  cat("Computing residuals, RSS etc .....\n")
  residuals <- y - x %*% t(beta)
  beta <- scale(beta, FALSE, normx)
  RSS <- apply(residuals^2, 2, sum)
  R2 <- 1 - RSS/RSS[1]
  Cp <- ((n - m - 1) * RSS)/rev(RSS)[1] - n + 2 * seq(k + 1) ####### -m or -k????
  object <- list(A = A)
  ### R2 = R2, RSS = RSS, Cp = Cp,  beta = beta
  ### actions = actions[seq(k)],entry = first.in,mu = mu, normx = normx, meanx = meanx, x=x, y=y
  object
}

updateR <- function(xnew, R = NULL, xold, eps = .Machine$double.eps)
{
  ###Gram argument determines the nature of xnew and xold
  xtx <- sum(xnew^2)
  norm.xnew <- sqrt(xtx)
  if(is.null(R)) {
    R <- matrix(norm.xnew, 1, 1)
    attr(R, "rank") <- 1
    return(R)
  }
  Xtx <- drop(t(xnew) %*% xold)
  r <- backsolve(R, Xtx, transpose=TRUE)
  rpp <- norm.xnew^2 - sum(r^2)
  rank <- attr(R, "rank")  ### check if R is machine singular
  if(rpp <= eps)
    rpp <- eps
  else {
    rpp <- sqrt(rpp)
    rank <- rank + 1
  }
  R <- cbind(rbind(R, 0), c(r, rpp))
  attr(R, "rank") <- rank
  R
}
downdateR <- function(R, k = p)
{
  p <- dim(R)[1]
  if(p == 1)
    return(NULL)
  R <- delcol(R, rep(1, p), k)[ - p,  , drop = FALSE]
  attr(R, "rank") <- p - 1
  R  # Built-in Splus utility
}

delcol <-function(r, z, k = p)
{
  p <- dim(r)[1]
  r <- r[,  - k, drop = FALSE]
  z <- as.matrix(z)
  dz <- dim(z)
  storage.mode(r) <- storage.mode(z) <- "double"
  .Fortran("delcol",
           r,
           as.integer(p),
           as.integer(k),
           z,
           as.integer(dz[1]),
           as.integer(dz[2]))[[1]]
}