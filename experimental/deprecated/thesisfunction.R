library(expm)
library(dplyr)
library(caret)
library(MASS)

# Function for Penalized Orthogonal Iteration with Coordinate-wise Sparsity --------

POI.coord.fun <- function(A,B,d,lambda){
  
  p <- dim(A)[1]
  delta.mat <- matrix(0,p,d)
  a.bold <- matrix(0,p,d)
  
  Z <- matrix(0,p,d)
  Z.new <- matrix(0,p,d)
  Q <- qr.Q(qr(Z))
  Q.new <- qr.Q(qr(Z.new))
  
  for(i in 1:p){
    for (j in 1:10000){
      delta.mat[i,] <- A[i,] %*% Q
      a.bold[i,] <- delta.mat[i,]-B[i,]%*%Z+B[i,i]*Z[i,]
      
      soft.obj <- 1-lambda/l2norm(a.bold[i,])
      if (soft.obj>0) {Z.new[i,] <- 1/B[i,i]*soft.obj*a.bold[i,]}
      if (soft.obj<=0) {Z.new[i,] <- 0}
      
      if (sqrt(sum((Z.new[i,]-Z[i,])^2))<1e-05) break
      Z[i,] <- Z.new[i,]
      Q <- qr.Q(qr(Z))
      
    }
    
  }
  
  return(Q)
}

# Function for Penalized Orthogonal Iteration with Element-wise Sparsity  -----

POI.el.fun <- function(A,B,d,lambda){
  
  p <- dim(A)[1]

  Z <- matrix(0,p,d)
  Z.new <- matrix(0,p,d)
  Q <- qr.Q(qr(Z))
  Q.new <- qr.Q(qr(Z.new))
  
  for(i in 1:p){
    for (j in 1:d){
      for (r in 1:10000){
        soft.obj <- abs(A[,i] %*% Q[,j] - B[,i] %*% Z[,j] + B[i,i]*Z[i,j])-lambda
        if (soft.obj>0) {Z.new[i,j] <- sign(soft.obj)*soft.obj/B[i,i]}
        if (soft.obj<=0) {Z.new[i,j] <- 0}
        
        if (sqrt(sum((Z.new[i,]-Z[i,])^2))<1e-05) break
        Z[i,j] <- Z.new[i,j]
        Q <- qr.Q(qr(Z))
      }
      
    }
    
  }
  
  return(Q)
}


#### Cross Validation function for group LASSO Parameter -----

# Cross Validation for Sliced Inverse Regression

POI.coord.CV <- function(A.train,A.val,B.train,B.val,d){
  
  CV <- c()
  lambda.max <- max(c(max(apply(A.train,1,l2norm)),max(apply(A.val,1,l2norm))))
  lambda.grid <- log(seq(from=exp(lambda.max), to=exp(0), length.out=100))
  p <- dim(A.train)[1]
  for (k in 1:length(lambda.grid)){
    Z <- matrix(0,p,d)
    Q <- qr.Q(qr(Z))
    delta.mat <- matrix(0,p,d)
    a.bold <- matrix(0,p,d)
    Z.new <- matrix(0,p,d)
    
    for (i in 1:p){
      for (j in 1:1000){
        delta.mat[i,] <- A.train[i,]%*%Q
        a.bold[i,] <- delta.mat[i,]-B.train[i,]%*%Z+B.train[i,i]*Z[i,]
        
        soft.obj <- 1-lambda.grid[k]/sqrt(sum(a.bold[i,]^2))
        if (soft.obj>0) {Z.new[i,] <- 1/B.train[i,i]*soft.obj*a.bold[i,]}
        if (soft.obj<=0) {Z.new[i,] <- 0}
        
        if (sqrt(sum((Z.new[i,]-Z[i,])^2))<1e-05) break
        Z[i,] <- Z.new[i,]
        Q <- qr.Q(qr(Z))
        
      }
      
    }
    
    Q.new <- qr.Q(qr(Z.new))
    CV[k] <- sum(diag(solve(t(Q.new)%*%B.val%*%Q.new)%*%t(Q.new)%*%A.val%*%Q.new))
    
  }
  return(CV)
  
}


cordsp.sir.cv <- function(dataset,yindex,d,k=10,H){
  A <- SIRcovfun(dataset,yindex,H)
  lambda.max <- max(apply(A,1,l2norm))
  lambda.grid <- log(seq(from=exp(lambda.max),to=exp(0),length.out=100))
  
  p <- dim(A)[1]
  
  cv <- list()
  
  for (i in 1:k){
    cat("computing",i,"th fold","\n")
    
    intrain <- createFolds(dataset[,yindex],k,returnTrain=TRUE)[[i]]
    
    A.train <- SIRcovfun(dataset[intrain,],yindex,H)
    B.train <- cov(dataset[intrain,-yindex])
    A.val <- SIRcovfun(dataset[-intrain,],yindex,H)
    B.val <- cov(dataset[-intrain,-yindex])
    
    cv[[i]] <- rep(0,length(lambda.grid))
    for(j in 1:length(lambda.grid)){
      
      Z <- matrix(0,p,d)
      Z.new <- matrix(0,p,d)
      Q <- qr.Q(qr(Z))
      Q.new <- qr.Q(qr(Z.new))
      delta.mat <- matrix(0,p,d)
      a.bold <- matrix(0,p,d)
      
      
      for (l in 1:p){
        for (m in 1:1000){
          delta.mat[l,] <- A.train[l,] %*% Q
          a.bold[l,] <- delta.mat[l,]-B.train[l,]%*%Z +B.train[l,l]*Z[l,]
          
          soft.obj <- 1-lambda.grid[j]/l2norm(a.bold[l,])
          if (soft.obj>0) {Z.new[l,] <- 1/B.train[l,l]*soft.obj*a.bold[l,]}
          if (soft.obj<=0) {Z.new[l,] <- 0}
          
          if (sqrt(sum((Z.new[l,]-Z[l,])^2))<1e-05) break
          
          Z[l,] <- Z.new[l,]
          Q <- qr.Q(qr(Z))
          
        }
      }
      
      Q.new <- qr.Q(qr(Z.new))
      cv[[i]][j] <- sum(diag(solve(t(Q.new)%*%B.val%*%Q.new)%*%t(Q.new)%*%A.val%*%Q.new))  
      
      
    }
    
    
    
  }
  
  
  return(which.max(Reduce('+',cv)/k))
  
}





# Cross Validation for SAVE

cordsp.save.cv <- function(dataset,yindex,d,k=10){
  
  
  A <- sqrtm(cov(dataset[,-yindex])) %*% save.mat.fun(dataset,yindex)  %*% sqrtm(cov(dataset[,-yindex]))
  lambda.max <- max(apply(A,1,l2norm))
  lambda.grid <- log(seq(from=exp(lambda.max),to=exp(0),length.out=100))
  p <- dim(A)[1]
  
  cv <- list()
  
  for (i in 1:k){
    cat("computing",i,"th fold","\n")
    
    intrain <- createFolds(dataset[,yindex],k,returnTrain=TRUE)[[i]]
    
    Sx2.train <- sqrtm(cov(dataset[intrain,-yindex]))
    Sx2.val <- sqrtm(cov(dataset[-intrain,-yindex]))
    
    A.train <- Sx2.train %*% save.mat.fun(dataset[intrain,],yindex) %*% Sx2.train
    B.train <- cov(dataset[intrain,-yindex])
    A.val <- Sx2.val %*% save.mat.fun(dataset[-intrain,],yindex) %*% Sx2.val
    B.val <- cov(dataset[-intrain,-yindex])
    
    cv[[i]] <- rep(0,length(lambda.grid))
    for(j in 1:length(lambda.grid)){
      
      Z <- matrix(0,p,d)
      Z.new <- matrix(0,p,d)
      Q <- qr.Q(qr(Z))
      Q.new <- qr.Q(qr(Z.new))
      delta.mat <- matrix(0,p,d)
      a.bold <- matrix(0,p,d)
      
      
      for (l in 1:p){
        for (m in 1:1000){
          delta.mat[l,] <- A.train[l,] %*% Q
          a.bold[l,] <- delta.mat[l,]-B.train[l,]%*%Z +B.train[l,l]*Z[l,]
          
          soft.obj <- 1-lambda.grid[j]/l2norm(a.bold[l,])
          if (soft.obj>0) {Z.new[l,] <- 1/B.train[l,l]*soft.obj*a.bold[l,]}
          if (soft.obj<=0) {Z.new[l,] <- 0}
          
          if (sqrt(sum((Z.new[l,]-Z[l,])^2))<1e-05) break
          
          Z[l,] <- Z.new[l,]
          Q <- qr.Q(qr(Z))
          
        }
      }
      
      Q.new <- qr.Q(qr(Z.new))
      cv[[i]][j] <- sum(diag(solve(t(Q.new)%*%B.val%*%Q.new)%*%t(Q.new)%*%A.val%*%Q.new))  
      
      
    }
    
    
    
  }
  
  
  return(which.max(Reduce('+',cv)/k))
  
  
  
}

# element-wise sparsity case: cross validation

elsp.save.cv <- function(dataset,yindex,d,k=10){
  
  
  A <- save.mat.fun.easy(dataset,yindex)
  lambda.max <- max(A)
  lambda.grid <- log(seq(from=exp(lambda.max),to=exp(0),length.out=100))
  p <- dim(A)[1]
  
  cv <- list()
  
  for (m in 1:k){
    cat("computing",m,"th fold","\n")
    set.seed(m)
    intrain <- sample(nrow(dataset),9*nrow(dataset)/10)

    
    A.train <- save.mat.fun.easy(dataset[intrain,],yindex)
    B.train <- cov(dataset[intrain,-yindex])
    A.val <- save.mat.fun.easy(dataset[-intrain,],yindex)
    B.val <- cov(dataset[-intrain,-yindex])
    
    cv[[m]] <- rep(0,length(lambda.grid))
    for(l in 1:length(lambda.grid)){
      
      Z <- matrix(0,p,d)
      Z.new <- matrix(0,p,d)
      Q <- qr.Q(qr(Z))
      Q.new <- qr.Q(qr(Z.new))

      
      for(i in 1:p){
        for (j in 1:d){
          for (r in 1:10000){
            soft.obj <- abs(A.train[,i] %*% Q[,j] - B.train[,i] %*% Z[,j] + B.train[i,i]*Z[i,j])-lambda.grid[l]
            if (soft.obj>0) {Z.new[i,j] <- sign(soft.obj)*soft.obj/B.train[i,i]}
            if (soft.obj<=0) {Z.new[i,j] <- 0}
            
            if (sqrt(sum((Z.new[i,]-Z[i,])^2))<1e-05) break
            Z[i,j] <- Z.new[i,j]
            Q <- qr.Q(qr(Z))
          }
        }
      }
      Q.new <- qr.Q(qr(Z.new))
      cv[[m]][l] <- sum(diag(solve(t(Q.new)%*%B.val%*%Q.new)%*%t(Q.new)%*%A.val%*%Q.new))  
      
    }
  }
  return(which.max(Reduce('+',cv)/k))
}




#### Coordinate-wise sparse SAVE for binary response ------

# SAVE function for binary response


SAVEfun.bin <- function(dataset,yindex){
  y <- dataset[,yindex]
  x <- dataset[,-yindex]
  fs <- c()
  for (i in 1:length(unique(y))){
    fs[i] <- nrow(dataset[y==unique(y)[i],])/nrow(dataset)
    
  }
  
  x <- as.matrix(x)
  z <- (x-t(matrix(apply(x,2,mean),ncol(x),nrow(x)))) %*% sqrtm(solve(cov(x)))
  
  mat <- list()
  for(i in 1:length(unique(y))){
    mat[[i]] <- fs[i]*((diag(1,ncol(z))-cov(z[y==unique(y)[i],]))%*%
                         (diag(1,ncol(z))-cov(z[y==unique(y)[i],])))
  }
  mat <- Reduce('+', mat)
  x11()
  plot(eigen(mat)$values, main="Eigenvalues for M.hat")
  
  cat("Select the appropriate dimension according to the plot(1, 2, 3, ... ): ")
  k <- scan(n=1, quiet=TRUE)
  
  eta <- matrix(0,ncol(z),k)
  
  for (i in 1:k){
    eta[,i] <- eigen(mat)$vectors[,i]
  }
  h <- sqrtm(solve(cov(x))) %*% eta
  return(h)
  
}

# SAVE function for binary response with fixed structural dimension

SAVEfun.bin.d <- function(dataset,yindex,d=2){
  y <- dataset[,yindex]
  x <- dataset[,-yindex]
  fs <- c()
  for (i in 1:length(unique(y))){
    fs[i] <- nrow(dataset[y==unique(y)[i],])/nrow(dataset)
    
  }
  
  x <- as.matrix(x)
  z <- (x-t(matrix(apply(x,2,mean),ncol(x),nrow(x)))) %*% sqrtm(solve(cov(x)))
  
  mat <- list()
  for(i in 1:length(unique(y))){
    mat[[i]] <- fs[i]*((diag(1,ncol(z))-cov(z[y==unique(y)[i],]))%*%
                         (diag(1,ncol(z))-cov(z[y==unique(y)[i],])))
  }
  mat <- Reduce('+', mat)
  k <- d
  
  eta <- matrix(0,ncol(z),k)
  
  for (i in 1:k){
    eta[,i] <- eigen(mat)$vectors[,i]
  }
  
  return(eta)
  
}
# SAVE function 

SAVEfun <- function(dataset, yindex, H){
  X <- as.matrix(dataset[,-yindex])
  y <- dataset[,yindex]
  idx <- 1:nrow(dataset)
  Z <- (X-t(matrix(apply(X,2,mean),ncol(X),nrow(X)))) %*% sqrtm(solve(cov(X)))
  
  V <- matrix(0,ncol(X),ncol(X))
  for (i in 1:H){
    h <- c(1:round(nrow(dataset)/H))+round(nrow(dataset)/H)*(i-1)
    h.idx <- idx[y %in% sort(y)[h]]
    ph <- 1/nrow(dataset)*sum(y %in% sort(y)[h])
    mh <- (diag(1,ncol(Z))-cov(Z[h.idx,]))%*%(diag(1,ncol(Z))-cov(Z[h.idx,]))
    V.h <- ph*mh
    V <- V+V.h
    
  }
  x11()
  plot(eigen(V)$values, main="Eigenvalues for V.hat")
  
  cat("Select the appropriate dimension according to the plot(1, 2, 3, ... ): ")
  k <- scan(n=1, quiet=TRUE)
  
  eta <- matrix(0,ncol(X),k)
  beta <- matrix(0,ncol(X),k)
  for (i in 1:k){
    eta[,i] <- eigen(V)$vectors[,i]
    beta[,i] <- eta[,i] %*% sqrtm(solve(cov(X)))
  }
  return(beta)
  
}
# SAVE function with fixed structural dimension
SAVEfun.d <- function(dataset, yindex, H, d=2){
  X <- as.matrix(dataset[,-yindex])
  y <- dataset[,yindex]
  idx <- 1:nrow(dataset)
  Z <- (X-t(matrix(apply(X,2,mean),ncol(X),nrow(X)))) %*% sqrtm(solve(cov(X)))
  
  V <- matrix(0,ncol(X),ncol(X))
  for (i in 1:H){
    h <- c(1:round(nrow(dataset)/H))+round(nrow(dataset)/H)*(i-1)
    h.idx <- idx[y %in% sort(y)[h]]
    ph <- 1/nrow(dataset)*sum(y %in% sort(y)[h])
    mh <- (diag(1,ncol(Z))-cov(Z[h.idx,]))%*%(diag(1,ncol(Z))-cov(Z[h.idx,]))
    V.h <- ph*mh
    V <- V+V.h
    
  }
  k <- d
  
  eta <- matrix(0,ncol(X),k)
  beta <- matrix(0,ncol(X),k)
  for (i in 1:k){
    eta[,i] <- eigen(V)$vectors[,i]
    beta[,i] <- eta[,i] %*% sqrtm(solve(cov(X)))
  }
  return(eta)
  
}

# GEP setting for SAVE

save.mat.fun <- function(dataset,yindex){
  y <- dataset[,yindex]
  x <- dataset[,-yindex]
  fs <- c()
  for (i in 1:length(unique(y))){
    fs[i] <- nrow(dataset[y==unique(y)[i],])/nrow(dataset)
    
  }
  
  x <- as.matrix(x)
  z <- (x-t(matrix(apply(x,2,mean),ncol(x),nrow(x)))) %*% sqrtm(solve(cov(x)))
  
  mat <- list()
  for(i in 1:length(unique(y))){
    mat[[i]] <- fs[i]*((diag(1,ncol(z))-cov(z[y==unique(y)[i],]))%*%
                         (diag(1,ncol(z))-cov(z[y==unique(y)[i],])))
  }
  mat <- Reduce('+', mat)
  return(mat)
  
}

save.covmat <- function(dataset,yindex,H){
  X <- as.matrix(dataset[,-yindex])
  y <- dataset[,yindex]
  idx <- 1:nrow(dataset)
  Z <- (X-t(matrix(apply(X,2,mean),ncol(X),nrow(X)))) %*% sqrtm(solve(cov(X)))
  
  V <- matrix(0,ncol(X),ncol(X))
  for (i in 1:H){
    h <- c(1:round(nrow(dataset)/H))+round(nrow(dataset)/H)*(i-1)
    h.idx <- idx[y %in% sort(y)[h]]
    ph <- 1/nrow(dataset)*sum(y %in% sort(y)[h])
    mh <- (diag(1,ncol(Z))-cov(Z[h.idx,]))%*%(diag(1,ncol(Z))-cov(Z[h.idx,]))
    V.h <- ph*mh
    V <- V+V.h
    
  }
  return(V)
  
}



# Coordinate-wise sparse SAVE

cordsp.save <- function(dataset,yindex,d,lambda){
  y <- dataset[,yindex]
  x <- dataset[,-yindex]
  fs <- c()
  for (i in 1:length(unique(y))){
    fs[i] <- nrow(dataset[y==unique(y)[i],])/nrow(dataset)
    
  }
  
  x <- as.matrix(x)
  z <- (x-t(matrix(apply(x,2,mean),ncol(x),nrow(x)))) %*% sqrtm(solve(cov(x)))
  
  mat <- list()
  for(i in 1:length(unique(y))){
    mat[[i]] <- fs[i]*((diag(1,ncol(z))-cov(z[y==unique(y)[i],]))%*%
                         (diag(1,ncol(z))-cov(z[y==unique(y)[i],])))
  }
  mat <- Reduce('+', mat)
  
  return(POI.coord.fun(mat,cov(x),d,lambda))
  
}

cdsp.save.con <- function(dataset,yindex,H,d,lambda){
  mat <- save.covmat(dataset,yindex,H)
  return(POI.coord.fun(mat,cov(dataset[,-yindex]),d,lambda))
}


# Element-wise sparse SAVE

elsp.save <- function(dataset,yindex,d,lambda){
  y <- dataset[,yindex]
  x <- dataset[,-yindex]
  fs <- c()
  for (i in 1:length(unique(y))){
    fs[i] <- nrow(dataset[y==unique(y)[i],])/nrow(dataset)
    
  }
  
  x <- as.matrix(x)
  z <- (x-t(matrix(apply(x,2,mean),ncol(x),nrow(x)))) %*% sqrtm(solve(cov(x)))
  
  mat <- list()
  for(i in 1:length(unique(y))){
    mat[[i]] <- fs[i]*((diag(1,ncol(z))-cov(z[y==unique(y)[i],]))%*%
                         (diag(1,ncol(z))-cov(z[y==unique(y)[i],])))
  }
  mat <- Reduce('+', mat)
  
  return(POI.el.fun(mat,cov(x),d,lambda))
  
  
  
}

elsp.save.con <- function(dataset,yindex,H,d,lambda){
  mat <- save.covmat(dataset,yindex,H)
  return(POI.el.fun(mat,cov(dataset[,-yindex]),d,lambda))
}


#### Coordinate-wise Sparse SIR -------

# SIR function

SIRfun <- function(dataset, yindex, H){
  X <- dataset[,-yindex]
  y <- dataset[,yindex]
  idx <- 1:nrow(dataset)
  
  V <- matrix(0,ncol(X),ncol(X))
  for (i in 1:H){
    h <- c(1:round(nrow(dataset)/H))+round(nrow(dataset)/H)*(i-1)
    h.idx <- idx[y %in% sort(y)[h]]
    ph <- 1/nrow(dataset)*sum(y %in% sort(y)[h])
    mh <- apply(X[h.idx,],2,mean)
    V.h <- ph*(mh %*% t(mh))
    V <- V+V.h
    
  }
  x11()
  plot(eigen(V)$values, main="Eigenvalues for V.hat")
  
  cat("Select the appropriate dimension according to the plot(1, 2, 3, ... ): ")
  k <- scan(n=1, quiet=TRUE)
  
  eta <- matrix(0,ncol(X),k)
  beta <- matrix(0,ncol(X),k)
  for (i in 1:k){
    eta[,i] <- eigen(V)$vectors[,i]
    beta[,i] <- eta[,i] %*% solve(cov(X))
  }
  return(beta)
  
}

# SIR function with fixed structural dimension

SIRfun.d <- function(dataset, yindex, H, k=2){
  X <- dataset[,-yindex]
  y <- dataset[,yindex]
  idx <- 1:nrow(dataset)
  
  V <- matrix(0,ncol(X),ncol(X))
  for (i in 1:H){
    h <- c(1:round(nrow(dataset)/H))+round(nrow(dataset)/H)*(i-1)
    h.idx <- idx[y %in% sort(y)[h]]
    ph <- 1/nrow(dataset)*sum(y %in% sort(y)[h])
    mh <- apply(X[h.idx,],2,mean)
    V.h <- ph*(mh %*% t(mh))
    V <- V+V.h
    
  }

  eta <- matrix(0,ncol(X),k)
  beta <- matrix(0,ncol(X),k)
  for (i in 1:k){
    eta[,i] <- eigen(V)$vectors[,i]
    #beta[,i] <- eta[,i] %*% solve(cov(X))
  }
  return(eta)
  
}


# GEP setting for SIR

SIRcovfun <- function(dataset, yindex, H){
  
  dataset <- as.matrix(dataset)
  X <- dataset[,-yindex]
  y <- dataset[,yindex]
  idx <- 1:nrow(dataset)
  X <- (X-t(matrix(apply(X,2,mean),ncol(X),nrow(X))))
  
  V <- matrix(0,ncol(X),ncol(X))
  for (i in 1:H){
    h <- c(1:round(nrow(dataset)/H))+round(nrow(dataset)/H)*(i-1)
    h.idx <- idx[y %in% sort(y)[h]]
    ph <- 1/nrow(dataset)*sum(y %in% sort(y)[h])
    mh <- apply(X[h.idx,],2,mean)
    V.h <- ph*(mh %*% t(mh))
    V <- V+V.h
    
  }
  return(V)
  
} 


# Coordinate-wise sparse SIR
cordsp.sir <- function(dataset,yindex,H,d,lambda){
  
  mat <- SIRcovfun(dataset,yindex,H)
  return(POI.coord.fun(mat,cov(dataset[,-yindex]),d,lambda))
  
}

elsp.sir <- function(dataset,yindex,H,d,lambda){
  mat <- SIRcovfun(dataset,yindex,H)
  return(POI.el.fun(mat,cov(dataset[,-yindex]),d,lambda))
}


# binary response SIR

SIRfun.bin <- function(dataset, yindex){
  X <- dataset[,-yindex]
  y <- dataset[,yindex]
  x <- as.matrix(X)
  z <- (x-t(matrix(apply(x,2,mean),ncol(x),nrow(x)))) %*% sqrtm(solve(cov(x)))
  idx <- 1:nrow(dataset)
  
  V <- matrix(0,ncol(X),ncol(X))
  for (i in 1:length(unique(y))){
    h.idx <- idx[y %in% unique(y)[i]]
    ph <- 1/nrow(dataset)*sum(y %in% unique(y)[i])
    mh <- apply(X[h.idx,],2,mean)
    V.h <- ph*(mh %*% t(mh))
    V <- V+V.h
    
  }
  x11()
  plot(eigen(V)$values, main="Eigenvalues for V.hat")
  
  cat("Select the appropriate dimension according to the plot(1, 2, 3, ... ): ")
  k <- scan(n=1, quiet=TRUE)
  
  eta <- matrix(0,ncol(X),k)
  beta <- matrix(0,ncol(X),k)
  for (i in 1:k){
    eta[,i] <- eigen(V)$vectors[,i]
    beta[,i] <- eta[,i] %*% solve(cov(X))
  }
  return(beta)
  
}

# binary response SIR with fixed structural dimension


SIRfun.bin.d <- function(dataset, yindex,d=2){
  X <- dataset[,-yindex]
  y <- dataset[,yindex]
  x <- as.matrix(X)
  z <- (x-t(matrix(apply(x,2,mean),ncol(x),nrow(x)))) %*% sqrtm(solve(cov(x)))
  idx <- 1:nrow(dataset)
  
  V <- matrix(0,ncol(X),ncol(X))
  for (i in 1:length(unique(y))){
    h.idx <- idx[y %in% unique(y)[i]]
    ph <- 1/nrow(dataset)*sum(y %in% unique(y)[i])
    mh <- apply(X[h.idx,],2,mean)
    V.h <- ph*(mh %*% t(mh))
    V <- V+V.h
    
  }

  k <- d
  
  eta <- matrix(0,ncol(X),k)
  beta <- matrix(0,ncol(X),k)
  for (i in 1:k){
    eta[,i] <- eigen(V)$vectors[,i]
    
  }
  return(eta)
  
}





# binary response GEP setting for SIR

SIRcovfun.bin <- function(dataset, yindex){
  
  dataset <- as.matrix(dataset)
  X <- dataset[,-yindex]
  y <- dataset[,yindex]
  idx <- 1:nrow(dataset)
  X <- (X-t(matrix(apply(X,2,mean),ncol(X),nrow(X))))
  
  V <- matrix(0,ncol(X),ncol(X))
  for (i in 1:length(unique(y))){
    h.idx <- idx[y %in% unique(y)[i]]
    ph <- 1/nrow(dataset)*sum(y %in% unique(y)[i])
    mh <- apply(X[h.idx,],2,mean)
    V.h <- ph*(mh %*% t(mh))
    V <- V+V.h
    
  }
  return(V)
  
}

# coordinate wise sparse binary response sir

cordsp.sir.bin <- function(dataset,yindex,d,lambda){
  
  mat <- SIRcovfun.bin(dataset,yindex)
  return(POI.coord.fun(mat,cov(dataset[,-yindex]),d,lambda))
  
}


elsp.sir.bin <- function(dataset,yindex,d,lambda){
  mat <- SIRcovfun.bin(dataset,yindex)
  return(POI.el.fun(mat,cov(dataset[,-yindex]),d,lambda))
  
  
}

# l2 norm function

l2norm <- function(vector){
  return(sqrt(sum(vector^2)))
}

