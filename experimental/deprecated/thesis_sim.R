
# Basic SIR case ------

time.basic1 <- Sys.time()
result.basic <- matrix(0,100,3)
for (r in 1:100){
  set.seed(r)
  
  x.basic <- mvrnorm(100,mu=rep(0,20),Sigma=diag(1,20))
  basis.basic <- c(1,-1,rep(0,18))
  y.basic <- exp(-0.5*x.basic %*% basis.basic)+0.5*rnorm(100)
  basic.dat <- cbind(y.basic,x.basic)
  
  beta.basic <- SIRfun.d(basic.dat,1,10,k=1)
  #SIRfun(basic.dat,1,10)
  
  lambda.max.basic <- max(apply(SIRcovfun(basic.dat,1,10),1,l2norm))
  lambda.grid.basic <- log(seq(from=exp(lambda.max.basic),to=exp(0),length.out=101))[-101]
  
  cdspbeta.basic <- cordsp.sir(basic.dat,1,10,1,lambda=lambda.grid.basic[80])
  
  lambda.max.basic.el <- max(abs(SIRcovfun(basic.dat,1,10)))
  lambda.grid.basic.el <- log(seq(from=exp(lambda.max.basic.el),to=exp(0),length.out=101))[-101]
  
  elspbeta.basic <- elsp.sir(basic.dat,1,10,1,lambda=lambda.grid.basic.el[80])
  
  # angle
  
  true.basis.basic <- c(1/sqrt(2),-1/sqrt(2),rep(0,18))
  a.mat <- beta.basic %*% t(beta.basic) - true.basis.basic %*% t(true.basis.basic)
  result.basic[r,1] <- max(svd(a.mat)$'d')
  
  b.mat <- elspbeta.basic %*% t(elspbeta.basic) - true.basis.basic %*% t(true.basis.basic)
  result.basic[r,2] <- max(svd(b.mat)$'d')
  
  c.mat <- cdspbeta.basic %*% t(cdspbeta.basic) - true.basis.basic %*% t(true.basis.basic)
  result.basic[r,3] <- max(svd(c.mat)$'d')

}
time.basic2 <- Sys.time()
time.df.basic <- time.basic2-time.basic1

apply(result.basic,2,mean)
sqrt(apply(result.basic,2,var)/100)


# logistic regression 2nd order -----

time.2nd.1 <- Sys.time()
result.2nd <- matrix(0,100,6)
for (r in 1:100){
  set.seed(r)
  x.log.data <- mvrnorm(1000,mu=rep(0,500),Sigma=diag(1,500))
  
  xbeta2nd <- 5*x.log.data[,1]^2+10*x.log.data[,1]+5*x.log.data[,2]^2+10*x.log.data[,2]
  
  p2nd <- exp(xbeta2nd)/(1+exp(xbeta2nd))
  y2nd <- c()
  for (i in 1:1000){
    y2nd[i] <- rbinom(1,1,p2nd[i])
  }
  
  log.dat.2nd <- as.data.frame(cbind(y2nd,x.log.data))

  save.beta.2nd <- SAVEfun.bin.d(log.dat.2nd,1)

  lambda.max <- max(apply(save.mat.fun(log.dat.2nd,1),1,l2norm))
  lambda.grid <- log(seq(from=exp(lambda.max),to=exp(0),length.out=101))[-101]
  
  cdspsave.2nd.beta <- cordsp.save(log.dat.2nd,1,2,lambda.grid[80])
  
  
  elspsave.2nd.beta <- elsp.save(log.dat.2nd,1,2,lambda.grid[98])
  
  
  cdspsir.2nd.beta <- cordsp.sir.bin(log.dat.2nd,1,2,lambda.grid[80])
  
  
  # sir
  sir.beta.2nd <- SIRfun.bin.d(log.dat.2nd,1)
  
  elspsir.2nd.beta <- elsp.sir.bin(log.dat.2nd,1,2,lambda.grid[96])
  cdspsir.2nd.beta <- cordsp.sir.bin(log.dat.2nd,1,2,lambda.grid[80])
  
  # angle
  true.basis.2nd <- cbind(c(1,rep(0,499)),c(0,1,rep(0,498)))
  
  a.mat <- save.beta.2nd %*% t(save.beta.2nd) - true.basis.2nd  %*%t(true.basis.2nd )
  result.2nd[r,1] <- max(svd(a.mat)$'d')
  
  b.mat <-  elspsave.2nd.beta %*% t(elspsave.2nd.beta) - true.basis.2nd  %*%t(true.basis.2nd )
  result.2nd[r,2] <- max(svd(b.mat)$'d')
  
  c.mat <-  cdspsave.2nd.beta %*% t(cdspsave.2nd.beta) - true.basis.2nd  %*%t(true.basis.2nd )
  result.2nd[r,3] <- max(svd(c.mat)$'d')
  
  d.mat <- sir.beta.2nd %*% t(sir.beta.2nd) -  true.basis.2nd  %*%t(true.basis.2nd )
  result.2nd[r,4] <- max(svd(d.mat)$'d')
  
  e.mat <- elspsir.2nd.beta %*% t(elspsir.2nd.beta) -  true.basis.2nd  %*%t(true.basis.2nd )
  result.2nd[r,5] <- max(svd(e.mat)$'d')
  
  f.mat <- cdspsir.2nd.beta %*% t(cdspsir.2nd.beta) -  true.basis.2nd  %*%t(true.basis.2nd )
  result.2nd[r,6] <- max(svd(f.mat)$'d')
}
time.2nd.2 <- Sys.time()
time.df.2nd <- time.2nd.2-time.2nd.1




# two circles (seperated) --------

time.spcir.1 <- Sys.time()
result.sc <- matrix(0,100,6)
for (r in 1:100){
  set.seed(r)
  theta <- runif(500,0,1)*2*pi
  r1 <- sqrt(runif(500,0,1))
  
  x1.left.spcir <- r1*cos(theta)
  x2.left.spcir <- r1*sin(theta)
  
  x1.right.spcir <- x1.left.spcir+2
  x2.right.spcir <- x2.left.spcir+2
  
  x1.spcir <- c(x1.left.spcir,x1.right.spcir)
  x2.spcir <- c(x2.left.spcir,x2.right.spcir)
  
  y.spcir <- rep(0,1000)
  for (i in 1:1000){
    if ((x1.spcir[i]^2+x2.spcir[i]^2)<=1) {y.spcir[i] <- 1}
    else {y.spcir[i] <- 0}
  }
  
  x.spcir <- mvrnorm(1000,mu=rep(0,498),Sigma=diag(1,498))
  x.spcir <- as.data.frame(cbind(x1.spcir,x2.spcir,x.spcir))
  spcir.data <- as.data.frame(cbind(y.spcir,x.spcir))
  
  
  # save
  
  save.beta.sc <- SAVEfun.bin.d(spcir.data,1)
  
  #cordsp.save.cv(save3dat,1,2)
  
  lambda.max.sc <- max(apply(save.mat.fun(spcir.data,1),1,l2norm))
  lambda.grid.sc <- log(seq(from=exp(lambda.max.sc),to=exp(0),length.out=101))[-101]
  
  cdspsave.sc.beta <- cordsp.save(spcir.data,1,2,lambda.grid.sc[80])
  elspsave.sc.beta <- elsp.save(spcir.data,1,2,lambda.grid.sc[95])
  
  
  # sir
  
  
  sir.sc.beta <- SIRfun.bin.d(spcir.data,1)
  cdspsir.sc.beta <- cordsp.sir.bin(spcir.data,1,2,lambda.grid.sc[80])
  elspsir.sc.beta <- elsp.sir.bin(spcir.data,1,2,lambda.grid.sc[80])
  
  # angle
  true.basis.spcir <- cbind(c(1,rep(0,499)),c(0,1,rep(0,498)))
  
  a.mat <- save.beta.sc %*% t(save.beta.sc) - true.basis.spcir  %*%t(true.basis.spcir )
  result.sc[r,1] <- max(svd(a.mat)$'d')
  
  b.mat <-  elspsave.sc.beta %*% t(elspsave.sc.beta) - true.basis.spcir  %*%t(true.basis.spcir )
  result.sc[r,2] <- max(svd(b.mat)$'d')
  
  c.mat <-  cdspsave.sc.beta %*% t(cdspsave.sc.beta) - true.basis.spcir  %*%t(true.basis.spcir )
  result.sc[r,3] <- max(svd(c.mat)$'d')
  
  d.mat <- sir.sc.beta  %*% t(sir.sc.beta ) -  true.basis.spcir  %*%t(true.basis.spcir )
  result.sc[r,4] <- max(svd(d.mat)$'d')
  
  e.mat <- elspsir.sc.beta %*% t(elspsir.sc.beta) -  true.basis.spcir  %*%t(true.basis.spcir )
  result.sc[r,5] <- max(svd(e.mat)$'d')
  
  f.mat <- cdspsir.sc.beta %*% t(cdspsir.sc.beta) - true.basis.spcir  %*%t(true.basis.spcir )
  result.sc[r,6] <- max(svd(f.mat)$'d')
  
}
time.spcir.2 <- Sys.time()
timd.df.spcir <- time.spcir.2-time.spcir.1

apply(result.sc,2,mean)

# SIR CASE 1 with H=10 ------
time.sir1 <- Sys.time()
result.sir1 <- matrix(0,100,6)
for (r in 1:100){
  set.seed(r)
  x.sirdat1 <- mvrnorm(200,mu=rep(0,20),Sigma=diag(1,20))
  y.sirdat1 <- x.sirdat1[,1]/(0.5+(x.sirdat1[,2]+1.5)^2)+0.2*rnorm(200)
  sirdat1 <- as.data.frame(cbind(y.sirdat1,x.sirdat1))
  
  beta.sirdat1 <- SIRfun.d(sirdat1,1,10)

  lambda.max.1 <- max(apply(SIRcovfun(sirdat1,1,10),1,l2norm))
  lambda.grid.1 <- log(seq(from=exp(lambda.max.1),to=exp(0),length.out=101))[-101]
  
  cdspbeta.sirdat1 <- cordsp.sir(sirdat1,1,10,2,lambda=lambda.grid.1[84])
  elspbeta.sirdat1 <- elsp.sir(sirdat1,1,10,2,lambda=lambda.grid.1[95])
  
  save.beta.1 <- SAVEfun.d(sirdat1,1,10)
  lambda.max.1.sv <- max(apply(save.covmat(sirdat1,1,10),1,l2norm))
  lambda.grid.1.sv <- log(seq(from=exp(lambda.max.1.sv),to=exp(0),length.out=100))
  
  cdspbeta.sv.1 <- cdsp.save.con(sirdat1,1,10,2,lambda.grid.1.sv[84])
  elspbeta.sv.1 <- elsp.save.con(sirdat1,1,10,2,lambda.grid.1.sv[95])
  
  # angle
  
  true.basis.1 <- cbind(c(1,rep(0,19)),c(0,1,rep(0,18)))
  
  a.mat <- save.beta.1 %*% t(save.beta.1) - true.basis.1 %*% t(true.basis.1)
  result.sir1[r,1] <- max(svd(a.mat)$'d')
  
  b.mat <-  elspbeta.sv.1 %*% t(elspbeta.sv.1) - true.basis.1 %*% t(true.basis.1)
  result.sir1[r,2] <- max(svd(b.mat)$'d')
  
  c.mat <-  cdspbeta.sv.1 %*% t(cdspbeta.sv.1) -  true.basis.1 %*% t(true.basis.1)
  result.sir1[r,3] <- max(svd(c.mat)$'d')
  
  d.mat <- beta.sirdat1 %*% t(beta.sirdat1) - true.basis.1 %*% t(true.basis.1)
  result.sir1[r,4] <- max(svd(d.mat)$'d')
  
  e.mat <- elspbeta.sirdat1 %*% t(elspbeta.sirdat1) - true.basis.1 %*% t(true.basis.1)
  result.sir1[r,5] <- max(svd(e.mat)$'d')
  
  f.mat <- cdspbeta.sirdat1 %*% t(cdspbeta.sirdat1) - true.basis.1 %*% t(true.basis.1)
  result.sir1[r,6] <- max(svd(f.mat)$'d')
  
}
time.sir1.2 <- Sys.time()
time.df.sir1 <- time.sir1.2-time.sir1

# SIR CASE 1 with H=5 -----

result.sir2 <- matrix(0,100,6)
for (r in 1:100){
  set.seed(r)
  x.sirdat1 <- mvrnorm(200,mu=rep(0,20),Sigma=diag(1,20))
  y.sirdat1 <- x.sirdat1[,1]/(0.5+(x.sirdat1[,2]+1.5)^2)+0.2*rnorm(200)
  sirdat1 <- as.data.frame(cbind(y.sirdat1,x.sirdat1))
  
  beta.sirdat2 <- SIRfun.d(sirdat1,1,5)
  
  lambda.max.2 <- max(apply(SIRcovfun(sirdat1,1,5),1,l2norm))
  lambda.grid.2 <- log(seq(from=exp(lambda.max.2),to=exp(0),length.out=101))[-101]
  
  cdspbeta.sirdat2 <- cordsp.sir(sirdat1,1,5,2,lambda=lambda.grid.1[84])
  elspbeta.sirdat2 <- elsp.sir(sirdat1,1,5,2,lambda=lambda.grid.1[95])
  
  save.beta.2 <- SAVEfun.d(sirdat1,1,5)
  lambda.max.2.sv <- max(apply(save.covmat(sirdat1,1,5),1,l2norm))
  lambda.grid.2.sv <- log(seq(from=exp(lambda.max.2.sv),to=exp(0),length.out=100))
  
  cdspbeta.sv.2 <- cdsp.save.con(sirdat1,1,5,2,lambda.grid.1.sv[84])
  elspbeta.sv.2 <- elsp.save.con(sirdat1,1,5,2,lambda.grid.1.sv[95])
  
  # angle
  
  true.basis.1 <- cbind(c(1,rep(0,19)),c(0,1,rep(0,18)))
  
  a.mat <- save.beta.2 %*% t(save.beta.2) - true.basis.1 %*% t(true.basis.1)
  result.sir2[r,1] <- max(svd(a.mat)$'d')
  
  b.mat <-  elspbeta.sv.2 %*% t(elspbeta.sv.2) - true.basis.1 %*% t(true.basis.1)
  result.sir2[r,2] <- max(svd(b.mat)$'d')
  
  c.mat <-  cdspbeta.sv.2 %*% t(cdspbeta.sv.2) -  true.basis.1 %*% t(true.basis.1)
  result.sir2[r,3] <- max(svd(c.mat)$'d')
  
  d.mat <- beta.sirdat2 %*% t(beta.sirdat2) - true.basis.1 %*% t(true.basis.1)
  result.sir2[r,4] <- max(svd(d.mat)$'d')
  
  e.mat <- elspbeta.sirdat2 %*% t(elspbeta.sirdat2) - true.basis.1 %*% t(true.basis.1)
  result.sir2[r,5] <- max(svd(e.mat)$'d')
  
  f.mat <- cdspbeta.sirdat2 %*% t(cdspbeta.sirdat2) - true.basis.1 %*% t(true.basis.1)
  result.sir2[r,6] <- max(svd(f.mat)$'d')
  
}


# SIR CASE 1 with H=2 ------

result.sir3 <- matrix(0,100,6)
for (r in 1:100){
  set.seed(r)
  x.sirdat1 <- mvrnorm(200,mu=rep(0,20),Sigma=diag(1,20))
  y.sirdat1 <- x.sirdat1[,1]/(0.5+(x.sirdat1[,2]+1.5)^2)+0.2*rnorm(200)
  sirdat1 <- as.data.frame(cbind(y.sirdat1,x.sirdat1))
  
  beta.sirdat3 <- SIRfun.d(sirdat1,1,2)
  
  lambda.max.3 <- max(apply(SIRcovfun(sirdat1,1,2),1,l2norm))
  lambda.grid.3 <- log(seq(from=exp(lambda.max.3),to=exp(0),length.out=101))[-101]
  
  cdspbeta.sirdat3 <- cordsp.sir(sirdat1,1,2,2,lambda=lambda.grid.1[84])
  elspbeta.sirdat3 <- elsp.sir(sirdat1,1,2,2,lambda=lambda.grid.1[95])
  
  save.beta.3 <- SAVEfun.d(sirdat1,1,2)
  lambda.max.3.sv <- max(apply(save.covmat(sirdat1,1,2),1,l2norm))
  lambda.grid.3.sv <- log(seq(from=exp(lambda.max.3.sv),to=exp(0),length.out=100))
  
  cdspbeta.sv.3 <- cdsp.save.con(sirdat1,1,2,2,lambda.grid.1.sv[80])
  elspbeta.sv.3 <- elsp.save.con(sirdat1,1,2,2,lambda.grid.1.sv[97])
  
  # angle
  
  true.basis.1 <- cbind(c(1,rep(0,19)),c(0,1,rep(0,18)))
  
  a.mat <- save.beta.3 %*% t(save.beta.3) - true.basis.1 %*% t(true.basis.1)
  result.sir3[r,1] <- max(svd(a.mat)$'d')
  
  b.mat <-  elspbeta.sv.3 %*% t(elspbeta.sv.3) - true.basis.1 %*% t(true.basis.1)
  result.sir3[r,2] <- max(svd(b.mat)$'d')
  
  c.mat <-  cdspbeta.sv.3 %*% t(cdspbeta.sv.3) -  true.basis.1 %*% t(true.basis.1)
  result.sir3[r,3] <- max(svd(c.mat)$'d')
  
  d.mat <- beta.sirdat3 %*% t(beta.sirdat3) - true.basis.1 %*% t(true.basis.1)
  result.sir3[r,4] <- max(svd(d.mat)$'d')
  
  e.mat <- elspbeta.sirdat3 %*% t(elspbeta.sirdat3) - true.basis.1 %*% t(true.basis.1)
  result.sir3[r,5] <- max(svd(e.mat)$'d')
  
  f.mat <- cdspbeta.sirdat3 %*% t(cdspbeta.sirdat3) - true.basis.1 %*% t(true.basis.1)
  result.sir3[r,6] <- max(svd(f.mat)$'d')
  
}

rbind(apply(result.sir1,2,mean),apply(result.sir2,2,mean),apply(result.sir3,2,mean))
sqrt(rbind(apply(result.sir1,2,var),apply(result.sir2,2,var),apply(result.sir3,2,var))/100)



# tai chi -----
result.tc <- matrix(0,100,6)
time.tc.1 <- Sys.time()
for (r in 1:100){
  set.seed(r)
  theta <- runif(1000,0,1)*2*pi
  rad <- sqrt(runif(1000,0,4))
  
  x1.tc <- rad*cos(theta)
  x2.tc <- rad*sin(theta)
  
  
  y.tc <- rep(0,1000)
  for (i in 1:1000){
    if (((x1.tc[i]-1)^2+(x2.tc[i]-1)^2)<=1/4) {y.tc[i] <- 1}
    if (x1.tc[i]<0) {y.tc[i]<-1}
    if (x2.tc[i]<=0 & 0<=x1.tc[i] & x1.tc[i]<=1 & x1.tc[i]<=sqrt(abs(1-(x2.tc[i]+1)^2))) {y.tc[i]<-1}
    if (x2.tc[i]>=0 & -1<=x1.tc[i] & x1.tc[i]<=0 & x1.tc[i]<=sqrt(abs(1-(x2.tc[i]-1)^2))) {y.tc[i] <- 0}
    if (((x1.tc[i]+1)^2+(x2.tc[i]+1)^2)<=1/4) {y.tc[i]<-0}
    
  }
  
  x.tai.chi <- mvrnorm(1000,mu=rep(0,498),Sigma=diag(1,498))
  
  x.tai.chi <- as.data.frame(cbind(x1.tc,x2.tc,x.tai.chi))
  tai.chi <- as.data.frame(cbind(y.tc,x.tai.chi))
  
  
  
  # save
  
  save.beta.tc <- SAVEfun.bin.d(tai.chi,1)
  
  lambda.max.tc <- max(apply(save.mat.fun(tai.chi,1),1,l2norm))
  lambda.grid.tc <- log(seq(from=exp(lambda.max.tc),to=exp(0),length.out=101))[-101]
  
  cdspsave.tc.beta <- cordsp.save(tai.chi,1,2,lambda.grid.tc[80])
  elspsave.tc.beta <- elsp.save(tai.chi,1,2,lambda.grid.tc[95])
  
  # sir
  
  lambda.max.tc.sir <- max(apply(SIRcovfun.bin(tai.chi,1),1,l2norm))
  lambda.grid.tc.sir <- log(seq(from=exp(lambda.max.tc.sir),to=exp(0),length.out=100))
  
  sir.tc.beta <- SIRfun.bin.d(tai.chi,1)
  cdspsir.tc.beta <- cordsp.sir.bin(tai.chi,1,2,lambda.grid.tc.sir[80])
  elspsir.tc.beta <- elsp.sir.bin(tai.chi,1,2,lambda.grid.tc.sir[95])
  
  # angle
  true.basis.tc <- cbind(c(1,rep(0,499)),c(0,1,rep(0,498)))
  
  a.mat <- save.beta.tc %*% t(save.beta.tc) - true.basis.tc  %*%t(true.basis.tc )
  result.tc[r,1] <- max(svd(a.mat)$'d')
  
  b.mat <-  elspsave.tc.beta %*% t(elspsave.tc.beta) - true.basis.tc  %*%t(true.basis.tc )
  result.tc[r,2] <- max(svd(b.mat)$'d')
  
  c.mat <-  cdspsave.tc.beta %*% t(cdspsave.tc.beta) - true.basis.tc  %*%t(true.basis.tc )
  result.tc[r,3] <- max(svd(c.mat)$'d')
  
  d.mat <- sir.tc.beta  %*% t(sir.tc.beta ) -  true.basis.tc  %*%t(true.basis.tc )
  result.tc[r,4] <- max(svd(d.mat)$'d')
  
  e.mat <- elspsir.tc.beta %*% t(elspsir.tc.beta) -  true.basis.tc  %*%t(true.basis.tc )
  result.tc[r,5] <- max(svd(e.mat)$'d')
  
  f.mat <- cdspsir.tc.beta %*% t(cdspsir.tc.beta) - true.basis.tc  %*%t(true.basis.tc )
  result.tc[r,6] <- max(svd(f.mat)$'d')
  result.tc
  
}  
time.tc.2 <- Sys.time()
timd.df.tc <- time.tc.2-time.tc.1
apply(result.tc,2,mean)
sqrt(apply(result.tc,2,var)/100)



rbind(apply(result.2nd,2,mean),apply(result.tc,2,mean),apply(result.sc,2,mean))

