
# SAVE experiment with Swiss Bank Notes -----

setwd("C://Users/sol.kwon/Desktop/Sol/Master/research/")
banknotes <- read.table("banknotes.txt",header=TRUE)
banknotes[,6]

bank.save.beta <- SAVEfun.bin(banknotes,6)
sdr.bank <- as.matrix(banknotes[,-6])%*%bank.save.beta
sdr.bank
plot(sdr.bank[,1],sdr.bank[,2],col=banknotes[,6]+2,pch=banknotes[,6]*15+4)

#pca.bank <- as.matrix(banknotes[,-6])%*%prcomp(banknotes[,-6])$rotation
#pca.bank <- pca.bank[,1:2]
#plot(pca.bank[,1],pca.bank[,2],col=banknotes[,6]+2,pch=banknotes[,6]+1)

#bank.sir.beta <- SIRfun.bin(banknotes,6)
#sdr.bank.sir <- as.matrix(banknotes[,-6])%*%bank.sir.beta
#plot(sdr.bank.sir[,1],sdr.bank.sir[,2],col=banknotes[,6]+2,pch=banknotes[,6]+1)

#x11()
#par(mfrow=c(1,3))
#plot(sdr.bank[,1],sdr.bank[,2],col=banknotes[,6]+2,pch=banknotes[,6]+1)
#plot(pca.bank[,1],pca.bank[,2],col=banknotes[,6]+2,pch=banknotes[,6]+1)
#plot(sdr.bank.sir[,1],sdr.bank.sir[,2],col=banknotes[,6]+2,pch=banknotes[,6]+1)


lambda.max <- max(apply(save.mat.fun(banknotes,6),1,l2norm))
lambda.grid <- log(seq(from=exp(lambda.max),to=exp(0),length.out=101))[-101]
#cordsp.save.cv(banknotes,6,2)
#cordsp.save(banknotes,6,2,lambda.grid[80])
cdspsave.beta.bank <- cordsp.save(banknotes,6,2,lambda.grid[80])
sdr.bank.cdspsave <- as.matrix(banknotes[,-6])%*%cdspsave.beta.bank

elspsave.2nd.beta <- elsp.save(log.dat.2nd,1,2,lambda.grid[96])

bank.color <- c("red4", "aquamarine1")


x11()
par(mfrow=c(1,2)) 
plot(sdr.bank[,1],sdr.bank[,2],pch=19,col=bank.color[(banknotes[,6]+1)], main="SAVE", xlab="", ylab="")
plot(sdr.bank.cdspsave[,1],sdr.bank.cdspsave[,2],pch=19,col=bank.color[(banknotes[,6]+1)],
     main="Coordinate-wise Sparse SAVE", xlab="", ylab="")


# logistic regression 2nd order -----

x.log.data <- mvrnorm(1000,mu=rep(0,500),Sigma=diag(1,500))

xbeta2nd <- 5*x.log.data[,1]^2+10*x.log.data[,1]+5*x.log.data[,2]^2+10*x.log.data[,2]

p2nd <- exp(xbeta2nd)/(1+exp(xbeta2nd))
y2nd <- c()
for (i in 1:1000){
  y2nd[i] <- rbinom(1,1,p2nd[i])
}

log.dat.2nd <- as.data.frame(cbind(y2nd,x.log.data))


#summary(glm(y2nd~x.log.data, family=binomial))
#summary(glm(y2nd~x.log.data[,1]+x.log.data[,2],family=binomial))

#plot(x.log.data[,1],x.log.data[,2],col=y2nd+1)

# save

save.beta.2nd <- SAVEfun.bin.d(log.dat.2nd,1)




#cordsp.save.cv(log.dat.2nd,1,2)


lambda.max <- max(apply(save.mat.fun(log.dat.2nd,1),1,l2norm))
lambda.grid <- log(seq(from=exp(lambda.max),to=exp(0),length.out=101))[-101]

# plot

save.beta.2nd <- SAVEfun.bin(log.dat.2nd,1)

elspsave.2nd.beta <- elsp.sir.bin(log.dat.2nd,1,2,lambda.grid[98])
cdspsir.2nd.beta <- cordsp.sir.bin(log.dat.2nd,1,2,lambda.grid[80])

sir.beta.2nd <- SIRfun.bin(log.dat.2nd,1)
sdr.2nd.sir <- as.matrix(log.dat.2nd[,-1]) %*% sir.beta.2nd
elspsir.2nd.beta <- elsp.sir.bin(log.dat.2nd,1,2,lambda.grid[96])
cdspsave.2nd.beta <- cordsp.save(log.dat.2nd,1,2,lambda.grid[80])

sdr.2nd.save <- as.matrix(log.dat.2nd[,-1]) %*% save.beta.2nd
sdr.2nd.cdspsave <- as.matrix(log.dat.2nd[,-1])%*%cdspsave.2nd.beta
sdr.2nd.elspsave <- as.matrix(log.dat.2nd[,-1])%*%elspsave.2nd.beta

sdr.2nd.sir <- as.matrix(log.dat.2nd[,-1]) %*% sir.beta.2nd
sdr.2nd.cdspsir <- as.matrix(log.dat.2nd[,-1])%*%cdspsir.2nd.beta
sdr.2nd.elspsir <- as.matrix(log.dat.2nd[,-1])%*%elspsir.2nd.beta


log.2nd.color <- c("red4", "aquamarine1")

x11()
plot(x.log.data[,1],x.log.data[,2],pch=19,col=log.2nd.color[(y2nd+1)], main="True",xlab="X1",ylab="X2")
x11()
par(mfrow=c(2,3))
plot(sdr.2nd.sir[,1],sdr.2nd.sir[,2],pch=19,col=log.2nd.color[(y2nd+1)],xlim=c(-3,3),ylim=c(-3,3), main="SIR",xlab="",ylab="")
plot(sdr.2nd.elspsir[,1],sdr.2nd.elspsir[,2],pch=19,col=log.2nd.color[(y2nd+1)], main="Element-wise Sparse SIR",xlab="",ylab="")
plot(sdr.2nd.cdspsir[,1],sdr.2nd.cdspsir[,2],pch=19,col=log.2nd.color[(y2nd+1)], main="Coordinate-wise Sparse SIR",xlab="",ylab="")

plot(sdr.2nd.save[,1],sdr.2nd.save[,2],pch=19,col=log.2nd.color[(y2nd+1)],xlim=c(-3,3),ylim=c(-3,3),main="SAVE",xlab="",ylab="")
plot(sdr.2nd.elspsave[,1],sdr.2nd.elspsave[,2],pch=19,col=log.2nd.color[(y2nd+1)], main="Element-wise Sparse SAVE",xlab="",ylab="")
plot(sdr.2nd.cdspsave[,1],sdr.2nd.cdspsave[,2],pch=19,col=log.2nd.color[(y2nd+1)],
     main="Coordinate-wise Sparse SAVE", xlab="",ylab="")



# two circles (seperated) --------

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



# plot


sdr.sc.save <- as.matrix(spcir.data[,-1]) %*% save.beta.sc
sdr.sc.elspsave <- as.matrix(spcir.data[,-1])%*%elspsave.sc.beta
sdr.sc.cdspsave <- as.matrix(spcir.data[,-1])%*%cdspsave.sc.beta

sdr.sc.sir <- as.matrix(spcir.data[,-1]) %*% sir.sc.beta
sdr.sc.elspsir <- as.matrix(spcir.data[,-1])%*%elspsir.sc.beta
sdr.sc.cdspsir <- as.matrix(spcir.data[,-1])%*%cdspsir.sc.beta


spcir.color <- c("red4", "aquamarine1")



x11()
plot(x.spcir[,1],x.spcir[,2],pch=19,col=spcir.color[y.spcir+1],main="True",xlab="X1",ylab="X2")

x11()
par(mfrow=c(2,3))

plot(sdr.sc.sir[,1],sdr.sc.sir[,2],pch=19,col=spcir.color[y.spcir+1],main="SIR",xlab="",ylab="")
plot(sdr.sc.elspsir[,1],sdr.sc.elspsir[,2],pch=19,col=spcir.color[y.spcir+1],main="Element-wise Sparse SIR",xlab="",ylab="")
plot(sdr.sc.cdspsir[,1],sdr.sc.cdspsir[,2],pch=19,col=spcir.color[y.spcir+1],main="Coordinate-wise Sparse SIR",xlab="",ylab="")

plot(sdr.sc.save[,1],sdr.sc.save[,2],pch=19,col=spcir.color[y.spcir+1],main="SAVE",xlab="",ylab="")
plot(sdr.sc.elspsave[,1],sdr.sc.elspsave[,2],pch=19,col=spcir.color[y.spcir+1],main="Element-wise Sparse SAVE",xlab="",ylab="")
plot(sdr.sc.cdspsir[,1],sdr.sc.cdspsir[,2],pch=19,col=spcir.color[y.spcir+1],main="Coordinate-wise Sparse SIR",xlab="",ylab="")




# SIR CASE 1 ------

time.sir1 <- Sys.time()
result.sir1.1 <- matrix(0,100,6)
for (r in 1:100){
  set.seed(r)
  x.sirdat1 <- mvrnorm(200,mu=rep(0,20),Sigma=diag(1,20))
  y.sirdat1 <- x.sirdat1[,1]/(0.5+(x.sirdat1[,2]+1.5)^2)+0.2*rnorm(200)
  sirdat1 <- as.data.frame(cbind(y.sirdat1,x.sirdat1))
  
  beta.sirdat1 <- SIRfun.d(sirdat1,1,10)
  #cordsp.sir.cv(sirdat1,1,2,k=10,10)
  
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
  #sqrt(max(eigen(t(a.mat) %*% a.mat)$values))
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
time.df.sir1 <- time.sir1.2-time.sir1.1



# tai chi -----

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
save.beta.tc <- SAVEfun.bin(tai.chi,1)

lambda.max.tc <- max(apply(save.mat.fun(tai.chi,1),1,l2norm))
lambda.grid.tc <- log(seq(from=exp(lambda.max.tc),to=exp(0),length.out=101))[-101]

cdspsave.tc.beta <- cordsp.save(tai.chi,1,2,lambda.grid.tc[80])
elspsave.tc.beta <- elsp.save(tai.chi,1,2,lambda.grid.tc[95])

# sir

sir.tc.beta <- SIRfun.bin(tai.chi,1)
lambda.max.tc.sir <- max(apply(SIRcovfun.bin(tai.chi,1),1,l2norm))
lambda.grid.tc.sir <- log(seq(from=exp(lambda.max.tc.sir),to=exp(0),length.out=100))

cdspsir.tc.beta <- cordsp.sir.bin(tai.chi,1,2,lambda.grid.tc.sir[80])
elspsir.tc.beta <- elsp.sir.bin(tai.chi,1,2,lambda.grid.tc.sir[95])



sdr.tc.sir <- as.matrix(tai.chi[,-1]) %*% sir.tc.beta
sdr.tc.elspsir <- as.matrix(tai.chi[,-1])%*%elspsir.tc.beta
sdr.tc.cdspsir <- as.matrix(tai.chi[,-1])%*%cdspsir.tc.beta
sdr.tc.save <- as.matrix(tai.chi[,-1]) %*% save.beta.tc
sdr.tc.elspsave <- as.matrix(tai.chi[,-1])%*%elspsave.tc.beta
sdr.tc.cdspsave <- as.matrix(tai.chi[,-1])%*%cdspsave.tc.beta


tai.chi.color <- c("red4", "aquamarine1")

x11()
plot(x.tai.chi[,1],x.tai.chi[,2],pch=19,col=tai.chi.color[(y.tc+1)],main="Tai-Chi",xlab="X1",ylab="X2")

x11()
par(mfrow=c(2,3))
plot(sdr.tc.sir[,1],sdr.tc.sir[,2],pch=19,col=tai.chi.color[(y.tc+1)],main="SIR",xlab="",ylab="")
plot(sdr.tc.elspsir[,1],sdr.tc.elspsir[,2],pch=19,col=tai.chi.color[(y.tc+1)],main="Element-wise Sparse SIR",xlab="",ylab="")
plot(sdr.tc.cdspsir[,1],sdr.tc.cdspsir[,2],pch=19,col=tai.chi.color[(y.tc+1)],main="Coordinate-wise Sparse SIR",xlab="",ylab="")
plot(sdr.tc.save[,1],sdr.tc.save[,2],pch=19,col=tai.chi.color[(y.tc+1)],main="SAVE",xlab="",ylab="")
plot(sdr.tc.elspsave[,1],sdr.tc.elspsave[,2],pch=19,col=tai.chi.color[(y.tc+1)],main="Element-wise Sparse SAVE",xlab="",ylab="")
plot(sdr.tc.cdspsave[,1],sdr.tc.cdspsave[,2],pch=19,col=tai.chi.color[(y.tc+1)],main="Coordinate-wise Sparse SAVE",xlab="",ylab="")




# SIR case 2 -----

x.sir2 <- mvrnorm(100,mu=rep(0,20),Sigma=diag(1,20))
b1.sir2 <- c(rep(1,4),rep(0,16))
b2.sir2 <- c(rep(0,16),rep(1,4))



y.sir2 <- sign(x.sir2 %*% b1.sir2) * log(abs(x.sir2 %*% b2.sir2 +5)) + rnorm(100)
sir2data <- cbind(y.sir2,x.sir2)


beta.sirdat2 <- SIRfun(sir2data,1,10)

lambda.max.1 <- max(apply(SIRcovfun(sir2data,1,10),1,l2norm))
lambda.grid.1 <- log(seq(from=exp(lambda.max.1),to=exp(0),length.out=101))[-101]


cdspbeta.sir2data <- cordsp.sir(sir2data,1,10,2,lambda=lambda.grid.1[92])

elspbeta.sir2data <- elsp.sir(sir2data,1,10,2,lambda=lambda.grid.1[84])

save.beta.1 <- SAVEfun(sirdat1,1,10)
lambda.max.1.sv <- max(apply(save.covmat(sirdat1,1,10),1,l2norm))
lambda.grid.1.sv <- log(seq(from=exp(lambda.max.1.sv),to=exp(0),length.out=101))[-101]

cdspbeta.sv.1 <- cdsp.save.con(sirdat1,1,10,2,lambda.grid.1.sv[80])
elspbeta.sv.1 <- elsp.save.con(sirdat1,1,10,2,lambda.grid.1.sv[95])

# cos angle

true.basis.1 <- cbind(b1.sir2,b2.sir2)


cos.angle(true.basis.1[,1],beta.sirdat2[,1]) * cos.angle(true.basis.1[,2],beta.sirdat1[,2])
cos.angle(true.basis.1[,1],cdspbeta.sir2data[,1]) * cos.angle(true.basis.1[,2],cdspbeta.sirdat1[,2])
cos.angle(true.basis.1[,1],elspbeta.sir2data[,1]) * cos.angle(true.basis.1[,2],elspbeta.sirdat1[,2])


cos.angle(true.basis.1[,1],save.beta.1[,1]) * cos.angle(true.basis.1[,2],save.beta.1[,2])
cos.angle(true.basis.1[,1],cdspbeta.sv.1[,1]) * cos.angle(true.basis.1[,2],cdspbeta.sv.1[,2])
cos.angle(true.basis.1[,1],elspbeta.sv.1[,1]) * cos.angle(true.basis.1[,2],elspbeta.sv.1[,2])







