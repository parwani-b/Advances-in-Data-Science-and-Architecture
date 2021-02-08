a <-1:20  #Question 1(a)
a

b <-20:1  #Question 1(b)
b

d <- c(1:20, 19:1)  #Question 1(c)
d

tmp <- c(4,6,3)  #Question 1(d)
tmp

rep(tmp, 10)    #Question 1(e)

rep(tmp, len =31) #Question 1(f)

rep(tmp, c(10, 20, 30))

tmp <- seq(3,6, by=0.1)  #Question 2
exp(tmp)*cos(tmp)

e = (0.1^seq(3,36, by=3))*(0.2^seq(1,34, by=3))  #Question 3(a)
e

f = (2^(1:25))/(1:25)        #Question 3(b)
f

tmp <- 10:100                #Question 4(a)
sum(tmp^3 + 4*tmp^2)

tmp <- 1:25                 #Question 4(b)
sum((2^tmp/tmp) + (3^tmp/tmp^2))

paste("label", 1:30)        #Question 5(a)

paste("fn", 1:30, sep="")   #Question 5(b)

set.seed(50)
VectorX <- sample(0:999, 250, replace = T)
VectorY <- sample(0:999, 250, replace = T)

1+sum(cumprod(seq(2,38,b=2)/seq(3,39,b=2)))    #Question 8

#Exercise 2

(mat <- matrix(c(1, 5, -2, 1, 2, -1, 3, 6, -3), nrow = 3))   #Question 1
mat%*%mat%*%mat                                              #Question 1(a)

mat[, 3] <- mat[, 2] + mat[, 3]                              #Question 1(b)
mat

mat <- matrix(c(10, -10, 10), byrow = T, nrow = 15, ncol = 3)   #Question 2
t(mat)%*%mat

matE <- matrix(data = 0, nrow = 6, ncol = 6)                #Question 3
matE [ abs(col(matE) - row(matE)) == 1] <- 1
matE

outer(0:4, 0:4, "+")                                     #Question 4

outer(0:4, 0:4, "+")%%5                                  #Question 5(a)

outer(0:9, 0:9, "+")%%10                                 #Question 5(b)

outer(0:8, 0:8, "-")%%9                                  #Question 5(c)


VectorY <- c(7, -1, -3, 5, 17)                           #Question 6
MatrixA <- matrix(0, nrow = 5, ncol = 5)
MatrixA <- abs(col(MatrixA) - row(MatrixA)) + 1


set.seed(75)                                              #Question 7
MatA <- matrix(sample(10, size = 60, replace = T), nrow = 6)

apply(MatA, 1, function(x){sum(x>4)})                     #Question 7(a)

which( apply(MatA, 1, function(x){sum(x==7)==2}))         #Question 7(b)

MatASumCols <- colSums(MatA)
which( outer(MatASumCols, MatASumCols,"+")>75, arr.ind = T)  #Question 7(c)

sum (( 1:20)^4) * sum(1/(4:8))                               #Question 8(a)

sum((1:20)^4/(3 + outer(1:20, 1:5, "*")))                    #Question 8(b)

sum( outer(1:10, 1:10, function(i,j){(i>=j)*i^4/(3+i*j)}))   #Question 8(c)

#Exercise 3

tmpFn1 <- function(VectorX)                                 #Question 1(a)                                  
{
  VectorX^(1:length(VectorX))
}
tmpFn2 <- function(VectorX)
{
  n <- length(VectorX)
  (VectorX^(1:n))/(1:n)
}


tmpFn3 <- function (x, n)                                  #Question 1(b)
{
  1 + sum((x^(1:n))/(1:n))
}

tmpFn <- function(VectorX)                                #Question 2
{
  n <- length(VectorX)
  ( VectorX[ -c(n-1, n)] + VectorX[-c(1, n)] + VectorX[-c(1,2)])/3
}
tmpFn(c(1:5, 6:1))



tmpFn <- function(x)                                       #Question 3
{
  ifelse(x < 0, x^2 + 2*x + 3, ifelse(x < 2, x+3, x^2 + 4*x - 7))
}
tmp <- seq(-3, 3, len=100)
plot(tmp, tmpFn(tmp), type="1")


tmpFn <- function(mat)                                  #Question 4
{
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  mat
}

tmp <- diag(2, nrow = 5)                              #Question 5
tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
tmp
#For general case

tmpFn <- function(n, k)
{
  tmp <- diag(k, nrow =  n)
  tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
  tmp
}


quadrant <- function(alpha)                        #Question 6
{
  1 + (alpha%%360)%%90
}

weekday <- function(day, month, year)             #Question 7
{
  month <- month - 2
  if(month <= 0){
    month <- month + 12
    year <- year - 1 
  }
  cc <- year %/% 100
  year <- year %% 100
  tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * ccc("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[1 + tmp%%7]
}


testLoop <- function(n)                          #Question 8(a)
{
  VectorX <- rep(NA, n-1)
  VectorX[1] <- 1
  VectorX[2] <- 2
  for(j in 3:(n-1))
    VectorX[j] <- VectorX[j - 1] + 2/VectorX[j - 1]
  VectorX
}

testLoop2 <- function (VectorY)                  #Question 8(b)
{
  n <- length(VectorY)
  sum (exp(seq(along = VectorY)))
}


                                                      
quadmap <- function(start, rho, niter)             #Question 9(a)
{
  VectorX <- rep(NA,niter)
  VectorX[1] <- start
  for(i in 1:(niter-1)) {
    VectorX[i + 1] <- rho * VectorX[i] * (1 - VectorX[i])
  }
  X
}


quad2 <- function(start, rho, eps = 0.02)         #Question 9(b)
{
  x1 <- start
  x2 <- rho*x1*(1 - x1)
  niter <- 1
  while(abs(x1 - x2) >= eps) {
    x1 <- x2
    x2 <- rho * x1 * (1 - x1)
    niter <- niter + 1
  }
  niter
}


tmpAcf <- function(VectorX)                        #Question 10(a)
{
  xc <- VectorX - mean(VectorX)
  den <- sum(xc^2)
  n <- length(x)
  r1 <- sum( xc[2:n] * xc[1:(n-1)] )/den
  r2 <- sum( xc[3:n] * xc[1:(n-2)] )/den
  list(r1 = r1, r2 = r2)
}

tmpAcf <- function(x, k)                            #Question 10(b)
{
  xc <- x - mean(x)
  den <- sum(xc^2)
  n <- length(x)
  tmpFn <- function(j){ sum( xc[(j+1):n] * xc[1:(n-j)] )/den }
  c(1, sapply(1:k, tmpFn))
}

#Exercise 4

fun1a <- function(VectorX, VectorY){                    #Question 1(a)
  colSums( outer(VectorY, VectorX, "<") )
}


fun1b <- function(VectorX, VectorY){                            #Question 1(b)
  rowSums( sapply(VectorY, FUN=function(y){y < VectorX}) )
}

fun1c <- function(VectorX, VectorY){                            #Question 1(c)
  rowSums( vapply(VectorY, FUN=function(y){y<VectorX}, FUN.VALUE=seq(along=VectorX)) )
}


fun1d <- function(VectorX,VectorY)                                  #Question 1(d)
{
  leny <- length(VectorY)
  mat <- matrix(rep(VectorX,leny), byrow=T, nrow=leny)
  apply( VectorY<mat, 2, sum )
}

rjr1 <- rnorm(10000)                                                   #Question 1(e)
rjr2 <- rnorm(12000)
system.time(fun1a(rjr1,rjr2))
system.time(fun1b(rjr1,rjr2))
system.time(fun1c(rjr1,rjr2))
system.time(fun1d(rjr1,rjr2))

tmpFn <- function(mat){                                               #Question 2(a)                      
  mat[, !apply(is.na(mat), 2, any), drop = F]     
}

tmpFn2 <- function(mat){                                               #Question 2(b)
  mat[!apply(is.na(mat), 1, any), !apply(is.na(mat), 2, any), drop = F]
}

empCopula <- function( u, v, VectorX, VectorY )                              #Question 3(a)
{
  n <- length(VectorX)
  rVecN <- rank(VectorX)/(n+1)
  sVecN <- rank(VectorY)/(n+1)
  sum( (rVecN <= u) & (sVecN <= v) ) /n
}

empCopula2 <- function( u, v, VectorX, VectorY )                                     #Question 3(b)
{
  n <- length(VectorX)
  rVecN <- rank(VectorX)/(n+1)
  sVecN <- rank(VectorY)/(n+1)
  valuesN <- colSums( outer(rVecN, u, "<=")&outer(sVecN, v, "<=") )
  cbind( uCoord = u, vCoord = v, empCop=valuesN/n )
}

fun4a <- function (n)                                                           #Question 4(a)
{
  su <- 0
  for(r in 1:n)
  {
    for(s in 1:r)
      su <- su+s^2/(10+4*r^3)
  }
  su
}

fun4b <- function (n)                                                #Question 4(b)
{
  mat <- matrix(0, ncol=n, nrow=n)
  sum( (col(mat)^2)/(10+4*row(mat)^3)*(col(mat)<=row(mat)) )
}

fun4c <- function (n)                                           #Question 4(c)
{
  sum( outer(1:n,1:n,FUN=function(r,s){ (s<=r)*(s^2)/(10+4*r^3) }) )
}

fun4d <- function (n)                                            #Question 4(d)
{
  tmpfn <- function(r){sum(((1:r)^2)/(10+4*r^3))}
  sum(sapply(1:n, FUN=tmpfn))
}

fun4e <- function (n)                                            
{
  tmpfn <- function(r){sum(((1:r)^2)/(10+4*r^3))}
  sum(unlist(lapply(1:n, FUN=tmpfn)))
}


fun4f <- function (n)                                            #Question 4(e)
{
  tmpf <- function(s,r){(s^2)/(10+4*r^3)*(s<=r)}
  sum(mapply(tmpf, rep(1:n, times=rep(n,n)), 1:n))
}


que5a <- function(n, aRate, sRate)                              #Question 5(a)
{
  w <- 0
  for(i in 1:n){
    w <- max(0, w+rexp(1,sRate)-rexp(1,aRate))
  }
  w
}
que5b <- function(n, aRate, sRate)
{
  w <- 0
  s <- rexp(n, sRate)
  a <- rexp(n, aRate)
  for(i in 1:n){
    w <- max(0, w+s[i]-a[i])
  }
  
  
  queueRep1 <- function (nReps, n, aRate, sRate)                 #Question 5(b)
  {
    wVec <- rep(NA, nReps)
    for(j in 1:nReps)
      wVec[j] <- queue2(n, aRate, sRate)
    wVec
  }
  queueRep2 <- function (nReps, n, aRate, sRate)
  {
    sapply( rep(n,nReps), queue2, aRate, sRate )
  }
  
  
  queueRep3 <- function (nReps, n, aRate, sRate)               #Question 5(c)
  {
    w <- rep(0, nReps)
    s <- matrix(rexp(n*nReps, sRate), ncol=nReps)
    a <- matrix(rexp(n*nReps, aRate), ncol=nReps)
    for(i in 1:n){
      w <- pmax(0, w+s[i,]-a[i,])
    }
    w
  }
  w
}  


rwalk <- function(n)                                            #Question 6(a)
{
  c( 0, cumsum(sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5))) )
}

rwalkPos <- function(n)                                        #Question 6(b)
{
  rw <- cumsum(c(0, sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5))))
  sum( (rw[-(n+1)] + rw[-1]) > 0 )
}

rwalkPos1 <- function(nReps, n)                             #Question 6(c)
{
  results <- rep(NA, nReps)
  for(i in 1:nReps)
    results[i]<-rwalkPos(n)
  results
}
rwalkPos2 <- function(nReps, n)
{
  replicate( nReps, rwalkPos(n) )
}

rwalkPos3 <- function(nReps, n)                             #Question 6(d)
{
  stepWalks <- matrix( sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5)), nr=nReps )
  for(j in 2:n)
    stepWalks[,j] <- stepWalks[,j] + stepWalks[,j-1]
  stepWalks <- cbind(0, stepWalks)
  rowSums( stepWalks[,1:n] + stepWalks[,2:(n+1)]>0 )
}


#Exercise 5

tsEwma <- function( tsDat, m0=0, delta=0.7)           #Question 1(a)        
{ 
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=start(tsDat), frequency=frequency(tsDat))
}

tsEwma2 <- function( tsDat, m0=0, delta=0.7)            #Question 1(b)
{
  tsPars <- tsp(tsDat)
  tsDat <- c(tsDat)
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=tsPars[1], frequency=tsPars[3])
}

#Question 1(b) testing
tmp <- ts(rnorm(400000), start=c(1960,3), frequency=12)
system.time(tsEwma2(tmp))
system.time(tsEwma(tmp))


myListFn <- function(n)                          #Question 2(a)
{
  xVec <- rnorm(n)
  xBar <- mean(xVec)
  yVec <- sign(xBar)*rexp(n, rate=abs(1/xBar))
  count <- sum( abs(yVec) > abs(xVec) )
  list(xVec=xVec, yVec=yVec, count=count)
}

myList <- lapply( rep(10,4), myListFn )           #Question 2(b)
myMatrix <- sapply( rep(10,4), myListFn )
myMatrix[1,1]

lapply(myList, FUN=function(x){x[[2]]})           #Question 2(c)

sapply(myList, FUN="[[", 2)                            #Question 2(d)

myList2 <- lapply(myList, function(x){list(xVec=x$xVec, yVec=x$yVec)})  #Question 2(e)

which( unlist( lapply(myList, function(x){x[[3]]>2}) ) )       #Question 2(f)
myList[which( unlist(lapply( myList, function(x){x[[3]]>2} )) )]


partA <- sapply(myList, function(x){ sum(x$xVec*(1:10))/sum(x$yVec*(1:10)) })  #Question 3(a)

myMat <- t(sapply( myList, function(x){x$xVec-x$yVec}))         #Question 3(b)

sum(sapply(myList, function(x){x$xVec[2]})*(1:1000)) /                       #Question 3(c)
  sum(sapply(myList, function(x){x$yVec[2]})*sapply(myList, function(x){x$count}))

shift <- function(X,a,b){                                    #Question 5(a)
  X[,1] <- X[,1] + a 
  X[,2] <- X[,2] + b
  X
}

rotate <- function(X,r){                                     #Question 5(b)
  X%*%matrix(c(cos(r), -sin(r), sin(r), cos(r)), nrow = 2)
}


arrayA <- array(0, dim=c(5,2,25))                            #Question 5(c)
for(i in 1:25){ 
  arrayA[,,i] <- rotate(A,2*pi*(i-1)/24)
}


scale <- function(X,a,b){                             #Question 5(d)
  X%*%matrix(c(a,0,0,b), nrow=2)
}
arAscaled <- vapply(1:25,
                    FUN=function(i){
                      scale(arrayA[,,i],2,3)
                    },
                    matrix(0,nrow=5, ncol=2)
)
plot(c(-10,10), c(-10,10), ann=F, type='n')
invisible(sapply( 1:25, FUN=function(i){ drawA(arrayA[,,i]) } ))
invisible(sapply( 1:25, FUN=function(i){ drawA(arAscaled[,,i]) } ))