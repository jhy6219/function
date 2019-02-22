mysecant<-function(g,x0,x1,e){   # Secant method 함수 생성
  maxiter<-1000
  err<-1
  niter<-0
  while(niter<=maxiter && err >= e) {
    df.dx0<- genD(func= g, x= x0)$D[1]
    df.dx1<- genD(func= g, x= x1)$D[1]
    oldx0<- x0 
    oldx1<- x1
    x0<- oldx1 
    x1<- oldx1- df.dx1*(oldx1 - oldx0)*(df.dx1-df.dx0)^(-1)
    err<-abs(oldx0-x0) 
    niter<-niter+1
    print(paste(niter, x0,x1, err, sep="  "))}
  return(x0)}
