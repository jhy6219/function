mynewton<-function(g,x0,e){   #Newton method 함수 생성
  maxiter<-1000
  err<-1
  niter<-0
  while(niter<=maxiter && err >= e) {
    df.dx1<- genD(func= g, x= x0)$D[1] # g함수가 x=x0에 대해 한번 미분된 값
    df.dx2<- genD(func= g, x= x0)$D[2] # g함수가 x=x0에 대해 두번 미분된 값
    oldx0<- x0 
    x0<- x0 - (df.dx1/df.dx2)
    err<-abs(oldx0-x0) 
    niter<-niter+1
    print(paste(niter, x0, err, sep="  "))}
  return(x0)}
