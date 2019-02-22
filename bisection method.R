mybisec<-function(g,a,b,e){      # bisection method �Լ� ����
  maxiter<-1000  # �ִ� ��������
  err<-1   # ���� �ʱⰪ ����
  niter<-0  # ���� ����
  x0<-(a+b)/2
  while ( niter<=maxiter && err >= e) {
    if (genD(func= g, x= a)$D[1]*genD(func= g, x= x0)$D[1] <=0)	{b<-x0}
    else	{a<-x0}
    oldx0<-x0
    x0<-(a+b)/2
    err<-abs(oldx0-x0)
    niter<-niter+1
    print(paste(niter,x0,err,sep="  "))}
  return(x0)}