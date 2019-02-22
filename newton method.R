mynewton<-function(g,x0,e){   #Newton method �Լ� ����
  maxiter<-1000
  err<-1
  niter<-0
  while(niter<=maxiter && err >= e) {
    df.dx1<- genD(func= g, x= x0)$D[1] # g�Լ��� x=x0�� ���� �ѹ� �̺е� ��
    df.dx2<- genD(func= g, x= x0)$D[2] # g�Լ��� x=x0�� ���� �ι� �̺е� ��
    oldx0<- x0 
    x0<- x0 - (df.dx1/df.dx2)
    err<-abs(oldx0-x0) 
    niter<-niter+1
    print(paste(niter, x0, err, sep="  "))}
  return(x0)}