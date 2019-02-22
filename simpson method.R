mysimpson<-function(func,a,b,e,maxiter){ # a,b�� ���� ����
  niter<-0
  area<-100
  k<-1
  while(1){
    h=(b-a)/(2^k) #(2^k)���� ���� (a,b)subset�� ����
    point<-h*as.numeric(seq(from=2,to=(2^k),by=2))
    area.new<-(h/3)*sum(func(a+point-2*h)+4*func(a+point-h)+func(a+point)) #���� ���
    
    ### Break rule ###
    if(niter > maxiter || (abs(area.new - area)< e)){
      return(list(niter=niter,area=area.new))
      break
    }else{
      k<-k+1
      niter = niter+1
      area<-area.new
    }
  }
}
mysimpson(f1,0,pi,10^(-10),100)