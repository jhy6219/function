myrieman<-function(func,a,b,e,maxiter){ # a,b�� ���� ����
  niter<-0
  area<-100
  k<-0
  while(1){
    h=(b-a)/(2^k) #(2^k)���� ���� (a,b)subset�� ����
    point<-h*c(1:(2^k)-1)
    area.new<-sum(h*func(a+point)) # ���� ���
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
myrieman(f1,0,pi,10^(-10),100)