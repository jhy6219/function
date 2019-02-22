myroute<-function(x){
  m<-matrix(c(   1,x[1],x[2],x[3],x[4],0,0,0,0,0,
                 x[1],1,x[5],0,0,x[8],x[9],0,0,0,
                 x[2],x[5],1,x[6],0,0,x[10],0,0,0,
                 x[3],0,x[6],1,x[7],0,0,x[11],0,0,
                 x[4],0,0,x[7],1,0,0,x[12],x[13],0,
                 0,x[8],0,0,0,1,x[14],0,0,x[17],
                 0,x[9],x[10],0,0,x[14],1,x[15],0,x[18],
                 0,0,0,x[11],x[12],0,x[15],1,x[16],x[19],
                 0,0,0,0,x[13],0,0,x[16],1,x[20],
                 0,0,0,0,0,x[17],x[18],x[19],x[20],1),ncol=10,byrow=T)
  new.m<-m
  check=10 # 10행에서 부터 시작 
  while(1){
    linked.p<-which(new.m[check,]>0) # 체크하려고 하는 행과 연결되어 있는 점 구하기 
    linked.p<-linked.p[which(linked.p<=check)] #이전에 했던 체크했던 행을 또 한번 해줄 필요는 없다
    extra.m<-rbind(new.m[linked.p,],matrix(rep(0,10),nrow=1))
    col.sum<-apply(extra.m,2,sum)  # 연결되어 있는 점의 행을 더하기
    to.put.row<-matrix(rep(col.sum,length(linked.p)),byrow=T,ncol=10)
    to.put.col<-matrix(rep(col.sum,length(linked.p)),nrow=10)
    new.m[linked.p,]<-to.put.row
    new.m[,linked.p]<-to.put.col
    res<-as.numeric(new.m[1,10]>0)
    ### Break rule ###
    if(check < 1 || new.m[1,10] > 0){
      return(res)
      break
    }else{
      check<-check-1
    }
  }
}
