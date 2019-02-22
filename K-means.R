mykmeans <- function(x, k, iter.max, nstart){
  savinglist<-list() # 각 kmeans 시행 결과 저장 리스트
  savingSWS.total<-c()
  for (j in 1:nstart){
    # 초기값 설정
    x<-as.matrix(x) # x를 행렬화하기
    ndata = nrow(x)
    means = x[sample(ndata,k),] # 초기 kmeans 시작값 구하기
    iter = 1
    SWS.total.list = c()  #시행별 그룹내 간격(within)
    while(1){
      ##### 거리행렬, 할당행렬 만들기 ####
      dist.mat = matrix(0,k,ndata) # 군집 평균값과 데이터간의 거리를 나타난 행렬
      assign.mat = matrix(0,k,ndata) # 어느 군집에 속해있는지 보여주는 할당 행렬
      for(i in 1:k){
        dist.mat[i,] = rowSums((x-matrix(1,ndata,1)%*%means[i,])^2) # 거리 행렬
      }
      for(i in 1:ndata){#거리행렬에서 각 데이터(열)마다 최솟값에 1넣고 나머지에 0넣기
        assign.mat[,i] = as.numeric(dist.mat[,i]==matrix(min(dist.mat[,i]),k,1)) # 할당 행렬
      }
      #### 군집화 결과 ####
      means.new = assign.mat %*% x / apply(assign.mat,1,sum)
      WSS<-apply(assign.mat*dist.mat,1,sum)
      SWS.total<- sum(assign.mat*dist.mat)
      SWS.total.list[iter] <- SWS.total
      #### while문 브레이크 규칙####
      if(iter > 1){ # first iteration?
        if(abs(SWS.total.list[iter]-SWS.total.list[iter-1])/SWS.total.list[iter-1] < 10^-8 ||  iter >= iter.max){
          # cluster내의 거리 전체 합 제곱값에 차이 거의 없거나 시행이 지정한 최대 시행값보다 커질 때
          cluster = apply(assign.mat * c(1:k), 2, sum) # 데이터의 cluster 배정
          break
        }else{
          iter <- iter+1
          means <- means.new # update means
        }
      }else{
        iter <- iter+1
        means <- means.new # update means
      }
    }
    savinglist[[j]]<-list(cluster=cluster, iter=iter, means=means, WSS=WSS,
                          SWS.total=SWS.total, SWS.total.list=SWS.total.list)
    savingSWS.total[j]<-SWS.total
  }
  return(savinglist[[which.min(savingSWS.total)]])
}
