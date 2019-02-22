mykmeans <- function(x, k, iter.max, nstart){
  savinglist<-list() # �� kmeans ���� ��� ���� ����Ʈ
  savingSWS.total<-c()
  for (j in 1:nstart){
    # �ʱⰪ ����
    x<-as.matrix(x) # x�� ���ȭ�ϱ�
    ndata = nrow(x)
    means = x[sample(ndata,k),] # �ʱ� kmeans ���۰� ���ϱ�
    iter = 1
    SWS.total.list = c()  #���ະ �׷쳻 ����(within)
    while(1){
      ##### �Ÿ����, �Ҵ���� ����� ####
      dist.mat = matrix(0,k,ndata) # ���� ��հ��� �����Ͱ��� �Ÿ��� ��Ÿ�� ���
      assign.mat = matrix(0,k,ndata) # ��� ������ �����ִ��� �����ִ� �Ҵ� ���
      for(i in 1:k){
        dist.mat[i,] = rowSums((x-matrix(1,ndata,1)%*%means[i,])^2) # �Ÿ� ���
      }
      for(i in 1:ndata){#�Ÿ���Ŀ��� �� ������(��)���� �ּڰ��� 1�ְ� �������� 0�ֱ�
        assign.mat[,i] = as.numeric(dist.mat[,i]==matrix(min(dist.mat[,i]),k,1)) # �Ҵ� ���
      }
      #### ����ȭ ��� ####
      means.new = assign.mat %*% x / apply(assign.mat,1,sum)
      WSS<-apply(assign.mat*dist.mat,1,sum)
      SWS.total<- sum(assign.mat*dist.mat)
      SWS.total.list[iter] <- SWS.total
      #### while�� �극��ũ ��Ģ####
      if(iter > 1){ # first iteration?
        if(abs(SWS.total.list[iter]-SWS.total.list[iter-1])/SWS.total.list[iter-1] < 10^-8 ||  iter >= iter.max){
          # cluster���� �Ÿ� ��ü �� �������� ���� ���� ���ų� ������ ������ �ִ� ���ప���� Ŀ�� ��
          cluster = apply(assign.mat * c(1:k), 2, sum) # �������� cluster ����
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