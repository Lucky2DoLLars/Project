동등화 방법 비교 모의 데이터 생성 함수
================
2DoLLars

아래는 동등화 방법 비교를 위한 모의 데이터 생성을 수행하는 함수들이다.

#### 상대빈도 생성함수

``` r
freq_v = function(score_mat,num_d){
  #score_mat = 점수벡터
 
  f = as.factor(score_mat)           #팩터로 변환
  t = table(f)                       #빈도테이블 생성
  l = c(as.integer(levels(f)))       #점수벡터 생성
  freq_v = rep(0,num_d+1)              #빈도벡터 생성
  for (i in 1:length(l)){            #빈도벡터에 각 점수에 해당하는 빈도를 입력
    freq_v[l[i]+1] <- t[i]
  }
  freq_v = freq_v/sum(freq_v)        #상대빈도 벡터생성
  return(freq_v)
}
```

#### bais 벡터 생성함수

``` r
bias = function(eq_hat,eq_pop){
  bias_vector = rep(0,nrow(eq_hat))
  for (i in 1:nrow(eq_hat)){
    bias_vector[i] <- mean(eq_hat[i,])-eq_pop[i]
  }
  return(bias_vector)
}  
```

#### w.bias 생성함수

``` r
w_bias = function(eq_hat,eq_pop,score_mat,num_d){
  n = nrow(eq_hat)
  
  bias_vector = rep(0,n)
  for (i in 1:n){
    bias_vector[i] <- mean(eq_hat[i,])-eq_pop[i]
  }
  sum_v = rep(0,R)
  f = freq_v(score_mat,num_d)
  for (i in 1:n){
    sum_v[i] <- f[i] * bias_vector[i]^2
  }
  
  return(sqrt(sum(sum_v)))
}
```

#### se 벡터 생성함수

``` r
se = function(eq_hat,R){
  n = nrow(eq_hat)
  
  se_v = rep(0,n)
  for (i in 1:n){
    sum_r = rep(0,R)
    eq_hat_mean = mean(eq_hat[i,])
    for (j in 1:R){
      sum_r[j] <- (eq_hat[i,j] - eq_hat_mean)^2
    }
    se_v[i] <- sqrt(sum(sum_r)/R)
  }
  se_v
  return(se_v)
}
```

#### w.se 생성함수

``` r
w_se = function(eq_hat,R,score_mat,num_d){
  n = nrow(eq_hat)
  
  se_v = rep(0,n)
  for (i in 1:n){
    sum_r = rep(0,R)
    eq_hat_mean = mean(eq_hat[i,])
    for (j in 1:R){
      sum_r[j] <- (eq_hat[i,j] - eq_hat_mean)^2
    }
    se_v[i] <- sqrt(sum(sum_r)/R)
  }
  
  sum_v = rep(0,n)
  f = freq_v(score_mat,num_d)
  for (i in 1:n){
    sum_v[i] <- f[i] * se_v[i]^2
  }
  return(sqrt(sum(sum_v)))
}
```

#### RMSE 벡터 생성함수

``` r
RMSE = function(eq_hat,eq_pop,R){
  n = nrow(eq_hat)
  
  rmse_v = rep(0,n)
  for (i in 1:n){
    sum_r = rep(0,R)
    for (j in 1:R){
      sum_r[j] <- (eq_hat[i,j] - eq_pop[i])^2
    }
    rmse_v[i] <- sqrt(sum(sum_r)/R)
  }
  return(rmse_v)
}
```

#### RMSE plot 생성함수

``` r
plot_RMSE = function(Q,P,N,w){
  #Q: 문항수, P: 공통문항% , N: 피험자수
  
  n <- 10000
  R <- 500
  num_d <- Q-Q*P   #비공통문항수
  num_c <- Q*P     #공통문항수
  sample_v <- 1:n
  
  
  output1 <- output2 <- output3 <- output4 <- output5 <- output6 <- output7 <-matrix(0,num_d+1,R)
  
  
  
  for (k in 1:R) {
    #0매트릭스 생성
    cat(" ", k, " ")   #계산속도점검을 위해 표시
    dataX <- dataY <- matrix(0, nrow=n, ncol=num_d)
    dataVx <- dataVy <- matrix(0, nrow=n, ncol=num_c)
    
    
    for(i in sample_v) {
      bX <- rnorm(num_d,rnf[w,1], rnf[w,2])
      bY <- rnorm(num_d,rnf[w,3], rnf[w,4])
      tX <- rnorm(num_d,rnf[w,5], rnf[w,6])
      tY <- rnorm(num_d,rnf[w,7], rnf[w,8])
      
      rQX<-runif(num_d)
      rQY<-runif(num_d)
      
      a<-rlnorm(num_d,0,0.3)
      c<-runif(num_d,0.05,0.35)
      
      dataX[i,] <-(c+(1-c)/(1+exp(-a*(tX-bX))))>rQX
      dataY[i,] <-(c+(1-c)/(1+exp(-a*(tY-bY))))>rQY
    }
    
    for (i in sample_v) {
      if (w <= 21) {b <- rnorm(num_c,-1.3,1)}
      else if (w >=22 &  w <= 42) {b <- rnorm(num_c,0,1)}
      else {b <- rnorm(num_c, 1.3,1)}
      
      a <- rlnorm(num_c,0,0.3)
      c <- runif(num_c,0.05,0.35)
      t <- rnorm(num_c)
      
      rQV <- runif(1)
      
      dataVx[i,] <- (c+(1-c)/(1+exp(-a*(t-b)))) > rQV
      dataVy[i,] <- (c+(1-c)/(1+exp(-a*(t-b)))) > rQV
    }
    
    
    sX <- dataX[sample(sample_v, N, replace=TRUE),]
    sY <- dataY[sample(sample_v, 100, replace=TRUE),]
    sVx <- dataVx[sample(sample_v, N, replace=TRUE),]
    sVy <- dataVy[sample(sample_v, 100, replace=TRUE),]
    
    X <- rowSums(sX)
    Y <- rowSums(sY)
    Vx <- rowSums(sVx)
    Vy <- rowSums(sVy)

    cX <- rowSums(dataX)
    cY <- rowSums(dataY)
    cVx <- rowSums(dataVx)
    cVy <- rowSums(dataVy)
    
    s = list(0:num_d,0:num_c)
    edataX <- freqtab(cbind(X,Vx),scales = s)
    edataY <- freqtab(cbind(Y,Vy),scales = s)
    cedataX <- freqtab(cbind(cX,cVx),scales = s)
    cedataY <- freqtab(cbind(cY,cVy),scales = s)
    
    
    fit1 <-equate(edataX, edataY, type = "mean")
    fit2 <-equate(edataX, edataY, type = "m", method = "nominal weight")
    fit3 <-equate(edataX, edataY, type = "l", method = "tucker")
    fit4 <-equate(edataX, edataY, type = "identity")
    fit5 <-equate(edataX, edataY, type = "l", method = "chained")
    fit6 <-equate(edataX, edataY, type = "circle")
    fit7 <-composite(list(fit4,fit5), wc=0.5, symmetric = T)
    cfit <-equate(cedataX, cedataY, type = "equip", method = "freq", smooth='log')
    
    
    output1[,k] <- fit1$conc$yx
    output2[,k] <- fit2$conc$yx
    output3[,k] <- fit3$conc$yx
    output4[,k] <- fit4$conc$yx
    output5[,k] <- fit5$conc$yx
    output6[,k] <- fit6$conc$yx
    output7[,k] <- fit7[[1]]$conc$yx
  }
  
  ccrit <- cfit$conc$yx
  
  
  RMSE_list <- list(RMSE(output1,ccrit,R),
                    RMSE(output2,ccrit,R),
                    RMSE(output3,ccrit,R),
                    RMSE(output4,ccrit,R),
                    RMSE(output5,ccrit,R),
                    RMSE(output6,ccrit,R),
                    RMSE(output7,ccrit,R))
  
  
  
  max_RMSE <- max(unlist(RMSE_list))
  min_RMSE <- min(unlist(RMSE_list))
  
  
  plot(1:(num_d+1),RMSE_list[[1]],ylim=c(min_RMSE,max_RMSE),
       type='b',col=1,lty=2,pch=1,xlab="Score",ylab="RMSE",cex=1.5)
  
  for (i in 2:7){
    lines(1:(num_d+1),RMSE_list[[i]],col=i,type='b',lty=2,pch=i,cex=1.5)}
  
  legend('topright', c('mean','nomi','tuck','iden','chai','chir','synthetic'),pch=c(1,2,3,4,5,6,7),col=1:7)

  
}
```

#### w.RMSE 생성함수

``` r
w_RMSE = function(eq_hat,eq_pop,R,score_mat,num_d){
  n = nrow(eq_hat)
  
  rmse_v = rep(0,n)
  for (i in 1:n){
    sum_r = rep(0,R)
    for (j in 1:R){
      sum_r[j] <- (eq_hat[i,j] - eq_pop[i])^2
    }
    rmse_v[i] <- sqrt(sum(sum_r)/R)
  }
  sum_v = rep(0,n)
  f = freq_v(score_mat,num_d)
  for (i in 1:n){
    sum_v[i] <- f[i] * rmse_v[i]^2
  }
  return(sqrt(sum(sum_v)))
}


#RMSE 값 시뮬레이션 함수
simul_RMSE <- function(Q,P,N,w){
  #Q: 문항수, P: 공통문항% , N: 피험자수
  
  n <- 10000
  R <- 500
  num_d <- Q-Q*P   #비공통문항수
  num_c <- Q*P     #공통문항수
  sample_v <- 1:n
  output1 <- output2 <- output3 <- output4 <- output5 <- output6 <- output7 <-matrix(0,num_d+1,R)
  

  for (k in 1:R){
    if ((k%%100)==0){
      cat(" ",k, " ")} #계산속도점검을 위해 표시
    dataX <- dataY <- matrix(0, nrow=n, ncol=num_d)
    dataVx <- dataVy <- matrix(0, nrow=n, ncol=num_c)
    
    
    for(i in sample_v) {
      bX <- rnorm(num_d,rnf[w,1], rnf[w,2])
      bY <- rnorm(num_d,rnf[w,3], rnf[w,4])
      tX <- rnorm(num_d,rnf[w,5], rnf[w,6])
      tY <- rnorm(num_d,rnf[w,7], rnf[w,8])
      
      rQX<-runif(num_d)
      rQY<-runif(num_d)
      
      a<-rlnorm(num_d,0,0.3)
      c<-runif(num_d,0.05,0.35)
      
      dataX[i,] <-(c+(1-c)/(1+exp(-a*(tX-bX))))>rQX
      dataY[i,] <-(c+(1-c)/(1+exp(-a*(tY-bY))))>rQY
    }
    
    for (i in sample_v) {
      if (w <= 21) {b <- rnorm(num_c,-1.3,1)}
      else if (w >=22 &  w <= 42) {b <- rnorm(num_c,0,1)}
      else {b <- rnorm(num_c, 1.3,1)}
      
      a <- rlnorm(num_c,0,0.3)
      c <- runif(num_c,0.05,0.35)
      t <- rnorm(num_c)
      
      rQV <- runif(1)
      
      dataVx[i,] <- (c+(1-c)/(1+exp(-a*(t-b)))) > rQV
      dataVy[i,] <- (c+(1-c)/(1+exp(-a*(t-b)))) > rQV
    }

    sX <- dataX[sample(sample_v, N, replace=TRUE),]
    sY <- dataY[sample(sample_v, 100, replace=TRUE),]
    sVx <- dataVx[sample(sample_v, N, replace=TRUE),]
    sVy <- dataVy[sample(sample_v, 100, replace=TRUE),]
    
   
    X <- rowSums(sX)
    Y <- rowSums(sY)
    Vx <- rowSums(sVx)
    Vy <- rowSums(sVy)
  
    
    cX <- rowSums(dataX)
    cY <- rowSums(dataY)
    cVx <- rowSums(dataVx)
    cVy <- rowSums(dataVy)
    

    s = list(0:num_d,0:num_c)
    edataX <- freqtab(cbind(X,Vx),scales = s)
    edataY <- freqtab(cbind(Y,Vy),scales = s)
    cedataX <- freqtab(cbind(cX,cVx),scales = s)
    cedataY <- freqtab(cbind(cY,cVy),scales = s)
    
    
    fit1 <- equate(edataX, edataY, type = "mean")
    fit2 <- equate(edataX, edataY, type = "m", method = "nominal weight")
    fit3 <- equate(edataX, edataY, type = "l", method = "tucker")
    fit4 <- equate(edataX, edataY, type = "identity")
    fit5 <- equate(edataX, edataY, type = "l", method = "chained")
    fit6 <- equate(edataX, edataY, type = "circle")
    fit7 <- composite(list(fit4,fit5), wc=0.5, symmetric = T)
    cfit <-equate(cedataX, cedataY, type = "equip", method = "freq")
    
    output1[,k] <- fit1$conc$yx
    output2[,k] <- fit2$conc$yx
    output3[,k] <- fit3$conc$yx
    output4[,k] <- fit4$conc$yx
    output5[,k] <- fit5$conc$yx
    output6[,k] <- fit6$conc$yx
    output7[,k] <- fit7[[1]]$conc$y
    
  }

  ccrit <- cfit$conc$yx
  
  
  
  #동등화 오차 산출
  #첫번째:wbias, 두번째:wSe, 세번째:wRMSE
  return(c(w_RMSE(output1,ccrit,R,cX,num_d),
           w_RMSE(output2,ccrit,R,cX,num_d),
           w_RMSE(output3,ccrit,R,cX,num_d),
           w_RMSE(output4,ccrit,R,cX,num_d),
           w_RMSE(output5,ccrit,R,cX,num_d),
           w_RMSE(output6,ccrit,R,cX,num_d),
           w_RMSE(output7,ccrit,R,cX,num_d)))
  
}
```

#### ANOVA를 위한 데이터테이블 생성함수

``` r
data_result <- function(Q,P,N){
  

  #빈 데이터프레임 생성
  data = data.frame(w.rmse=0,level=0,level_diff=0,
                    abil_mean=0,abil_std=0,eq_way=0,num_subject=0)
  
  
    for (w in 1:63){
      
      cat(w)
      
      idx_v <- (w+6*(w-1)):(w*7) #인덱스생성
      
      
      if (rnf[w,3]==-1.3){level='e'         #난이도판별 e:easy, i:intermediate, h:hard
      } else if (rnf[w,3]==0){level='i'
      } else {level='h'}      
      
      
      diff = rnf[w,1]-rnf[w,3]
      if (diff==0) {level_diff='s'              #난이도차판별 s:same, f:few, l:lot, fy:few(y>x), ly:lot(y>x)
      } else if (diff==0.3){level_diff='f'  
      } else {level_diff='l'}

   
      
      if (rnf[w,5]==0) {abil_mean='s'             #피험자능력차이(평균)
      } else if (rnf[w,5]==0.1) {abil_mean='f'
      } else {abil_mean='l'}

      
      
      if (rnf[w,6]==1) {abil_std='s'              #피험자능력차이(표준편차)
      } else if (rnf[w,6]==0.64) {abil_std='low1'
      } else {abil_std='low2'}
      
      
      output <- simul_RMSE(Q,P,N,w)
      
      
      data[idx_v[1],] <- c(output[1],level,level_diff,abil_mean,abil_std,'me',N) #mean
      data[idx_v[2],] <- c(output[2],level,level_diff,abil_mean,abil_std,'no',N) #nomi
      data[idx_v[3],] <- c(output[3],level,level_diff,abil_mean,abil_std,'tu',N) #tuck
      data[idx_v[4],] <- c(output[4],level,level_diff,abil_mean,abil_std,'id',N) #iden
      data[idx_v[5],] <- c(output[5],level,level_diff,abil_mean,abil_std,'ch',N) #chai
      data[idx_v[6],] <- c(output[6],level,level_diff,abil_mean,abil_std,'ci',N) #circ
      data[idx_v[7],] <- c(output[7],level,level_diff,abil_mean,abil_std,'sy',N) #synthetic
    }
  

  data$level <- factor(as.vector(data$level),levels=c('e','i','h'))
  data$level_diff <- factor(as.vector(data$level_diff),levels=c('s','f','l'))
  data$abil_mean <- factor(as.vector(data$abil_mean),levels=c('s','f','l'))
  data$abil_std <- factor(as.vector(data$abil_std),levels=c('s','low1','low2'))
  data$eq_way <- factor(as.vector(data$eq_way),levels=c('me','no','tu','id','ch','ci','sy'))
  data$num_subject <- factor(as.vector(data$num_subject),levels=c('25','50','75','100','125','150'))
  
  
  write.csv(data,paste('data',as.character(Q), as.character(P),as.character(N),'.csv', sep='_'))
  
  
  return(data)
}
```

``` r
data_result_uncorrected <- function(Q,P){

  #빈 데이터프레임 생성
  data = data.frame(w.rmse=0,level=0,level_diff=0,
                    abil_mean=0,abil_std=0,eq_way=0,num_subject=0)
  
  for (n in 1:6){
    
    cat("|N=", n*25, "|")
    num_subject <- n*25   #피험자수
    
    for (w in 1:195){
      
      idx_v <- (1365*(n-1)+w+6*(w-1)):(1365*(n-1)+w*7) #인덱스생성
      

      if (rnf[w,3]==-1.3){level='e'         #난이도판별 e:easy, i:intermediate, h:hard
      } else if (rnf[w,3]==0){level='i'
      } else {level='h'}      
      
    
      diff = rnf[w,1]-rnf[w,3]
      if (diff==0) {level_diff='s'              #난이도차판별 s:same, f:few, l:lot, fy:few(y>x), ly:lot(y>x)
      } else if (diff==0.3){level_diff='f'  
      } else if (diff==0.5){level_diff='l'
      } else if (diff==-0.3){level_diff='fy'
      } else {level_diff='ly'}
      
      
      
      if (rnf[w,5]==0) {abil_mean='s'             #피험자능력차이(평균)
      } else if (rnf[w,5]==0.1) {abil_mean='f'
      } else if (rnf[w,5]==0.25) {abil_mean='l'
      } else if (rnf[w,5]==-0.1) {abil_mean='fy'
      } else {abil_mean='ly'}
      
      
      
      if (rnf[w,6]==1) {abil_std='s'              #피험자능력차이(표준편차)
      } else if (rnf[w,6]==0.64) {abil_std='low1'
      } else {abil_std='low2'}
      
      
      
      output <- simul_RMSE(Q,P,num_subject,w)
      
      data[idx_v[1],] <- c(output[1],level,level_diff,abil_mean,abil_std,'me',num_subject) #mean
      data[idx_v[2],] <- c(output[2],level,level_diff,abil_mean,abil_std,'no',num_subject) #nomi
      data[idx_v[3],] <- c(output[3],level,level_diff,abil_mean,abil_std,'tu',num_subject) #tuck
      data[idx_v[4],] <- c(output[4],level,level_diff,abil_mean,abil_std,'id',num_subject) #iden
      data[idx_v[5],] <- c(output[5],level,level_diff,abil_mean,abil_std,'ch',num_subject) #chai
      data[idx_v[6],] <- c(output[6],level,level_diff,abil_mean,abil_std,'ci',num_subject) #circ
      data[idx_v[7],] <- c(output[7],level,level_diff,abil_mean,abil_std,'sy',num_subject) #synthetic
    }
  }
  
  
  data$level <- factor(as.vector(data$level),levels=c('e','i','h'))
  data$level_diff <- factor(as.vector(data$level_diff),levels=c('s','f','l','fy','ly'))
  data$abil_mean <- factor(as.vector(data$abil_mean),levels=c('s','f','l','fy','ly'))
  data$abil_std <- factor(as.vector(data$abil_std),levels=c('s','low1','low2'))
  data$eq_way <- factor(as.vector(data$eq_way),levels=c('me','no','tu','id','ch','ci','sy'))
  data$num_subject <- factor(as.vector(data$num_subject),levels=c('25','50','75','100','125','150'))
  
  
  write.csv(data,paste('data',as.character(Q), as.character(P),'.csv', sep='_'))
  
  
  return(data)
}
```
