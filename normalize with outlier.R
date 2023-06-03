CancerM_norm <- apply(CancerM[,totalRange], 2, function(x){
    # case 1. yjPower
    if(sum(x < 0) > 0){
        temp <- boxCox(x~1, family = "yjPower")
        lambda <- temp$x[which.max(temp$y)]
        yjPower(x, lambda)
    } 
    # case 2. Just BoxCox
    else {
        temp <- boxCox(x~1)
        lambda <- temp$x[which.max(temp$y)]
        forecast::BoxCox(x, lambda)
    }
})
CancerM_norm_transformtype <- apply(CancerM[,totalRange], 2, function(x){
    # case 1. yjPower
    if(sum(x < 0) > 0){
        temp <- boxCox(x~1, family = "yjPower")
        lambda <- temp$x[which.max(temp$y)]
        c("yj", lambda)
    } 
    # case 2. Just BoxCox
    else {
        temp <- boxCox(x~1)
        lambda <- temp$x[which.max(temp$y)]
        c("bc", lambda)
    }
})
### 얼마나 정규화가 잘 되었는가
CancerM_normTest <- apply(CancerM_norm, 2, function(x){
    shapiro.test(x)$p.value
})
CancerM_normTest %>% hist()
CancerM_normTest %>% sort() %>% head()
which(CancerM_normTest < 0.05, arr.ind = T)
### M에 대해서 정규화가 꽤 잘됐음.

CancerB_norm <- apply(CancerB[,totalRange], 2, function(x){
    # case 1. yjPower
    if(sum(x <= 0) >= 0){
        temp <- boxCox(x~1, family = "yjPower")
        lambda <- temp$x[which.max(temp$y)]
        yjPower(x, lambda)
    } 
    # case 2. Just BoxCox
    else {
        temp <- boxCox(x~1)
        lambda <- temp$x[which.max(temp$y)]
        forecast::BoxCox(x, lambda)
    }
})
CancerB_norm_transformtype <- apply(CancerB[,totalRange], 2, function(x){
    # case 1. yjPower
    if(sum(x <= 0) >= 0){
        temp <- boxCox(x~1, family = "yjPower")
        lambda <- temp$x[which.max(temp$y)]
        c("yj", lambda)
    } 
    # case 2. Just BoxCox
    else {
        temp <- boxCox(x~1)
        lambda <- temp$x[which.max(temp$y)]
        c("bc", lambda)
    }
})
CancerB_normTest <- apply(CancerB_norm, 2, function(x){
    shapiro.test(x)$p.value
})
CancerB_normTest %>% hist()
CancerB_normTest %>% sort() %>% head() %>% round(3)
paste0("Normal이 아닌 변수의 갯수 : ",
       sum(CancerB_normTest<0.05) )
### B는 정규화 잘 안됐음.

## 지금은 정규화 코드가 최적화돼서 그런건지 모르겟는데, 정규화가 꽤 잘됨.
## 이전 정규화 코드 : just using forecast::BoxCox
## BoxCox의 lambda를 함수 자체에서 auto로 설정시켰었음.
## 지금 정규화 코드 : lambda를 car packages의 lambda 구하는 코드와 병행해서 씀.
