# Removed outliers
# Loading
CancerDataRm <- read.csv("~/Multivariate_Statistics_and_Lab/Cancer_data_removed_outliers.csv",
                         stringsAsFactors = T)

# Simple preprocessing
colnames(CancerData)
colnames(CancerDataRm)
# colnames 수정 및 순서 재배치
colnames(CancerDataRm)[c(1,32)] <- c("id", "diagnosis")
CancerDataRm <- CancerDataRm[,c(1,32,2:31)]

# Glimpse
glimpse(CancerDataRm) # id는 x로 값이 변함. 큰 의미 없으니 상관없음.
nrow(CancerData) -nrow(CancerDataRm) # 82개의 data가 삭제됨.

# Each Group preprocessing
CancerM_rm <- CancerDataRm[CancerDataRm$diagnosis == "M",]
CancerB_rm <- CancerDataRm[CancerDataRm$diagnosis == "B",]
c(nrow(CancerM), nrow(CancerM_rm)) ; nrow(CancerM) - nrow(CancerM_rm)
c(nrow(CancerB), nrow(CancerB_rm)) ; nrow(CancerB) - nrow(CancerB_rm)
# M에선 22개, B에선 60개가 제거됨.


### Yeo-Johnson 변환 연습
# library(car)
?yjPower()
test <- CancerM[,3]
temp <- boxCox(test~1, family = "yjPower")
str(temp)
temp$x[which.max(temp$y)] # <- 이게 Yeo-Johnsom transformation의 lambda

# yj과 boxcox의 transformed dataset 비교

# hist
par(mfrow = c(1,2))
yjPower(test, -0.06) %>% hist()
forecast::BoxCox(test, "auto") %>% hist()

# norm test
yjPower(test, -0.06) %>% shapiro.test()
forecast::BoxCox(test, "auto") %>% shapiro.test()

# yj가 boxcox보다 좀 더 나은 결과를 보이는 것 같음. 모든 경우에 대해 한게
# 아니기때문에 섣부른 판단은 조심해야 하지만, 아무튼 더 나은 것 같음.
# rm(temp) ; rm(test)
# yeo-johnson이 더 안좋은 경우가 나왔음. (참고-1)
# 따라서, transform 할때 음수가 있을때만 yeo를 쓰도록 조치하겠음


### Yeo-Johnson 변환 실전 using RmData

# mal에 대하여..
# 1. 변환
CancerM_rm_norm <- apply(CancerM_rm[,totalRange], 2, function(x){
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
CancerM_rm_norm_transformtype <- apply(CancerM_rm[,totalRange], 2, function(x){
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


# 2. nortest
CancerM_rm_normTest <- apply(CancerM_rm_norm, 2, function(x){
    shapiro.test(x)$p.value
})
CancerM_rm_normTest %>% hist()
CancerM_rm_normTest %>% sort() %>% head()
which(CancerM_rm_normTest < 0.05, arr.ind = T)
# 모든 transform의 p.value가 크게 나왔으면 좋았겠지만,
# 굳이 억지로 그렇게 할 필요는 없어보임(해석의 문제라던지, 과적합이라던지)
# 이정도에서 만족하고자 함.

# ben에 대하여..
# 1. 변환
CancerB_rm_norm <- apply(CancerB_rm[,totalRange], 2, function(x){
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
CancerB_rm_norm_transformtype <- apply(CancerB_rm[,totalRange], 2, function(x){
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

# 2. nortest
CancerB_rm_normTest <- apply(CancerB_rm_norm, 2, function(x){
    shapiro.test(x)$p.value
})
CancerB_rm_normTest %>% hist()
CancerB_rm_normTest %>% sort() %>% head() %>% round(3)
paste0("Normal이 아닌 변수의 갯수 : ",
       sum(CancerB_rm_normTest<0.05) )
# 0.05가 안되는게 훨씬 더 많음. 
# B에 대해서 power-based normalize는 욕심을 버려야할 듯 함.


# Data merge
CancerDataRm_norm <- CancerDataRm
CancerDataRm_norm[,totalRange] <- rbind(CancerM_rm_norm, CancerB_rm_norm) %>%
    as.data.frame(CancerDataRm_norm)
# write.csv(CancerDataRm_norm, file = "C:/Users/p5682/Documents/Multivariate_Statistics_and_Lab/CancerDataRm_normalized.csv", row.names = F)

glimpse(CancerDataRm_norm)
##################################################


# 참고1
test <- CancerM_rm[,20]
hist(test)
shapiro.test(test)
# temp : yj method
# temp2 : boxcox method
temp <- boxCox(test~1, family = "yjPower")
temp2 <- boxCox(test~1)
temp$x[which.max(temp$y)] -> lambdaA
temp2$x[which.max(temp2$y)] -> lambdaB
# lambdaA : yj method
# lambdaB : boxcox method
par(mfrow = c(1,3))
forecast::BoxCox(test, lambdaA) %>% hist()
yjPower(test, lambdaA) %>% hist()
forecast::BoxCox(test, lambdaB) %>% hist()
forecast::BoxCox(test, lambdaA) %>% shapiro.test()
yjPower(test, lambdaA) %>% shapiro.test()
forecast::BoxCox(test, lambdaB) %>% shapiro.test()
forecast::BoxCox(test, "auto") %>% shapiro.test()
# yj method는 car와 forecast를 혼용해서 쓸 수 없음을 알아냄.
# yj가 꼭 더 좋은 경우는 아님.
# lambda는 car packages를 통해 구하는게 더좋아보임.