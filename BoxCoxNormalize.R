# Normalize Data

# library(MASS)
test <- CancerM[,3]
head(test)
hist(test)
ad.test(test)

MASS::boxcox(test~1)
# library(forecast)
forecast::BoxCox(test, "auto")
# 원래 DescTools에서 썼는데, forecast라는 packages에서 더 좋은
# 방법이 있어서 이 방법을 하려고 함.

test_boxcox <- log(test)
hist(test_boxcox)
sd(test_boxcox)/sqrt(length(test_boxcox))
ad.test(test_boxcox)
sf.test(test_boxcox)
lillie.test(test_boxcox)
shapiro.test(test_boxcox)
qqnorm(test_boxcox)
qqline(test_boxcox, col = "red")
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3693611/
# 해당 사이트를 참고하여, shapiro.test를 사용하겠음.
test_boxcox <- forecast::BoxCox(test, "auto")
test_boxcox %>% shapiro.test()
# 0.1정도면 아쉽긴 한데, 아무튼 유의수준 0.05에서 정규성 가정 가능함.

# 단변량에 대해, BoxCox transform 적용을 테스트해보았음.


## 전체 변수에 대해 일괄 적용
# CancerM에 대해 transformed value
CancerM_BoxCox <- apply(CancerM[,totalRange], 2, function(x){
    forecast::BoxCox(x, "auto")
})

# CancerM에 대해 normal transform 한 다음 p.value
CancerM_BoxCox_pval <- apply(CancerM_BoxCox, 2, function(x){
    shapiro.test(x)$p.value
})

# 변환했는데도 normal이 아니라고 생각되는 경우를 확인
CancerM_BoxCox_pval[CancerM_BoxCox_pval<0.05]
(notNormIndx <- which(CancerM_BoxCox_pval<0.05) )
# 그런 경우의 transformed자료 형태
CancerM_BoxCox[,notNormIndx] %>% as.data.frame() %>% ggpairs()
# outlier 때문에 제대로 적용이 안된 상황
## -> anomaly detection 이후 재적용 하면 좋은 결과를 얻을 수 있을 것 같음.

# 그런 경우의 원본 자료 형태
CancerData[,notNormIndx + 2] %>% ggpairs()

#####
# CancerB에 대해 transformed value
CancerB_BoxCox <- apply(CancerB[,totalRange], 2, function(x){
    forecast::BoxCox(x, "auto")
}) # Error
# not positive라서 BoxCox가 진행되지 않음.
# Mal에 대해서도, 사실 음수때문에 BoxCox를 loglik 추정법으로 얻은 lambda를 이용했을때
# 적용이 안되는 case가 있었음. guerrero method로 하면 적용이 되었기에 무시했지만..

## -> 음수 value 때문에 box-cox 제대로 적용이 안됨
## -> 두가지 해결 방법 1. 최솟값을 적당히 더해서 양수화 2. Yeo-johnson transformation
# 2번 선택.
# 음수에 대해서도 적용 가능한 Yeo-Johnson transform을 적용해보려함.
?car::yjPower()
par(mfrow = c(1,2))
temp <- boxCox(test~1, family = "yjPower")
str(temp)
temp$x[which.max(temp$y)]
boxCox(test~1)
?BoxCoxLambda()
yj_trans(test)
# CancerM에 대해 normal transform 한 다음 p.value
CancerM_BoxCox_pval <- apply(CancerM_BoxCox, 2, function(x){
    shapiro.test(x)$p.value
})

# 변환했는데도 normal이 아니라고 생각되는 경우를 확인
CancerM_BoxCox_pval[CancerM_BoxCox_pval<0.05]
(notNormIndx <- which(CancerM_BoxCox_pval<0.05) )
# 그런 경우의 transformed자료 형태
CancerM_BoxCox[,notNormIndx] %>% as.data.frame() %>% ggpairs()
# outlier 때문에 제대로 적용이 안된 상황
## -> anomaly detection 이후 재적용 하면 좋은 결과를 얻을 수 있을 것 같음.

# 그런 경우의 원본 자료 형태
CancerData[,notNormIndx + 2] %>% ggpairs()




# 함수 필요 없어짐.