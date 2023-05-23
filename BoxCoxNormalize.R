# Normalize Data

# library(MASS)
test <- CancerM[,3]
head(test)
hist(test)
ad.test(test)

boxcox(test~1)
# library(DescTools)
# ?BoxCoxLambda()
BoxCoxLambda(test, "loglik", lower = -2)
# BoxCoxLambda(test, "guerrero", lower = -2)
# Guerrero는 어떨때 쓰는걸까? 모르겠음.

test_boxcox <- log(test)
hist(test_boxcox)
ad.test(test_boxcox)
BoxCox(test, BoxCoxLambda(test, "guerrero", lower = -2)) %>%
    ad.test()
BoxCox(test, BoxCoxLambda(test, "loglik", lower = -2)) %>%
    ad.test()
# loglik이 더 좋으니까 이걸로 변환. 근데, normal 0.1정도면 좀 아쉽긴 함.

## 전체 변수에 대해 일괄 적용
CancerM_BoxCox <- tibble()
for(group in c("M", "B")){
    for(i in totalRange){
        # Mal에대해서 
        if(group == "M"){
            
        }
    }
}


## 함수 하나 제작
# BoxCox로 univariate dataset을 Normalize 시키는 함수
# Parameter : data
# return : normalized data
# 사용할때 DescTools, nortest packages가 필요함.
BoxCoxNormalize <- function(dat){
    guerreroLambda <- BoxCox(test, BoxCoxLambda(test, "guerrero", lower = -2)) %>%
        ad.test()
    loglikLambda <- BoxCox(test, BoxCoxLambda(test, "loglik", lower = -2)) %>%
        ad.test()
}