rm(list=ls())

setwd("C:\\Users\\suin\\Desktop\\inha\\3-1\\다변량\\기말 팀플")
CancerDataRm_norm <- read.csv("CancerDataRm_normalized.csv")
install.packages("caret")
library(caret)
summary(CancerDataRm_norm)
glimpse(CancerDataRm_norm)


# Classify Test
ECM_rule_for_normalized <- function(x, inputdat = CancerDataRm_norm) {
  totalRange = 3:32
  mu_M = inputdat[inputdat$diagnosis=="M",totalRange] %>% apply(., 2, mean)
  sigma_M = inputdat[inputdat$diagnosis=="M",totalRange] %>% cov()
  mu_B = inputdat[inputdat$diagnosis=="B",totalRange] %>% apply(., 2, mean)
  sigma_B = inputdat[inputdat$diagnosis=="B",totalRange] %>% cov()
  
  temp1 = log(base::det(sigma_M)/base::det(sigma_B))
  temp2 = (solve(sigma_M) - solve(sigma_B))
  temp3 = (mu_M %*% solve(sigma_M) - mu_B %*% solve(sigma_B))
  temp4 = (mu_M %*% solve(sigma_M) %*% mu_M  - mu_B %*% solve(sigma_B) %*% mu_B)
  result = -0.5*x%*%temp2%*%x + temp3 %*% x -0.5*temp4 -0.5*temp1
  return(result)
}

### Leave one out cross validation (LOOCV)
# 하나의 데이터(training data)만 남기고 나머지 모든 데이터로 학습을 시켜 모델을 만드는 방법
# 그리고 다시 다른 하나의 데이터(taining data)만 남기고 나머지 모든 데이터로 학습을 시킨 후
# 데이터의 수(n) 만큼 반복해서 검증을 하는 방법

# 장점 : 거의 모든 데이터로 모델링해서 bias가 적음 
# 단점 : 1. n번 반복시행할 경우 n배 만큼 실행시간이 길어진다.
# 2. 검증을 위해서 뽑은 하나의 training data가 예측에 큰 영향을 미칠 수 있음
# 선택한 trainig data가 outlier였다면 모델은 최적의 모델이 될 수 있나, 검증 결과는 최악의 모델이 될수도 있고,
# 모델은 최악의 모델이었으나 검증 결과는 최적의 모델로 검증될수도 있는 오류가 발생함.

n <- nrow(CancerDataRm_norm)  # #data sample
accuracy <- numeric(n)  # 각각의 정확도를 저장할 벡터를 만듦

for (i in 1:n) {
  # taining data와 test data로 분할
  train_data <- CancerDataRm_norm[-i, ]  # i번째 샘플을 제외한 trainig data
  test_data <- CancerDataRm_norm[i, ]  # i번째 샘플을 test data
  
  # 모델 학습 및 예측
  Rule1 <- apply(test_data[, totalRange], 1, function(x) {
    input <- unlist(x)
    ifelse(ECM_rule_for_normalized(x, train_data) >= log(p2/p1), "M", "B")
  })
  
  # 정확도 계산하기
  accuracy[i] <- mean(Rule1 == test_data$diagnosis)
}

# LOOCV 결과 
cat("LOOCV Results:\n")
cat(paste0("Sample", 1:n, ": ", accuracy, "\n"))
cat("Mean Accuracy:", mean(accuracy), "\n")
# data sample개수인 487번을 반복하여 정확도를 계산한 방법
# 정확도의 평균을 계산





### k-fold cross validation
# LOOCV의 단점을 보완하기 위한 최적의 검증 방법
# bias되는 것을 막고, 최적의 비율로 trainin data와 test data를 분리해야함
# 대체적으로 k를 5 or 10으로 설정해서 검정함


k <- 10  # Fold 수를 10으로 설정
folds <- createFolds(CancerDataRm_norm$diagnosis, k = k, list = TRUE)  # 데이터를 10개의 Fold로 분할

# 교차 검증 수행
accuracy <- numeric(k)  # 각 Fold의 정확도를 저장할 벡터를 만듦

for (i in 1:k) {
  # training data와 test data 분할
  train_indices <- unlist(folds[-i])  # i번째 Fold를 제외한 나머지 Fold 인덱스
  test_indices <- folds[[i]]  # i번째 Fold 인덱스
  
  train_data <- CancerDataRm_norm[train_indices, ]  # training data
  test_data <- CancerDataRm_norm[test_indices, ]  # test data
  
  Rule1 <- apply(test_data[, totalRange], 1, function(x) {
    input <- unlist(x)
    ifelse(ECM_rule_for_normalized(x, train_data) >= log(p2/p1), "M", "B")
  })
  
  accuracy[i] <- mean(Rule1 == test_data$diagnosis) # 정확도 계산
}

# 교차 검증 결과 출력
cat("Cross Validation Results:\n")
cat(paste0("Fold", 1:k, ": ", accuracy, "\n"))
cat("Mean Accuracy:", mean(accuracy), "\n")

# 검증이 잘 이루어졌는지 fold 1을 통해서 확인
fold_1 <- apply(CancerDataRm_norm[folds==1,totalRange], 1, function(x){
  input <- unlist(x)
  ifelse(ECM_rule_for_normalized(x, CancerDataRm_norm[folds!=1,])>=log(p2/p1), "M", "B")
})
fold_1 <- factor(fold_1, levels = c("B", "M"))
mean(fold_1 == CancerDataRm_norm$diagnosis[folds==1]) # 정답률


## 교차검증의 결과
# 1. 평균 정확도가 높고 각 Fold의 정확도가 일관되게 높은 경우
# 모델이 잘 일반화되어 다른 데이터에 대해서도 좋은 예측을 수행할 것으로 기대할 수 있음.
# 2. 평균 정확도가 낮거나 각 Fold의 정확도가 크게 다른 경우
# 모델이 과적합된 것일 수 있음.