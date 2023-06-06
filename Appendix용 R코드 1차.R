# 통합 R 코드
# Word 첨부용 코드

# 사용한 Packages
library(tidyverse) # dplyr..etc
library(forecast) # BoxCox transformation
library(GGally) # Better visualize pairs plot
library(car) # BoxCox transformation
library(biotools) # cov.mat의 동질성 검정
library(MASS) # PCA를 통한 Classification

# Load data and Declare notation
CancerData <- read.csv("C:/Users/p5682/Documents/Multivariate_Statistics_and_Lab/Cancer_data.csv", stringsAsFactors = T)
CancerDataRm <- read.csv("C:/Users/p5682/Documents/Multivariate_Statistics_and_Lab/Cancer_data_removed_outliers.csv", stringsAsFactors = T)
CancerData <- CancerData[,-33]
CancerM <- CancerData[CancerData$diagnosis=="M",]
CancerB <- CancerData[CancerData$diagnosis=="B",]
meanRange <- 3:12
seRange <- 13:22
worstRange <- 23:32
totalRange <- 3:32
variNames <- colnames(CancerData)[meanRange] %>% substr(., 0, nchar(.) -5)







## 데이터셋 간단한 EDA
data <- CancerData
data.1 <- data[,3:12]
head(data.1)
sd_data <- vector()
for (i in 1:10){
    print(summary(data.1[,i]))
    mean.sd_data <- append(sd(data.1[,i]), sd_data)
}
print(mean.sd_data)
which.min(mean.sd_data); which.max(mean.sd_data)


par(mfrow=c(2,5))
hist(data$radius_mean)
hist(data$texture_mean)
hist(data$perimeter_mean)
hist(data$area_mean)
hist(data$smoothness_mean)
hist(data$compactness_mean)
hist(data$concavity_mean)
hist(data$concave.points_mean)
hist(data$symmetry_mean)
hist(data$fractal_dimension_mean)

# 전반적으로 왼쪽으로 치우쳐진 모양이다.
# 그나마 smoothness와 symmerty의 평균이 정규분포를 따르는 것처럼 보인다.

par(mfrow=c(1,1))

qqnorm(data$smoothness_mean, main='smoothness_mean')
qqline(data$smoothness_mean, col="red")
# 대체적으로 직선에 가까워 보이지만, 꼬리쪽 부분이 직선에 가깝지 않은 것을 볼 수 있다.

qqnorm(data$symmetry_mean, main='symmetry_mean')
qqline(data$symmetry_mean, col="red")
# smoothness보다 덜 직선에 가까워 보인다.





# PCA를 거친 후 Classification
a <- CancerDataRm
glimpse(a)
cancer=a[,-1]

# Changing dignosis as factor
cancer$diagonosis <- as.factor(cancer$diagonosis)
dim(cancer)

###PCA분석

colnames(cancer)
# Removing last column from data
cancer2 <- cancer[, -31]

pca_cancer<-prcomp(cancer2, center=T, scale=T)
screeplot(pca_cancer, npcs=30, type="l")
summary(pca_cancer)
#Cumulative Proportion 이 85% 이상이 될때까지사용
#PC5까지 사용 #원 데이터의 85% 정도를 보존한다고 이해할 수 있다. 

##차원 축소된 데이터 생성
x_pca <- as.data.frame(pca_cancer$x[,1:5])
x_pca
#diagonosis를 데이터프레임에 추가
x_pca$diag <- as.factor(cancer$diagonosis)
x_pca
View(x_pca)

glimpse(x_pca)

#p1 <- nrow(cancerM)/nrow(x_pca)
#p2 <- 1-p1  
#p1/p2

# library(MASS)

#Then split the dataset into training and test data.
set.seed(11)
tr_index = sample(1:nrow(x_pca),0.7*nrow(x_pca), replace=FALSE)
train=x_pca[tr_index,]
test=x_pca[-tr_index,]


#다변량 정규분포 따를 때, 공분산 행렬까지 같다고 가정 LDA(Linear discriminant analysis)
##LDA

#정규화 가정 후 Classification 진행
cancerM <- x_pca[x_pca$diag=="M",]
mpca <- cancerM[,1:5]
cancerB <- x_pca[x_pca$diag=="B",]
bpca <- cancerB[,1:5]

mu_M =apply(mpca,2,mean)
sigma_M = cov(mpca)
mu_B = apply(bpca,2,mean)
sigma_B = cov(bpca)
# X_Mal ~ N(mu_M, sigma)을 따른다고 가정.
# X_Ben ~ N(mu_B, sigma)를 따른다고 가정.


lda.fit <- lda(diag~PC1+PC2+PC3+PC4+PC5, data=train)
lda.fit
#Prior probabilities of groups: 확률의 가설을 분석하기 전에 사용
#Group means: 각 변수의 평균
#Coefficients of linear discriminants: LDA분석으로 계산된 판별함수식의 계수

#반응변수 Y 예측
lda.pred <- predict(lda.fit, test)
lda.class <- lda.pred$class
head(lda.class)

tab1 <- table(lda.class, test$diag )
tab1
#정분류율= accuracy (90+50)/147
sum(tab1[row(tab1)==col(tab1)] / sum(tab1))
#0.9455782
#오류율= 1-정분류율  =0.0544218
1-0.9455782

#Accuracy가 LDA모형 94.56%로 상당히 높다는 것을 알 수 있다.


plot(lda.fit)

#다변량 정규분포 따를 때 공분산 행렬 다르다고 가정 QDA(quadratic discriminant analysis)

##QDA

#정규화 가정 후 Classification 진행
cancerM <- x_pca[x_pca$diag=="M",]
mpca <- cancerM[,1:5]
cancerB <- x_pca[x_pca$diag=="B",]
bpca <- cancerB[,1:5]

mu_M =apply(mpca,2,mean)
sigma_M = cov(mpca)
mu_B = apply(bpca,2,mean)
sigma_B = cov(bpca)
# X_Mal ~ N(mu_M, sigma_M)을 따른다고 가정.
# X_Ben ~ N(mu_B, sigma_B)를 따른다고 가정.

qda.fit <- qda(diag~PC1+PC2+PC3+PC4+PC5, data=train)
qda.fit

qda.pred <- predict(qda.fit, test)
qda.class <- qda.pred$class
head(qda.class)

tab2 <- table(qda.class, test$diag)
tab2
#정분류율
sum(tab2[row(tab2)==col(tab2)] / sum(tab2))
#0.9591837
#오류율= 1-정분류율  =0.0408163
1-0.9591837

#Accuracy가 QDA모형 약 96%로 상당히 높다는 것을 알 수 있다.
#QDA 방식이 LDA 방식보다 우수하다고 볼 수 있음







### Normal Test for original data
mal.p.value <- c()
for(i in totalRange){
    mal.p.value <- c(mal.p.value,
                     nortest::ad.test(CancerM[,i])$p.value)
}
# mal.p.value %>% round(3)
benign.p.value <- c()
for(i in totalRange){
    benign.p.value <- c(benign.p.value,
                        nortest::ad.test(CancerB[,i])$p.value)
}
# benign.p.value %>% round(3)
CancerNormT <- tibble(
    variableName = colnames(CancerData[,3:32]),
    mal.p.value, benign.p.value,
)
# CancerNormT
CancerNormT %>%
    pivot_longer(cols = 2:3, names_to = "group",
                 values_to = "p.value") %>%
    ggplot(aes(x = p.value, fill = group)) +
    geom_histogram() +
    geom_vline(xintercept = 0.05, linetype = "dashed") +
    facet_grid(~group)
# 대부분 normal을 따르지 않음.
CancerNormT %>%
    pivot_longer(cols = 2:3, names_to = "group",
                 values_to = "p.value") %>%
    filter(p.value > 0.05)
# Original dataset이 mean, se, worst의 자료들을 모았으므로,
# CLT에 의해 최소 mean과 se는 정규를 따를 것이라 기대했었음.
# 근데 오히려 worst value가 normal을 따르는 경향을 보임.
# -> Original Data의 radius, texture 같은 암세포 variables의 분포가
# -> 357, 212개의 n으로는 정규분포로 근사하기 어림도 없는 분포임을 추론할 수 있음

### Normalize dataset with BoxCox Method
# BoxCox-정규화
CancerM_BoxCox <- apply(CancerM[,totalRange], 2, function(x){
    forecast::BoxCox(x, "auto")
})
CancerB_BoxCox <- apply(CancerB[,totalRange], 2, function(x){
    forecast::BoxCox(x, "auto")
})
# 정규화 이후 normality test
CancerM_BoxCox_pval <- apply(CancerM_BoxCox, 2, function(x){
    shapiro.test(x)$p.value
})

# BoxCox변환을 했는데도 normal이 아니라고 생각되는 경우를 확인
head(CancerM_BoxCox_pval[CancerM_BoxCox_pval<0.05]) %>% round(3)
paste0("Normal이 아닌 변수의 갯수 : ",
       length(CancerM_BoxCox_pval[CancerM_BoxCox_pval<0.05]) )
notNormIndx <- which(CancerM_BoxCox_pval<0.05)
# transform 하고도 정규를 따르지 않는 자료의 원래 분포
CancerData[,notNormIndx + 2] %>% ggpairs()
# outlier때문에 정규화가 잘 진행되지 않은건가? 싶음
# 따라서, outlier를 제거 한 후에 정규화를 진행하고자 함
# 또한 B-group에 대해서는 BoxCox 변환이 잘 적용되지 않음 : 음수 자료값 때문에.
# 따라서, 실수 자료에 대해 power-transformation이 가능한 Yeo-johnson transform을 도입.

### After outlier detection and preprocessing...
# Load data, Declare notation and Preprocessing data
# CancerDataRm <- read.csv("C:/Users/p5682/Documents/Multivariate_Statistics_and_Lab/Cancer_data_removed_outliers.csv", stringsAsFactors = T)
colnames(CancerDataRm)[c(1,32)] <- c("id", "diagnosis")
CancerDataRm <- CancerDataRm[,c(1,32,2:31)]
CancerM_rm <- CancerDataRm[CancerDataRm$diagnosis == "M",]
CancerB_rm <- CancerDataRm[CancerDataRm$diagnosis == "B",]

# 정규화
CancerM_rm_norm <- apply(CancerM_rm[,totalRange], 2, function(x){
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

# 정규화 이후 normality test
CancerM_rm_normTest <- apply(CancerM_rm_norm, 2, function(x){
    shapiro.test(x)$p.value
})
CancerM_rm_normTest %>% hist()
CancerM_rm_normTest %>% sort() %>% head() %>% round(3)
paste0("Normal이 아닌 변수의 갯수 : ",
       sum(CancerM_rm_normTest<0.05) )

CancerB_rm_normTest <- apply(CancerB_rm_norm, 2, function(x){
    shapiro.test(x)$p.value
})
CancerB_rm_normTest %>% hist()
CancerB_rm_normTest %>% sort() %>% head() %>% round(3)
paste0("Normal이 아닌 변수의 갯수 : ",
       sum(CancerB_rm_normTest<0.05) )
# Normalize가 M-group에 대해서는 잘 작동했고, B-group에 대해서는 잘 잘동하지 않았음.
# 아무래도 yeo-johnson method일지라도 잘 변환이 적용되지 않은 듯 보임.

# Data merge
CancerDataRm_norm <- CancerDataRm
CancerDataRm_norm[,totalRange] <- rbind(CancerM_rm_norm, CancerB_rm_norm) %>%
    as.data.frame(CancerDataRm_norm)
# write.csv(CancerDataRm_norm, file = "C:/Users/p5682/Documents/Multivariate_Statistics_and_Lab/CancerDataRm_normalized.csv", row.names = F)

### Classify
# Cov.mat 동질성 검사
biotools::boxM(CancerDataRm_norm[,totalRange], CancerDataRm[,2])
# sigma1 != sigma2 라고 할 수 있을 것 같다.
# 기본값으로 CancerDataRm_norm이라는 이름을 가진 data.frame을 필요로함.
# log(f1/f2)를 return하는 함수.
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
# log(f1/f2)와 비교하기 위한 log(p2/p1)
# p1=p2로 가정할수도 있고, p1:p2를 최초 dataset의 개수로 정할수도 있음
# 여기서는 최초 dataset의 비율로 p1과 p2를 설정하겠음..
p1 = nrow(CancerM_rm)/nrow(CancerDataRm)
p2 = 1-p1
log(p2/p1) # = 0.4467


# CV
# Fold 생성
k <- 10  # Fold 수
folds <- cut(1:nrow(CancerDataRm_norm), breaks = k, labels = FALSE)  # 데이터 인덱스를 k개의 Fold로 분할

# i-th fold에 대해
kfold_list = rep(0,10)
for(i in 1:10){
    temp_fold <- apply(CancerDataRm_norm[folds==i,totalRange], 1, function(x){
        input <- unlist(x)
        ifelse(ECM_rule_for_normalized(x, CancerDataRm_norm[folds!=i,])>=log(p2/p1), "M", "B")
    })
    temp_fold <- factor(temp_fold, levels = c("B", "M"))
    kfold_list[i] = mean(temp_fold == CancerDataRm_norm$diagnosis[folds==i])
}
kfold_list
# 과적합 된 것 같음. 이게 맞나? 전부 정답이 나오는게?
# 식을 하나하나 뜯어 본 결과, transformation의 여파로 보임.
# 정규화 이전의 data를 ECM_rule에 대입한 결과, result가 이해 가능한 value로 return됨.
# 하지만 정규화 이후의 data를 ECM_rule에 대입한 결과, result가 수십만으로 널뛰기함.
# Power-based transformation이다보니 variance의 scale이 transformation에 영향을 받는데
# 이때 solve(), 다시 말해 sigma의 inverse value가 널뛰기하기 때문으로 보임.
# 무작정 정규화 하는건 좋지 않을 것 같기도 함.

## 이하는 ECM_rule 체크 과정 코드
# Case1 : Original Data에 대해 ECM 적용
kfold_list1 = rep(0,10)
for(i in 1:10){
    temp_fold <- apply(CancerData[folds==i,totalRange], 1, function(x){
        input <- unlist(x)
        ifelse(ECM_rule_for_normalized(x, CancerData[folds!=i,])>=log(p2/p1), "M", "B")
    })
    temp_fold <- factor(temp_fold, levels = c("B", "M"))
    kfold_list1[i] = mean(temp_fold == CancerData$diagnosis[folds==i])
}
kfold_list1
mean(kfold_list1)
# 96%정도로 괜찮은 정답률

# Case2 : Outlier Detection 이후 Data에 대해 ECM 적용
kfold_list2 = rep(0,10)
for(i in 1:10){
    temp_fold <- apply(CancerDataRm[folds==i,totalRange], 1, function(x){
        input <- unlist(x)
        ifelse(ECM_rule_for_normalized(x, CancerDataRm[folds!=i,])>=log(p2/p1), "M", "B")
    })
    temp_fold <- factor(temp_fold, levels = c("B", "M"))
    kfold_list2[i] = mean(temp_fold == CancerDataRm$diagnosis[folds==i])
}
kfold_list2
mean(kfold_list2)
# 94%정도로 감소했지만 괜찮은 정답률

# Case3 : Outlier Detection + Normalize 이후 Data에 대해 ECM
kfold_list3 = rep(0,10)
for(i in 1:10){
    temp_fold <- apply(CancerDataRm_norm[folds==i,totalRange], 1, function(x){
        input <- unlist(x)
        ifelse(ECM_rule_for_normalized(x, CancerDataRm_norm[folds!=i,])>=log(p2/p1), "M", "B")
    })
    temp_fold <- factor(temp_fold, levels = c("B", "M"))
    kfold_list3[i] = mean(temp_fold == CancerDataRm_norm$diagnosis[folds==i])
}
kfold_list3
mean(kfold_list3)
# 100%정도로 이상한 정답률

# Case 4 : Just Normalize 이후 Data에 대해 ECM
CancerM_norm <- apply(CancerM[,totalRange], 2, function(x){
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
CancerData_norm <- CancerData
CancerData_norm[,totalRange] <- rbind(CancerM_norm, CancerB_norm) %>%
    as.data.frame(CancerData_norm)

kfold_list4 = rep(0,10)
for(i in 1:10){
    temp_fold <- apply(CancerData_norm[folds==i,totalRange], 1, function(x){
        input <- unlist(x)
        ifelse(ECM_rule_for_normalized(x, CancerData_norm[folds!=i,])>=log(p2/p1), "M", "B")
    })
    temp_fold <- factor(temp_fold, levels = c("B", "M"))
    kfold_list4[i] = mean(temp_fold == CancerData_norm$diagnosis[folds==i])
}
kfold_list4
mean(kfold_list4)
# 58% 정도로 처절한 정답률. outlier detection은 의미가 있었던 듯.
# 위에서 이야기했던 대로, BoxCox가 power transformation이다 보니까 벌어진 일 같음.
# 다른 ML 분석 방법에서 정확도 100%가 나온것을 보아, power tranformation의 과적합 문제가 아니라
# 그냥 데이터셋 group의 feature가 너무 명확해서 나온 정확도일 수도 있을 것이라고 생각됨.

# 전체 비교 : 가독성 좋게
data.frame("ECM_rule_based_on" = c("원본 자료", "이상치 제거한 자료", "이상치 제거 후 정규화한 자료", "이상치 제거 없이 정규화한 자료"),
           "평균_정답률_kfold" = round(c(mean(kfold_list1),
                                    mean(kfold_list2),
                                    mean(kfold_list3),
                                    mean(kfold_list4)), 3)) %>% DT::datatable()







### Cross Validation
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
folds <- cut(1:nrow(CancerDataRm_norm), breaks = k, labels = FALSE)  # 데이터 인덱스를 k개의 Fold로 분할
fold_1 <- apply(CancerDataRm_norm[folds==1,totalRange], 1, function(x){
    input <- unlist(x)
    ifelse(ECM_rule_for_normalized(x, CancerDataRm_norm[folds!=1,])>=log(p2/p1), "M", "B")
})
fold_1 <- factor(fold_1, levels = c("B", "M"))
mean(fold_1 == CancerDataRm_norm$diagnosis[folds==1]) # 정답률


## 교차 검증의 결과
# 1. 평균 정확도가 높고 각 Fold의 정확도가 일관되게 높은 경우
# 모델이 잘 일반화되어 다른 데이터에 대해서도 좋은 예측을 수행할 것으로 기대할 수 있음.
# 2. 평균 정확도가 낮거나 각 Fold의 정확도가 크게 다른 경우
# 모델이 과적합된 것일 수 있음. 최적의 교차검증이 아니라고 
