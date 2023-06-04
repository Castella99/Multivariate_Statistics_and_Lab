# Word 첨부용 코드

# 사용한 Packages
library(tidyverse) # dplyr..etc
library(forecast) # BoxCox transformation
library(GGally) # Better visualize pairs plot
library(car) # BoxCox transformation
library(biotools) # cov.mat의 동질성 검정

# Load data and Declare notation
CancerData <- read.csv("C:/Users/p5682/Documents/Multivariate_Statistics_and_Lab/Cancer_data.csv", stringsAsFactors = T)
CancerData <- CancerData[,-33]
CancerM <- CancerData[CancerData$diagnosis=="M",]
CancerB <- CancerData[CancerData$diagnosis=="B",]
meanRange <- 3:12
seRange <- 13:22
worstRange <- 23:32
totalRange <- 3:32
variNames <- colnames(CancerData)[meanRange] %>% substr(., 0, nchar(.) -5)

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
CancerDataRm <- read.csv("C:/Users/p5682/Documents/Multivariate_Statistics_and_Lab/Cancer_data_removed_outliers.csv", stringsAsFactors = T)
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
for(i in 1:10){
    temp_fold <- apply(CancerData[folds==i,totalRange], 1, function(x){
        input <- unlist(x)
        ifelse(ECM_rule_for_normalized(x, CancerData[folds!=i,])>=log(p2/p1), "M", "B")
    })
    temp_fold <- factor(temp_fold, levels = c("B", "M"))
    kfold_list[i] = mean(temp_fold == CancerData$diagnosis[folds==i])
}
kfold_list
mean(kfold_list)
# 96%정도로 괜찮은 정답률

# Case2 : Outlier Detection 이후 Data에 대해 ECM 적용
for(i in 1:10){
    temp_fold <- apply(CancerDataRm[folds==i,totalRange], 1, function(x){
        input <- unlist(x)
        ifelse(ECM_rule_for_normalized(x, CancerDataRm[folds!=i,])>=log(p2/p1), "M", "B")
    })
    temp_fold <- factor(temp_fold, levels = c("B", "M"))
    kfold_list[i] = mean(temp_fold == CancerDataRm$diagnosis[folds==i])
}
kfold_list
mean(kfold_list)
# 94%정도로 감소했지만 괜찮은 정답률

# Case3 : Outlier Detection + Normalize 이후 Data에 대해 ECM
for(i in 1:10){
    temp_fold <- apply(CancerDataRm_norm[folds==i,totalRange], 1, function(x){
        input <- unlist(x)
        ifelse(ECM_rule_for_normalized(x, CancerDataRm_norm[folds!=i,])>=log(p2/p1), "M", "B")
    })
    temp_fold <- factor(temp_fold, levels = c("B", "M"))
    kfold_list[i] = mean(temp_fold == CancerDataRm_norm$diagnosis[folds==i])
}
kfold_list
mean(kfold_list)
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

for(i in 1:10){
    temp_fold <- apply(CancerData_norm[folds==i,totalRange], 1, function(x){
        input <- unlist(x)
        ifelse(ECM_rule_for_normalized(x, CancerData_norm[folds!=i,])>=log(p2/p1), "M", "B")
    })
    temp_fold <- factor(temp_fold, levels = c("B", "M"))
    kfold_list[i] = mean(temp_fold == CancerData_norm$diagnosis[folds==i])
}
kfold_list
mean(kfold_list)
# 58% 정도로 처절한 정답률. outlier detection은 의미가 있었던 듯.
# 위에서 이야기했던 대로, BoxCox가 power transformation이다 보니까 벌어진 일 같음.