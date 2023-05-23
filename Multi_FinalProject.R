# Cancer Data
# https://www.kaggle.com/datasets/erdemtaha/cancer-data

# Data Load
CancerData <- read.csv("./R_wd_2023/kaggle/Cancer_Data.csv", stringsAsFactors = T)

# Glimpse
glimpse(CancerData)

# Preprocessing
CancerData <- CancerData[,-33]
which(is.na(CancerData), arr.ind = T)
glimpse(CancerData)

# EDA

## Declare for visibility
attach(CancerData)
CancerM <- CancerData[diagnosis=="M",]
CancerB <- CancerData[diagnosis=="B",]
meanRange <- 3:12
seRange <- 13:22
worstRange <- 23:32
totalRange <- 3:32
variNames <- colnames(CancerData)[meanRange] %>% substr(., 0, nchar(.) -5)

# 그래프
# ggpairs(CancerM[,meanRange])
# ggpairs(CancerB[,meanRange])
ggpairs(CancerData[, meanRange], aes(col = diagnosis))
ggpairs(CancerData[, seRange], aes(col = diagnosis))
ggpairs(CancerData[, worstRange], aes(col = diagnosis))
# se의 경우 유난히 좌측으로 치우침.
# 허나, 좌측으로 치우친 이유가 outlier(내지 영향점)때문으로 보임.


# 통계량 확인
psych::describe(CancerM[,totalRange])
# 근데 생각해보니까, 자료 자체가 mean, se, worst여서
# 데이터 자체가 이미 표본통계량임.
# oberservation의 value는, id 842302를 예를 들자면
# 1. 이 사람의 몸에 있는 암세포들에 대해서
# 2. 각각의 radius, texture, perimeter...를 측정함
# 3. radius, texture, perimeter,...의 분포가 나옴
# 3-1. radius만 예를 들자면, 큰거 작은거 다 모아서 distribution이 생길것임
# 3-2. 이 distribution은 정규분포를 따를수도 있고, 다른 임의의 분포를 따를 수도 있음.
# 4. 아무튼 이 경험적 분포에서 mean, se, worst를 얻을 수 있음.
# 5. 그런 값들을 모두 모은다면, 개인당 암세포 특성*3 만큼 자료가 생김
# 그런 값들의 집합dataset이 CancerData.

# 암세포 mal과 ben의 구분을 개인의 distribution을 통해 알 수도 있지 않을까?
# 예를 들어, mal은 우측으로 치우친 분포이고 ben은 좌우 대칭 분포이지 않을까?
# 근데 그런 정보는 없음. 우리가 가진 정보는 오직 mean, se, worst임

# normality test를 통해 원래 분포가 정규분포와 비슷한지 아닌지 추측할 수 있음
# 만약 normal을 따르면, 원래 분포가 정규분포와 비슷한거임
# CLT때문에 모든 sum type 표본통계량은 정규분포로 근사하므로,
# 적어도 mean과 se는 CLT에 의해 n이 커질수록 정규분포로 근사하게됨.

# 정규 분포 확인
###### malignant group p.value
mal.p.value <- c()
for(i in totalRange){
    mal.p.value <- c(mal.p.value,
                     nortest::ad.test(CancerM[,i])$p.value)
}
mal.p.value %>% round(3)

##### benign group p.value
benign.p.value <- c()
for(i in totalRange){
    benign.p.value <- c(benign.p.value,
                     nortest::ad.test(CancerM[,i])$p.value)
}
benign.p.value %>% round(3)


CancerNormT <- tibble(
    variableName = colnames(CancerData[,3:32]),
    mal.p.value, benign.p.value,
)
CancerNormT

###### Visualize Normality Test Result
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
# 애초에 worst는 norm을 기대도 안했음
# 근데 오히려 normal을 따르는건 worst value임.
# 357, 212개의 n으로는 정규분포로 근사하기 어림도 없는 분포임을 추론할 수 있음


#### Multivariate

##### Correlation
corM <- cor(CancerM[,totalRange])
corB <- cor(CancerB[,totalRange])
corM[upper.tri(corM)] %>% abs()
