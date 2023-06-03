# Classification

# CancerDataRm_norm <- read.csv("CancerDataRm_normalized")
# 동등한 cost 하에서 진행
# probability는 두가지 경우를 생각해볼 수 있음.
# 1. 같은 경우
# 2. 최초 dataset의 비율과 같음
# 2번의 경우를 가정하겠음.

p1 = nrow(CancerM_rm)/nrow(CancerDataRm)
p2 = 1-p1
log(p2/p1) # = 0.4467

# Covariance Matrix가 두 그룹간에 일치하는가?
# 동질성 검정 boxM method
CancerDataRm_norm %>% tibble()
CancerDataRm_norm[,totalRange] %>% cov()

biotools::boxM(CancerDataRm_norm[,totalRange], CancerDataRm[,2])
# 일치하지 않음.

# 통계량 추정량 구하기

# X_Mal ~ N(mu_M, sigma_M)을 따른다고 가정.
# X_Ben ~ N(mu_B, sigma_B)를 따른다고 가정.

# CancerDataRm_normalized.csv을
# CancerDataRm_norm 이라는 이름으로 저장한 상태에서 사용해야함.
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


# Case1 : p1 p2를 최초 dataset을 기준으로 잡았을 때때
Rule1 <- apply(CancerDataRm_norm[,totalRange], 1, function(x){
    input <- unlist(x)
    ifelse(ECM_rule_for_normalized(x)>=log(p2/p1), "M", "B")
})
Rule1 <- as.factor(Rule1)
mean(Rule1 == CancerDataRm$diagnosis)

# Case2 : p1 p2가 같다고 가정했을 때때
Rule2 <- apply(CancerDataRm_norm[,totalRange], 1, function(x){
    input <- unlist(x)
    ifelse(ECM_rule_for_normalized(x)>=0, "M", "B")
})
Rule2 <- as.factor(Rule2)
mean(Rule2 == CancerDataRm$diagnosis)
# p1 p2가 큰 영향 X(값이 너무 튀어서)


#######
### Trash Code
#######
### Normalized Data 기준..
# Test Case : 
# Case1 : Original Data
mu_M <- CancerM[,totalRange] %>% apply(., 2, mean)
sigma_M <- CancerM[,totalRange] %>% cov()
mu_B <- CancerB[,totalRange] %>% apply(., 2, mean)
sigma_B <- CancerB[,totalRange] %>% cov()

# Case2 : No-Outlier Data
mu_M <- CancerM_rm[,totalRange] %>% apply(., 2, mean)
sigma_M <- CancerM_rm[,totalRange] %>% cov()
mu_B <- CancerB_rm[,totalRange] %>% apply(., 2, mean)
sigma_B <- CancerB_rm[,totalRange] %>% cov()

# Case3 : No-Outlier and Normalized Data
mu_M <- CancerM_rm_norm %>% apply(., 2, mean)
sigma_M <- CancerM_rm_norm %>% cov()
mu_B <- CancerB_rm_norm %>% apply(., 2, mean)
sigma_B <- CancerB_rm_norm %>% cov()

