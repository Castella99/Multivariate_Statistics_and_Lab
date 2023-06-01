# Classification

# 동등한 cost 하에서 진행

# Covariance Matrix가 두 그룹간에 일치하는가?
# 동질성 검정 boxM method
CancerDataRm_norm %>% tibble()
CancerDataRm_norm[,totalRange] %>% cov()

biotools::boxM(CancerDataRm_norm[,totalRange], CancerDataRm[,2])
# 일치하지 않음.

# 통계량 추정량 구하기


# Mal group
mu_M <- CancerDataRm_norm[CancerDataRm_norm$diagnosis=="M", totalRange] %>%
    apply(., 2, mean)
sigma_M <- CancerDataRm_norm[CancerDataRm_norm$diagnosis=="M", totalRange] %>%
    cov()
# X_Mal ~ N(mu_M, sigma_M)을 따른다고 가정.

# Ben group
mu_B <- CancerDataRm_norm[CancerDataRm_norm$diagnosis=="B", totalRange] %>%
    apply(., 2, mean)
sigma_B <- CancerDataRm_norm[CancerDataRm_norm$diagnosis=="B", totalRange] %>%
    cov()
# X_Ben ~ N(mu_B, sigma_B)를 따른다고 가정.
# probability는 두가지 경우를 생각해볼 수 있음.
# 1. 같은 경우
# 2. 최초 dataset의 비율과 같음
# 2번의 경우를 가정하겠음.
1:3 %*% matrix(c(1,1,1,2,2,2,3,3,3), 3)

ECM_rule <- function(x) {
    temp1 = log(base::det(sigma_M)/base::det(sigma_B))
    temp2 = (solve(sigma_M) - solve(sigma_B))
    temp3 = (mu_M %*% solve(sigma_M) - mu_B %*% solve(sigma_B))
    temp4 = (mu_M %*% solve(sigma_M) %*% mu_M  - mu_B %*% solve(sigma_B) %*% mu_B)
    result = -0.5*x%*%temp2%*%x + temp3 %*% x -0.5*temp4 -0.5*temp1
    return(result)
}
x <- rep(1,30)
ECM_rule(testx)

x <- unlist(CancerDataRm[200,totalRange])

ECM_rule(x)

typeof(x)
typeof(unlist(x))
 

