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

CancerDataRm_norm[300,totalRange] %>% unlist() %>% ECM_rule_for_normalized(.,)

Rule1 <- apply(CancerDataRm_norm[,totalRange], 1, function(x){
    input <- unlist(x)
    ifelse(ECM_rule_for_normalized(x)>=log(p2/p1), "M", "B")
})
Rule1 <- as.factor(Rule1)
mean(Rule1 == CancerDataRm$diagnosis)


# CV
# Fold 생성
k <- 10  # Fold 수
folds <- cut(1:nrow(CancerDataRm_norm), breaks = k, labels = FALSE)  # 데이터 인덱스를 k개의 Fold로 분할

# 각 Fold 확인
for (i in 1:k) {
    fold_indices <- which(folds == i)  # 현재 Fold에 해당하는 데이터 인덱스
    print(paste0("Fold", i, " : ", length(fold_indices), "samples"))
}

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



### Test code
# 1st fold에 대해..
fold_1 <- apply(CancerDataRm_norm[folds==1,totalRange], 1, function(x){
    input <- unlist(x)
    ifelse(ECM_rule_for_normalized(x, CancerDataRm_norm[folds!=1,])>=log(p2/p1), "M", "B")
})
fold_1 <- factor(fold_1, levels = c("B", "M"))
mean(fold_1 == CancerDataRm_norm$diagnosis[folds==1]) # 정답률

