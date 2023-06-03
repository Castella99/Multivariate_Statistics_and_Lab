rm(list=ls())

getwd()
a = read.csv("Cancer_data_removed_outliers.csv")

library(dplyr)
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
#PC5까지 사용 (주성분으로 판단) #원 데이터의 85% 정도를 보존한다고 이해할 수 있다. 

##차원 축소된 데이터 생성
x_pca <- as.data.frame(pca_cancer$x[,1:5])
x_pca
#diagonosis를 데이터프레임에 추가
x_pca$diag <- as.factor(cancer$diagonosis)
x_pca
View(x_pca)




glimpse(x_pca)

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


#p1 <- nrow(cancerM)/nrow(x_pca)
#p2 <- 1-p1  
#p1/p2



library(MASS)


#Then split the dataset into training and test data.
set.seed(11)
tr_index = sample(1:nrow(x_pca),0.7*nrow(x_pca), replace=FALSE)
train=x_pca[tr_index,]
test=x_pca[-tr_index,]




#다변량 정규분포 따를 때, 공분산 행렬까지 같다고 가정 LDA(Linear discriminant analysis)
##LDA
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
















