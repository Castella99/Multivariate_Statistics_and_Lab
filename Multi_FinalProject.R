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
# Declare for visibility
attach(CancerData)
CancerM <- CancerData[diagnosis=="M",]
CancerB <- CancerData[diagnosis=="B",]
meanRange <- 3:12
seRange <- 13:22
worstRange <- 23:32
variNames <- colnames(CancerData)[meanRange] %>% substr(., 0, nchar(.) -5)

# Plot
ggpairs(CancerM[,meanRange])
ggpairs(CancerB[,meanRange])
ggpairs(CancerData[, meanRange], aes(col = diagnosis))
# 

# Statistics

# Correlation
corM <- cor(CancerM[,meanRange])
corM[upper.tri(corM)] %>% abs() %>% hist()

# Normality Test
mal.p.value <- c()
mal.p.value[i] <- 
    for(i in 3:32){
        nortest::ad.test(CancerM[,i])$p.value
    }
CancerNormT <- tibble(
    
)
