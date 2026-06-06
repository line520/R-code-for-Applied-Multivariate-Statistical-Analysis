library(psych)
library(readxl)

data <- read_excel("./data/exec8.5.xlsx", sheet = "Sheet")
data <- as.data.frame(data)
rownames(data) <- data$编号
data <- data[,-1]
names(data) <- c("人口总数", "居民的教育程度", "佣人总数", "各种服务行业的人数", "房价中位数")


#======主成分法=======
#先进行未旋转的因子分析
pc <- principal(data, nfactors = 2, residuals = TRUE, rotate = "none")
print(pc$loadings, digits = 3, cutoff = 0)
print(round(pc$communality, 3))          #输出共性方差

residual_mat <- pc$residual - diag(diag(pc$residual))
print(round(residual_mat, 3))

#再进行方差最大的因子旋转
pc_varimax <- principal(data, nfactors = 2, rotate = "varimax", scores = TRUE)
print(pc_varimax$loadings, digits = 3, cutoff = 0)
print(round(pc_varimax$communality, 3))   #输出共性方差

#因子得分
scores <- round(pc_varimax$scores, 3)
print(scores)
#对因子得分进行排序
print(scores[order(scores[, 1]), ])

