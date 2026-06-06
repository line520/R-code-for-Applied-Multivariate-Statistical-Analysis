library(psych)
library(readxl)

data <- read_excel("./data/exec8.5.xlsx", sheet = "Sheet")
data <- as.data.frame(data)
rownames(data) <- data$编号
data <- data[,-1]
names(data) <- c("人口总数", "居民的教育程度", "佣人总数", "各种服务行业的人数", "房价中位数")

#======极大似然法=======
#先进行未旋转的因子分析
fa_ml <- fa(data, nfactors = 2, residuals = TRUE, rotate = "none", fm = "ml")
print(fa_ml$loadings, digits = 3, cutoff = 0)
print(round(fa_ml$communality, 3))        #输出共性方差

residual_mat <- fa_ml$residual - diag(diag(fa_ml$residual))
print(round(residual_mat, 3))

#再进行方差最大的因子旋转
fa_varimax <- fa(data, nfactors = 2, rotate = "varimax", fm = "ml", scores = "regression")
print(fa_varimax$loadings, digits = 3, cutoff = 0)
print(round(fa_varimax$communality, 3))   #输出共性方差

#因子得分
scores <- round(fa_varimax$scores, 3)
print(scores)
#对因子得分进行排序
print(scores[order(scores[, 1]), ])
print(scores[order(scores[, 2]), ])