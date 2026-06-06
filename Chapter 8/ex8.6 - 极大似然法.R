library(psych)
library(readxl)

data <- read_excel("./data/exec8.6.xlsx", sheet = "Sheet")
data <- as.data.frame(data)
rownames(data) <- data$应征者
data <- data[,-1]
names(data) <- c("申请书的形式", "外貌", "专业能力", "讨人喜欢", "自信心",
                 "精明", "诚实", "推销能力", "经验", "积极性", "抱负", "理解能力",
                 "潜力", "交际能力", "适应性")

#======极大似然法=======
#先进行未旋转的因子分析
fa_ml <- fa(data, nfactors = 5, residuals = TRUE, rotate = "none", fm = "ml")
print(fa_ml$loadings, digits = 3, cutoff = 0)
print(round(fa_ml$communality, 3))        #输出共性方差

residual_mat <- fa_ml$residual - diag(diag(fa_ml$residual))
print(round(residual_mat, 3))

#再进行方差最大的因子旋转
fa_varimax <- fa(data, nfactors = 5, rotate = "varimax", fm = "ml", scores = "regression")
print(fa_varimax$loadings, digits = 3, cutoff = 0)
print(round(fa_varimax$communality, 3))   #输出共性方差

#因子得分
scores <- round(fa_varimax$scores, 3)
print(scores)
#对因子得分进行排序
print(scores[order(scores[, 1]), ])
print(scores[order(scores[, 2]), ])
print(scores[order(scores[, 3]), ])
print(scores[order(scores[, 4]), ])
print(scores[order(scores[, 5]), ])