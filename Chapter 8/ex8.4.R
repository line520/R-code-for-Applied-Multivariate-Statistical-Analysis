library(psych)
library(readxl)

data <- read_excel("./data/exec8.4.xlsx")

# 将数据框转换为相关矩阵
R <- as.matrix(data[, -1])                # 去掉第一列（项目名）
rownames(R) <- colnames(R) <- data[[1]]   # 设置行名和列名

#======极大似然法=======
#先进行未旋转的因子分析
fam1 <- fa(R, nfactors = 4, residuals = TRUE, rotate = "none", fm = "ml")
print(fam1$loadings, digits = 3, cutoff = 0)

#输出共性方差
print(round(fam1$communality, 3))

#计算并输出残差矩阵
residual <- fam1$residual - diag(diag(fam1$residual))
print(round(residual, 3))

#再进行方差最大的因子旋转
fam1.varimax <- fa(R, nfactors = 4, rotate = "varimax", scores = TRUE, fm = "ml")
print(fam1.varimax$loadings, digits = 3, cutoff = 0)