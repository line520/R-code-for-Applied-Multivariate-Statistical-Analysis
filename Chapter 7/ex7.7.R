library(readxl)

exec7.7 <- as.data.frame(read_excel("./data/exec7.7.xlsx"))
rownames(exec7.7) <- exec7.7[[1]]
exec7.7 <- exec7.7[, -1]

# 计算相关系数矩阵（保留五位小数）
round(cor(exec7.7), 5)

# 主成分分析（基于相关矩阵，即对标准化后的变量进行 PCA）
PCA <- princomp(exec7.7, cor = TRUE)

# 展示方差解释比例和载荷（保留载荷矩阵）
summary(PCA)
print(PCA$loadings, cutoff = 0)   # cutoff=0 表示显示所有非零值

# 绘制碎石图（折线型）
screeplot(PCA, type = "lines", main = "Scree Plot")