library(readxl)
library(MASS)

data <- read_excel("data/exec5.5.xlsx")
colnames(data) <- c("x1", "x2", "g")          # 重命名列名
data$g <- factor(data$g, levels = c(1, 2), labels = c("雨天", "非雨天")) # 分类变量变为因子
x_new <- data.frame(x1 = 0.6, x2 = 3.0)       # 新的观测值

# 距离判别计算均值
group1 <- as.matrix(data[data$g == "雨天", 1:2])
group2 <- as.matrix(data[data$g == "非雨天", 1:2])
n1 <- nrow(group1)
n2 <- nrow(group2)
mean1 <- colMeans(group1)                     # 雨天均值
mean2 <- colMeans(group2)                     # 非雨天均值
# 合并协方差矩阵 Sp
S1 <- cov(group1)
S2 <- cov(group2)
Sp <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)
cat("\n合并协方差矩阵 Sp:\n")
print(Sp)

# 进行两组协方差阵相等的贝叶斯判别
lda_eq <- lda(g ~ x1 + x2, data = data, prior = c(0.3, 0.7))
pred_eq <- predict(lda_eq, newdata = x_new)
cat("(1) 等先验判别结果:\n")
print(pred_eq$class)
cat("后验概率:\n")
print(pred_eq$posterior)

