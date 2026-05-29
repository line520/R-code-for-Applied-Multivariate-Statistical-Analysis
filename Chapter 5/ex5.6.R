library(readxl)
library(MASS)

data <- read_excel("data/exec5.6.xlsx")
data_new <- read_excel("data/exec5.6a.xlsx")                  # 需判断的运动员数据
colnames(data) <- c("x1", "x2", "x3", "x4", "x5", "x6", "g")  # 重命名列名
colnames(data_new) <- c("x1", "x2", "x3", "x4", "x5", "x6")   # 重命名列名
data$g <- factor(data$g, levels = c(1, 2), labels = c("一级", "健将")) # 分类变量变为因子

# --------------------------------------------------------
# 协方差阵相等的贝叶斯判别
lda_eq <- lda(g ~ x1+x2+x3+x4+x5+x6, data=data, prior=c(0.5, 0.5))
pred_eq <- predict(lda_eq, newdata=data_new)
cat("协方差阵相等判别结果:\n")
print(pred_eq$class)
cat("后验概率:\n")
print(pred_eq$posterior)

# 协方差阵不等的贝叶斯判别
qda_neq <- qda(g ~ x1+x2+x3+x4+x5+x6, data=data, prior = c(0.5, 0.5))
pred_qda <- predict(qda_neq, newdata = data_new)
cat("\n协方差阵不等判别结果:\n")
print(data.frame(id = 1:14, 预测类别 = pred_qda$class))

# --------------------------------------------------------
# 回代法估计
lda_resub <- predict(lda_eq, newdata = data)
table_lda_resub <- table(实际 = data$g, 预测 = lda_resub$class)
cat("\n协方差阵相等回代法混淆矩阵:\n")
print(table_lda_resub)
cat("回代法误判率:\n")
print(round(prop.table(table_lda_resub, 1), 3))

qda_resub <- predict(qda_neq, newdata = data)
table_qda_resub <- table(实际 = data$g, 预测 = qda_resub$class)
cat("\n协方差阵不等回代法混淆矩阵:\n")
print(table_qda_resub)
cat("回代法误判率:\n")
print(round(prop.table(table_qda_resub, 1), 3))
# --------------------------------------------------------
# 交叉验证法估计
lda_cv <- lda(g ~ x1+x2+x3+x4+x5+x6, data=data, prior = c(0.5, 0.5), CV = TRUE)
table_lda_cv <- table(实际 = data$g, 预测 = lda_cv$class)
cat("\n协方差阵相等交叉验证混淆矩阵:\n")
print(table_lda_cv)
cat("交叉验证误判率:\n")
print(round(prop.table(table_lda_cv, 1), 3))

qda_cv <- qda(g ~ x1+x2+x3+x4+x5+x6, data=data, prior = c(0.5, 0.5), CV = TRUE)
table_qda_cv <- table(实际 = data$g, 预测 = qda_cv$class)
cat("\n协方差阵不等交叉验证混淆矩阵:\n")
print(table_qda_cv)
cat("交叉验证误判率:\n")
print(round(prop.table(table_qda_cv, 1), 3))

# --------------------------------------------------------
# 知道先验概率的贝叶斯判别
lda_p <- lda(g ~ x1+x2+x3+x4+x5+x6, data=data, prior=c(0.8, 0.2))
pred_eq <- predict(lda_p, newdata=data_new)
cat("判别结果:\n")
print(pred_eq$class)
cat("后验概率:\n")
print(pred_eq$posterior)