library(readxl)
data <- read_excel("data/exec4.8.xlsx")

# 根据"产品批"列将数据分为甲组(1)和乙组(2)
group1 <- as.matrix(data[data$产品批 == 1, 1:5])  # 甲组，取前5列指标
group2 <- as.matrix(data[data$产品批 == 2, 1:5])  # 乙组，取前5列指标

# 样本量
n1 <- nrow(group1)
n2 <- nrow(group2)

# 计算各组均值向量
mean1 <- colMeans(group1)
mean2 <- colMeans(group2)

# 均值差向量
diff_mean <- mean1 - mean2
cat("均值差向量 (甲 - 乙):\n")
print(diff_mean)

# 合并协方差矩阵 Sp
S1 <- cov(group1)
S2 <- cov(group2)
Sp <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)
cat("\n合并协方差矩阵 Sp:\n")
print(Sp)

# 对比矩阵 C
C <- matrix(c(1, -1,  0,  0,  0,
              0,  1, -1,  0,  0,
              0,  0,  1, -1,  0,
              0,  0,  0,  1, -1),
            nrow = 4, byrow = TRUE)
cat("\n对比矩阵 C:\n")
print(C)

# 计算 C * (均值差)
C_diff <- C %*% diff_mean
cat("\nC * (均值差):\n")
print(C_diff)

# 计算 C * Sp * C'
CSpCt <- C %*% Sp %*% t(C)
cat("\nC * Sp * C':\n")
print(CSpCt)

# 计算 (C Sp C')^{-1}
inv_CSpCt <- solve(CSpCt)
cat("\n(C Sp C')^{-1}:\n")
print(inv_CSpCt)

# 计算 Hotelling T^2 统计量
T2 <- (n1 * n2 / (n1 + n2)) * t(C_diff) %*% inv_CSpCt %*% C_diff
T2 <- as.numeric(T2)
cat("\nT^2 统计量:", T2, "\n")

# 转换为 F 统计量
k <- nrow(C)  # 对比的个数
F_stat <- ((n1 + n2 - k - 1) / (k * (n1 + n2 - 2))) * T2
cat("F 统计量:", F_stat, "\n")

# 自由度
df1 <- k
df2 <- n1 + n2 - k - 1
cat("自由度: df1 =", df1, ", df2 =", df2, "\n")

# 临界值和 p 值
alpha <- 0.05
F_crit <- qf(1 - alpha, df1, df2)
p_value <- 1 - pf(F_stat, df1, df2)

cat("F 临界值 (α=0.05):", F_crit, "\n")
cat("p 值:", p_value, "\n")

# 结论
if (F_stat > F_crit) {
  cat("\n结论: 拒绝 H0，认为甲、乙两种品牌产品的每个指标间的差异有显著的不同。\n")
} else {
  cat("\n结论: 不拒绝 H0，尚不能认为差异有显著的不同。\n")
}