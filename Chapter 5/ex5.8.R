library(readxl)
library(MASS)

data <- read_excel("data/exec5.8.xlsx")
colnames(data) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "g")
data$g <- factor(data$g, levels = c(1, 2, 3), labels = c("通用牛奶", "克罗格", "夸克"))

# 1. 费希尔判别（线性判别分析）
lda_model <- lda(g ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, data = data)

# 2. 输出判别函数系数（Fisher判别函数）
cat("费希尔判别函数系数（标准化）:\n")
print(lda_model$scaling)  # 每列为一个判别函数 LD1, LD2

# 3. 计算每个样本在两个判别函数上的得分
scores <- predict(lda_model)$x  # 包含 LD1 和 LD2
data_scores <- cbind(data, scores)

# 定义符号：1=通用牛奶 用实心圆，2=克罗格 用三角，3=夸克 用方块
pch_vec <- c(16, 17, 15)
names(pch_vec) <- c("通用牛奶", "克罗格", "夸克")
col_vec <- c("red", "blue", "darkgreen")
names(col_vec) <- c("通用牛奶", "克罗格", "夸克")

# 基础绘图
dev.new()
plot(data_scores$LD1, data_scores$LD2, type = "n",
     xlab = "第一判别函数得分 (LD1)", ylab = "第二判别函数得分 (LD2)",
     main = "Fisher判别函数得分散点图")
for (grp in levels(data_scores$g)) {
  idx <- data_scores$g == grp
  points(data_scores$LD1[idx], data_scores$LD2[idx],
         pch = pch_vec[grp], col = col_vec[grp], cex = 1.2)
}
legend("topright", legend = levels(data_scores$g),
       pch = pch_vec, col = col_vec, title = "厂商")