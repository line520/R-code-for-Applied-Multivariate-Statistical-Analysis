library(readxl)
library(MASS)
library(klaR)

data <- read_excel("data/exec5.8.xlsx")
colnames(data) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "g")
data$g <- factor(data$g, levels = c(1,2,3), labels = c("通用牛奶","克罗格","夸克"))
data <- as.data.frame(data)

# 逐步判别（偏F检验）
step_model <- stepclass(g ~ ., data = data, method = "lda",
                        direction = "both", criterion = "AS",
                        alpha.in = 0.15, alpha.out = 0.15)

# 手动记录入选变量（从输出可见）
selected_vars <- c("x3", "x5", "x1", "x8")

# 最终判别模型
final_lda <- lda(g ~ x3 + x5 + x1 + x8, data = data)
summary(final_lda)

# 输出判别结果
pred <- predict(final_lda, data)
table(真实 = data$g, 预测 = pred$class)