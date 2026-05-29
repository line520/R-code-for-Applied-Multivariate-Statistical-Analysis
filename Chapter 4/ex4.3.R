X1 <- matrix(c(
  194, 192, 141,
  208, 188, 165,
  233, 217, 171,
  241, 222, 201,
  265, 252, 207,
  269, 283, 191
), ncol = 3, byrow = TRUE)
colnames(X1) <- c("阶段1", "阶段2", "阶段3")

# 乙品牌数据
X2 <- matrix(c(
  239, 127, 90,
  189, 105, 85,
  224, 123, 79,
  243, 123, 110,
  243, 117, 100,
  226, 125, 75
), ncol = 3, byrow = TRUE)
colnames(X2) <- c("阶段1", "阶段2", "阶段3")

n1 <- nrow(X1)
n2 <- nrow(X2)
p <- ncol(X1)

data_man <- data.frame(
  品牌 = factor(rep(c("甲", "乙"), each = n1)),
  rbind(X1, X2)
)
fit <- manova(cbind(阶段1, 阶段2, 阶段3) ~ 品牌, data = data_man)
summary(fit, test = "Hotelling-Lawley")  # 给出近似F检验，p值应与上面一致
summary.aov(fit)