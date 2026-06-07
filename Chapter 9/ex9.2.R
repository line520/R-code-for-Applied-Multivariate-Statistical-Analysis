library(readxl)
library(vcd)
library(ca)

data <- read_excel("./data/exec9.2table.xlsx", sheet = 1)

#构建列联表
mytable <- as.matrix(data[, -1])
rownames(mytable) <- c("P0","P1","P2","P3","P4","P5","P6") #代表考古场所
colnames(mytable) <- c("A", "B", "C", "D")                 #代表陶器类型

#增减边际估计
addmargins(mytable)
round(prop.table(mytable, 1), 3)    # 行轮廓矩阵
round(prop.table(mytable, 2), 3)    # 列轮廓矩阵

#对应分析
ca_result <- ca(mytable)
summary(ca_result)
ca_result$sv

#标准坐标
X <- ca_result$rowcoord %*% diag(ca_result$sv)
round(X, 3)
Y <- ca_result$colcoord %*% diag(ca_result$sv)
round(Y, 3)

#绘制图像
plot(ca_result, main = "对应分析图")