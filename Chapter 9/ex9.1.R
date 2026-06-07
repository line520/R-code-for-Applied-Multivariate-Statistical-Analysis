library(readxl)
library(vcd)
library(ca)

data <- read_excel("./data/exec9.1table.xlsx", sheet = 1)

#构建列联表
#由于后面ca函数只能用英文字符，故列名和行名用数字和字母代替
#其中1(4万以下)，2(4-8万)，3(8万以上)
#其中A(非常不满意)，B(有些不满意)，C(比较满意)，D(非常满意)
mytable <- as.matrix(data[, -1])
rownames(mytable) <- c("1", "2", "3")
colnames(mytable) <- c("A", "B", "C", "D")

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