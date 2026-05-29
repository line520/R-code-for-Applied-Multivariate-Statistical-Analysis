# ============================================================
# 田径国家成绩聚类分析
# 数据文件: exec6.6.xlsx
# 方法: 类平均法(average), Ward法, k-均值法
# 预处理: 对所有变量进行Z-score标准化
# 依赖: my_hclust.R (提供 hclust_manual 和 plot_dendrogram)
# ============================================================

library(readxl)   # 读取Excel文件
library(ggplot2)  # 绘制肘部图
library(cluster)  # 轮廓系数

# 导入自定义层次聚类函数 ---------------------------------------
source("my_hclust.R")   # 加载 hclust_manual 和 plot_dendrogram

# 导入数据 ----------------------------------------------------
data <- read_excel("./data/exec6.6.xlsx")
country_names <- data$国家和地区

# 删除该列，并将剩余部分转为矩阵
data_matrix <- as.matrix(data[, -1])
rownames(data_matrix) <- country_names

# 数据标准化 
data_scaled <- scale(data_matrix)

# 计算欧氏距离矩阵 
dist_mat <- dist(data_scaled, method = "euclidean")

# 利用类平均法 (average)并绘图
hc_average <- hclust_manual(as.matrix(dist_mat), method = "average",
                            labels = rownames(data_scaled))
plot_dendrogram(hc_average, main = "类平均法聚类树状图")

# 利用Ward法并绘图
hc_ward <- hclust_manual(as.matrix(dist_mat), method = "ward",
                         labels = rownames(data_scaled))
plot_dendrogram(hc_ward, main = "Ward法聚类树状图")

# 8. k-均值聚类 
# 确定最佳聚类数 k（肘部法 + 轮廓系数法）
set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(data_scaled, centers = k, nstart = 25)$tot.withinss
})

# 肘部图
elbow_df <- data.frame(k = 1:10, wss = wss)
ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line() + geom_point() +
  labs(title = "肘部图确定最佳k值", x = "聚类数 k", y = "组内平方和") +
  theme_minimal()
ggsave("elbow_plot.png", width = 6, height = 4)

# 轮廓系数 (k=2..10)
sil_width <- sapply(2:10, function(k) {
  km <- kmeans(data_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(data_scaled))
  mean(ss[, 3])
})
sil_df <- data.frame(k = 2:10, sil_width = sil_width)
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() + geom_point() +
  labs(title = "轮廓系数法", x = "聚类数 k", y = "平均轮廓宽度") +
  theme_minimal()
ggsave("silhouette_plot.png", width = 6, height = 4)

# 根据图形选择 k=3（示例）
k_optimal <- 4
kmeans_result <- kmeans(data_scaled, centers = k_optimal, nstart = 25)

# 输出k-均值结果
cat("\n===== k-均值聚类结果 (k =", k_optimal, ") =====\n")
print(table(kmeans_result$cluster))
cat("\n各类中心 (标准化后的均值):\n")
print(kmeans_result$centers)

# 将聚类标签添加到原始数据框
data$kmeans_cluster <- kmeans_result$cluster
# 显示各类别中的国家
for (cl in 1:k_optimal) {
  cat("\n类别", cl, ":", 
      paste(rownames(data_scaled)[kmeans_result$cluster == cl], collapse = ", "), "\n")
}