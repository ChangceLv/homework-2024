install.packages("corrplot")

# 加载所需的库
library(tidyverse)
library(corrplot)

# 加载Doubs数据集
data("doubs")

# 删除具有缺失数据的站点
doubs_clean <- na.omit(doubs)

# 检查环境因素之间的共线性
#在此之前，加载：
# 加载ade4包
library(ade4)
library(car)
library(caret)
# 读取环境和鱼类数据
data(doubs)
env <- doubs$env
fish <- doubs$fish

# 按站点汇总鱼类丰度数据
fish_sum <- apply(fish, 1, sum)
fish_df <- data.frame(Site = rownames(fish), Fish_Abundance = fish_sum)

# 将环境和总鱼类组合成一个名为“env_fish”的新数据框
env_fish <- cbind(env, fish_df[match(rownames(env), fish_df$Site), 2])

# 使用scatterplot()可视化新的env_fish集的特征
names(env_fish)[12] <- "Fish_Abundance"


# 设置 x 变量和 y 变量
x_vars <- c("dfs", "alt", "slo", "flo", "pH", "har", "pho", "nit", "amm", "oxy", "bdo")
y_var <- "Fish_Abundance"

# 从数据集中提取 x 变量和 y 变量
x_data <- env_fish[, x_vars]
y_data <- env_fish[[y_var]]


# 检查环境变量和站点总鱼类丰度之间的线性关系
cor(env_fish[, 1:11], env_fish[, 12], method = "pearson")

# 删除没有鱼的站点
env_fish <- env_fish[rowSums(fish) > 0, ]

# 删除空值或异常值的行
env_fish <- env_fish[complete.cases(env_fish), ]

nzv <- nearZeroVar(env_fish[, 1:11])
if (length(nzv) > 0) {
  env_fish <- env_fish[, -nzv]
}
#进行
cor(env_fish[, 1:11], env_fish[, 12], method = "pearson")

# 计算相关系数矩阵
corr_matrix <- cor(env_fish)

# 可视化相关系数矩阵
corrplot(corr_matrix, method = "circle")

# 加载所需的库
library(GGally)

# 假设鱼类数据在一个名为“fish_data”的单独数据框中

# 线性回归分析
fish_env_lm <- lapply(env_fish[, -1], function(x) {
  lm(x ~ dfs + alt + slo + flo + pH + har + pho + nit + amm + oxy + bdo, data = env_fish)
})

# 可视化
scatterplotMatrix(x_data, diagonal = list(method = "boxplot"), smooth = TRUE, 
                  regLine = TRUE, legend = TRUE,
                  var.labels = c("\n\n\nDFS", "\n\n\nALT", "\n\n\nSLO", "\n\n\nFLO", "\n\n\npH", 
                                 "\n\n\nHAR", "\n\n\nPHO", "\n\n\nNIT", "\n\n\nAMM", "\n\n\nOXY", "\n\n\nBDO"),
                  cex = 1, cex.labels = 1.5, cex.axis = 1.5,
                  pch = c(16, 16, 16), col = c("red", "green3", "blue"), row1attop = TRUE)
