# --------------------------------------------
# Script Name: ts_fs_ml
# Purpose: This script is to show how conduct 
#          EDA and ML of time series，be familiar 
#          with the main packages, such as timetk,
#          tidymodels, modeltime……
# Author:  Changce Lv
# Email:   thisislcc@mail.ustc.edu.cn
# Date:    2024-5-31
# --------------------------------------------
####任务一：创建一个时间序列对象并进行可视化
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables
# A) create timestamp

##将一个字符串表示的日期转换成程序中能够识别和处理的日期对象。

library(lubridate) 
date = ymd("2017-01-31")
print(date)

##根据日期的各个组成部分创建一个完整的日期时间对象
library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
data(flights, package="nycflights13")

head(flights)
flights %>% #选择flights数据集中的年、月、日、小时、分钟这些字段
  select(year, month, day, hour, minute) %>% 
  mutate(dep_date_time = make_datetime(year, month, day, hour, minute)) %>% head()#使用make_datetime()函数创建一个新的日期时间变量dep_date_time，并展示前几行数据。

##在日期和日期时间之间进行转换

as_date(now())

###-------------------------------------------------------
##创建时间序列
#使用ts()函数来创建时间序列
# devtools::install_github("PascalIrz/aspe")
# library(aspe)


#读取数据，进行清洗并去除重复的记录
data=read.table("D:/CC/研一下/数据驱动的生态学研究方法/RawBiomassData.txt",h=TRUE)#读取数据
head(data)
data_clean <- data |>
  dplyr::select(-YEAR) |>#从数据中移除名为'YEAR'的列
  # drop_na() |>
  distinct() # 识别并删除重复的记录，确保每一行都是唯一的


#对数据进行站点和鱼类种类的统计、检查，基于“VERCah站点和VAI种类”条件筛选出感兴趣的数据，
unique(data_clean$STATION) 
table(data_clean$STATION)
unique(data_clean$SP) 
table(data_clean$SP)
mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI")

data_ts = ts(data = mydata[, -c(1:5)], 
             start = c(1994),
             frequency = 1)  


#使用ggplot2和forecast库中的函数来绘制数据的分面图（faceted plot） 
install.packages("forecast")
install.packages("urca")
library(urca)
library(forecast) #autoplot()与ggplot2库配合使用，可以用来绘制时间序列数据的图形
library(ggplot2)
autoplot(data_ts, facets = TRUE) +#调用autoplot函数，绘制时间序列的自动可视化
  ggtitle("CHE of Doubs river") +
  ylab("Changes") + xlab("Year")

 
# 使用timetk库中的函数来创建时间序列数据
install.packages("timetk")
library(timetk)
mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI")#使用 timetk 库中的函数来创建一个新的数据集 mydata，其中包含了满足条件 STATION=="VERCah" & SP == "VAI" 的记录

library(tidyverse)
install.packages("tsibble")
library(tsibble)


#对数据进行格式转换和处理，将数据整理为适合做时间序列分析的形式
datatk_ts <- mydata |>
  tk_tbl() |> 
  # mutate(DATE = as_date(as.POSIXct.Date(DATE))) |>
  select(-1) |>
  rename(date = DATE) |>
  relocate(date, .before = STATION) |> 
  pivot_longer( 
    cols = c("BIOMASS", "DENSITY"))


# 对时间序列数据进行分组、汇总和可视化
datatk_ts |>
  group_by(name) |>
  plot_time_series(date, value, 
                   .facet_ncol = 2, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "VAI of Le Doubs river"
  )

datatk_ts1 <- 
  datatk_ts |>
  group_by(name) |>
  summarise_by_time(
    date, 
    .by = "year",
    value = first(value)) |>
  pad_by_time(date, .by = "year") |>
  plot_time_series(date, value,
                   .facet_ncol = 2,
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "VAI of Le Doubs river"
  )

#展示Doubs河VAI生物量的变化情况，并通过小波变换对数据进行降维处理
# 可视化、降维处理
library(TSrepr)
mydata <- data_clean |>
  filter(STATION=="VERCah" & SP == "VAI")#数据中筛选出站点为 "VERCah" 且物种为"VAI"的数据，并将结果保存到 mydata 中。

biom_ts <- ts(mydata[,-c(1:5,7)],
              start= c(1994,1),
              frequency =1)

plot(biom_ts, main="VAI biomass of Doubs river", ylab="Changes", xlab="Year")

data_dwt <- repr_dwt(mydata$BIOMASS, level = 1) 
data_dwt_ts <- ts(data_dwt,
                  start= c(1994,1),
                  frequency =1)

p2 <- autoplot(data_dwt_ts) +
  ggtitle("VAI biomass of Doubs river") +
  ylab("Changes") + xlab("Year")#转换成时间序列对象 ，设置起始时间和频率

library(patchwork)
p1+p2
#---------------------------------------------------------------------
####任务二：构建一个预测模型
## 加载包和数据
library(tidyverse)  
library(timetk) 
library(tidymodels)
install.packages("modeltime")
library(modeltime)
library(timetk)

mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI")

biomtk_ts <- mydata |> # Convert to tibble
  tk_tbl() |> 
  select(index, DATE, BIOMASS) # keep date and target

# biomtk_ts |>
#   plot_time_series(DATE, BIOMASS,
#                    .facet_ncol  = NULL,
#                    .smooth      = FALSE,
#                    .interactive = TRUE,
#                    .title = "Biomass timeseries")

install.packages("tidyquant")
library(tidyquant)
ggplot(biomtk_ts, aes(x = DATE, y = BIOMASS)) +
  geom_line() +
  ggtitle("Biomass of Fishes in Doubs")


##使用训练集来训练模型，然后使用测试集来评估模型的性能

# splits <- biomtk_ts |>
#   time_series_split(DATE,
#                     assess = "3 year", 
#                     cumulative = TRUE)
# 
# splits

n_rows <- nrow(biomtk_ts)
train_rows <- round(0.8 * n_rows)

train_data <- biomtk_ts |>
  slice(1:train_rows) # slice() from dplyr
test_data <- biomtk_ts |>
  slice((train_rows):n_rows)

ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Training"), 
            linewidth = 1) +
  geom_line(data = test_data, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  scale_color_manual(values = c("Training" = "blue", 
                                "Test" = "red")) +
  labs(title = "Training and Test Sets", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

# 使用R语言中的recipes包，对原始数据进行预处理和特征工程，生成适用于机器学习模型的特征变量
install.packages("recipes")
library(recipes)
install.packages("tidymodels")
library(tidymodels)

recipe_spec_final <- recipe(BIOMASS ~ ., train_data) |>
  step_mutate_at(index, fn = ~if_else(is.na(.), -12345, . )) |>
  step_timeseries_signature(DATE) |>
  step_rm(DATE) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

summary(prep(recipe_spec_final))


## 训练和评估模型：利用梯度提升树算法对数据进行训练
install.packages("parsnip")
install.packages("recipes")
install.packages("workflows")
library(parsnip)
library(recipes)
library(workflows)
# 创建一个工作流程（workflow），并使用梯度提升树（boosted tree）模型对数据进行训练
bt <- workflow() |>
  add_model(
    boost_tree("regression") |> set_engine("xgboost")
  ) |>
  add_recipe(recipe_spec_final) |>
  fit(train_data)

bt

# 评估模型性能并可视化模型预测结果

bt_test <- bt |> 
  predict(test_data) |>
  bind_cols(test_data) 

bt_test

pbt <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Train"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "bt-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

# 使用yardstick包中的metrics函数来计算模型预测结果与实际观测之间的误差指标
bt_test |>
  metrics(BIOMASS, .pred)


##比较不同算法（bt和rf）的表现，并进行模型校准和评估
# 创建一个包含不同算法模型的 Modeltime Table model_tbl
model_tbl <- modeltime_table(
  bt,
  rf
)
model_tbl

# 使用modeltime_calibrate函数对model_tbl进行校准，在新数据上验证模型的性能
calibrated_tbl <- model_tbl |>
  modeltime_calibrate(new_data = test_data)
calibrated_tbl 

# 模型评估
calibrated_tbl |>
  modeltime_accuracy(test_data) |>
  arrange(rmse)

#创建包含模型预测结果和实际观测数据的交互式预测图表
calibrated_tbl |>
  modeltime_forecast(
    new_data    = test_data,
    actual_data = biomtk_ts,
    keep_data   = TRUE 
  ) |>
  plot_modeltime_forecast(
    .facet_ncol         = 2, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE
  )
#----------------------------------------------------------------