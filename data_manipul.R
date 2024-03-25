# 创建数据框
music_data <- data.frame(
  name = c("lemon tree","shape of you","city of stars","closer"),
  year = c(2015,2016,2017,2022),
  singer = c(5,1,2,2),
  score = c(98,96,97,99)
)

write.csv(music_data,"music_data.csv",row.names = FALSE)


# 查看数据结构
str(music_data)

# 检查整个数据框是否有缺失数据
summary(music_data)

# 读取歌手和年份数据
singer <- doubs$singer
year <- doubs$year
name <- doubs$name
score <- doubs$score

# 检查某列是否有缺失数据
sum(is.na(year))

# 从列中提取值或选择/添加列
music_data$year[3]

# 将宽表格转换为长格式
library(tidyverse)
surveys_wide <- read.csv("music_data.csv")
head(surveys_wide)
surveys_long <- surveys_wide |>
  pivot_longer(
    cols = singer:score,
    names_to = "variable",
    values_to = "information"
  )
surveys_long

#  数据可视化
library(ggplot2)
surveys_long %>% 
  ggplot(aes(x = year, y = information, color = name)) +
  geom_line()

# 添加annotate

# 1.创建一个图层
p <- surveys_long %>% 
  ggplot(aes(x = year, y = information, color = name)) +
  geom_line()
# 2.在图层上添加文本
p + annotate("text", x = 2019, y = 5, label = "score") + annotate("text", x = 2019, 
                                                                  y = 98, label = "year")