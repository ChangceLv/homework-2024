#-----------------------------------------
#Script name:"pak_info.R"
#Author:ChangceLv
#Email:thisislcc@mail.ustc.edu.cn
#Date:20240319
#-----------------------------------------
install.packages("tidyverse")

#加载包
library("tidyverse")

#获得帮助文件
help("tidyverse") #获得帮助文件

#了解tidyverse包功能
browseVignettes("tidyverse")  #跳转到tidyverse网页
demo("tidyverse")             
apropos("^tidyverse")         #查询tidyverse包含的各组分

#查看包中的函数和数据集
ls("package:tidyverse")

#查询包中包含关键字的函数
apropos("tidy", mode = "function")

#查看包中的Vignettes
browseVignettes(package = "tidyverse")

