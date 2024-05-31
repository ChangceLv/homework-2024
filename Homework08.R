#----------------------------------------------------------------------------
#Script Name：Homework08.R
#Purpose:This scribes How to perform spatial data analysis using R, which includes tasks like creating buffers, clipping, extracting raster values, merging datasets, and handling spatial data.
#Author:Changce Lv
#Email:thisislcc@mail.ustc.edu.cn
#Date:2024-4-27
#Revise date:2024-5-31
#-----------------------------------------------------------------------------
# 载入所需的包
library(qgisprocess)
library(sf)
library(raster)

# 读取 Doubs 数据集
doubs <- st_read("path_to_doubs_dataset.shp")

# 读取集水面积和坡度的栅格数据
catchment_raster <- raster("path_to_catchment_raster.tif")
slope_raster <- raster("path_to_slope_raster.tif")

# 沿 Doubs 河设定 2 公里缓冲区
doubs_buffer <- st_buffer(doubs, dist = 2000)

# 裁剪集水面积和坡度栅格
catchment_clip <- qgis_clip_raster(catchment_raster, doubs_buffer)
slope_clip <- qgis_clip_raster(slope_raster, doubs_buffer)

# 提取每个点的栅格值
catchment_values <- extract(catchment_clip, doubs_buffer)
slope_values <- extract(slope_clip, doubs_buffer)

# 将提取的数据与 Doubs 数据集的其他环境因素合并
doubs_data <- data.frame(doubs$other_factors, catchment = catchment_values, slope = slope_values)

# 将数据框转换为带有几何列的 sf 对象
doubs_sf <- st_as_sf(doubs_data, coords = c("longitude_column_name", "latitude_column_name"), crs = st_crs(doubs))
