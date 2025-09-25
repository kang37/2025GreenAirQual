# 加载包。
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(readr)

# 国家空气污染站点。
pollution_data <- read.csv("data_raw/airquality_stations_locations.csv") %>%
  rename_with(~ tolower(.x)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# 气象站点。
# 函数：只读取文件的第一行并解析元数据。
extract_station_metadata <- function(file_path) {
  # 读取文件的第一行站点元数据。
  header_line <- readLines(file_path, n = 1)

  # 解析元数据。第一行格式为：站点编号,经度,纬度。
  header_parts <- strsplit(header_line, ",")[[1]]

  # 创建一个单行data frame。
  data.frame(
    meteo_stat_id = as.integer(header_parts[1]),
    longitude = as.numeric(header_parts[2]),
    latitude = as.numeric(header_parts[3])
  )
}

# 获取所有txt文件的文件名列表。
file_list <- list.files(
  path = "data_raw/meteo_data_1961-2023",
  pattern = "\\.txt$", # 匹配所有以.txt结尾的文件。
  full.names = TRUE
) %>%
  .[!grepl("sta_lonlat_china.txt", .)]
# 循环处理所有文件并合并为一个表格。
meteo_station <- map_dfr(file_list, extract_station_metadata) %>%
  # 转换为 sf 对象。
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# 找到各污染站点距离最近的气象点的索引，即行号。
nearest_index <- st_nearest_feature(x = pollution_data, y = meteo_station)

# 计算污染站点到最近的气象站点的距离。
distances <- st_distance(
  x = pollution_data,
  y = meteo_station[nearest_index, ], # 只使用最近的气象站点子集。
  by_element = TRUE # 确保只计算按行配对元素，即每个点到其最近邻点的距离。
)

# 将距离结果添加到污染站点数据框中。
pollution_near_meteo <-
  # 将最近天气站点ID添加到污染站点数据中。
  pollution_data %>%
  bind_cols(
    # 距离污染站点最近的天气站点ID。
    meteo_station %>%
      st_drop_geometry() %>%
      slice(nearest_index) %>%
      select(near_meteo_stat_id = meteo_stat_id)
  ) %>%
  # 将距离由默认为单位米转换为公里。
  mutate(dist_to_meteo = as.numeric(distances) / 1000)

# 配对分析。
# 小于一定距离的配对站点数。
ggplot(pollution_near_meteo) +
  geom_histogram(aes(dist_to_meteo))
sum(pollution_near_meteo$dist_to_meteo <= 1)
