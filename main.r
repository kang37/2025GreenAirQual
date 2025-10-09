# 加载包 ----
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(mapview)
library(lubridate)

# 测试变量 ----
var_meteo <- c("tmax", "tmin", "tavg", "rh", "precip")
var_pollute <- c("o3_8h", "so2_avg", "no2_avg", "pm2_5_avg")

# 配对站点 ----
# 国家空气污染站点。
pollute_nat_stat <- read.csv("data_raw/airquality_stations_locations.csv") %>%
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
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# 函数：找到距一个点数据最近的另一个点数据的点，并计算该最近距离。
pair_near_point <- function(point_x, point_y) {
  # 找到各污染站点距离最近的气象点的索引，即行号。
  nearest_index <- st_nearest_feature(x = point_x, y = point_y)

  # 计算污染站点到最近的气象站点的距离。
  distances <- st_distance(
    x = point_x,
    y = point_y[nearest_index, ], # 只使用最近的气象站点子集。
    by_element = TRUE # 确保只计算按行配对元素，即每个点到其最近邻点的距离。
  )

  # 将距离结果添加到污染站点数据框中。
  # 将最近天气站点ID添加到污染站点数据中。
    point_x %>%
    bind_cols(
      # 距离污染站点最近的天气站点ID。
      point_y %>%
        st_drop_geometry() %>%
        slice(nearest_index) %>%
        select(near_meteo_stat_id = meteo_stat_id)
    ) %>%
    # 将距离由默认为单位米转换为公里。
    mutate(dist_to_meteo = as.numeric(distances) / 1000) %>%
      return()
}
pair_pollutenat_meteo_stat <- pair_near_point(pollute_nat_stat, meteo_station)

# 配对分析。
# 小于一定距离的配对站点数。
ggplot(pair_pollutenat_meteo_stat) +
  geom_histogram(aes(dist_to_meteo))
sum(pair_pollutenat_meteo_stat$dist_to_meteo <= 1)

# 省控站点情况分析。
# 省污染站点。
pollute_prov_stat <- read.csv("data_raw/prolonlat_filtered.csv") %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  select(lon_site, lat_site) %>%
  # Bug: 删除重复行，每个站点保留一行。
  distinct() %>%
  # 添加ID。
  mutate(pollut_stat_id = row_number(), .before = 1) %>%
  st_as_sf(coords = c("lon_site", "lat_site"), crs = 4326, remove = FALSE)
# 距省污染站点最近的天气站点。
prov_pollut_near_meteo <- pair_near_point(pollute_prov_stat, meteo_station)

# 配对站点距离分布情况。
ggplot(prov_pollut_near_meteo) +
  geom_histogram(aes(dist_to_meteo))
sum(prov_pollut_near_meteo$dist_to_meteo <= 0.5)

# 目标省污染站点和气象站点。
prov_pollut_near_meteo_tar <- prov_pollut_near_meteo %>%
  filter(dist_to_meteo <= 0.5)
mapview(prov_pollut_near_meteo_tar, col.regions = "blue") +
  mapview(
    meteo_station %>%
      filter(meteo_stat_id %in% prov_pollut_near_meteo_tar$near_meteo_stat_id),
    col.regions = "red"
  )

# 读取各站点数据。
# 读取气象数据，并且仅保留目标站点数据。
pollute_data <- read_csv("data_raw/province_DATA_clean.csv") %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  rename_with(~ tolower(.x)) %>%
  select(lon_site, lat_site, year, month, day, o3_8h, so2_avg, no2_avg, pm2_5_avg) %>%
  left_join(
    prov_pollut_near_meteo_tar %>%
      st_drop_geometry() %>%
      select(pollut_stat_id, lon_site, lat_site) %>%
      distinct(),
    by = c("lon_site", "lat_site")
  ) %>%
  filter(!is.na(pollut_stat_id)) %>%
  mutate(date = as_date(paste(
    year, sprintf("%02i", month), sprintf("%02i", day), sep = "-"
  )))

# 读取NDVI数据。
ndvi_data <- rbind(
  read.csv("data_raw/ndvi_landsat_china_2019_500m.csv"),
  read.csv("data_raw/ndvi_landsat_china_2020_500m.csv"),
  read.csv("data_raw/ndvi_landsat_china_2021_500m.csv")
) %>%
  tibble() %>%
  select(-"system.index", -".geo", -buffer) %>%
  mutate(
    date = ymd_hms(date),
    date = as_date(date),
    meteo_stat_id = as.character(meteo_stat_id)
  ) %>%
  # 每天不止一个数值，因此取平均值。
  group_by(meteo_stat_id, date) %>%
  summarise(ndvi = mean(ndvi), .groups = "drop")

# 函数：
#' 计算数据集中指定多列的周平均值
#'
#' @param data 要处理的 data.frame 或 tibble。
#' @param value_cols 一个包含要计算平均值的列名的**字符向量** (例如: c("ndvi", "air_temp"))。
#' @param date_col 包含日期的列的名称 (必须是日期或可转换为日期的格式，**传入字符串**)。
#' @param group_id_col (可选) 用于分组的 ID 列的名称 (**传入字符串**)。
#'
#' @return 一个新的 data.frame/tibble，包含 'year', 'week' 和所有计算出的周平均值列。
#'
calc_week_avg <- function(data, value_cols, date_col = "date", group_id_col) {
  # 1. 提取日期、年份和周数 (使用 !!sym() 确保字符串被识别为列名)
  df_processed <- data %>%
    mutate(
      # 1.1 确保日期类型正确
      !!sym(date_col) := as_date(!!sym(date_col)),
      # 1.2 提取年份和周数
      year = year(!!sym(date_col)),
      week = isoweek(!!sym(date_col))
    )

  # 2. 设置分组变量
  grouping_vars <- c("year", "week")
  if (!is.null(group_id_col)) {
    # 如果 group_id_col 被提供，将其字符串加入分组变量
    grouping_vars <- c(group_id_col, grouping_vars)
  }

  # 3. 按分组计算多列的平均值
  df_result <- df_processed %>%
    group_by(!!!syms(grouping_vars)) %>% # 使用 !!!syms 动态分组
    summarise(
      across(
        .cols = all_of(value_cols),
        .fns = list(weekly_mean = ~mean(.x, na.rm = TRUE)),
        .names = "{col}"
      ),
      .groups = 'drop'
    )

  return(df_result)
}

# Bug.
var_target <- c(var_meteo, var_pollute, "ndvi")

# 将污染数据、气象数据、NDVI数据组合起来。
pollute_meteo_tar <- prov_pollut_near_meteo_tar %>%
  st_drop_geometry() %>%
  select(-lon_site, -lat_site) %>%
  left_join(
    pollute_data %>%
      select(-lon_site, -lat_site) %>%
      mutate(date = as_date(paste(
        year, sprintf("%02i", month), sprintf("%02i", day), sep = "-"
      ))),
    by = "pollut_stat_id"
  ) %>%
  # Bug.
  mutate(near_meteo_stat_id = as.character(near_meteo_stat_id)) %>%
  # 加入气象数据。
  left_join(
    # Bug.
    meteo_dt %>% mutate(date = as_date(date)),
    by = c("near_meteo_stat_id" = "meteo_stat_id", "date")
  ) %>%
  # 3. 为每日数据计算年份和周数，以便与周 NDVI 合并
  mutate(
    year = year(date),
    week = isoweek(date)
  ) %>%
  tibble() %>%
  # Bug.
  select(-year, -month, -day) %>%
  relocate(date, .before = 1) %>%
  mutate(year = year(date)) %>%
  # 加入NDVI数值。
  left_join(
    ndvi_data,
    by = c("date", "near_meteo_stat_id" = "meteo_stat_id")
  ) %>%
  # 对每个配对赋予一个独特编号。
  mutate(
    res_stat_id = paste(near_meteo_stat_id, pollut_stat_id, sep = "-"),
    .before = 1
  ) %>%
  # 计算周平均值。
  calc_week_avg(
    data = ., value_cols = c(var_meteo, var_pollute, "ndvi"),
    group_id_col = "res_stat_id"
  ) %>%
  # 补全日期。
  complete(
    res_stat_id,
    year,
    week = 1:53,
    fill = as.list(setNames(rep(NA, length(var_target)), var_target))
  ) %>%
  mutate(
    year_week = paste0(year, str_pad(week, width = 2, pad = "0"))
  ) %>%
  arrange(res_stat_id, year, week)

# 查看数据完整性。
# 查看单列数据完整性。
pollute_meteo_tar %>%
  ggplot(aes(x = year_week, y = as.character(res_stat_id))) +
  geom_tile(aes(fill = !is.na(tavg)), color = "white", linewidth = 0.1) +
  theme(axis.text.x = element_text(angle = 90))
# 查看多列数据完整性。
pollute_meteo_tar %>%
  # 创建一个综合指标列 'all_vars_present'
  mutate(
    # if_all() 检查 var_target 列表中的所有列是否都满足给定的条件
    # 条件是 !is.na()，即“非缺失”
    all_vars_present = if_all(all_of(var_target), ~!is.na(.))
  ) %>%
  # 可视化：将综合指标映射到填充颜色
  ggplot(aes(x = year_week, y = as.character(res_stat_id))) +
  # 使用综合指标 'all_vars_present' 填充颜色
  geom_tile(aes(fill = all_vars_present), color = "white", linewidth = 0.1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )



