library(sf)
library(readr)
library(lubridate)

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
  filter(!is.na(pollut_stat_id))

# 读取NDVI数据。
ndvi_data <- rbind(
  read.csv("data_raw/ndvi_landsat_china_2020_500m.csv"),
  read.csv("data_raw/ndvi_landsat_china_2021_500m.csv")
) %>%
  tibble() %>%
  select(-"system.index", -".geo", -buffer) %>%
  mutate(date = ymd_hms(date), date = as_date(date)) %>%
  # 每天不止一个数值，因此取平均值。
  group_by(meteo_stat_id, date) %>%
  summarise(ndvi = mean(ndvi), .groups = "drop")

# 计算NDVI周平均值。
ndvi_weekly_avg <- ndvi_data %>%
  # 1. 确保日期类型正确，并提取年份和周数
  mutate(
    date = as_date(date), # 确保日期格式正确
    # 使用 isoweek() 可以避免年份跨越时周数计算的歧义
    year = year(date),
    week = isoweek(date)
  ) %>%
  # 2. 按站点、年份和周数进行分组
  group_by(meteo_stat_id, year, week) %>%
  # 3. 计算周平均 NDVI
  summarise(
    # 取该周所有 Landsat 观测值的平均 NDVI
    ndvi_weekly_mean = mean(ndvi, na.rm = TRUE),
    .groups = 'drop'
  )

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
  na.omit() %>%
  # 对每个配对赋予一个独特编号。
  mutate(res_stat_id = paste(near_meteo_stat_id, pollut_stat_id, sep = "-"), .before = 1)
pollute_meteo_ndvi_tar <- pollute_meteo_tar %>%
  # 加入NDVI数据。
  # 4. 加入周平均 NDVI 数据 (替换了原有的 left_join(ndvi_data, ...))
  left_join(
    ndvi_weekly_avg %>% mutate(meteo_stat_id = as.character(meteo_stat_id)),
    by = c("near_meteo_stat_id" = "meteo_stat_id", "year", "week")
  ) %>%
  # 移除用于合并的周标识符
  select(-week, -year) %>%
  na.omit() %>%
  arrange(res_stat_id, date)
# write_csv(pollute_meteo_ndvi_tar, "pollute_meteo_ndvi_tar.csv")

# 仅保留连续日期长于30天的数据。
pollute_meteo_ndvi_tar_filt <- pollute_meteo_ndvi_tar %>%
  # 按站点分组
  group_by(res_stat_id) %>%
  # 识别连续观测期
  mutate(
    # 计算当前日期与前一个日期的差值
    date_diff = as.numeric(date - lag(date, default = first(date))),
    # 识别间断点：如果日期差大于 1 天，则认为这是一个新的连续期
    # cumsum(date_diff > 1) 会为每个连续期生成一个唯一的 ID
    # 注意：我们将第一个日期也视为一个新组的开始 (0)
    # R 中的逻辑：TURE 视为 1，FALSE 视为 0
    group_id = cumsum(date_diff > 1)
  ) %>%
  # 重新分组：按站点和连续期 ID
  group_by(res_stat_id, group_id) %>%
  # 计算每个连续期的时间跨度 (天数)
  mutate(
    period_length = as.numeric(max(date) - min(date)) + 1
  ) %>%
  # 4. 筛选：仅保留周期长度大于阈值 (30天) 的所有记录
  filter(period_length >= 30) %>%
  # 移除用于计算的临时列
  ungroup() %>%
  select(-date_diff) %>%
  # 按站点和日期排序最终数据
  arrange(res_stat_id, date) %>%
  # Bug.
  rename(ndvi = ndvi_weekly_mean) %>%
  mutate(res_stat_grp_id = paste(res_stat_id, group_id, sep = "-"))
# write.csv(pollute_meteo_ndvi_tar_filt, "df_segments.csv", row.names = F)







# 1.1 创建唯一的站点ID进行合并 (以防经纬度略微不同)
prov_station_info <- prov_station_info %>%
  mutate(station_key = paste(lon_site, lat_site, sep = "_"))

daily_pollution_data <- daily_pollution_data %>%
  mutate(station_key = paste(lon_site, lat_site, sep = "_"))

# 1.2 合并每日污染数据与配对气象站ID
daily_pollution_with_id <- daily_pollution_data %>%
  # 只选择需要的列进行合并，确保不引入重复的经纬度列
  select(-lon_site, -lat_site) %>%
  left_join(
    prov_station_info %>% select(station_key, weather_id),
    by = "station_key"
  ) %>%
  # 清理临时的key。
  select(-station_key)

# 此时 daily_pollution_with_id 包含了每天的污染数据和对应的 weather_id







# 读取目标气象站点数据。
# 函数：读取单个文件的气象数据。
get_meteo_wea_data <- function(file_path) {
  # 从文件名中提取气象站ID
  meteo_stat_id <- basename(file_path) %>%
    str_replace("\\.txt$", "")

  # 读取数据：
  # skip = 1 跳过第一行（包含站点ID和经纬度的行）
  # col_names = TRUE 假设第二行是列名
  data <- read_tsv(file_path, skip = 1, show_col_types = FALSE) %>%
    # 添加气象站ID列
    mutate(meteo_stat_id = meteo_stat_id, .before = 1)

  return(data)
}

# 2.3 批量读取并合并所有气象数据
meteo_dt <- paste0(
  "data_raw/meteo_data_1961-2023/",
  prov_pollut_near_meteo_tar$near_meteo_stat_id,
  ".txt"
) %>%
  map_df(~ get_meteo_wea_data(.x)) %>%
  separate(
    col = `date,tmax,tmin,tavg,RH,CF,precip`,
    into = c("date", "tmax", "tmin", "tavg", "rh", "cf", "precip") ,
    sep = ",",
    convert = TRUE, # 尝试自动将拆分后的列转换为适当的类型（如数值或日期）
    extra = "drop" # 如果逗号数量不一致，多余的列将丢弃。
  ) %>%
  distinct()



# 3.1 统一日期列名以便合并
daily_pollution_to_join <- daily_pollution_with_id %>%
  rename(p_year = year, p_month = month, p_day = day)

# 3.2 合并污染数据与气象数据
final_combined_data <- daily_pollution_to_join %>%
  mutate(date = as_date(paste(
    p_year,
    sprintf("%02i", p_month),
    sprintf("%02i", p_day),
    sep = "-"
  )), .before = 1) %>%
  left_join(
    all_weather_data %>%
      mutate(meteo_stat_id = as.integer(meteo_stat_id), date = as_date(date)),
    by = c("near_meteo_stat_id" = "meteo_stat_id", "date")
  )

# 3.3 (可选) 重命名日期列
final_combined_data <- final_combined_data %>%
  rename(year = p_year, month = p_month, day = p_day) %>%
  # 整理列顺序，将污染数据和气象数据放在一起
  select(lon_site, lat_site, weather_id, year, month, day,
         O3_8h, everything())


find_continuous_periods_for_column_long <- function(data_subset, variable_name) {
  # 1. 筛选：只保留 Value 列非空值的数据
  data_filtered <- data_subset %>%
    # 关键修改：检查 Value 列是否为 NA
    filter(!is.na(Value)) %>%
    select(date) %>%
    distinct(date) # 确保单个站点单个日期无重复

  if (nrow(data_filtered) == 0) {
    return(tibble(
      Variable = variable_name,
      Start_Date = as.Date(NA),
      End_Date = as.Date(NA),
      Days_Count = 0
    ))
  }

  # 2. 识别间断点并分组
  data_grouped <- data_filtered %>%
    mutate(
      # 创建一个分组ID：不连续点（T/F）的累积和
      group_id = cumsum(c(0, diff(date) > 1))
    )

  # 3. 汇总：计算每个 group_id 的起始日期、结束日期和天数
  data_summary <- data_grouped %>%
    group_by(group_id) %>%
    summarise(
      Variable = variable_name,
      Start_Date = min(date),
      End_Date = max(date),
      Days_Count = as.numeric(End_Date - Start_Date) + 1,
      .groups = 'drop'
    ) %>%
    # Bug:
    # 只保留最长的时间段
    filter(Days_Count >= 60) %>%
    # 最终返回前进行去重
    select(var = Variable, Start_Date, End_Date, Days_Count) %>%
    distinct()

  return(data_summary)
}

pollutant_cols <- c("O3_8h", "so2_avg", "tmax", "precip")

final_combined_data_lng <- final_combined_data %>%
  select(station_key, date, O3_8h, so2_avg, tmax, precip) %>%
  mutate(tmax = as.numeric(tmax), precip = as.numeric(precip)) %>%
  # 1. 仅选择关键列进行长格式转换 (确保 final_combined_data 包含 date 和 station_key)
  select(station_key, date, all_of(pollutant_cols)) %>%
  pivot_longer(
    cols = all_of(pollutant_cols),
    names_to = "Variable",
    values_to = "Value"
  )



final_results_by_station =
  final_combined_data_lng %>%
  # 2. 按站点和污染物分组
  group_by(station_key, Variable) %>%
  # 3. 进行计算：对每个分组应用新的函数
  summarise(
    # 使用 cur_data() 传递当前分组的数据，并用 first(Variable) 传递污染物名称
    periods = list(find_continuous_periods_for_column_long(cur_data(), first(Variable))),
    .groups = 'drop'
  ) %>%
  # 4. 展开 periods 列表得到最终结果
  unnest(periods)

# 打印结果
print(final_results_by_station)

# 可视化：各类污染物有数据的时间段。
ggplot(final_results_by_station) +
  geom_segment(aes(x = Start_Date, xend = End_Date, y = var)) +
  facet_wrap(.~ station_key) +
  theme(axis.text.x = element_text(angle = 90))

# 选择118.167553_40.766671作为例子。
example_data <- final_combined_data %>%
  filter(
    station_key == "118.167553_40.766671",
    date >= as_date("2020-01-07"),
    date <= as_date("2021-04-15")
  )






# 目标气象站点。
meteo_station %>%
  filter(meteo_stat_id %in% final_combined_data$meteo_stat_id)



# 读取气象站附近的NDVI。
ndvi_stat <- rbind(
  read.csv("data_raw/ndvi_landsat_china_2021_250.csv"),
  read.csv("data_raw/ndvi_landsat_china_2021_250.csv")
) %>%
  tibble()

ndvi_stat_loc <- read.csv("data_raw/airquality_station_locations.csv") %>%
  tibble() %>%
  st_as_sf(coords = c("lon_site", "lat_site"), crs = 4326)

temp_stat <- data.frame(lon = 118.167553, lat = 40.766671) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

temp_stat <- meteo_station %>%
  filter(meteo_stat_id == "54430")

library(mapview)
mapview(ndvi_stat_loc, col.regions = "red") +
  mapview(meteo_station, col.regions = "blue")
