library(magick)
library(stringr)

#' 拼接指定文件夹中的所有 PNG 图片，并支持网格布局
#'
#' @param folder_path 包含 PNG 文件的文件夹路径。
#' @param output_path 拼接后大图的保存路径和文件名。
#' @param columns 指定网格的列数。
#' @param rows (可选) 指定网格的行数。
#' @param sub_image_size (可选) 指定网格中每个子图的统一尺寸。
#' @param border_spacing (可选) 指定子图之间的间距，例如 "+10+10"。
#' @param inner_border_color (可选) 子图边框的颜色，例如 "black"。
#' @param inner_border_width (可选) 子图边框的宽度，例如 "2x2"。
#'
#' @return 返回拼接后的 magick 图像对象，并保存到文件。
#'
combine_pngs_grid <- function(folder_path,
                              output_path,
                              columns,
                              rows = NULL,
                              sub_image_size = NULL,
                              border_spacing = "+10+10",
                              inner_border_color = "black",  # <-- 新参数：边框颜色
                              inner_border_width = "2x2") { # <-- 新参数：边框宽度

  # 1. 查找和读取图片 (省略... 保持不变)
  file_list <- list.files(path = folder_path, pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
  if (length(file_list) == 0) {
    stop("Error: No PNG files found in the specified folder.")
  }
  image_stack <- image_read(file_list)

  # 2. 预处理：调整子图尺寸
  if (!is.null(sub_image_size)) {
    image_stack <- image_resize(image_stack, sub_image_size)
  }

  # ----------------------------------------------------
  # 3. 关键步骤：在每个子图周围添加边框
  # ----------------------------------------------------
  if (!is.null(inner_border_width) && !is.null(inner_border_color)) {
    image_stack <- image_border(
      image_stack,
      color = inner_border_color,
      geometry = inner_border_width # 例如 "2x2" 表示上下左右各 2 像素
    )
  }

  # 4. 计算蒙太奇几何参数 (tile) (省略... 保持不变)
  num_images <- length(image_stack)
  if (is.null(rows)) {
    rows <- ceiling(num_images / columns)
  }
  montage_tile <- str_c(columns, "x", rows)

  # 5. 执行网格拼接 (省略... 保持不变)
  combined_image <- image_montage(
    image_stack,
    tile = montage_tile,
    geometry = border_spacing
  )

  # 6. 保存结果 (省略... 保持不变)
  image_write(combined_image, path = output_path, format = "png")

  return(combined_image)
}

FOLDER <- "data_proc/pcmci_2"
OUTPUT_FILE <- "new_montage_with_border.png"

final_image_with_border <- combine_pngs_grid(
  folder_path = FOLDER,
  output_path = OUTPUT_FILE,
  columns = 3,                          # 3 列
  sub_image_size = "1000x1000",           # 统一尺寸
  border_spacing = "+10+10",            # 子图间距 10 像素
  inner_border_color = "darkblue",      # <-- 新增：边框颜色
  inner_border_width = "4x4"            # <-- 新增：边框宽度 (4像素厚)
)

