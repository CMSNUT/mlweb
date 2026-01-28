# utils.R - 工具函数

# 初始化日志系统
init_logging <- function(log_file = "logs/model.log") {
  # 确保日志目录存在
  if (!dir.exists(dirname(log_file))) {
    dir.create(dirname(log_file), recursive = TRUE)
  }
  
  # 配置基本日志
  basicConfig(level = "DEBUG")
  
  # 添加文件处理器
  addHandler(writeToFile, file = log_file, level = "DEBUG")
  
  # 添加控制台处理器
  addHandler(writeToConsole, level = "INFO")
  
  loginfo("日志系统初始化完成")
  loginfo(sprintf("日志文件: %s", log_file))
}

# 数据质量检查
check_data_quality <- function(df) {
  loginfo("开始数据质量检查...")
  
  # 基本统计
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  
  # 缺失值统计
  missing_values <- sum(is.na(df))
  missing_percentage <- mean(is.na(df)) * 100
  
  # 重复行统计
  duplicate_rows <- sum(duplicated(df))
  
  # 数据类型统计
  numeric_cols <- sum(sapply(df, is.numeric))
  factor_cols <- sum(sapply(df, is.factor))
  character_cols <- sum(sapply(df, is.character))
  logical_cols <- sum(sapply(df, is.logical))
  date_cols <- sum(sapply(df, function(x) inherits(x, "Date")))
  
  # 列级别的缺失值统计
  missing_by_col <- sapply(df, function(x) sum(is.na(x)))
  missing_pct_by_col <- sapply(df, function(x) mean(is.na(x)) * 100)
  
  # 唯一值统计
  unique_by_col <- sapply(df, function(x) length(unique(x)))
  
  # 数值列的统计摘要
  numeric_stats <- list()
  numeric_columns <- names(df)[sapply(df, is.numeric)]
  
  for (col in numeric_columns) {
    col_data <- df[[col]]
    numeric_stats[[col]] <- list(
      min = min(col_data, na.rm = TRUE),
      max = max(col_data, na.rm = TRUE),
      mean = mean(col_data, na.rm = TRUE),
      median = median(col_data, na.rm = TRUE),
      sd = sd(col_data, na.rm = TRUE),
      missing = sum(is.na(col_data))
    )
  }
  
  # 分类列的统计摘要
  categorical_stats <- list()
  categorical_columns <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  
  for (col in categorical_columns) {
    col_data <- df[[col]]
    value_counts <- table(col_data, useNA = "ifany")
    categorical_stats[[col]] <- list(
      n_unique = length(unique(col_data)),
      top_values = head(sort(value_counts, decreasing = TRUE), 5),
      missing = sum(is.na(col_data))
    )
  }
  
  # 检测潜在问题
  issues <- list()
  
  # 高缺失率列
  high_missing_cols <- names(missing_pct_by_col[missing_pct_by_col > 50])
  if (length(high_missing_cols) > 0) {
    issues$high_missing_columns <- high_missing_cols
  }
  
  # 常数列（只有一个唯一值）
  constant_cols <- names(unique_by_col[unique_by_col == 1])
  if (length(constant_cols) > 0) {
    issues$constant_columns <- constant_cols
  }
  
  # 高度偏斜的数值列
  skewed_cols <- character()
  for (col in numeric_columns) {
    col_data <- df[[col]]
    if (length(unique(col_data)) > 10) {  # 忽略常数列
      skewness <- abs(moments::skewness(col_data, na.rm = TRUE))
      if (skewness > 3) {  # 高度偏斜
        skewed_cols <- c(skewed_cols, col)
      }
    }
  }
  if (length(skewed_cols) > 0) {
    issues$skewed_columns <- skewed_cols
  }
  
  # 构建结果
  result <- list(
    basic = list(
      n_rows = n_rows,
      n_cols = n_cols,
      missing_values = missing_values,
      missing_percentage = missing_percentage,
      duplicate_rows = duplicate_rows
    ),
    data_types = list(
      numeric = numeric_cols,
      factor = factor_cols,
      character = character_cols,
      logical = logical_cols,
      date = date_cols
    ),
    column_details = list(
      missing_by_col = missing_by_col,
      missing_pct_by_col = missing_pct_by_col,
      unique_by_col = unique_by_col
    ),
    numeric_stats = numeric_stats,
    categorical_stats = categorical_stats,
    issues = issues,
    summary = sprintf(
      "数据维度: %d 行 × %d 列 | 缺失值: %d (%.2f%%) | 重复行: %d",
      n_rows, n_cols, missing_values, missing_percentage, duplicate_rows
    )
  )
  
  loginfo(result$summary)
  
  if (length(issues) > 0) {
    logwarn("发现数据质量问题:")
    for (issue_name in names(issues)) {
      logwarn(sprintf("  %s: %s", issue_name, paste(issues[[issue_name]], collapse = ", ")))
    }
  }
  
  return(result)
}

# 保存检查点
save_checkpoint <- function(obj, name, dir = "checkpoints") {
  # 确保目录存在
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # 生成文件名
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- sprintf("%s_%s.rds", name, timestamp)
  filepath <- file.path(dir, filename)
  
  # 保存对象
  saveRDS(obj, filepath)
  
  loginfo(sprintf("检查点保存到: %s", filepath))
  
  return(filepath)
}

# 加载检查点
load_checkpoint <- function(pattern, dir = "checkpoints", latest = TRUE) {
  # 检查目录是否存在
  if (!dir.exists(dir)) {
    logwarn(sprintf("检查点目录不存在: %s", dir))
    return(NULL)
  }
  
  # 查找文件
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    logwarn(sprintf("未找到匹配的检查点文件: %s", pattern))
    return(NULL)
  }
  
  # 选择文件
  if (latest) {
    # 选择最新的文件
    file_info <- file.info(files)
    latest_file <- files[which.max(file_info$mtime)]
    selected_file <- latest_file
  } else {
    # 选择最早的文件
    file_info <- file.info(files)
    earliest_file <- files[which.min(file_info$mtime)]
    selected_file <- earliest_file
  }
  
  # 加载对象
  obj <- readRDS(selected_file)
  
  loginfo(sprintf("从检查点加载: %s", selected_file))
  
  return(obj)
}

# 计算执行时间
time_execution <- function(expr, description = "操作") {
  start_time <- Sys.time()
  
  # 执行表达式
  result <- tryCatch({
    eval(expr)
  }, error = function(e) {
    logerror(sprintf("%s执行失败: %s", description, e$message))
    stop(e)
  })
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  loginfo(sprintf("%s完成，耗时: %.3f秒", description, elapsed))
  
  return(list(
    result = result,
    time = elapsed,
    start_time = start_time,
    end_time = end_time
  ))
}

# 验证参数
validate_parameters <- function(params, required = NULL, optional = NULL) {
  # 检查必选参数
  if (!is.null(required)) {
    missing_params <- setdiff(required, names(params))
    if (length(missing_params) > 0) {
      stop(sprintf("缺少必选参数: %s", paste(missing_params, collapse = ", ")))
    }
  }
  
  # 检查未知参数
  if (!is.null(optional)) {
    all_valid <- c(required, optional)
    unknown_params <- setdiff(names(params), all_valid)
    if (length(unknown_params) > 0) {
      logwarn(sprintf("发现未知参数: %s", paste(unknown_params, collapse = ", ")))
    }
  }
  
  # 参数类型检查
  type_checks <- list(
    numeric = function(x) is.numeric(x) && length(x) == 1,
    integer = function(x) is.integer(x) && length(x) == 1,
    character = function(x) is.character(x) && length(x) == 1,
    logical = function(x) is.logical(x) && length(x) == 1,
    vector = function(x) is.vector(x) && length(x) > 0,
    list = is.list,
    data.frame = is.data.frame,
    matrix = is.matrix
  )
  
  # 这里可以添加更详细的类型检查
  
  return(TRUE)
}

# 生成唯一ID
generate_id <- function(prefix = "id", length = 8) {
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  random_str <- paste(sample(c(letters, 0:9), length, replace = TRUE), collapse = "")
  return(paste(prefix, timestamp, random_str, sep = "_"))
}

# 安全保存文件
safe_save <- function(obj, filepath, backup = TRUE) {
  # 确保目录存在
  dir_path <- dirname(filepath)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # 备份现有文件
  if (backup && file.exists(filepath)) {
    backup_file <- paste0(filepath, ".backup_", format(Sys.time(), "%Y%m%d%H%M%S"))
    file.copy(filepath, backup_file)
    loginfo(sprintf("已备份原文件到: %s", backup_file))
  }
  
  # 保存文件
  saveRDS(obj, filepath)
  
  loginfo(sprintf("文件保存到: %s", filepath))
  
  return(filepath)
}

# 安全加载文件
safe_load <- function(filepath, default = NULL) {
  if (!file.exists(filepath)) {
    logwarn(sprintf("文件不存在: %s", filepath))
    return(default)
  }
  
  tryCatch({
    obj <- readRDS(filepath)
    loginfo(sprintf("从文件加载: %s", filepath))
    return(obj)
  }, error = function(e) {
    logerror(sprintf("加载文件失败: %s, 错误: %s", filepath, e$message))
    return(default)
  })
}

# 内存使用报告
memory_report <- function() {
  mem <- memory.size()
  mem_limit <- memory.limit()
  
  report <- list(
    used_mb = round(mem, 2),
    limit_mb = mem_limit,
    percentage = round(mem / mem_limit * 100, 2),
    status = if (mem / mem_limit > 0.9) "warning" else "ok"
  )
  
  if (report$status == "warning") {
    logwarn(sprintf("内存使用率较高: %.1f%% (%.1f/%.1f MB)", 
                    report$percentage, report$used_mb, report$limit_mb))
  }
  
  return(report)
}

# 清理临时文件
clean_temp_files <- function(dir = "temp", pattern = NULL, older_than_hours = 24) {
  if (!dir.exists(dir)) {
    return(0)
  }
  
  files <- list.files(dir, full.names = TRUE)
  
  if (!is.null(pattern)) {
    files <- files[grepl(pattern, basename(files))]
  }
  
  # 过滤超过指定时间的文件
  file_info <- file.info(files)
  cutoff_time <- Sys.time() - older_than_hours * 3600
  old_files <- files[file_info$mtime < cutoff_time]
  
  if (length(old_files) > 0) {
    unlink(old_files)
    loginfo(sprintf("清理了 %d 个临时文件", length(old_files)))
    return(length(old_files))
  }
  
  return(0)
}

# 格式化数字
format_number <- function(x, digits = 3, scientific = FALSE) {
  if (is.numeric(x)) {
    if (scientific) {
      return(formatC(x, digits = digits, format = "e"))
    } else {
      return(round(x, digits))
    }
  }
  return(x)
}

# 创建进度条
create_progress_bar <- function(total, title = "进度") {
  pb <- list(
    total = total,
    current = 0,
    start_time = Sys.time(),
    title = title
  )
  
  class(pb) <- "progress_bar"
  
  # 进度更新方法
  pb$update <- function(increment = 1) {
    pb$current <<- pb$current + increment
    percentage <- round(pb$current / pb$total * 100, 1)
    
    elapsed <- as.numeric(difftime(Sys.time(), pb$start_time, units = "secs"))
    if (pb$current > 0) {
      estimated_total <- elapsed * pb$total / pb$current
      remaining <- estimated_total - elapsed
      
      cat(sprintf("\r%s: %.1f%% | 耗时: %.1fs | 剩余: %.1fs", 
                  pb$title, percentage, elapsed, remaining))
    }
    
    if (pb$current >= pb$total) {
      cat("\n")
      loginfo(sprintf("%s完成，总耗时: %.1f秒", pb$title, elapsed))
    }
  }
  
  # 完成方法
  pb$complete <- function() {
    pb$current <<- pb$total
    pb$update(0)
  }
  
  return(pb)
}

# 异常处理包装器
safe_execute <- function(expr, error_value = NULL, warning_value = NULL, 
                         finally_expr = NULL) {
  result <- tryCatch(
    {
      # 尝试执行表达式
      eval(expr)
    },
    error = function(e) {
      logerror(sprintf("执行失败: %s", e$message))
      if (!is.null(error_value)) {
        return(error_value)
      } else {
        stop(e)
      }
    },
    warning = function(w) {
      logwarn(sprintf("执行警告: %s", w$message))
      if (!is.null(warning_value)) {
        return(warning_value)
      }
      # 继续执行
      invokeRestart("muffleWarning")
    }
  )
  
  # 执行finally表达式
  if (!is.null(finally_expr)) {
    tryCatch(
      {
        eval(finally_expr)
      },
      error = function(e) {
        logerror(sprintf("finally表达式执行失败: %s", e$message))
      }
    )
  }
  
  return(result)
}