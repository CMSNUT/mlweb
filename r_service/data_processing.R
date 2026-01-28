# data_processing.R - 数据处理模块

# 加载数据
load_data <- function(filepath, format = NULL) {
  loginfo(sprintf("加载数据: %s", filepath))
  
  # 自动检测格式
  if (is.null(format)) {
    ext <- tools::file_ext(filepath)
    format <- tolower(ext)
  }
  
  # 根据格式加载数据
  df <- switch(
    format,
    csv = read.csv(filepath, stringsAsFactors = FALSE),
    tsv = read.delim(filepath, stringsAsFactors = FALSE),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        install.packages("readxl", repos = "https://cloud.r-project.org")
      }
      library(readxl)
      read_excel(filepath)
    },
    xls = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        install.packages("readxl", repos = "https://cloud.r-project.org")
      }
      library(readxl)
      read_excel(filepath)
    },
    json = {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        install.packages("jsonlite", repos = "https://cloud.r-project.org")
      }
      library(jsonlite)
      fromJSON(filepath)
    },
    rds = readRDS(filepath),
    rdata = {
      env <- new.env()
      load(filepath, envir = env)
      # 返回第一个数据框对象
      obj_names <- ls(env)
      for (name in obj_names) {
        if (is.data.frame(env[[name]])) {
          return(env[[name]])
        }
      }
      stop("未找到数据框对象")
    },
    stop(sprintf("不支持的文件格式: %s", format))
  )
  
  # 确保返回数据框
  if (!is.data.frame(df)) {
    df <- as.data.frame(df)
  }
  
  loginfo(sprintf("数据加载完成: %d 行, %d 列", nrow(df), ncol(df)))
  
  return(df)
}

# 数据清洗
clean_data <- function(df, config = list()) {
  loginfo("开始数据清洗...")
  
  # 默认配置
  default_config <- list(
    remove_duplicates = TRUE,
    imputation_method = "median",  # "mean", "median", "mode", "knn", "mice"
    handle_outliers = TRUE,
    outlier_method = "iqr",  # "zscore", "isolation"
    min_missing_pct = 50,    # 缺失率超过此值的列将被删除
    convert_types = TRUE,
    date_format = NULL,
    text_cleaning = FALSE
  )
  
  # 合并配置
  config <- modifyList(default_config, config)
  
  original_shape <- dim(df)
  
  # 1. 删除完全空的行和列
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  df <- df[, colSums(is.na(df)) != nrow(df)]
  
  # 2. 删除缺失率过高的列
  missing_pct <- colMeans(is.na(df)) * 100
  high_missing_cols <- names(missing_pct[missing_pct > config$min_missing_pct])
  
  if (length(high_missing_cols) > 0) {
    loginfo(sprintf("删除缺失率超过 %d%% 的列: %s", 
                    config$min_missing_pct, paste(high_missing_cols, collapse = ", ")))
    df <- df[, !colnames(df) %in% high_missing_cols]
  }
  
  # 3. 删除重复行
  if (config$remove_duplicates) {
    duplicates <- sum(duplicated(df))
    if (duplicates > 0) {
      loginfo(sprintf("删除 %d 个重复行", duplicates))
      df <- df[!duplicated(df), ]
    }
  }
  
  # 4. 处理缺失值
  df <- impute_missing_values(df, config$imputation_method)
  
  # 5. 处理异常值
  if (config$handle_outliers) {
    df <- handle_outliers(df, config$outlier_method)
  }
  
  # 6. 转换数据类型
  if (config$convert_types) {
    df <- convert_data_types(df, config$date_format)
  }
  
  # 7. 文本清洗（如果启用）
  if (config$text_cleaning) {
    df <- clean_text_data(df)
  }
  
  final_shape <- dim(df)
  removed_rows <- original_shape[1] - final_shape[1]
  removed_cols <- original_shape[2] - final_shape[2]
  
  loginfo(sprintf("数据清洗完成: 移除了 %d 行, %d 列", removed_rows, removed_cols))
  loginfo(sprintf("最终维度: %d 行, %d 列", final_shape[1], final_shape[2]))
  
  return(df)
}

# 缺失值插补
impute_missing_values <- function(df, method = "median") {
  loginfo(sprintf("使用 %s 方法插补缺失值", method))
  
  if (method == "remove") {
    # 删除有缺失值的行
    complete_cases <- complete.cases(df)
    if (sum(!complete_cases) > 0) {
      loginfo(sprintf("删除 %d 个有缺失值的行", sum(!complete_cases)))
      df <- df[complete_cases, ]
    }
    return(df)
  }
  
  # 对每列进行插补
  for (col in colnames(df)) {
    missing_count <- sum(is.na(df[[col]]))
    
    if (missing_count > 0) {
      if (is.numeric(df[[col]])) {
        # 数值列
        if (method == "mean") {
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        } else if (method == "median") {
          df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
        } else if (method == "mode") {
          # 计算众数
          freq_table <- table(df[[col]])
          mode_value <- as.numeric(names(freq_table)[which.max(freq_table)])
          df[[col]][is.na(df[[col]])] <- mode_value
        } else if (method == "zero") {
          df[[col]][is.na(df[[col]])] <- 0
        }
      } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
        # 分类列
        if (method == "mode") {
          freq_table <- table(df[[col]])
          mode_value <- names(freq_table)[which.max(freq_table)]
          df[[col]][is.na(df[[col]])] <- mode_value
        } else {
          # 默认为"missing"类别
          df[[col]][is.na(df[[col]])] <- "missing"
        }
      }
      
      logdebug(sprintf("列 %s: 插补了 %d 个缺失值", col, missing_count))
    }
  }
  
  return(df)
}

# 异常值处理
handle_outliers <- function(df, method = "iqr") {
  loginfo(sprintf("使用 %s 方法处理异常值", method))
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  if (length(numeric_cols) == 0) {
    loginfo("没有数值列需要处理异常值")
    return(df)
  }
  
  outlier_counts <- list()
  
  for (col in numeric_cols) {
    data <- df[[col]]
    
    if (method == "iqr") {
      # IQR方法
      Q1 <- quantile(data, 0.25, na.rm = TRUE)
      Q3 <- quantile(data, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      outliers <- data < lower_bound | data > upper_bound
      
    } else if (method == "zscore") {
      # Z-score方法
      z_scores <- scale(data)
      outliers <- abs(z_scores) > 3
      
    } else if (method == "percentile") {
      # 百分位数方法
      lower_bound <- quantile(data, 0.01, na.rm = TRUE)
      upper_bound <- quantile(data, 0.99, na.rm = TRUE)
      outliers <- data < lower_bound | data > upper_bound
      
    } else {
      logwarn(sprintf("不支持的异常值检测方法: %s", method))
      next
    }
    
    outlier_count <- sum(outliers, na.rm = TRUE)
    
    if (outlier_count > 0) {
      # 将异常值替换为边界值
      if (method == "iqr" || method == "percentile") {
        df[[col]][data < lower_bound] <- lower_bound
        df[[col]][data > upper_bound] <- upper_bound
      } else if (method == "zscore") {
        # 对Z-score方法，使用中位数替代
        median_val <- median(data, na.rm = TRUE)
        df[[col]][outliers] <- median_val
      }
      
      outlier_counts[[col]] <- outlier_count
    }
  }
  
  if (length(outlier_counts) > 0) {
    loginfo("异常值处理完成:")
    for (col in names(outlier_counts)) {
      loginfo(sprintf("  列 %s: 处理了 %d 个异常值", col, outlier_counts[[col]]))
    }
  } else {
    loginfo("未发现异常值")
  }
  
  return(df)
}

# 数据类型转换
convert_data_types <- function(df, date_format = NULL) {
  loginfo("转换数据类型...")
  
  for (col in colnames(df)) {
    current_type <- class(df[[col]])[1]
    
    # 尝试转换为数值
    if (!is.numeric(df[[col]])) {
      numeric_test <- suppressWarnings(as.numeric(df[[col]]))
      if (sum(is.na(numeric_test)) == sum(is.na(df[[col]]))) {
        # 可以安全转换为数值
        df[[col]] <- numeric_test
        logdebug(sprintf("列 %s: 转换为数值类型", col))
        next
      }
    }
    
    # 尝试转换为日期
    if (!is.null(date_format) && !inherits(df[[col]], "Date")) {
      date_test <- try(as.Date(df[[col]], format = date_format), silent = TRUE)
      if (!inherits(date_test, "try-error") && 
          sum(is.na(date_test)) < length(date_test) * 0.5) {
        # 超过一半的值可以转换为日期
        df[[col]] <- date_test
        logdebug(sprintf("列 %s: 转换为日期类型", col))
        next
      }
    }
    
    # 转换为因子（如果唯一值较少）
    if (is.character(df[[col]])) {
      unique_count <- length(unique(df[[col]]))
      if (unique_count <= 20 && unique_count < nrow(df) * 0.5) {
        df[[col]] <- as.factor(df[[col]])
        logdebug(sprintf("列 %s: 转换为因子类型 (%d 个水平)", col, unique_count))
      }
    }
  }
  
  return(df)
}

# 文本数据清洗
clean_text_data <- function(df) {
  loginfo("清洗文本数据...")
  
  text_cols <- names(df)[sapply(df, is.character)]
  
  for (col in text_cols) {
    # 去除前后空格
    df[[col]] <- trimws(df[[col]])
    
    # 转换为小写
    df[[col]] <- tolower(df[[col]])
    
    # 替换多个空格为单个空格
    df[[col]] <- gsub("\\s+", " ", df[[col]])
    
    # 去除特殊字符（保留字母、数字、空格和基本标点）
    df[[col]] <- gsub("[^[:alnum:][:space:].,!?-]", "", df[[col]])
    
    # 处理空字符串
    df[[col]][df[[col]] == ""] <- NA
  }
  
  return(df)
}

# 数据分割
split_data <- function(df, target_col, test_size = 0.2, validation_size = 0, 
                       stratify = TRUE, seed = 42) {
  loginfo(sprintf("数据分割: 测试集 %.0f%%, 验证集 %.0f%%", 
                  test_size * 100, validation_size * 100))
  
  set.seed(seed)
  
  # 分离特征和目标
  X <- df[, !colnames(df) %in% target_col, drop = FALSE]
  y <- df[[target_col]]
  
  # 计算验证集的实际比例
  if (validation_size > 0) {
    val_ratio <- validation_size / (1 - test_size)
  } else {
    val_ratio <- 0
  }
  
  if (stratify && (is.factor(y) || is.character(y))) {
    # 分层抽样
    library(caret)
    
    # 首次分割：训练+验证 vs 测试
    train_val_idx <- createDataPartition(
      y, 
      p = 1 - test_size, 
      list = FALSE
    )
    
    X_train_val <- X[train_val_idx, , drop = FALSE]
    y_train_val <- y[train_val_idx]
    X_test <- X[-train_val_idx, , drop = FALSE]
    y_test <- y[-train_val_idx]
    
    if (val_ratio > 0) {
      # 第二次分割：训练 vs 验证
      train_idx <- createDataPartition(
        y_train_val,
        p = 1 - val_ratio,
        list = FALSE
      )
      
      X_train <- X_train_val[train_idx, , drop = FALSE]
      y_train <- y_train_val[train_idx]
      X_val <- X_train_val[-train_idx, , drop = FALSE]
      y_val <- y_train_val[-train_idx]
      
      result <- list(
        X_train = X_train,
        y_train = y_train,
        X_val = X_val,
        y_val = y_val,
        X_test = X_test,
        y_test = y_test
      )
    } else {
      result <- list(
        X_train = X_train_val,
        y_train = y_train_val,
        X_test = X_test,
        y_test = y_test
      )
    }
    
  } else {
    # 随机抽样
    n <- nrow(df)
    test_idx <- sample(1:n, size = round(n * test_size))
    
    X_test <- X[test_idx, , drop = FALSE]
    y_test <- y[test_idx]
    X_train_val <- X[-test_idx, , drop = FALSE]
    y_train_val <- y[-test_idx]
    
    if (val_ratio > 0) {
      n_train_val <- nrow(X_train_val)
      val_idx <- sample(1:n_train_val, size = round(n_train_val * val_ratio))
      
      X_val <- X_train_val[val_idx, , drop = FALSE]
      y_val <- y_train_val[val_idx]
      X_train <- X_train_val[-val_idx, , drop = FALSE]
      y_train <- y_train_val[-val_idx]
      
      result <- list(
        X_train = X_train,
        y_train = y_train,
        X_val = X_val,
        y_val = y_val,
        X_test = X_test,
        y_test = y_test
      )
    } else {
      result <- list(
        X_train = X_train_val,
        y_train = y_train_val,
        X_test = X_test,
        y_test = y_test
      )
    }
  }
  
  # 记录分割结果
  loginfo(sprintf("训练集: %d 样本", length(result$y_train)))
  if (!is.null(result$y_val)) {
    loginfo(sprintf("验证集: %d 样本", length(result$y_val)))
  }
  loginfo(sprintf("测试集: %d 样本", length(result$y_test)))
  
  # 检查类别分布（分类问题）
  if (is.factor(y) || is.character(y)) {
    loginfo("类别分布:")
    
    for (set_name in c("train", "val", "test")) {
      y_name <- paste0("y_", set_name)
      if (exists(y_name, where = result)) {
        y_set <- result[[y_name]]
        dist_table <- table(y_set)
        dist_pct <- prop.table(dist_table) * 100
        
        loginfo(sprintf("  %s集:", set_name))
        for (class_name in names(dist_table)) {
          loginfo(sprintf("    %s: %d (%.1f%%)", 
                          class_name, dist_table[class_name], dist_pct[class_name]))
        }
      }
    }
  }
  
  return(result)
}

# 创建预处理配方
create_preprocessing_recipe <- function(train_data, target_col, config = list()) {
  loginfo("创建预处理配方...")
  
  # 默认配置
  default_config <- list(
    scale_numeric = TRUE,
    center_numeric = TRUE,
    normalize_numeric = FALSE,
    encode_categorical = TRUE,
    one_hot_encoding = FALSE,
    pca = FALSE,
    pca_threshold = 0.95,
    remove_zero_variance = TRUE,
    remove_correlated = FALSE,
    correlation_threshold = 0.9,
    create_interactions = FALSE,
    polynomial_degree = 2,
    date_features = TRUE
  )
  
  # 合并配置
  config <- modifyList(default_config, config)
  
  # 分离特征和目标
  formula <- as.formula(paste(target_col, "~ ."))
  
  # 创建配方
  library(recipes)
  recipe <- recipe(formula, data = train_data)
  
  # 处理日期特征
  if (config$date_features) {
    date_cols <- sapply(train_data, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
    date_col_names <- names(date_cols)[date_cols]
    
    for (date_col in date_col_names) {
      recipe <- recipe %>%
        step_date(all_of(date_col), features = c("year", "month", "day", "doy", "week"))
    }
  }
  
  # 编码分类变量
  if (config$encode_categorical) {
    if (config$one_hot_encoding) {
      recipe <- recipe %>% step_dummy(all_nominal(), -all_outcomes())
    } else {
      recipe <- recipe %>% step_integer(all_nominal(), -all_outcomes())
    }
  }
  
  # 移除零方差特征
  if (config$remove_zero_variance) {
    recipe <- recipe %>% step_zv(all_predictors())
  }
  
  # 移除高度相关特征
  if (config$remove_correlated) {
    recipe <- recipe %>% 
      step_corr(all_numeric(), -all_outcomes(), 
                threshold = config$correlation_threshold)
  }
  
  # 标准化数值特征
  if (config$scale_numeric) {
    recipe <- recipe %>% step_scale(all_numeric(), -all_outcomes())
  }
  
  # 中心化数值特征
  if (config$center_numeric) {
    recipe <- recipe %>% step_center(all_numeric(), -all_outcomes())
  }
  
  # 正则化数值特征
  if (config$normalize_numeric) {
    recipe <- recipe %>% step_normalize(all_numeric(), -all_outcomes())
  }
  
  # 创建交互特征
  if (config$create_interactions) {
    recipe <- recipe %>% step_interact(~ all_numeric():all_numeric())
  }
  
  # 创建多项式特征
  if (config$polynomial_degree > 1) {
    recipe <- recipe %>% 
      step_poly(all_numeric(), -all_outcomes(), 
                degree = config$polynomial_degree)
  }
  
  # PCA
  if (config$pca) {
    recipe <- recipe %>% 
      step_pca(all_numeric(), -all_outcomes(),
               threshold = config$pca_threshold)
  }
  
  # 训练配方
  trained_recipe <- prep(recipe, training = train_data)
  
  loginfo("预处理配方创建完成")
  
  # 显示配方步骤
  logdebug("配方步骤:")
  for (i in seq_along(trained_recipe$steps)) {
    step <- trained_recipe$steps[[i]]
    logdebug(sprintf("  步骤 %d: %s", i, class(step)[1]))
  }
  
  return(trained_recipe)
}

# 应用预处理
apply_preprocessing <- function(recipe, data) {
  loginfo("应用预处理...")
  
  processed_data <- bake(recipe, new_data = data)
  
  loginfo(sprintf("预处理完成: %d 行, %d 列", 
                  nrow(processed_data), ncol(processed_data)))
  
  return(processed_data)
}

# 特征工程
create_features <- function(df, config = list()) {
  loginfo("创建特征工程...")
  
  # 默认配置
  default_config <- list(
    create_interactions = TRUE,
    create_polynomials = FALSE,
    polynomial_degree = 2,
    create_statistical = TRUE,
    create_date_features = TRUE,
    create_text_features = FALSE,
    remove_original = FALSE
  )
  
  # 合并配置
  config <- modifyList(default_config, config)
  
  original_cols <- colnames(df)
  new_features <- list()
  
  # 数值列
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  # 创建交互特征
  if (config$create_interactions && length(numeric_cols) >= 2) {
    loginfo("创建交互特征...")
    
    # 创建两两交互
    for (i in 1:(length(numeric_cols)-1)) {
      for (j in (i+1):length(numeric_cols)) {
        col1 <- numeric_cols[i]
        col2 <- numeric_cols[j]
        interaction_name <- paste(col1, col2, sep = "_x_")
        
        df[[interaction_name]] <- df[[col1]] * df[[col2]]
        new_features[[interaction_name]] <- "interaction"
      }
    }
  }
  
  # 创建多项式特征
  if (config$create_polynomials && length(numeric_cols) > 0) {
    loginfo("创建多项式特征...")
    
    for (col in numeric_cols) {
      for (degree in 2:config$polynomial_degree) {
        poly_name <- paste(col, degree, sep = "^")
        df[[poly_name]] <- df[[col]] ^ degree
        new_features[[poly_name]] <- "polynomial"
      }
    }
  }
  
  # 创建统计特征
  if (config$create_statistical && length(numeric_cols) >= 2) {
    loginfo("创建统计特征...")
    
    # 行级别的统计
    if (length(numeric_cols) > 1) {
      df$row_mean <- rowMeans(df[, numeric_cols], na.rm = TRUE)
      df$row_sd <- apply(df[, numeric_cols], 1, sd, na.rm = TRUE)
      df$row_min <- apply(df[, numeric_cols], 1, min, na.rm = TRUE)
      df$row_max <- apply(df[, numeric_cols], 1, max, na.rm = TRUE)
      
      new_features$row_mean <- "statistical"
      new_features$row_sd <- "statistical"
      new_features$row_min <- "statistical"
      new_features$row_max <- "statistical"
    }
  }
  
  # 创建日期特征
  if (config$create_date_features) {
    date_cols <- names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
    
    for (date_col in date_cols) {
      loginfo(sprintf("从日期列 %s 创建特征", date_col))
      
      df[[paste(date_col, "year", sep = "_")]] <- as.numeric(format(df[[date_col]], "%Y"))
      df[[paste(date_col, "month", sep = "_")]] <- as.numeric(format(df[[date_col]], "%m"))
      df[[paste(date_col, "day", sep = "_")]] <- as.numeric(format(df[[date_col]], "%d"))
      df[[paste(date_col, "weekday", sep = "_")]] <- as.numeric(format(df[[date_col]], "%u"))
      df[[paste(date_col, "quarter", sep = "_")]] <- as.numeric(format(df[[date_col]], "%q"))
      
      new_features[[paste(date_col, "year", sep = "_")]] <- "date"
      new_features[[paste(date_col, "month", sep = "_")]] <- "date"
      new_features[[paste(date_col, "day", sep = "_")]] <- "date"
      new_features[[paste(date_col, "weekday", sep = "_")]] <- "date"
      new_features[[paste(date_col, "quarter", sep = "_")]] <- "date"
    }
  }
  
  # 创建文本特征（如果启用）
  if (config$create_text_features) {
    text_cols <- names(df)[sapply(df, is.character)]
    
    for (text_col in text_cols) {
      loginfo(sprintf("从文本列 %s 创建特征", text_col))
      
      # 文本长度
      df[[paste(text_col, "length", sep = "_")]] <- nchar(df[[text_col]])
      
      # 单词数量
      df[[paste(text_col, "word_count", sep = "_")]] <- sapply(
        strsplit(df[[text_col]], "\\s+"), 
        function(x) length(x)
      )
      
      new_features[[paste(text_col, "length", sep = "_")]] <- "text"
      new_features[[paste(text_col, "word_count", sep = "_")]] <- "text"
    }
  }
  
  # 移除原始特征（如果配置）
  if (config$remove_original) {
    df <- df[, !colnames(df) %in% original_cols]
  }
  
  loginfo(sprintf("特征工程完成: 创建了 %d 个新特征", length(new_features)))
  loginfo("新特征类型:")
  for (feat_type in unique(new_features)) {
    feat_names <- names(new_features)[new_features == feat_type]
    loginfo(sprintf("  %s: %s", feat_type, paste(feat_names, collapse = ", ")))
  }
  
  return(df)
}

# 特征选择
select_features <- function(X, y, method = "correlation", top_k = 20, 
                           classification = TRUE) {
  loginfo(sprintf("特征选择: 方法=%s, top_k=%d", method, top_k))
  
  if (method == "correlation") {
    # 基于相关性选择
    if (classification && (is.factor(y) || is.character(y))) {
      # 分类问题：使用方差分析
      scores <- apply(X, 2, function(x) {
        if (is.numeric(x)) {
          anova_result <- summary(aov(x ~ as.factor(y)))
          return(anova_result[[1]][1, "F value"])
        } else {
          return(0)
        }
      })
    } else {
      # 回归问题：使用相关性
      if (is.numeric(y)) {
        scores <- apply(X, 2, function(x) {
          if (is.numeric(x)) {
            return(abs(cor(x, y, use = "complete.obs")))
          } else {
            return(0)
          }
        })
      } else {
        stop("对于分类问题，y必须是因子或字符向量")
      }
    }
    
  } else if (method == "variance") {
    # 基于方差选择
    scores <- apply(X, 2, var, na.rm = TRUE)
    
  } else if (method == "random_forest") {
    # 基于随机森林的重要性
    if (!requireNamespace("randomForest", quietly = TRUE)) {
      install.packages("randomForest", repos = "https://cloud.r-project.org")
    }
    library(randomForest)
    
    if (classification && (is.factor(y) || is.character(y))) {
      rf_model <- randomForest(X, as.factor(y), ntree = 100, importance = TRUE)
      scores <- importance(rf_model)[, "MeanDecreaseAccuracy"]
    } else if (is.numeric(y)) {
      rf_model <- randomForest(X, y, ntree = 100, importance = TRUE)
      scores <- importance(rf_model)[, "%IncMSE"]
    } else {
      stop("不支持的y类型")
    }
    
  } else if (method == "xgboost") {
    # 基于XGBoost的重要性
    if (!requireNamespace("xgboost", quietly = TRUE)) {
      install.packages("xgboost", repos = "https://cloud.r-project.org")
    }
    library(xgboost)
    
    # 转换数据
    if (is.factor(y) || is.character(y)) {
      y_numeric <- as.numeric(as.factor(y)) - 1
      objective <- "multi:softprob"
      num_class <- length(unique(y))
    } else {
      y_numeric <- y
      objective <- "reg:squarederror"
      num_class <- NULL
    }
    
    dtrain <- xgb.DMatrix(data = as.matrix(X), label = y_numeric)
    
    params <- list(
      objective = objective,
      max_depth = 3,
      eta = 0.1,
      nthread = 2,
      num_class = num_class
    )
    
    xgb_model <- xgb.train(params, dtrain, nrounds = 50, verbose = 0)
    importance_matrix <- xgb.importance(colnames(X), model = xgb_model)
    scores <- importance_matrix$Gain
    names(scores) <- importance_matrix$Feature
    
  } else {
    stop(sprintf("不支持的特征选择方法: %s", method))
  }
  
  # 选择top_k特征
  if (top_k > length(scores)) {
    top_k <- length(scores)
  }
  
  selected_features <- names(sort(scores, decreasing = TRUE))[1:top_k]
  
  loginfo(sprintf("选择了 %d 个特征:", length(selected_features)))
  for (i in 1:min(10, length(selected_features))) {
    feat <- selected_features[i]
    score <- scores[feat]
    loginfo(sprintf("  %d. %s: %.4f", i, feat, score))
  }
  
  if (length(selected_features) > 10) {
    loginfo(sprintf("  ... 还有 %d 个特征", length(selected_features) - 10))
  }
  
  return(selected_features)
}