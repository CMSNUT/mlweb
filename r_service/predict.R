# predict.R - 模型预测模块

# 单样本预测
predict_single <- function(model, input_data, preprocessor = NULL, return_prob = TRUE) {
  start_time <- Sys.time()
  
  loginfo("开始单样本预测...")
  
  # 转换输入数据
  if (is.list(input_data) && !is.data.frame(input_data)) {
    input_df <- as.data.frame(input_data)
  } else {
    input_df <- as.data.frame(input_data)
  }
  
  # 应用预处理
  if (!is.null(preprocessor)) {
    loginfo("应用预处理...")
    tryCatch({
      input_df <- bake(preprocessor, new_data = input_df)
    }, error = function(e) {
      logwarn(sprintf("预处理失败: %s", e$message))
      # 继续使用原始数据
    })
  }
  
  # 确保列名匹配
  if (inherits(model, "train")) {
    # caret模型
    model_features <- names(model$trainingData)[-ncol(model$trainingData)]
    input_features <- colnames(input_df)
    
    # 检查缺失的特征
    missing_features <- setdiff(model_features, input_features)
    if (length(missing_features) > 0) {
      logwarn(sprintf("输入数据缺少特征: %s", paste(missing_features, collapse = ", ")))
      
      # 添加缺失的特征（填充为0或NA）
      for (feat in missing_features) {
        input_df[[feat]] <- 0
      }
    }
    
    # 移除多余的特征
    extra_features <- setdiff(input_features, model_features)
    if (length(extra_features) > 0) {
      logwarn(sprintf("输入数据有多余特征: %s", paste(extra_features, collapse = ", ")))
      input_df <- input_df[, model_features, drop = FALSE]
    }
    
    # 重新排列列顺序以匹配模型
    input_df <- input_df[, model_features, drop = FALSE]
  }
  
  # 进行预测
  loginfo("进行模型预测...")
  
  tryCatch({
    if (inherits(model, "train")) {
      # caret模型
      if (model$modelType == "Classification" && return_prob) {
        # 分类问题，返回概率
        prob_predictions <- predict(model, newdata = input_df, type = "prob")
        class_predictions <- predict(model, newdata = input_df)
        
        # 获取预测类别的概率
        if (ncol(prob_predictions) == 2) {
          # 二分类
          predicted_class <- as.character(class_predictions)
          probability <- prob_predictions[, predicted_class]
        } else {
          # 多分类
          predicted_class <- as.character(class_predictions)
          probability <- apply(prob_predictions, 1, max)
        }
        
        prediction_result <- predicted_class
        probability_result <- probability
        
      } else if (model$modelType == "Classification") {
        # 分类问题，只返回类别
        prediction_result <- as.character(predict(model, newdata = input_df))
        probability_result <- NULL
        
      } else {
        # 回归问题
        prediction_result <- as.numeric(predict(model, newdata = input_df))
        probability_result <- NULL
      }
      
    } else if (inherits(model, "randomForest")) {
      # randomForest模型
      if (model$type == "classification" && return_prob) {
        prob_predictions <- predict(model, newdata = input_df, type = "prob")
        class_predictions <- predict(model, newdata = input_df)
        
        if (ncol(prob_predictions) == 2) {
          # 二分类
          predicted_class <- as.character(class_predictions)
          probability <- prob_predictions[, predicted_class]
        } else {
          # 多分类
          predicted_class <- as.character(class_predictions)
          probability <- apply(prob_predictions, 1, max)
        }
        
        prediction_result <- predicted_class
        probability_result <- probability
        
      } else if (model$type == "classification") {
        # 分类问题，只返回类别
        prediction_result <- as.character(predict(model, newdata = input_df))
        probability_result <- NULL
        
      } else {
        # 回归问题
        prediction_result <- as.numeric(predict(model, newdata = input_df))
        probability_result <- NULL
      }
      
    } else if (inherits(model, "xgb.Booster")) {
      # xgboost模型
      # 转换输入数据为矩阵
      input_matrix <- as.matrix(input_df)
      
      raw_predictions <- predict(model, newdata = input_matrix)
      
      if (return_prob) {
        # 假设是二分类问题
        if (length(unique(raw_predictions)) <= 2) {
          # 二分类的概率输出
          probability <- raw_predictions
          prediction_result <- ifelse(probability > 0.5, 1, 0)
          probability_result <- ifelse(prediction_result == 1, probability, 1 - probability)
        } else {
          # 多分类或回归
          prediction_result <- raw_predictions
          probability_result <- NULL
        }
      } else {
        prediction_result <- raw_predictions
        probability_result <- NULL
      }
      
    } else {
      # 其他模型
      prediction_result <- predict(model, newdata = input_df)
      probability_result <- NULL
    }
    
  }, error = function(e) {
    logerror(sprintf("预测失败: %s", e$message))
    prediction_result <- NA
    probability_result <- NULL
  })
  
  # 计算响应时间
  response_time <- as.numeric(Sys.time() - start_time)
  
  # 构建结果
  result <- list(
    prediction = prediction_result,
    response_time = response_time,
    timestamp = Sys.time()
  )
  
  if (!is.null(probability_result)) {
    result$probability <- probability_result
  }
  
  # 添加特征重要性（如果可用）
  if (inherits(model, "randomForest") && !is.null(model$importance)) {
    feature_importance <- as.data.frame(model$importance)
    result$feature_importance <- feature_importance
  }
  
  loginfo(sprintf("单样本预测完成，耗时: %.3f秒", response_time))
  
  return(result)
}

# 批量预测
predict_batch <- function(model, batch_data, preprocessor = NULL, return_prob = TRUE) {
  start_time <- Sys.time()
  
  loginfo(sprintf("开始批量预测，数据量: %d", ifelse(is.data.frame(batch_data), 
                                                    nrow(batch_data), length(batch_data))))
  
  # 转换输入数据
  if (is.list(batch_data) && !is.data.frame(batch_data)) {
    # 列表转换为数据框
    batch_df <- as.data.frame(do.call(rbind, batch_data))
  } else {
    batch_df <- as.data.frame(batch_data)
  }
  
  # 应用预处理
  if (!is.null(preprocessor)) {
    loginfo("应用预处理...")
    tryCatch({
      batch_df <- bake(preprocessor, new_data = batch_df)
    }, error = function(e) {
      logwarn(sprintf("预处理失败: %s", e$message))
    })
  }
  
  # 确保列名匹配
  if (inherits(model, "train")) {
    # caret模型
    model_features <- names(model$trainingData)[-ncol(model$trainingData)]
    input_features <- colnames(batch_df)
    
    # 检查缺失的特征
    missing_features <- setdiff(model_features, input_features)
    if (length(missing_features) > 0) {
      logwarn(sprintf("输入数据缺少特征: %s", paste(missing_features, collapse = ", ")))
      
      # 添加缺失的特征（填充为0）
      for (feat in missing_features) {
        batch_df[[feat]] <- 0
      }
    }
    
    # 移除多余的特征
    extra_features <- setdiff(input_features, model_features)
    if (length(extra_features) > 0) {
      logwarn(sprintf("输入数据有多余特征: %s", paste(extra_features, collapse = ", ")))
      batch_df <- batch_df[, model_features, drop = FALSE]
    }
    
    # 重新排列列顺序以匹配模型
    batch_df <- batch_df[, model_features, drop = FALSE]
  }
  
  # 批量预测
  loginfo("进行批量预测...")
  
  tryCatch({
    if (inherits(model, "train")) {
      # caret模型
      if (model$modelType == "Classification" && return_prob) {
        # 分类问题，返回概率
        prob_predictions <- predict(model, newdata = batch_df, type = "prob")
        class_predictions <- predict(model, newdata = batch_df)
        
        if (ncol(prob_predictions) == 2) {
          # 二分类
          predictions <- as.character(class_predictions)
          probabilities <- prob_predictions[, predictions]
        } else {
          # 多分类
          predictions <- as.character(class_predictions)
          probabilities <- apply(prob_predictions, 1, max)
        }
        
      } else if (model$modelType == "Classification") {
        # 分类问题，只返回类别
        predictions <- as.character(predict(model, newdata = batch_df))
        probabilities <- NULL
        
      } else {
        # 回归问题
        predictions <- as.numeric(predict(model, newdata = batch_df))
        probabilities <- NULL
      }
      
    } else if (inherits(model, "randomForest")) {
      # randomForest模型
      if (model$type == "classification" && return_prob) {
        prob_predictions <- predict(model, newdata = batch_df, type = "prob")
        class_predictions <- predict(model, newdata = batch_df)
        
        if (ncol(prob_predictions) == 2) {
          # 二分类
          predictions <- as.character(class_predictions)
          probabilities <- prob_predictions[, predictions]
        } else {
          # 多分类
          predictions <- as.character(class_predictions)
          probabilities <- apply(prob_predictions, 1, max)
        }
        
      } else if (model$type == "classification") {
        # 分类问题，只返回类别
        predictions <- as.character(predict(model, newdata = batch_df))
        probabilities <- NULL
        
      } else {
        # 回归问题
        predictions <- as.numeric(predict(model, newdata = batch_df))
        probabilities <- NULL
      }
      
    } else if (inherits(model, "xgb.Booster")) {
      # xgboost模型
      input_matrix <- as.matrix(batch_df)
      raw_predictions <- predict(model, newdata = input_matrix)
      
      if (return_prob) {
        # 假设是二分类问题
        if (length(unique(raw_predictions)) <= 2) {
          # 二分类的概率输出
          probabilities <- raw_predictions
          predictions <- ifelse(probabilities > 0.5, 1, 0)
        } else {
          # 多分类或回归
          predictions <- raw_predictions
          probabilities <- NULL
        }
      } else {
        predictions <- raw_predictions
        probabilities <- NULL
      }
      
    } else {
      # 其他模型
      predictions <- predict(model, newdata = batch_df)
      probabilities <- NULL
    }
    
  }, error = function(e) {
    logerror(sprintf("批量预测失败: %s", e$message))
    predictions <- rep(NA, nrow(batch_df))
    probabilities <- NULL
  })
  
  # 计算总时间
  total_time <- as.numeric(Sys.time() - start_time)
  
  # 构建结果
  result <- list(
    predictions = predictions,
    count = length(predictions),
    total_time = total_time,
    avg_time_per_prediction = total_time / length(predictions)
  )
  
  if (!is.null(probabilities)) {
    result$probabilities <- probabilities
  }
  
  # 统计信息
  if (!is.null(probabilities)) {
    result$statistics <- list(
      mean_prediction = mean(as.numeric(predictions), na.rm = TRUE),
      sd_prediction = sd(as.numeric(predictions), na.rm = TRUE),
      mean_probability = mean(probabilities, na.rm = TRUE),
      confidence_level = mean(probabilities > 0.5, na.rm = TRUE)
    )
  } else {
    numeric_predictions <- as.numeric(predictions)
    result$statistics <- list(
      mean_prediction = mean(numeric_predictions, na.rm = TRUE),
      sd_prediction = sd(numeric_predictions, na.rm = TRUE),
      min_prediction = min(numeric_predictions, na.rm = TRUE),
      max_prediction = max(numeric_predictions, na.rm = TRUE)
    )
  }
  
  loginfo(sprintf("批量预测完成，总耗时: %.3f秒，平均: %.3f秒/样本", 
                  total_time, total_time / length(predictions)))
  
  return(result)
}

# 从文件预测
predict_from_file <- function(model, filepath, preprocessor = NULL, return_prob = TRUE) {
  loginfo(sprintf("从文件预测: %s", filepath))
  
  # 加载数据
  ext <- tools::file_ext(filepath)
  
  if (ext == "csv") {
    data <- read.csv(filepath, stringsAsFactors = FALSE)
  } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      install.packages("readxl", repos = "https://cloud.r-project.org")
    }
    library(readxl)
    data <- read_excel(filepath)
  } else if (ext == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      install.packages("jsonlite", repos = "https://cloud.r-project.org")
    }
    library(jsonlite)
    data <- fromJSON(filepath)
    if (is.list(data) && !is.data.frame(data)) {
      data <- as.data.frame(data)
    }
  } else if (ext == "rds") {
    data <- readRDS(filepath)
  } else {
    stop(sprintf("不支持的文件格式: %s", ext))
  }
  
  loginfo(sprintf("加载数据: %d 行, %d 列", nrow(data), ncol(data)))
  
  # 执行批量预测
  result <- predict_batch(model, data, preprocessor, return_prob)
  
  # 添加文件信息
  result$filename <- basename(filepath)
  result$file_size <- file.size(filepath)
  result$original_rows <- nrow(data)
  
  return(result)
}

# 解释预测
explain_prediction <- function(model, input_data, preprocessor = NULL) {
  loginfo("生成预测解释...")
  
  # 转换输入数据
  if (is.list(input_data) && !is.data.frame(input_data)) {
    input_df <- as.data.frame(input_data)
  } else {
    input_df <- as.data.frame(input_data)
  }
  
  # 应用预处理
  if (!is.null(preprocessor)) {
    input_df <- bake(preprocessor, new_data = input_df)
  }
  
  explanation <- list()
  
  # 特征重要性
  if (inherits(model, "randomForest") && !is.null(model$importance)) {
    importance_df <- as.data.frame(model$importance)
    explanation$feature_importance <- as.list(importance_df)
  }
  
  if (inherits(model, "train")) {
    if (!is.null(model$finalModel$importance)) {
      importance_df <- as.data.frame(model$finalModel$importance)
      explanation$feature_importance <- as.list(importance_df)
    }
  }
  
  # 预测分布
  tryCatch({
    if (inherits(model, "train") && model$modelType == "Classification") {
      predictions <- predict(model, newdata = input_df, type = "prob")
      if (!is.null(predictions)) {
        explanation$prediction_distribution <- as.list(predictions[1, ])
      }
    }
  }, error = function(e) {
    logwarn(sprintf("无法获取预测分布: %s", e$message))
  })
  
  # 决策边界分析（对于简单模型）
  if (ncol(input_df) <= 2) {
    explanation$decision_boundary <- analyze_decision_boundary(model, input_df)
  }
  
  loginfo("预测解释生成完成")
  
  return(explanation)
}

# 决策边界分析
analyze_decision_boundary <- function(model, data) {
  # 生成网格数据
  x_range <- range(data[, 1])
  y_range <- range(data[, 2])
  
  x_seq <- seq(x_range[1], x_range[2], length.out = 20)
  y_seq <- seq(y_range[1], y_range[2], length.out = 20)
  
  grid <- expand.grid(x = x_seq, y = y_seq)
  colnames(grid) <- colnames(data)[1:2]
  
  # 预测网格点
  tryCatch({
    predictions <- predict(model, newdata = grid)
    
    return(list(
      grid = grid,
      predictions = predictions
    ))
  }, error = function(e) {
    logwarn(sprintf("决策边界分析失败: %s", e$message))
    return(NULL)
  })
}

# 保存预测结果
save_prediction_results <- function(results, output_dir = "predictions", filename = NULL) {
  # 创建目录
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 生成文件名
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("predictions_%s.rds", timestamp)
  }
  
  output_path <- file.path(output_dir, filename)
  
  # 保存结果
  saveRDS(results, output_path)
  loginfo(sprintf("预测结果保存到: %s", output_path))
  
  # 同时保存为CSV（如果可能）
  if ("predictions" %in% names(results)) {
    csv_filename <- sub("\\.rds$", ".csv", filename)
    csv_path <- file.path(output_dir, csv_filename)
    
    if (is.vector(results$predictions)) {
      df <- data.frame(prediction = results$predictions)
      if ("probabilities" %in% names(results)) {
        df$probability <- results$probabilities
      }
      write.csv(df, csv_path, row.names = FALSE)
      loginfo(sprintf("预测结果CSV保存到: %s", csv_path))
    }
  }
  
  return(output_path)
}

# 预测性能评估
evaluate_prediction_performance <- function(predictions, actual_values, problem_type = "classification") {
  loginfo("评估预测性能...")
  
  if (length(predictions) != length(actual_values)) {
    stop("预测值和实际值长度不一致")
  }
  
  if (problem_type == "classification") {
    # 分类问题
    # 确保因子水平一致
    if (is.factor(actual_values) && is.factor(predictions)) {
      all_levels <- unique(c(levels(actual_values), levels(predictions)))
      actual_values <- factor(actual_values, levels = all_levels)
      predictions <- factor(predictions, levels = all_levels)
    }
    
    confusion_matrix <- table(predictions, actual_values)
    
    # 计算各类指标
    if (nrow(confusion_matrix) == 2 && ncol(confusion_matrix) == 2) {
      # 二分类
      tp <- confusion_matrix[2, 2]
      fp <- confusion_matrix[2, 1]
      fn <- confusion_matrix[1, 2]
      tn <- confusion_matrix[1, 1]
      
      accuracy <- (tp + tn) / sum(confusion_matrix)
      precision <- tp / (tp + fp)
      recall <- tp / (tp + fn)
      f1_score <- 2 * precision * recall / (precision + recall)
      specificity <- tn / (tn + fp)
      
      metrics <- list(
        accuracy = accuracy,
        precision = precision,
        recall = recall,
        f1_score = f1_score,
        specificity = specificity,
        confusion_matrix = confusion_matrix
      )
    } else {
      # 多分类
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      
      # 计算宏平均
      precision_per_class <- diag(confusion_matrix) / rowSums(confusion_matrix)
      recall_per_class <- diag(confusion_matrix) / colSums(confusion_matrix)
      f1_per_class <- 2 * precision_per_class * recall_per_class / (precision_per_class + recall_per_class)
      
      precision_macro <- mean(precision_per_class, na.rm = TRUE)
      recall_macro <- mean(recall_per_class, na.rm = TRUE)
      f1_macro <- mean(f1_per_class, na.rm = TRUE)
      
      metrics <- list(
        accuracy = accuracy,
        precision_macro = precision_macro,
        recall_macro = recall_macro,
        f1_macro = f1_macro,
        confusion_matrix = confusion_matrix,
        by_class = data.frame(
          precision = precision_per_class,
          recall = recall_per_class,
          f1 = f1_per_class
        )
      )
    }
  } else {
    # 回归问题
    residuals <- actual_values - predictions
    
    mae <- mean(abs(residuals))
    mse <- mean(residuals^2)
    rmse <- sqrt(mse)
    mape <- mean(abs(residuals / actual_values)) * 100
    r_squared <- 1 - sum(residuals^2) / sum((actual_values - mean(actual_values))^2)
    
    metrics <- list(
      mae = mae,
      mse = mse,
      rmse = rmse,
      mape = mape,
      r_squared = r_squared,
      residuals = residuals
    )
  }
  
  loginfo("预测性能评估完成")
  
  return(metrics)
}

# 生成预测报告
generate_prediction_report <- function(predictions, actual_values = NULL, model_info = NULL) {
  loginfo("生成预测报告...")
  
  report <- list(
    timestamp = Sys.time(),
    prediction_summary = list(
      count = length(predictions$predictions),
      mean = mean(predictions$predictions, na.rm = TRUE),
      sd = sd(predictions$predictions, na.rm = TRUE),
      min = min(predictions$predictions, na.rm = TRUE),
      max = max(predictions$predictions, na.rm = TRUE)
    ),
    performance = list(
      total_time = predictions$total_time,
      avg_time_per_prediction = predictions$total_time / length(predictions$predictions)
    )
  )
  
  if (!is.null(model_info)) {
    report$model_info <- model_info
  }
  
  if (!is.null(actual_values)) {
    # 确定问题类型
    if (all(actual_values %in% c(0, 1)) || is.factor(actual_values)) {
      problem_type <- "classification"
    } else {
      problem_type <- "regression"
    }
    
    performance <- evaluate_prediction_performance(
      predictions$predictions, 
      actual_values,
      problem_type
    )
    report$evaluation <- performance
  }
  
  if ("probabilities" %in% names(predictions)) {
    report$probability_summary <- list(
      mean = mean(predictions$probabilities, na.rm = TRUE),
      sd = sd(predictions$probabilities, na.rm = TRUE),
      confidence_rate = mean(predictions$probabilities > 0.5, na.rm = TRUE)
    )
  }
  
  # 保存报告
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  report_file <- sprintf("prediction_report_%s.json", timestamp)
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
  }
  library(jsonlite)
  
  write_json(report, report_file, pretty = TRUE)
  
  loginfo(sprintf("预测报告保存到: %s", report_file))
  
  return(report)
}

# 批量预测任务处理
process_batch_prediction <- function(task_config) {
  loginfo("处理批量预测任务...")
  
  # 验证任务配置
  required_fields <- c("model_name", "input_data")
  missing_fields <- setdiff(required_fields, names(task_config))
  
  if (length(missing_fields) > 0) {
    stop(sprintf("任务配置缺少字段: %s", paste(missing_fields, collapse = ", ")))
  }
  
  # 加载模型
  model <- load_model(task_config$model_name, task_config$models_dir)
  
  # 加载预处理管道
  preprocessor <- NULL
  if (!is.null(task_config$preprocessor_name)) {
    preprocessor <- load_preprocessor(task_config$preprocessor_name, task_config$preprocessors_dir)
  }
  
  # 执行预测
  if (task_config$input_type == "file") {
    results <- predict_from_file(model, task_config$input_data, preprocessor, 
                                 task_config$return_prob)
  } else if (task_config$input_type == "data") {
    results <- predict_batch(model, task_config$input_data, preprocessor, 
                             task_config$return_prob)
  } else {
    stop(sprintf("不支持的输入类型: %s", task_config$input_type))
  }
  
  # 保存结果
  if (!is.null(task_config$output_dir)) {
    save_prediction_results(results, task_config$output_dir, task_config$output_filename)
  }
  
  # 生成报告
  if (!is.null(task_config$actual_values)) {
    report <- generate_prediction_report(results, task_config$actual_values, 
                                         list(model_name = task_config$model_name))
  }
  
  loginfo("批量预测任务完成")
  
  return(results)
}