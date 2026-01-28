# evaluate_model.R - 模型评估模块

# 评估模型
evaluate_model <- function(model, test_data, target_col, config = list()) {
  loginfo("开始模型评估...")
  
  # 默认配置
  default_config <- list(
    classification = TRUE,
    detailed = TRUE,
    generate_plots = TRUE,
    save_results = TRUE,
    results_dir = "evaluations"
  )
  
  # 合并配置
  config <- modifyList(default_config, config)
  
  # 分离特征和目标
  test_x <- test_data[, !colnames(test_data) %in% target_col, drop = FALSE]
  test_y <- test_data[[target_col]]
  
  # 预测
  loginfo("进行预测...")
  predictions <- predict(model, newdata = test_x)
  
  # 获取预测概率（如果是分类问题）
  if (config$classification && model$modelType == "Classification") {
    tryCatch({
      prob_predictions <- predict(model, newdata = test_x, type = "prob")
    }, error = function(e) {
      logwarn(sprintf("无法获取概率预测: %s", e$message))
      prob_predictions <- NULL
    })
  } else {
    prob_predictions <- NULL
  }
  
  # 计算性能指标
  loginfo("计算性能指标...")
  metrics <- calculate_metrics(test_y, predictions, prob_predictions, config$classification)
  
  # 生成可视化
  plots <- list()
  if (config$generate_plots) {
    loginfo("生成可视化图表...")
    plots <- generate_evaluation_plots(test_y, predictions, prob_predictions, 
                                      config$classification, model)
  }
  
  # 保存结果
  result <- list(
    metrics = metrics,
    predictions = as.vector(predictions),
    actuals = as.vector(test_y),
    plots = plots,
    config = config
  )
  
  if (config$save_results) {
    save_evaluation_results(result, model, config$results_dir)
  }
  
  loginfo("模型评估完成")
  
  return(result)
}

# 计算性能指标
calculate_metrics <- function(actual, predicted, prob = NULL, classification = TRUE) {
  if (classification) {
    # 分类指标
    
    # 确保因子水平一致
    if (is.factor(actual) && is.factor(predicted)) {
      all_levels <- unique(c(levels(actual), levels(predicted)))
      actual <- factor(actual, levels = all_levels)
      predicted <- factor(predicted, levels = all_levels)
    }
    
    # 混淆矩阵
    conf_matrix <- caret::confusionMatrix(predicted, actual)
    
    # 基本指标
    accuracy <- conf_matrix$overall["Accuracy"]
    kappa <- conf_matrix$overall["Kappa"]
    
    # 多类指标或二分类指标
    if (length(unique(actual)) == 2) {
      # 二分类
      precision <- conf_matrix$byClass["Precision"]
      recall <- conf_matrix$byClass["Recall"]
      f1 <- conf_matrix$byClass["F1"]
      specificity <- conf_matrix$byClass["Specificity"]
      
      # AUC（如果有概率预测）
      auc <- NA
      if (!is.null(prob) && ncol(prob) == 2) {
        tryCatch({
          library(pROC)
          roc_obj <- roc(actual, prob[, 2])
          auc <- auc(roc_obj)
        }, error = function(e) {
          logwarn(sprintf("计算AUC失败: %s", e$message))
        })
      }
      
      metrics <- list(
        accuracy = accuracy,
        kappa = kappa,
        precision = precision,
        recall = recall,
        f1 = f1,
        specificity = specificity,
        auc = auc,
        confusion_matrix = conf_matrix$table
      )
      
    } else {
      # 多分类
      # 计算宏平均和微平均
      by_class <- conf_matrix$byClass
      
      if (is.matrix(by_class)) {
        # 多类分类
        precision_macro <- mean(by_class[, "Precision"], na.rm = TRUE)
        recall_macro <- mean(by_class[, "Recall"], na.rm = TRUE)
        f1_macro <- mean(by_class[, "F1"], na.rm = TRUE)
        
        metrics <- list(
          accuracy = accuracy,
          kappa = kappa,
          precision_macro = precision_macro,
          recall_macro = recall_macro,
          f1_macro = f1_macro,
          confusion_matrix = conf_matrix$table,
          by_class = by_class
        )
      } else {
        # 单行情况
        metrics <- list(
          accuracy = accuracy,
          kappa = kappa,
          confusion_matrix = conf_matrix$table
        )
      }
    }
    
  } else {
    # 回归指标
    residuals <- actual - predicted
    
    mae <- mean(abs(residuals))
    mse <- mean(residuals^2)
    rmse <- sqrt(mse)
    mape <- mean(abs(residuals / actual)) * 100
    r2 <- 1 - sum(residuals^2) / sum((actual - mean(actual))^2)
    
    metrics <- list(
      mae = mae,
      mse = mse,
      rmse = rmse,
      mape = mape,
      r2 = r2,
      residuals = residuals
    )
  }
  
  return(metrics)
}

# 生成评估图表
generate_evaluation_plots <- function(actual, predicted, prob = NULL, 
                                     classification = TRUE, model = NULL) {
  plots <- list()
  
  if (classification) {
    # 分类问题可视化
    
    # 1. 混淆矩阵热图
    plots$confusion_matrix <- plot_confusion_matrix(actual, predicted)
    
    # 2. ROC曲线（二分类）
    if (!is.null(prob) && length(unique(actual)) == 2) {
      plots$roc_curve <- plot_roc_curve(actual, prob)
    }
    
    # 3. 精确率-召回率曲线
    if (!is.null(prob) && length(unique(actual)) == 2) {
      plots$pr_curve <- plot_pr_curve(actual, prob)
    }
    
    # 4. 类别分布图
    plots$class_distribution <- plot_class_distribution(actual, predicted)
    
  } else {
    # 回归问题可视化
    
    # 1. 实际值 vs 预测值
    plots$actual_vs_predicted <- plot_actual_vs_predicted(actual, predicted)
    
    # 2. 残差图
    plots$residuals <- plot_residuals(actual, predicted)
    
    # 3. 预测误差分布
    plots$error_distribution <- plot_error_distribution(actual, predicted)
  }
  
  # 5. 特征重要性（如果模型支持）
  if (!is.null(model)) {
    importance_plot <- plot_feature_importance(model)
    if (!is.null(importance_plot)) {
      plots$feature_importance <- importance_plot
    }
  }
  
  # 6. 学习曲线（如果有训练历史）
  if (!is.null(model) && !is.null(model$results)) {
    plots$learning_curve <- plot_learning_curve(model)
  }
  
  return(plots)
}

# 绘制混淆矩阵
plot_confusion_matrix <- function(actual, predicted) {
  # 创建混淆矩阵
  conf_matrix <- table(predicted, actual)
  
  # 转换为数据框用于ggplot
  conf_df <- as.data.frame(conf_matrix)
  colnames(conf_df) <- c("predicted", "actual", "count")
  
  # 计算百分比
  conf_df$percentage <- conf_df$count / sum(conf_df$count) * 100
  
  # 创建热图
  library(ggplot2)
  p <- ggplot(conf_df, aes(x = actual, y = predicted, fill = count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", count, percentage)), 
              color = "black", size = 4) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = "混淆矩阵",
         x = "实际类别",
         y = "预测类别",
         fill = "数量") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# 绘制ROC曲线
plot_roc_curve <- function(actual, prob) {
  library(pROC)
  library(ggplot2)
  
  # 计算ROC
  roc_obj <- roc(actual, prob[, 2])
  auc_value <- auc(roc_obj)
  
  # 创建ROC数据框
  roc_data <- data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  )
  
  # 绘制ROC曲线
  p <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
    geom_line(color = "steelblue", size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    labs(title = paste0("ROC曲线 (AUC = ", round(auc_value, 3), ")"),
         x = "假阳性率",
         y = "真阳性率") +
    theme_minimal() +
    coord_equal()
  
  return(p)
}

# 绘制PR曲线
plot_pr_curve <- function(actual, prob) {
  library(PRROC)
  library(ggplot2)
  
  # 计算PR曲线
  pr_obj <- pr.curve(scores.class0 = prob[, 2], 
                     weights.class0 = ifelse(actual == colnames(prob)[2], 1, 0))
  
  # 创建PR数据框
  pr_data <- data.frame(
    recall = pr_obj$curve[, 1],
    precision = pr_obj$curve[, 2]
  )
  
  # 绘制PR曲线
  p <- ggplot(pr_data, aes(x = recall, y = precision)) +
    geom_line(color = "darkorange", size = 1.5) +
    labs(title = paste0("精确率-召回率曲线 (AUC = ", round(pr_obj$auc.integral, 3), ")"),
         x = "召回率",
         y = "精确率") +
    theme_minimal() +
    ylim(0, 1)
  
  return(p)
}

# 绘制类别分布图
plot_class_distribution <- function(actual, predicted) {
  library(ggplot2)
  
  # 创建数据框
  plot_data <- data.frame(
    actual = actual,
    predicted = predicted,
    type = ifelse(actual == predicted, "正确", "错误")
  )
  
  # 绘制分布图
  p <- ggplot(plot_data, aes(x = actual, fill = type)) +
    geom_bar(position = "dodge") +
    labs(title = "类别分布与预测结果",
         x = "类别",
         y = "数量",
         fill = "预测结果") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("正确" = "steelblue", "错误" = "firebrick"))
  
  return(p)
}

# 绘制实际值 vs 预测值
plot_actual_vs_predicted <- function(actual, predicted) {
  library(ggplot2)
  
  # 创建数据框
  plot_data <- data.frame(
    actual = actual,
    predicted = predicted
  )
  
  # 计算R²
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  
  # 绘制散点图
  p <- ggplot(plot_data, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = paste0("实际值 vs 预测值 (R² = ", round(r2, 3), ")"),
         x = "实际值",
         y = "预测值") +
    theme_minimal() +
    coord_equal()
  
  return(p)
}

# 绘制残差图
plot_residuals <- function(actual, predicted) {
  library(ggplot2)
  
  # 计算残差
  residuals <- actual - predicted
  
  # 创建数据框
  plot_data <- data.frame(
    predicted = predicted,
    residuals = residuals
  )
  
  # 绘制残差图
  p <- ggplot(plot_data, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", color = "darkorange", se = FALSE) +
    labs(title = "残差图",
         x = "预测值",
         y = "残差") +
    theme_minimal()
  
  return(p)
}

# 绘制误差分布
plot_error_distribution <- function(actual, predicted) {
  library(ggplot2)
  
  # 计算误差
  errors <- actual - predicted
  
  # 创建数据框
  plot_data <- data.frame(error = errors)
  
  # 绘制直方图
  p <- ggplot(plot_data, aes(x = error)) +
    geom_histogram(aes(y = ..density..), bins = 30, 
                   fill = "steelblue", alpha = 0.7) +
    geom_density(color = "darkorange", size = 1) +
    geom_vline(xintercept = mean(errors), color = "red", linetype = "dashed") +
    labs(title = "预测误差分布",
         x = "误差",
         y = "密度") +
    theme_minimal()
  
  return(p)
}

# 绘制特征重要性
plot_feature_importance <- function(model) {
  tryCatch({
    # 获取特征重要性
    importance <- varImp(model)
    
    if (!is.null(importance)) {
      # 提取重要性数据
      imp_data <- as.data.frame(importance$importance)
      imp_data$feature <- rownames(imp_data)
      
      # 排序
      imp_data <- imp_data[order(imp_data$Overall, decreasing = FALSE), ]
      imp_data$feature <- factor(imp_data$feature, levels = imp_data$feature)
      
      # 绘制条形图
      library(ggplot2)
      p <- ggplot(imp_data, aes(x = Overall, y = feature)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
        labs(title = "特征重要性",
             x = "重要性",
             y = "特征") +
        theme_minimal()
      
      return(p)
    }
  }, error = function(e) {
    logwarn(sprintf("无法绘制特征重要性: %s", e$message))
  })
  
  return(NULL)
}

# 绘制学习曲线
plot_learning_curve <- function(model) {
  tryCatch({
    if (!is.null(model$results)) {
      # 获取训练结果
      results <- model$results
      
      # 查找包含训练大小的列
      size_col <- grep("size|samples|observations", colnames(results), 
                       ignore.case = TRUE, value = TRUE)
      
      if (length(size_col) > 0) {
        # 使用第一个找到的大小列
        size_col <- size_col[1]
        
        # 查找性能指标列
        metric_cols <- c("Accuracy", "RMSE", "ROC", "Kappa", "MAE")
        metric_col <- intersect(metric_cols, colnames(results))
        
        if (length(metric_col) > 0) {
          metric_col <- metric_col[1]
          
          # 创建数据框
          plot_data <- data.frame(
            size = results[[size_col]],
            performance = results[[metric_col]]
          )
          
          # 绘制学习曲线
          library(ggplot2)
          p <- ggplot(plot_data, aes(x = size, y = performance)) +
            geom_line(color = "steelblue", size = 1.5) +
            geom_point(color = "steelblue", size = 3) +
            labs(title = "学习曲线",
                 x = "训练样本数",
                 y = metric_col) +
            theme_minimal()
          
          return(p)
        }
      }
    }
  }, error = function(e) {
    logwarn(sprintf("无法绘制学习曲线: %s", e$message))
  })
  
  return(NULL)
}

# 模型对比
compare_models <- function(models_performance) {
  loginfo("对比模型性能...")
  
  # 提取性能数据
  comparison_data <- data.frame()
  
  for (model_name in names(models_performance)) {
    perf <- models_performance[[model_name]]
    
    # 创建行数据
    row_data <- data.frame(
      model = model_name,
      algorithm = perf$algorithm,
      score = perf$score,
      metric = perf$metric,
      training_time = perf$training_time,
      stringsAsFactors = FALSE
    )
    
    # 添加其他指标
    if (!is.null(perf$accuracy)) row_data$accuracy <- perf$accuracy
    if (!is.null(perf$rmse)) row_data$rmse <- perf$rmse
    if (!is.null(perf$r2)) row_data$r2 <- perf$r2
    if (!is.null(perf$auc)) row_data$auc <- perf$auc
    
    comparison_data <- rbind(comparison_data, row_data)
  }
  
  # 排序
  if (nrow(comparison_data) > 0) {
    if (comparison_data$metric[1] %in% c("Accuracy", "ROC", "Precision", "Recall", "F1")) {
      # 分类指标：越高越好
      comparison_data <- comparison_data[order(comparison_data$score, decreasing = TRUE), ]
    } else {
      # 回归指标：越低越好（除了R²）
      if (comparison_data$metric[1] == "R2") {
        comparison_data <- comparison_data[order(comparison_data$score, decreasing = TRUE), ]
      } else {
        comparison_data <- comparison_data[order(comparison_data$score, decreasing = FALSE), ]
      }
    }
  }
  
  # 创建对比图
  if (nrow(comparison_data) > 1) {
    library(ggplot2)
    
    p <- ggplot(comparison_data, aes(x = reorder(model, score), y = score, fill = algorithm)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_text(aes(label = round(score, 3)), hjust = -0.2, size = 4) +
      coord_flip() +
      labs(title = "模型性能对比",
           x = "模型",
           y = comparison_data$metric[1],
           fill = "算法") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    return(list(
      comparison_table = comparison_data,
      plot = p
    ))
  }
  
  return(list(comparison_table = comparison_data))
}

# 保存评估结果
save_evaluation_results <- function(results, model, results_dir = "evaluations") {
  # 确保目录存在
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE)
  }
  
  # 生成文件名
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  model_name <- if (!is.null(model$method)) model$method else "unknown"
  filename <- paste0("evaluation_", model_name, "_", timestamp, ".rds")
  filepath <- file.path(results_dir, filename)
  
  # 添加元数据
  results$metadata <- list(
    saved_time = Sys.time(),
    model_name = model_name,
    model_type = model$modelType,
    timestamp = timestamp
  )
  
  # 保存结果
  saveRDS(results, filepath)
  
  loginfo(sprintf("评估结果保存到: %s", filepath))
  
  # 保存文本报告
  save_evaluation_report(results, model, results_dir, timestamp)
  
  return(filepath)
}

# 保存评估报告
save_evaluation_report <- function(results, model, results_dir, timestamp) {
  report_file <- file.path(results_dir, paste0("report_", timestamp, ".txt"))
  
  sink(report_file)
  
  cat("模型评估报告\n")
  cat("=============\n\n")
  
  cat(sprintf("生成时间: %s\n", Sys.time()))
  cat(sprintf("模型名称: %s\n", if (!is.null(model$method)) model$method else "unknown"))
  cat(sprintf("模型类型: %s\n", model$modelType))
  cat("\n")
  
  # 性能指标
  cat("性能指标:\n")
  cat("---------\n")
  
  metrics <- results$metrics
  
  if (model$modelType == "Classification") {
    # 分类指标
    cat(sprintf("准确率: %.4f\n", metrics$accuracy))
    cat(sprintf("Kappa: %.4f\n", metrics$kappa))
    
    if (!is.null(metrics$precision)) {
      cat(sprintf("精确率: %.4f\n", metrics$precision))
    }
    
    if (!is.null(metrics$recall)) {
      cat(sprintf("召回率: %.4f\n", metrics$recall))
    }
    
    if (!is.null(metrics$f1)) {
      cat(sprintf("F1分数: %.4f\n", metrics$f1))
    }
    
    if (!is.null(metrics$auc)) {
      cat(sprintf("AUC: %.4f\n", metrics$auc))
    }
    
  } else {
    # 回归指标
    cat(sprintf("MAE: %.4f\n", metrics$mae))
    cat(sprintf("MSE: %.4f\n", metrics$mse))
    cat(sprintf("RMSE: %.4f\n", metrics$rmse))
    cat(sprintf("MAPE: %.4f%%\n", metrics$mape))
    cat(sprintf("R²: %.4f\n", metrics$r2))
  }
  
  cat("\n")
  
  # 混淆矩阵（如果是分类问题）
  if (!is.null(metrics$confusion_matrix)) {
    cat("混淆矩阵:\n")
    cat("---------\n")
    print(metrics$confusion_matrix)
    cat("\n")
  }
  
  # 最佳参数
  if (!is.null(model$bestTune)) {
    cat("最佳超参数:\n")
    cat("-----------\n")
    print(model$bestTune)
    cat("\n")
  }
  
  # 预测示例
  cat("预测示例（前10个样本）:\n")
  cat("----------------------\n")
  
  example_data <- data.frame(
    实际值 = head(results$actuals, 10),
    预测值 = head(results$predictions, 10)
  )
  
  if (model$modelType == "Classification") {
    example_data$正确 <- ifelse(example_data$实际值 == example_data$预测值, "是", "否")
  } else {
    example_data$误差 <- round(abs(example_data$实际值 - example_data$预测值), 4)
  }
  
  print(example_data)
  
  sink()
  
  loginfo(sprintf("评估报告保存到: %s", report_file))
}