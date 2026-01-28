# train_model.R - 模型训练模块

# 训练多个模型
train_multiple_models <- function(train_data, target_col, config = list()) {
  loginfo("开始训练多个模型...")
  
  # 默认配置
  default_config <- list(
    models_to_train = c("glm", "rf", "xgb", "svm", "knn"),
    problem_type = "classification",  # "classification" 或 "regression"
    cv_method = "cv",
    cv_folds = 5,
    cv_repeats = 1,
    search_method = "grid",
    metric = NULL,
    enable_parallel = FALSE,
    n_cores = parallel::detectCores() - 1,
    save_models = TRUE,
    model_dir = "models",
    verbose = TRUE
  )
  
  # 根据问题类型设置默认评估指标
  if (is.null(config$metric)) {
    if (config$problem_type == "classification") {
      default_config$metric <- "Accuracy"
    } else {
      default_config$metric <- "RMSE"
    }
  }
  
  # 合并配置
  config <- modifyList(default_config, config)
  
  # 分离特征和目标
  train_y <- train_data[[target_col]]
  train_x <- train_data[, !colnames(train_data) %in% target_col, drop = FALSE]
  
  # 设置并行计算（如果启用）
  if (config$enable_parallel && config$n_cores > 1) {
    library(doParallel)
    cl <- makePSOCKcluster(config$n_cores)
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
    
    loginfo(sprintf("启用并行计算，使用 %d 个核心", config$n_cores))
  }
  
  # 定义训练控制参数
  train_control <- trainControl(
    method = config$cv_method,
    number = config$cv_folds,
    repeats = config$cv_repeats,
    search = config$search_method,
    verboseIter = config$verbose,
    classProbs = config$problem_type == "classification",
    savePredictions = "final",
    summaryFunction = if (config$problem_type == "classification") {
      twoClassSummary
    } else {
      defaultSummary
    }
  )
  
  # 初始化结果存储
  models <- list()
  performance <- list()
  training_times <- list()
  
  # 训练逻辑回归/线性回归
  if ("glm" %in% config$models_to_train) {
    loginfo("训练GLM模型...")
    
    start_time <- Sys.time()
    
    if (config$problem_type == "classification") {
      # 逻辑回归
      glm_model <- train(
        x = train_x,
        y = train_y,
        method = "glm",
        family = binomial(),
        trControl = train_control,
        metric = config$metric
      )
    } else {
      # 线性回归
      glm_model <- train(
        x = train_x,
        y = train_y,
        method = "glm",
        family = gaussian(),
        trControl = train_control,
        metric = config$metric
      )
    }
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    models$glm <- glm_model
    performance$glm <- extract_model_performance(glm_model, config$metric)
    training_times$glm <- training_time
    
    loginfo(sprintf("GLM训练完成，耗时: %.2f秒，性能: %.4f", 
                    training_time, performance$glm$score))
  }
  
  # 训练随机森林
  if ("rf" %in% config$models_to_train) {
    loginfo("训练随机森林模型...")
    
    start_time <- Sys.time()
    
    # 定义参数网格
    rf_grid <- expand.grid(
      mtry = seq(2, min(20, ncol(train_x)), length.out = 3),
      splitrule = if (config$problem_type == "classification") "gini" else "variance",
      min.node.size = c(1, 5, 10)
    )
    
    rf_model <- train(
      x = train_x,
      y = train_y,
      method = "ranger",
      trControl = train_control,
      tuneGrid = rf_grid,
      importance = "impurity",
      metric = config$metric,
      num.trees = 100
    )
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    models$rf <- rf_model
    performance$rf <- extract_model_performance(rf_model, config$metric)
    training_times$rf <- training_time
    
    loginfo(sprintf("随机森林训练完成，耗时: %.2f秒，性能: %.4f", 
                    training_time, performance$rf$score))
  }
  
  # 训练XGBoost
  if ("xgb" %in% config$models_to_train) {
    loginfo("训练XGBoost模型...")
    
    start_time <- Sys.time()
    
    # 定义参数网格
    xgb_grid <- expand.grid(
      nrounds = c(50, 100),
      max_depth = c(3, 6),
      eta = c(0.01, 0.1),
      gamma = c(0, 1),
      colsample_bytree = c(0.6, 0.8),
      min_child_weight = c(1, 3),
      subsample = c(0.6, 0.8)
    )
    
    xgb_model <- train(
      x = train_x,
      y = train_y,
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = xgb_grid,
      metric = config$metric,
      verbose = 0
    )
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    models$xgb <- xgb_model
    performance$xgb <- extract_model_performance(xgb_model, config$metric)
    training_times$xgb <- training_time
    
    loginfo(sprintf("XGBoost训练完成，耗时: %.2f秒，性能: %.4f", 
                    training_time, performance$xgb$score))
  }
  
  # 训练支持向量机
  if ("svm" %in% config$models_to_train) {
    loginfo("训练SVM模型...")
    
    start_time <- Sys.time()
    
    svm_model <- train(
      x = train_x,
      y = train_y,
      method = "svmRadial",
      trControl = train_control,
      tuneLength = 5,
      metric = config$metric
    )
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    models$svm <- svm_model
    performance$svm <- extract_model_performance(svm_model, config$metric)
    training_times$svm <- training_time
    
    loginfo(sprintf("SVM训练完成，耗时: %.2f秒，性能: %.4f", 
                    training_time, performance$svm$score))
  }
  
  # 训练K最近邻
  if ("knn" %in% config$models_to_train) {
    loginfo("训练KNN模型...")
    
    start_time <- Sys.time()
    
    knn_model <- train(
      x = train_x,
      y = train_y,
      method = "knn",
      trControl = train_control,
      tuneLength = 5,
      metric = config$metric
    )
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    models$knn <- knn_model
    performance$knn <- extract_model_performance(knn_model, config$metric)
    training_times$knn <- training_time
    
    loginfo(sprintf("KNN训练完成，耗时: %.2f秒，性能: %.4f", 
                    training_time, performance$knn$score))
  }
  
  # 训练神经网络
  if ("neural_network" %in% config$models_to_train) {
    loginfo("训练神经网络模型...")
    
    start_time <- Sys.time()
    
    nn_model <- train(
      x = train_x,
      y = train_y,
      method = "nnet",
      trControl = train_control,
      tuneLength = 3,
      metric = config$metric,
      trace = FALSE
    )
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    models$neural_network <- nn_model
    performance$neural_network <- extract_model_performance(nn_model, config$metric)
    training_times$neural_network <- training_time
    
    loginfo(sprintf("神经网络训练完成，耗时: %.2f秒，性能: %.4f", 
                    training_time, performance$neural_network$score))
  }
  
  # 训练决策树
  if ("decision_tree" %in% config$models_to_train) {
    loginfo("训练决策树模型...")
    
    start_time <- Sys.time()
    
    dt_model <- train(
      x = train_x,
      y = train_y,
      method = "rpart",
      trControl = train_control,
      tuneLength = 5,
      metric = config$metric
    )
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    models$decision_tree <- dt_model
    performance$decision_tree <- extract_model_performance(dt_model, config$metric)
    training_times$decision_tree <- training_time
    
    loginfo(sprintf("决策树训练完成，耗时: %.2f秒，性能: %.4f", 
                    training_time, performance$decision_tree$score))
  }
  
  # 训练朴素贝叶斯
  if ("naive_bayes" %in% config$models_to_train && config$problem_type == "classification") {
    loginfo("训练朴素贝叶斯模型...")
    
    start_time <- Sys.time()
    
    nb_model <- train(
      x = train_x,
      y = train_y,
      method = "nb",
      trControl = train_control,
      tuneLength = 3,
      metric = config$metric
    )
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    models$naive_bayes <- nb_model
    performance$naive_bayes <- extract_model_performance(nb_model, config$metric)
    training_times$naive_bayes <- training_time
    
    loginfo(sprintf("朴素贝叶斯训练完成，耗时: %.2f秒，性能: %.4f", 
                    training_time, performance$naive_bayes$score))
  }
  
  # 训练弹性网络
  if ("glmnet" %in% config$models_to_train) {
    loginfo("训练弹性网络模型...")
    
    start_time <- Sys.time()
    
    glmnet_model <- train(
      x = train_x,
      y = train_y,
      method = "glmnet",
      trControl = train_control,
      tuneLength = 5,
      metric = config$metric
    )
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    models$glmnet <- glmnet_model
    performance$glmnet <- extract_model_performance(glmnet_model, config$metric)
    training_times$glmnet <- training_time
    
    loginfo(sprintf("弹性网络训练完成，耗时: %.2f秒，性能: %.4f", 
                    training_time, performance$glmnet$score))
  }
  
  # 保存模型
  if (config$save_models) {
    loginfo("保存训练好的模型...")
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    for (model_name in names(models)) {
      model <- models[[model_name]]
      model_filename <- paste0(model_name, "_", timestamp, ".rds")
      model_filepath <- file.path(config$model_dir, model_filename)
      
      safe_save(model, model_filepath)
      
      loginfo(sprintf("  模型 %s 保存到: %s", model_name, model_filepath))
    }
  }
  
  # 汇总结果
  result <- list(
    models = models,
    performance = performance,
    training_times = training_times,
    config = config,
    best_model = select_best_model(performance, config$metric)
  )
  
  loginfo("所有模型训练完成")
  loginfo("模型性能汇总:")
  for (model_name in names(performance)) {
    perf <- performance[[model_name]]
    time <- training_times[[model_name]]
    loginfo(sprintf("  %s: %.4f (%.2f秒)", model_name, perf$score, time))
  }
  
  loginfo(sprintf("最佳模型: %s (%.4f)", 
                  result$best_model$name, result$best_model$score))
  
  # 保存检查点
  checkpoint_file <- save_checkpoint(result, "model_training")
  loginfo(sprintf("训练结果保存到检查点: %s", checkpoint_file))
  
  return(result)
}

# 提取模型性能
extract_model_performance <- function(model, metric) {
  if (is.null(model)) {
    return(list(score = NA, details = NULL))
  }
  
  # 获取最佳性能
  best_idx <- which.max(model$results[[metric]])
  best_score <- model$results[[metric]][best_idx]
  
  # 获取最佳参数
  best_params <- model$bestTune
  
  # 获取其他指标
  if ("Accuracy" %in% colnames(model$results)) {
    accuracy <- model$results$Accuracy[best_idx]
  } else {
    accuracy <- NA
  }
  
  if ("Kappa" %in% colnames(model$results)) {
    kappa <- model$results$Kappa[best_idx]
  } else {
    kappa <- NA
  }
  
  if ("RMSE" %in% colnames(model$results)) {
    rmse <- model$results$RMSE[best_idx]
  } else {
    rmse <- NA
  }
  
  if ("Rsquared" %in% colnames(model$results)) {
    rsquared <- model$results$Rsquared[best_idx]
  } else {
    rsquared <- NA
  }
  
  # 获取训练时间
  training_time <- as.numeric(model$times$everything[3])
  
  return(list(
    score = best_score,
    metric = metric,
    accuracy = accuracy,
    kappa = kappa,
    rmse = rmse,
    rsquared = rsquared,
    best_params = best_params,
    training_time = training_time,
    model_type = model$modelType,
    algorithm = model$method
  ))
}

# 选择最佳模型
select_best_model <- function(performance, metric) {
  best_score <- -Inf
  best_model_name <- NULL
  best_model_perf <- NULL
  
  for (model_name in names(performance)) {
    perf <- performance[[model_name]]
    
    if (!is.na(perf$score) && perf$score > best_score) {
      best_score <- perf$score
      best_model_name <- model_name
      best_model_perf <- perf
    }
  }
  
  if (is.null(best_model_name)) {
    logwarn("无法选择最佳模型")
    return(list(name = NULL, score = NA))
  }
  
  loginfo(sprintf("最佳模型: %s (分数: %.4f)", best_model_name, best_score))
  
  return(list(
    name = best_model_name,
    score = best_score,
    performance = best_model_perf
  ))
}

# 超参数调优
tune_hyperparameters <- function(model, train_data, target_col, config = list()) {
  loginfo("进行超参数调优...")
  
  # 分离特征和目标
  train_y <- train_data[[target_col]]
  train_x <- train_data[, !colnames(train_data) %in% target_col, drop = FALSE]
  
  # 根据模型类型定义调优网格
  if (model$method == "ranger") {
    # 随机森林
    tune_grid <- expand.grid(
      mtry = seq(2, min(30, ncol(train_x)), length.out = 5),
      splitrule = if (model$modelType == "Classification") "gini" else "variance",
      min.node.size = c(1, 5, 10, 20)
    )
    
  } else if (model$method == "xgbTree") {
    # XGBoost
    tune_grid <- expand.grid(
      nrounds = c(50, 100, 200),
      max_depth = c(3, 6, 9),
      eta = c(0.01, 0.05, 0.1),
      gamma = c(0, 1, 5),
      colsample_bytree = c(0.6, 0.8, 1.0),
      min_child_weight = c(1, 3, 5),
      subsample = c(0.6, 0.8, 1.0)
    )
    
  } else if (model$method == "glmnet") {
    # 弹性网络
    tune_grid <- expand.grid(
      alpha = seq(0, 1, 0.1),
      lambda = 10^seq(-3, 3, length.out = 20)
    )
    
  } else if (model$method == "svmRadial") {
    # SVM
    tune_grid <- expand.grid(
      sigma = 2^seq(-10, 2, length.out = 10),
      C = 2^seq(-2, 10, length.out = 10)
    )
    
  } else if (model$method == "knn") {
    # KNN
    tune_grid <- expand.grid(
      k = seq(1, 30, by = 2)
    )
    
  } else {
    logwarn(sprintf("不支持的模型类型进行调优: %s", model$method))
    return(model)
  }
  
  # 使用相同的训练控制
  train_control <- model$control
  
  # 重新训练模型
  tuned_model <- train(
    x = train_x,
    y = train_y,
    method = model$method,
    trControl = train_control,
    tuneGrid = tune_grid,
    metric = config$metric %||% "Accuracy"
  )
  
  loginfo("超参数调优完成")
  
  # 比较性能
  original_score <- max(model$results[[config$metric %||% "Accuracy"]], na.rm = TRUE)
  tuned_score <- max(tuned_model$results[[config$metric %||% "Accuracy"]], na.rm = TRUE)
  
  improvement <- tuned_score - original_score
  
  loginfo(sprintf("调优结果: 原始分数=%.4f, 调优后分数=%.4f, 提升=%.4f", 
                  original_score, tuned_score, improvement))
  
  return(tuned_model)
}

# 集成学习
create_ensemble <- function(models, method = "voting", weights = NULL) {
  loginfo(sprintf("创建集成模型，方法: %s", method))
  
  if (length(models) < 2) {
    logwarn("至少需要2个模型进行集成")
    return(models[[1]])
  }
  
  # 检查模型一致性
  model_types <- sapply(models, function(m) m$modelType)
  if (length(unique(model_types)) > 1) {
    logwarn("模型类型不一致，无法集成")
    return(NULL)
  }
  
  is_classification <- unique(model_types) == "Classification"
  
  if (method == "voting") {
    # 投票集成
    ensemble <- list(
      models = models,
      method = "voting",
      is_classification = is_classification,
      n_models = length(models)
    )
    
    if (is_classification) {
      ensemble$type <- "classification"
    } else {
      ensemble$type <- "regression"
    }
    
    class(ensemble) <- "voting_ensemble"
    
  } else if (method == "stacking") {
    # 堆叠集成
    ensemble <- list(
      models = models,
      method = "stacking",
      is_classification = is_classification,
      n_models = length(models)
    )
    
    if (is_classification) {
      ensemble$meta_model <- train(
        x = do.call(cbind, lapply(models, function(m) m$pred$pred)),
        y = models[[1]]$pred$obs,
        method = "glm",
        trControl = trainControl(method = "cv", number = 5)
      )
      ensemble$type <- "classification"
    } else {
      ensemble$meta_model <- train(
        x = do.call(cbind, lapply(models, function(m) m$pred$pred)),
        y = models[[1]]$pred$obs,
        method = "glm",
        trControl = trainControl(method = "cv", number = 5)
      )
      ensemble$type <- "regression"
    }
    
    class(ensemble) <- "stacking_ensemble"
    
  } else if (method == "averaging") {
    # 平均集成
    ensemble <- list(
      models = models,
      method = "averaging",
      is_classification = is_classification,
      n_models = length(models)
    )
    
    if (!is.null(weights)) {
      if (length(weights) != length(models)) {
        logwarn("权重数量与模型数量不匹配，使用等权重")
        weights <- rep(1/length(models), length(models))
      }
      ensemble$weights <- weights
    } else {
      ensemble$weights <- rep(1/length(models), length(models))
    }
    
    class(ensemble) <- "averaging_ensemble"
    
  } else {
    logwarn(sprintf("不支持的集成方法: %s", method))
    return(NULL)
  }
  
  loginfo(sprintf("集成模型创建完成: %d 个模型", length(models)))
  
  return(ensemble)
}

# 模型预测方法
predict.ensemble <- function(object, newdata, type = "raw", ...) {
  if (object$method == "voting") {
    # 投票集成预测
    if (object$is_classification) {
      # 分类：多数投票
      predictions <- sapply(object$models, function(model) {
        predict(model, newdata = newdata)
      })
      
      # 计算每个类别的票数
      class_counts <- apply(predictions, 1, function(row) {
        table(factor(row, levels = unique(as.vector(predictions))))
      })
      
      # 选择票数最多的类别
      final_predictions <- apply(class_counts, 2, function(counts) {
        names(which.max(counts))
      })
      
      return(final_predictions)
      
    } else {
      # 回归：平均预测
      predictions <- sapply(object$models, function(model) {
        predict(model, newdata = newdata)
      })
      
      final_predictions <- rowMeans(predictions)
      return(final_predictions)
    }
    
  } else if (object$method == "averaging") {
    # 平均集成预测
    predictions <- sapply(object$models, function(model) {
      predict(model, newdata = newdata)
    })
    
    if (object$is_classification) {
      # 分类：加权投票
      if (type == "prob") {
        # 获取概率预测
        probs_list <- lapply(object$models, function(model) {
          predict(model, newdata = newdata, type = "prob")
        })
        
        # 加权平均概率
        weighted_probs <- 0
        for (i in 1:length(probs_list)) {
          weighted_probs <- weighted_probs + probs_list[[i]] * object$weights[i]
        }
        
        return(weighted_probs)
      } else {
        # 获取类别预测
        class_predictions <- sapply(object$models, function(model) {
          predict(model, newdata = newdata)
        })
        
        # 加权投票
        final_predictions <- apply(class_predictions, 1, function(row) {
          weighted_table <- tapply(object$weights, row, sum)
          names(which.max(weighted_table))
        })
        
        return(final_predictions)
      }
    } else {
      # 回归：加权平均
      final_predictions <- rowSums(predictions * matrix(object$weights, 
                                                        nrow = nrow(predictions),
                                                        ncol = length(object$weights),
                                                        byrow = TRUE))
      return(final_predictions)
    }
    
  } else if (object$method == "stacking") {
    # 堆叠集成预测
    # 获取基学习器的预测
    base_predictions <- sapply(object$models, function(model) {
      predict(model, newdata = newdata)
    })
    
    # 使用元学习器进行最终预测
    final_predictions <- predict(object$meta_model, newdata = base_predictions)
    
    return(final_predictions)
    
  } else {
    stop("不支持的集成方法")
  }
}

# 模型交叉验证
cross_validate_model <- function(model, X, y, n_folds = 5, 
                                 classification = TRUE, 
                                 metric = "Accuracy",
                                 random_seed = 42) {
  loginfo(sprintf("交叉验证: %d折, 指标: %s", n_folds, metric))
  
  set.seed(random_seed)
  
  # 创建折叠
  n <- nrow(X)
  folds <- cut(seq(1, n), breaks = n_folds, labels = FALSE)
  folds <- sample(folds)
  
  # 存储结果
  fold_scores <- numeric(n_folds)
  fold_predictions <- list()
  fold_actuals <- list()
  
  for (fold in 1:n_folds) {
    loginfo(sprintf("  折叠 %d/%d", fold, n_folds))
    
    # 划分训练集和验证集
    val_idx <- which(folds == fold)
    train_idx <- which(folds != fold)
    
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    X_val <- X[val_idx, , drop = FALSE]
    y_val <- y[val_idx]
    
    # 训练模型
    fold_model <- train(
      x = X_train,
      y = y_train,
      method = model$method,
      trControl = trainControl(method = "none"),
      tuneGrid = model$bestTune
    )
    
    # 预测
    if (classification) {
      predictions <- predict(fold_model, newdata = X_val)
      
      # 计算指标
      if (metric == "Accuracy") {
        score <- mean(predictions == y_val)
      } else if (metric == "Precision") {
        # 二分类的精确率
        if (length(unique(y_val)) == 2) {
          conf_matrix <- table(predictions, y_val)
          tp <- conf_matrix[2, 2]
          fp <- conf_matrix[2, 1]
          score <- tp / (tp + fp)
        } else {
          score <- NA
        }
      } else if (metric == "Recall") {
        # 二分类的召回率
        if (length(unique(y_val)) == 2) {
          conf_matrix <- table(predictions, y_val)
          tp <- conf_matrix[2, 2]
          fn <- conf_matrix[1, 2]
          score <- tp / (tp + fn)
        } else {
          score <- NA
        }
      } else if (metric == "F1") {
        # 二分类的F1分数
        if (length(unique(y_val)) == 2) {
          conf_matrix <- table(predictions, y_val)
          tp <- conf_matrix[2, 2]
          fp <- conf_matrix[2, 1]
          fn <- conf_matrix[1, 2]
          precision <- tp / (tp + fp)
          recall <- tp / (tp + fn)
          score <- 2 * precision * recall / (precision + recall)
        } else {
          score <- NA
        }
      } else {
        score <- NA
      }
    } else {
      # 回归问题
      predictions <- predict(fold_model, newdata = X_val)
      
      if (metric == "RMSE") {
        score <- sqrt(mean((predictions - y_val)^2))
      } else if (metric == "MAE") {
        score <- mean(abs(predictions - y_val))
      } else if (metric == "R2") {
        ss_res <- sum((predictions - y_val)^2)
        ss_tot <- sum((y_val - mean(y_val))^2)
        score <- 1 - ss_res/ss_tot
      } else {
        score <- NA
      }
    }
    
    fold_scores[fold] <- score
    fold_predictions[[fold]] <- predictions
    fold_actuals[[fold]] <- y_val
    
    loginfo(sprintf("    折叠 %d %s: %.4f", fold, metric, score))
  }
  
  # 汇总结果
  result <- list(
    fold_scores = fold_scores,
    mean_score = mean(fold_scores),
    sd_score = sd(fold_scores),
    predictions = unlist(fold_predictions),
    actuals = unlist(fold_actuals),
    metric = metric,
    n_folds = n_folds
  )
  
  loginfo(sprintf("交叉验证完成: 平均%s=%.4f (标准差=%.4f)", 
                  metric, result$mean_score, result$sd_score))
  
  return(result)
}

# 模型持久化
save_model <- function(model, filepath, metadata = NULL) {
  loginfo(sprintf("保存模型到: %s", filepath))
  
  # 创建模型对象
  model_obj <- list(
    model = model,
    metadata = list(
      saved_time = Sys.time(),
      model_type = model$modelType,
      algorithm = model$method,
      performance = if (!is.null(model$results)) {
        best_idx <- which.max(model$results[[model$metric %||% "Accuracy"]])
        model$results[best_idx, ]
      } else {
        NULL
      }
    )
  )
  
  # 添加自定义元数据
  if (!is.null(metadata)) {
    model_obj$metadata <- c(model_obj$metadata, metadata)
  }
  
  # 保存模型
  safe_save(model_obj, filepath)
  
  return(filepath)
}

# 加载模型
load_model <- function(filepath) {
  loginfo(sprintf("从文件加载模型: %s", filepath))
  
  model_obj <- safe_load(filepath)
  
  if (is.null(model_obj)) {
    logerror(sprintf("加载模型失败: %s", filepath))
    return(NULL)
  }
  
  loginfo(sprintf("模型加载成功: %s, 算法: %s", 
                  model_obj$metadata$model_type, model_obj$metadata$algorithm))
  
  return(model_obj$model)
}