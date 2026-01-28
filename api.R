#!/usr/bin/env Rscript
# mlweb - R Plumber API
# 机器学习模型训练和预测API

library(plumber)
library(jsonlite)
library(logging)
library(caret)
library(randomForest)
library(xgboost)
library(recipes)
library(dplyr)
library(pROC)
library(ggplot2)
library(plotly)

# 设置工作目录
setwd(dirname(sys.frame(1)$ofile))

# 加载R脚本
source("r_service/utils.R")
source("r_service/data_processing.R")
source("r_service/train_model.R")
source("r_service/evaluate_model.R")
source("r_service/predict.R")

# 配置日志
basicConfig(level = "INFO")
addHandler(writeToFile, file = "logs/api.log", level = "DEBUG")

# 全局变量
models <- list()
preprocessors <- list()
training_history <- list()
api_stats <- list(
  total_requests = 0,
  successful_requests = 0,
  failed_requests = 0,
  start_time = Sys.time(),
  recent_requests = data.frame()
)

# API中间件
#* @filter log
function(req) {
  # 记录请求
  api_stats$total_requests <<- api_stats$total_requests + 1
  
  request_record <- data.frame(
    timestamp = Sys.time(),
    method = req$REQUEST_METHOD,
    endpoint = req$PATH_INFO,
    client_ip = req$REMOTE_ADDR,
    user_agent = req$HTTP_USER_AGENT %||% "Unknown",
    status = "received"
  )
  
  # 添加到最近请求记录
  if (nrow(api_stats$recent_requests) >= 1000) {
    api_stats$recent_requests <- api_stats$recent_requests[-1, ]
  }
  api_stats$recent_requests <<- rbind(api_stats$recent_requests, request_record)
  
  # 记录日志
  loginfo(sprintf("Request: %s %s from %s", 
                  req$REQUEST_METHOD, req$PATH_INFO, req$REMOTE_ADDR))
  
  # 继续处理请求
  forward()
}

#* @filter cors
function(req, res) {
  # 允许跨域请求
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  forward()
}

# API端点定义
#* @apiTitle mlweb机器学习API
#* @apiDescription 提供机器学习模型训练、评估和预测功能
#* @apiVersion 1.0.0
#* @apiTag 健康检查 API健康状态检查
#* @apiTag 数据 数据加载和处理
#* @apiTag 模型 模型训练和评估
#* @apiTag 预测 实时预测功能
#* @apiTag 管理 API管理和监控

#* 健康检查
#* @get /health
#* @tag 健康检查
function() {
  api_stats$successful_requests <<- api_stats$successful_requests + 1
  
  list(
    status = "healthy",
    service = "mlweb-api",
    version = "1.0.0",
    timestamp = Sys.time(),
    uptime = as.numeric(difftime(Sys.time(), api_stats$start_time, units = "secs")),
    models_loaded = length(models),
    memory_usage = paste(round(memory.size() / 1024 / 1024, 2), "MB"),
    r_version = R.version.string
  )
}

#* 获取API统计信息
#* @get /stats
#* @tag 管理
function() {
  list(
    total_requests = api_stats$total_requests,
    successful_requests = api_stats$successful_requests,
    failed_requests = api_stats$failed_requests,
    success_rate = ifelse(api_stats$total_requests > 0, 
                         api_stats$successful_requests / api_stats$total_requests, 
                         0),
    uptime = as.numeric(difftime(Sys.time(), api_stats$start_time, units = "secs")),
    recent_requests_count = nrow(api_stats$recent_requests)
  )
}

#* 加载示例数据
#* @param dataset 数据集名称
#* @post /load_example
#* @tag 数据
function(req, dataset = "iris") {
  tryCatch({
    # 加载数据
    data <- switch(
      dataset,
      "iris" = iris,
      "mtcars" = mtcars,
      "titanic" = {
        # 加载泰坦尼克号数据
        if (!requireNamespace("titanic", quietly = TRUE)) {
          install.packages("titanic", repos = "https://cloud.r-project.org")
        }
        library(titanic)
        titanic_train
      },
      "boston" = {
        # 波士顿房价数据
        if (!requireNamespace("MASS", quietly = TRUE)) {
          install.packages("MASS", repos = "https://cloud.r-project.org")
        }
        library(MASS)
        Boston
      },
      "diabetes" = {
        # 糖尿病数据
        if (!requireNamespace("lars", quietly = TRUE)) {
          install.packages("lars", repos = "https://cloud.r-project.org")
        }
        library(lars)
        data(diabetes)
        cbind(as.data.frame(diabetes$x), y = diabetes$y)
      },
      stop("不支持的示例数据集")
    )
    
    api_stats$successful_requests <<- api_stats$successful_requests + 1
    
    list(
      success = TRUE,
      dataset = dataset,
      n_rows = nrow(data),
      n_cols = ncol(data),
      columns = colnames(data),
      data = data
    )
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("加载示例数据失败:", e$message))
    
    list(
      success = FALSE,
      error = e$message
    )
  })
}

#* 数据质量检查
#* @param data 数据
#* @post /check_data_quality
#* @tag 数据
function(req, data) {
  tryCatch({
    # 解析数据
    df <- as.data.frame(data)
    
    # 检查数据质量
    quality_metrics <- check_data_quality(df)
    
    api_stats$successful_requests <<- api_stats$successful_requests + 1
    
    list(
      success = TRUE,
      metrics = quality_metrics
    )
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("数据质量检查失败:", e$message))
    
    list(
      success = FALSE,
      error = e$message
    )
  })
}

#* 数据预处理
#* @param data 原始数据
#* @param target_col 目标列名
#* @param config 预处理配置
#* @post /preprocess_data
#* @tag 数据
function(req, data, target_col, config = NULL) {
  tryCatch({
    # 解析数据
    df <- as.data.frame(data)
    
    # 默认配置
    if (is.null(config)) {
      config <- list(
        imputation_method = "median",
        handle_outliers = TRUE,
        scale_features = TRUE,
        encode_categorical = TRUE,
        test_size = 0.2,
        random_state = 42
      )
    }
    
    # 数据清洗
    cleaned_data <- clean_data(df, config)
    
    # 数据分割
    split_result <- split_data(cleaned_data, target_col, 
                               test_size = config$test_size, 
                               seed = config$random_state)
    
    # 创建预处理管道
    recipe <- create_preprocessing_recipe(split_result$train, target_col, config)
    
    # 应用预处理
    train_processed <- apply_preprocessing(recipe, split_result$train)
    test_processed <- apply_preprocessing(recipe, split_result$test)
    
    # 保存预处理管道
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    preprocessor_name <- paste0("preprocessor_", timestamp)
    
    if (!dir.exists("preprocessors")) {
      dir.create("preprocessors", recursive = TRUE)
    }
    saveRDS(recipe, file.path("preprocessors", paste0(preprocessor_name, ".rds")))
    
    preprocessors[[preprocessor_name]] <<- recipe
    
    api_stats$successful_requests <<- api_stats$successful_requests + 1
    
    list(
      success = TRUE,
      train_data = train_processed,
      test_data = test_processed,
      preprocessor_name = preprocessor_name,
      feature_names = setdiff(colnames(train_processed), target_col)
    )
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("数据预处理失败:", e$message))
    
    list(
      success = FALSE,
      error = e$message
    )
  })
}

#* 训练多个模型
#* @param train_data 训练数据
#* @param target_col 目标列名
#* @param config 训练配置
#* @post /train_models
#* @tag 模型
function(req, train_data, target_col, config = NULL) {
  tryCatch({
    # 解析数据
    df <- as.data.frame(train_data)
    
    # 默认配置
    if (is.null(config)) {
      config <- list(
        models_to_train = c("glm", "rf", "xgb"),
        cv_folds = 5,
        metric = "Accuracy",
        classification = TRUE
      )
    }
    
    # 训练模型
    training_result <- train_multiple_models(df, target_col, config)
    
    # 选择最佳模型
    best_model_info <- select_best_model(training_result$models, config$metric)
    
    # 保存模型
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    for (model_name in names(training_result$models)) {
      model <- training_result$models[[model_name]]
      model_filename <- paste0(model_name, "_", timestamp, ".rds")
      
      if (!dir.exists("models")) {
        dir.create("models", recursive = TRUE)
      }
      saveRDS(model, file.path("models", model_filename))
      
      # 保存到内存
      models[[model_name]] <<- model
    }
    
    # 保存训练历史
    training_history[[timestamp]] <<- list(
      models = names(training_result$models),
      best_model = best_model_info$name,
      performance = training_result$performance
    )
    
    api_stats$successful_requests <<- api_stats$successful_requests + 1
    
    # 准备返回结果
    performance_summary <- list()
    for (model_name in names(training_result$models)) {
      model <- training_result$models[[model_name]]
      
      if (config$metric %in% colnames(model$results)) {
        best_score <- max(model$results[[config$metric]], na.rm = TRUE)
        
        performance_summary[[model_name]] <- list(
          algorithm = model$method,
          accuracy = ifelse("Accuracy" %in% colnames(model$results), 
                           max(model$results$Accuracy, na.rm = TRUE), NA),
          precision = ifelse("Precision" %in% colnames(model$results), 
                            max(model$results$Precision, na.rm = TRUE), NA),
          recall = ifelse("Recall" %in% colnames(model$results), 
                         max(model$results$Recall, na.rm = TRUE), NA),
          f1 = ifelse("F1" %in% colnames(model$results), 
                      max(model$results$F1, na.rm = TRUE), NA),
          auc = ifelse("ROC" %in% colnames(model$results), 
                       max(model$results$ROC, na.rm = TRUE), NA),
          training_time = as.numeric(model$times$everything[3])
        )
      }
    }
    
    list(
      success = TRUE,
      model_performance = performance_summary,
      best_model = list(
        name = best_model_info$name,
        algorithm = best_model_info$model$method,
        accuracy = ifelse("Accuracy" %in% colnames(best_model_info$model$results), 
                         max(best_model_info$model$results$Accuracy, na.rm = TRUE), NA),
        training_time = as.numeric(best_model_info$model$times$everything[3])
      ),
      timestamp = timestamp
    )
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("模型训练失败:", e$message))
    
    list(
      success = FALSE,
      error = e$message
    )
  })
}

#* 评估模型
#* @param model_name 模型名称
#* @param test_data 测试数据
#* @param target_col 目标列名
#* @post /evaluate_model
#* @tag 模型
function(req, model_name, test_data, target_col) {
  tryCatch({
    # 检查模型是否存在
    if (!model_name %in% names(models)) {
      stop(sprintf("模型 '%s' 不存在", model_name))
    }
    
    model <- models[[model_name]]
    
    # 解析测试数据
    test_df <- as.data.frame(test_data)
    
    # 评估配置
    config <- list(
      classification = model$modelType == "Classification"
    )
    
    # 评估模型
    evaluation_result <- evaluate_model(model, test_df, target_col, config)
    
    api_stats$successful_requests <<- api_stats$successful_requests + 1
    
    list(
      success = TRUE,
      metrics = evaluation_result$metrics,
      plots = evaluation_result$plots
    )
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("模型评估失败:", e$message))
    
    list(
      success = FALSE,
      error = e$message
    )
  })
}

#* 单样本预测
#* @param model_name 模型名称
#* @param input_data 输入数据
#* @param preprocessor_name 预处理管道名称
#* @post /predict
#* @tag 预测
function(req, model_name, input_data, preprocessor_name = NULL) {
  start_time <- Sys.time()
  
  tryCatch({
    # 检查模型是否存在
    if (!model_name %in% names(models)) {
      stop(sprintf("模型 '%s' 不存在", model_name))
    }
    
    model <- models[[model_name]]
    
    # 加载预处理管道
    preprocessor <- NULL
    if (!is.null(preprocessor_name)) {
      if (preprocessor_name %in% names(preprocessors)) {
        preprocessor <- preprocessors[[preprocessor_name]]
      } else {
        # 尝试从文件加载
        preprocessor_file <- file.path("preprocessors", paste0(preprocessor_name, ".rds"))
        if (file.exists(preprocessor_file)) {
          preprocessor <- readRDS(preprocessor_file)
          preprocessors[[preprocessor_name]] <<- preprocessor
        }
      }
    }
    
    # 执行预测
    result <- predict_single(model, input_data, preprocessor)
    
    api_stats$successful_requests <<- api_stats$successful_requests + 1
    
    list(
      success = TRUE,
      result = result
    )
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("预测失败:", e$message))
    
    list(
      success = FALSE,
      result = list(
        prediction = NA,
        error = e$message,
        response_time = as.numeric(Sys.time() - start_time)
      )
    )
  })
}

#* 批量预测
#* @param model_name 模型名称
#* @param batch_data 批量数据
#* @param preprocessor_name 预处理管道名称
#* @post /batch_predict
#* @tag 预测
function(req, model_name, batch_data, preprocessor_name = NULL) {
  start_time <- Sys.time()
  
  tryCatch({
    # 检查模型是否存在
    if (!model_name %in% names(models)) {
      stop(sprintf("模型 '%s' 不存在", model_name))
    }
    
    model <- models[[model_name]]
    
    # 加载预处理管道
    preprocessor <- NULL
    if (!is.null(preprocessor_name)) {
      if (preprocessor_name %in% names(preprocessors)) {
        preprocessor <- preprocessors[[preprocessor_name]]
      } else {
        # 尝试从文件加载
        preprocessor_file <- file.path("preprocessors", paste0(preprocessor_name, ".rds"))
        if (file.exists(preprocessor_file)) {
          preprocessor <- readRDS(preprocessor_file)
          preprocessors[[preprocessor_name]] <<- preprocessor
        }
      }
    }
    
    # 执行批量预测
    result <- predict_batch(model, batch_data, preprocessor)
    
    api_stats$successful_requests <<- api_stats$successful_requests + 1
    
    list(
      success = TRUE,
      result = result
    )
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("批量预测失败:", e$message))
    
    list(
      success = FALSE,
      result = list(
        predictions = list(),
        error = e$message,
        total_time = as.numeric(Sys.time() - start_time)
      )
    )
  })
}

#* 文件预测
#* @param model_name 模型名称
#* @param file_path 文件路径
#* @param preprocessor_name 预处理管道名称
#* @post /file_predict
#* @tag 预测
function(req, model_name, file_path, preprocessor_name = NULL) {
  start_time <- Sys.time()
  
  tryCatch({
    # 检查模型是否存在
    if (!model_name %in% names(models)) {
      stop(sprintf("模型 '%s' 不存在", model_name))
    }
    
    model <- models[[model_name]]
    
    # 加载预处理管道
    preprocessor <- NULL
    if (!is.null(preprocessor_name)) {
      if (preprocessor_name %in% names(preprocessors)) {
        preprocessor <- preprocessors[[preprocessor_name]]
      } else {
        # 尝试从文件加载
        preprocessor_file <- file.path("preprocessors", paste0(preprocessor_name, ".rds"))
        if (file.exists(preprocessor_file)) {
          preprocessor <- readRDS(preprocessor_file)
          preprocessors[[preprocessor_name]] <<- preprocessor
        }
      }
    }
    
    # 从文件预测
    result <- predict_from_file(model, file_path, preprocessor)
    
    api_stats$successful_requests <<- api_stats$successful_requests + 1
    
    list(
      success = TRUE,
      result = result
    )
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("文件预测失败:", e$message))
    
    list(
      success = FALSE,
      result = list(
        predictions = list(),
        error = e$message,
        total_time = as.numeric(Sys.time() - start_time)
      )
    )
  })
}

#* 解释模型
#* @param model_name 模型名称
#* @param sample_data 样本数据
#* @post /explain_model
#* @tag 模型
function(req, model_name, sample_data) {
  tryCatch({
    # 检查模型是否存在
    if (!model_name %in% names(models)) {
      stop(sprintf("模型 '%s' 不存在", model_name))
    }
    
    model <- models[[model_name]]
    
    # 解释预测
    explanation <- explain_prediction(model, sample_data)
    
    api_stats$successful_requests <<- api_stats$successful_requests + 1
    
    list(
      success = TRUE,
      explanation = explanation
    )
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("模型解释失败:", e$message))
    
    list(
      success = FALSE,
      explanation = list(error = e$message)
    )
  })
}

#* 部署模型
#* @param model_name 模型名称
#* @param deployment_type 部署类型
#* @param config 部署配置
#* @post /deploy_model
#* @tag 管理
function(req, model_name, deployment_type = "api", config = NULL) {
  tryCatch({
    # 检查模型是否存在
    if (!model_name %in% names(models)) {
      stop(sprintf("模型 '%s' 不存在", model_name))
    }
    
    model <- models[[model_name]]
    
    if (deployment_type == "api") {
      # API部署 - 已通过Plumber提供
      
      api_stats$successful_requests <<- api_stats$successful_requests + 1
      
      list(
        success = TRUE,
        message = "模型已通过Plumber API部署",
        api_info = list(
          endpoints = c("/predict", "/batch_predict", "/health"),
          documentation = "__swagger__/"
        )
      )
    } else if (deployment_type == "file") {
      # 保存为文件
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename <- paste0(model_name, "_", timestamp, ".rds")
      
      if (!dir.exists("deployments")) {
        dir.create("deployments", recursive = TRUE)
      }
      
      saveRDS(model, file.path("deployments", filename))
      
      api_stats$successful_requests <<- api_stats$successful_requests + 1
      
      list(
        success = TRUE,
        message = "模型已保存为文件",
        filename = filename,
        filepath = file.path("deployments", filename)
      )
    } else {
      stop(sprintf("不支持的部署类型: %s", deployment_type))
    }
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("模型部署失败:", e$message))
    
    list(
      success = FALSE,
      error = e$message
    )
  })
}

#* 导出模型
#* @param model_name 模型名称
#* @param export_format 导出格式
#* @post /export_model
#* @tag 管理
function(req, model_name, export_format = "rds") {
  tryCatch({
    # 检查模型是否存在
    if (!model_name %in% names(models)) {
      stop(sprintf("模型 '%s' 不存在", model_name))
    }
    
    model <- models[[model_name]]
    
    if (export_format == "rds") {
      # 导出为RDS格式
      model_data <- serialize(model, NULL)
      
      api_stats$successful_requests <<- api_stats$successful_requests + 1
      
      list(
        success = TRUE,
        format = "rds",
        file_content = rawToChar(model_data),
        filename = paste0(model_name, ".rds")
      )
    } else if (export_format == "pmml") {
      # 导出为PMML格式（需要pmml包）
      if (!requireNamespace("pmml", quietly = TRUE)) {
        install.packages("pmml", repos = "https://cloud.r-project.org")
      }
      library(pmml)
      
      pmml_model <- pmml(model)
      pmml_string <- toString(pmml_model)
      
      api_stats$successful_requests <<- api_stats$successful_requests + 1
      
      list(
        success = TRUE,
        format = "pmml",
        file_content = pmml_string,
        filename = paste0(model_name, ".pmml")
      )
    } else {
      stop(sprintf("不支持的导出格式: %s", export_format))
    }
  }, error = function(e) {
    api_stats$failed_requests <<- api_stats$failed_requests + 1
    logerror(paste("模型导出失败:", e$message))
    
    list(
      success = FALSE,
      error = e$message
    )
  })
}

#* 获取可用模型列表
#* @get /models
#* @tag 管理
function() {
  api_stats$successful_requests <<- api_stats$successful_requests + 1
  
  list(
    success = TRUE,
    models = names(models),
    count = length(models),
    details = lapply(models, function(model) {
      list(
        algorithm = model$method,
        type = model$modelType,
        trained_time = model$times$everything[1]
      )
    })
  )
}

#* 获取系统信息
#* @get /system_info
#* @tag 管理
function() {
  api_stats$successful_requests <<- api_stats$successful_requests + 1
  
  list(
    r_version = R.version.string,
    platform = R.version$platform,
    memory_limit = memory.limit(),
    memory_usage = memory.size(),
    loaded_packages = .packages(),
    working_directory = getwd(),
    api_start_time = api_stats$start_time,
    api_uptime = as.numeric(difftime(Sys.time(), api_stats$start_time, units = "secs"))
  )
}

#* @plumber
function(pr) {
  # 配置Plumber
  pr %>%
    # 启用Swagger文档
    pr_set_docs(TRUE) %>%
    # 设置API规范
    pr_set_api_spec(function(spec) {
      spec$info$description <- "mlweb机器学习API - 提供机器学习模型训练、评估和预测功能"
      spec$info$contact <- list(
        name = "API支持",
        email = "support@mlweb.example.com"
      )
      spec$info$license <- list(
        name = "MIT",
        url = "https://opensource.org/licenses/MIT"
      )
      spec$servers <- list(
        list(url = "http://localhost:8000", description = "开发服务器"),
        list(url = "https://api.mlweb.example.com", description = "生产服务器")
      )
      spec
    }) %>%
    # 添加请求前钩子
    pr_hook("preroute", function(req) {
      req$start_time <- Sys.time()
      NULL
    }) %>%
    # 添加响应后钩子
    pr_hook("postroute", function(req, res) {
      elapsed <- as.numeric(Sys.time() - req$start_time)
      
      # 更新请求记录状态
      if (nrow(api_stats$recent_requests) > 0) {
        last_idx <- nrow(api_stats$recent_requests)
        api_stats$recent_requests$status[last_idx] <<- "completed"
        api_stats$recent_requests$response_time[last_idx] <<- elapsed
        api_stats$recent_requests$status_code[last_idx] <<- res$status
      }
      
      # 记录响应日志
      loginfo(sprintf("Response: %s %s -> %d (%.3fs)", 
                      req$REQUEST_METHOD, req$PATH_INFO, res$status, elapsed))
      NULL
    })
}

# 启动API服务
if (sys.nframe() == 0) {
  # 解析命令行参数
  args <- commandArgs(trailingOnly = TRUE)
  
  port <- 8000
  host <- "127.0.0.1"
  
  for (arg in args) {
    if (grepl("^--port=", arg)) {
      port <- as.numeric(sub("^--port=", "", arg))
    } else if (grepl("^--host=", arg)) {
      host <- sub("^--host=", "", arg)
    }
  }
  
  # 确保目录存在
  dirs <- c("logs", "models", "preprocessors", "deployments", "predictions")
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  
  # 启动日志
  loginfo(sprintf("启动mlweb API服务 (端口: %d, 主机: %s)", port, host))
  loginfo(sprintf("工作目录: %s", getwd()))
  loginfo(sprintf("R版本: %s", R.version.string))
  
  # 启动Plumber API
  pr <- plumb("plumber_api.R")
  pr$run(host = host, port = port, swagger = TRUE)
}