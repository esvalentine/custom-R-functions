#### EDWARD'S CUSTOM FUNCTIONS
load_packages <- function(package_vector, edward = FALSE) {
    if (edward == TRUE) {
        package_vector <- c("tidyr", "dplyr", "ggplot2", 
                            "GGally", "lubridate", "foreach",
                            "doParallel", "xgboost", "Matrix",
                            "caret", "forecast", "RODBC")
    }
    for (i in package_vector) {
        if(!require(i, character.only=TRUE)) install.packages(i)
        require(i, character.only = TRUE)
    }
}

mround <- function(x,base){
  base*round(x/base)
}

apply_ch_model <- function(data, column_name) {
    wm_fit <- readRDS("Model Training/wm_fit_rf.rds")
    kr_fit <- readRDS("Model Training/kr_fit_rf.rds")
    ch_fit <- readRDS("Model Training/ch_fit_rf.rds")
    wm_clmn <- grepl_column("wm_", data)
    kr_clmn <- grepl_column("kr_", data)
    for (i in 1:nrow(data)) {
        #message(paste0("iteration: ", i))
        if (all(!is.na(data[i, wm_clmn])) & length(kr_clmn) > 0) {
            data[i, column_name] <- predict(wm_fit, data[i, ])
        } else if (all(!is.na(data[i, kr_clmn])) & length(kr_clmn) > 0) {
            data[i, column_name] <- predict(kr_fit, data[i, ])
        } else {
            data[i, column_name] <- predict(ch_fit, data[i, ])
        }
    }
    return(data)
}

grepl_column <- function(pattern, data) {
    colnames(data)[grepl(pattern, colnames(data))]
}

moveMe <- function(data, tomove, where = "last", ba = NULL) {
    temp <- setdiff(names(data), tomove)
    x <- switch(
        where,
        first = data[c(tomove, temp)],
        last = data[c(temp, tomove)],
        before = {
            if (is.null(ba)) stop("must specify ba column")
            if (length(ba) > 1) stop("ba must be a single character string")
            data[append(temp, values = tomove, after = (match(ba, temp)-1))]
        },
        after = {
            if (is.null(ba)) stop("must specify ba column")
            if (length(ba) > 1) stop("ba must be a single character string")
            data[append(temp, values = tomove, after = (match(ba, temp)))]
        })
    x
}

# VECTORIZED NORMALIZATION OF VARIABLES
norm_vars <- function(data, columns) {
  temp <- as.data.frame(data)
  for (i in columns) {
    temp_sd <- sd(temp[ , i])
    temp_mean <- mean(temp[ , i])
    if (class(i) == "character"){
      temp_col <- paste0("norm", "_", i)
    } else {
      temp_col <- paste0("norm", "_", colnames(temp)[i])
    }
    temp[ , temp_col] <- (temp[ , i] - temp_mean) / temp_sd
  }
  return(temp)
}

# SIMPLE NORMALIZATION FOR USE IN DPLYR MUTATE CALLS
scale_var <- function(x) {
  norm <- (x - mean(x)) / sd(x)
  return(norm)
}

# EITHER AUTO SELECT THE # OF CLUSTERS FOR K-MEANS BASED ON A % CHANGE THRESHOLD
# OR PRODUCE A PLOT TO DO SO
cluster_select <- function(matrix, threshold, type) {
  pct_change <- 1
  centers <- 2
  temp_cluster <- kmeans(matrix, centers = centers, nstart = 25)
  prev_stat <- temp_cluster$tot.withinss
  elbow_data <- data.frame(centers = centers, tot_withinss = prev_stat)
  while (pct_change >= threshold) {
    centers <- centers + 1
    temp_cluster <- kmeans(matrix, centers = centers, iter.max = 30, nstart = 25)
    pct_change <- (prev_stat - temp_cluster$tot.withinss) / prev_stat
    prev_stat <- temp_cluster$tot.withinss
    elbow_temp <- data.frame(centers = centers, tot_withinss = prev_stat)
    elbow_data <- rbind(elbow_data, elbow_temp)
  }
  switch(type,
         "plot" = return(ggplot2::ggplot(elbow_data, aes(x = centers, y = tot_withinss)) +
                           geom_line() +
                           labs(title = "Elbow Cluster Selection")),
         "fit" = return(temp_cluster)
  )
}

# RENAME COLUMNS AS A VECTOR
col_rename <- function(x, ...) {
  options(error = NULL)
  input_list <- list(...)
  if (all(unlist(lapply(input_list, function(x) is.character(x)))) == FALSE) {
    stop("Input must be quoted strings, not objects")
  }
  for (i in 1:length(input_list)) {
    if(any(colnames(x) == names(input_list)[i]) == FALSE) {
      stop(paste0("The column \"",
                  names(input_list)[i], 
                  "\" does not match any of the columns in the specified data frame"))
    }
    colnames(x)[colnames(x) == names(input_list)[i]] <- input_list[[i]]
  }
  return(x)
}

# INTERACT ONE OR MANY VARIABLES
interact <- function(data, leftSide, rightSide) {
  if (class(leftSide) == "character" 
      | class(leftSide) == "integer"
      | class(leftSide) == "numeric") {
    if (class(leftSide) == "character") {
      left_names <- colnames(data)[which(names(data) %in% leftSide)]
    } else {
      left_names <- colnames(data)[leftSide]
    }
  } else {
    stop("leftSide column vectors must be of class character or numeric")
  }
  if (class(rightSide) == "character" 
      | class(rightSide) == "integer"
      | class(rightSide) == "numeric") {
    if (class(rightSide) == "character") {
      right_names <- colnames(data)[which(names(data) %in% rightSide)]
    } else {
      right_names <- colnames(data)[rightSide]
    }
  } else {
    stop("leftSide column vectors must be of class character or numeric")
  }
  f <- vector()
  for (i in left_names) {
    f <- append(f,
                paste0(i,
                       "X",
                       right_names,
                       "=",
                       i,
                       "*",
                       right_names))
  }
  s <- paste0(f, collapse = ", ")
  q <- quote(dplyr::mutate(data, z = s))
  eval(parse(text = sub("z = s", s, deparse(q))))
}

# PARALLELIZED DUMMY ENCODING
par_dummy <- function(data, variables, id) {
  require(foreach)
  require(doParallel)
  require(dplyr)
  require(tidyr)
  cols <- colnames(data)
  hold <- data[ , !cols %in% variables]
  cores <- detectCores()
  cl <- makeCluster(cores[1] - 2)
  registerDoParallel(cl)
  final_data <- foreach(i = 1:length(variables),
                        .combine = data.frame,
                        .packages = c("dplyr", "tidyr")
  ) %dopar% {
    v <- variables[i]
    data %>%
      mutate_("y" = v) %>%
      mutate(yesno = 1,
             y = paste0(v,"_", y)) %>%
      spread_("y", "yesno", fill = 0) %>%
      select_(.dots = paste0("-", cols))
  }
  stopCluster(cl)
  final_data <- bind_cols(hold, final_data)
  return(final_data)
}

# CLASSIC DUMMY ENCODING
dummy <- function(data, variables, id) {
  require(foreach)
  require(doParallel)
  require(dplyr)
  require(tidyr)
  cols <- colnames(data)
  hold <- data[ , !cols %in% variables]
  final_data <- foreach(i = 1:length(variables)) %do% {
    v <- variables[i]
    data %>%
      mutate_("y" = v) %>%
      mutate(yesno = 1,
             y = paste0(v,"_", y)) %>%
      spread_("y", "yesno", fill = 0) %>%
      select_(.dots = paste0("-", cols))
  }
  final_data <- bind_cols(hold, final_data)
  return(final_data)
}

# SET NA DATA TO A SPECIFIED VALUE IN A VECTOR OF VARIABLES/COLUMNS
set_na <- function(data, columns, value) {
  temp <- data
  for (i in  columns) {
    t_parse <- paste0("ifelse(is.na(", "x", "),",
                      value, ",",
                      "x", ")")
    temp <- temp %>%
      mutate_(.dots = setNames(lazyeval::interp(t_parse, 
                                                x = as.name(i)), 
                               i))
  }
  return(temp)
}
# CHECKS IF THE COLUMNS EXIST, IF NOT ADDS THEM WITH THE SPECIFIED VALUE
add_columns <- function(data, columns_to_check, value) {
    require(dplyr)
    temp <- data
    for (name in columns_to_check) {
        if (name %in% colnames(data)) {
            next
        } else {
            t_parse <- paste0("x", " = " , value)
            temp <- temp %>%
                mutate_(.dots = setNames(lazyeval::interp(t_parse, 
                                                          x = as.name(name)), 
                                         name))
        }
    }
    return(temp)
}
# XGB GRIDSEARCH TUNING
# XGB GRIDSEARCH TUNING
xgb_tune <- function(data_y, data_x, subsample, colsample, depth, ntrees, lambda) {
    require(foreach)
    require(doParallel)
    require(dplyr)
    require(tidyr)
    paramGrid <- expand.grid(ss = subsample,
                             cs = colsample,
                             dpth = depth,
                             ntrees = ntrees)
    cores <- detectCores()
    cl <- makeCluster(cores[1] - 2)
    registerDoParallel(cl)
    final_data <- foreach(i = 1:nrow(paramGrid),
                          .combine = rbind,
                          .packages = c("dplyr", "tidyr", "xgboost", "Matrix")
    ) %dopar% {
        ss <- paramGrid[i, "ss"]
        cs <- paramGrid[i, "cs"]
        dpth <- paramGrid[i, "dpth"]
        ntrees <- paramGrid[i, "ntrees"]
        xgb_model_temp <- xgb.cv(data =  data_x,
                                 label = data_y,
                                 nrounds = ntrees, 
                                 nfold = 5, 
                                 nthread = 1,
                                 showsd = TRUE, 
                                 metrics = "rmse", 
                                 verbose = TRUE,
                                 lambda = lambda,
                                 eval_metric = "rmse",
                                 objective = "reg:linear", 
                                 eta = 2/ntrees,                               
                                 subsample = ss, 
                                 colsample_bytree = cs,
                                 max_depth = dpth,
                                 print_every_n = 10)
        eval_log <- as.data.frame(xgb_model_temp["evaluation_log"])
        rmse <- tail(eval_log$evaluation_log.test_rmse_mean, 1)
        data.frame(rmse = rmse,
                   subsample = ss,
                   colsample = cs,
                   tree_depth = dpth,
                   n_trees = ntrees
        )
    }
    # select the model with the lowest RMSE
    final_data <- final_data %>%
        arrange(rmse) %>%
        top_n(1, desc(rmse))
    return(final_data)
    stopCluster(cl)
}

# automated gridSearch forecast tuning
forecast_tune <- function(train_ts, test_y, p, q, ft, lambda = 1) {
    require(foreach)
    require(doParallel)
    require(dplyr)
    require(tidyr)
    require(forecast)
    paramGrid <- expand.grid(p = p,
                             q = q,
                             ft = ft)
    cores <- detectCores()
    cl <- makeCluster(cores[1] - 1)
    registerDoParallel(cl)
    final_data <- foreach(i = 1:nrow(paramGrid),
                          .combine = rbind,
                          .packages = c("forecast"),
                          .errorhandling = "remove"
    ) %dopar% {
        p <- paramGrid[i, "p"]
        q <- paramGrid[i, "q"]
        ft <- paramGrid[i, "ft"]
        rmse <- function(actual, pred) {
            rmse <- sqrt(mean(abs(actual - pred)^2))
            return(rmse)
        }
        fit <- Arima(train_ts, 
                     xreg = fourier(train_ts, K = ft),
                     order = c(p, 1, q),
                     lambda = lambda)
        forecast_fit <- forecast(fit, 
                                 xreg = fourier(train_ts, K = ft, h = length(test_y)),
                                 h = length(test_y))
        data.frame(rmse = rmse(test_y,
                               forecast_fit$mean),
                   p = p,
                   q = q,
                   ft = ft)
    }
    stopCluster(cl)
    # select the model with the lowest RMSE
    final_data <- final_data %>%
        arrange(rmse) %>%
        top_n(1, desc(rmse))
    return(final_data)
}

# EVALUATION METRICS
mape <- function(actual,pred) {
    mape <- mean(abs((actual - pred)/actual))*100
    return (mape)
}
rmse <- function(actual, pred) {
    rmse <- sqrt(mean(abs(actual - pred)^2))
    return(rmse)
}