# Utility Functions
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

# Round to the nearest multiple
mround <- function(x,base){
    base*round(x/base)
}

# Used for quickly manipulating columns based on a pattern
grepl_column <- function(pattern, data) {
    colnames(data)[grepl(pattern, colnames(data))]
}

# Move columns based on specified location
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

# Rename a vector of columns
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

# Set NA elements to a specified value
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