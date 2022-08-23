make_data_correlated <- function(data, transformation_length, max_el = NULL){
  len <- length(data)
  max_el <- if(is.null(max_el)) max(data) else max_el
  
  # sample data at random spot and multiply by a sequence going from the max element of the data to its sd
  result <- data[sample(len, 1)] * seq(
    from = max_el,
    to = sd(data),
    len = transformation_length)

  return(result)
}


# Transform the erorr term using the formula
# sqrt(c * N) * et
# Where c
# c = (1 + r)˜c
# r is a number of factors?
get_transformed_error_term <- function(r, c_tilde, N, eps){
  c <- (1 + r) * c_tilde
  eps_transformed <- sqrt(c * N) * eps
  
  return(eps_transformed)
}

remove_columns <- function(data_frame, column_names_to_remove) {
  if (is.data.frame(data_frame) == FALSE) {
    # check if given object is of type time series
    has_ts_type <-
      unlist(lapply(class(data_frame), grepl, x = "ts"))
    
    if (any(has_ts_type)) {
      return(data_frame[, !colnames(data_frame) %in% column_names_to_remove])
    } else{
      stop("[data_frame] argument must be of type data_frame or time series")
    }
  }
  
  return(data_frame[, !names(data_frame) %in% column_names_to_remove])
}

get_transformed_time_series <- function(ds,
                                        enddate,
                                        h = 0,
                                        vars = NULL) {
  data <- window(ds$rawdata, end = enddate)
  names <- colnames(data)
  
  transcodes <- ds$tcodes
  
  if (!is.null(vars))
    idx <- match(vars, names)
  else
    idx <- 1:ncol(data)
  
  data <- data[, idx]
  names <- names[idx]
  transcodes <- transcodes[idx]
  
  out <- c()
  
  for (i in 1:length(names)) {
    out <-
      cbind(out,
            transform_series_by_transcode(data[, names[i]], transcodes[i], h = h))
  }
  
  colnames(out) <- names
  return(out)
}


transform_series_by_transcode <- function(X, trans, h = 0) {
  if (!all(class(X) == "ts"))
    stop("X must be time-series!")
  
  
  if (h == 0) {
    return(switch(
      trans,
      X,
      # transcode 1
      diff(X),
      # transcode 2
      diff(X, differences = 2),
      # transcode 3
      log(X),
      # transcode 4
      diff(log(X)),
      # transcode 5
      diff(log(X), differences = 2) # transcode 6
    ))
  } else {
    to_growth_rate <- function(X, is_log = FALSE) {
      initial <- if (is_log == TRUE)
        diff(log(X)) else diff(X)
      yth <- initial * 0
      for (H in 1:h) {
        yth <- yth + lag(initial, h - H + 1)
      }
      return(yth)
    }
    
    return(
      switch(trans),
      lag(X, h),
      # transcode 1
      lag(X, h) - X,
      # transcode 2
      to_growth_rate(X),
      # transcode 3
      lag(log(X), h),
      # transcode 4,
      lag(log(X), h) - log(X),
      # transcode 5
      to_growth_rate(X, is_log = TRUE)
    )
  }
}