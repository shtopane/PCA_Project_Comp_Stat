# TODO: UNDERSTAND THE TRANSFORMATIONS OF CODE
ds <- WORKING_DATA
enddate <- as.Date("1980/12/01")
vars <- NULL
screen <- FALSE
h <- 0

get_transformed_time_series <- function(ds,
                                        enddate,
                                        h = 0,
                                        vars = NULL){
  data <- window(ds$rawdata, end = enddate)
  names <- colnames(ds$rowdata)
  transcodes <- ds$tcodes
  
  if (!is.null(vars)) idx <- match(vars, names) else idx <- 1:ncol(data);
  data <- data[, idx]
  names <- names[idx]
  transcodes <- transcodes[idx]
  
  out <- c();
  
  for(i in 1:length(names)){
    out <- cbind(out, transform_series_by_transcode(data[,names[i]], transcodes[i], h=h))
  }
  
  print(ncol(out))
  print(nrow(out))
  
  colnames(out) <- names
  return(out)
}


transform_series_by_transcode <- function(X, trans, h=0){
  if (!all(class(X) == "ts")) stop("X must be time-series!");
  
  if(h == 0){
    return(switch(trans,
                  X, # transcode 1
                  diff(X), # transcode 2
                  diff(X, differences = 2), # transcode 3
                  log(X), # transcode 4
                  diff(log(X)), # transcode 5
                  diff(log(X), differences = 2) # transcode 6
                  ))
  } else {
    to_growth_rate <- function(X, is_log = FALSE){
      initial <- if(is_log == TRUE) diff(log(X)) : diff(X)
      yth <- initial * 0
      for(H in 1:h){
        yth <- yth + lag(initial, h - H+1)
      }
      return(yth)
    }
    
    return(switch(trans),
           lag(X, h), # transcode 1
           lag(X,h) - X, # transcode 2
           to_growth_rate(X), # transcode 3
           lag(log(X), h), # transcode 4,
           lag(log(X), h) - log(X), # transcode 5
           to_growth_rate(X, is_log = TRUE)
           )
  }
}