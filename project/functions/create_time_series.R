library("gdata")
library("Hmisc")
library("readxl") # read Excel files

create_time_series <- function() {
  # Exclude the Date column, the other two are just redundant
  montly_data_cols_to_remove_names <- c("Date", "...110", "...111")
  
  data_by_month <- read_excel("data/data/hendry_data.xls")
  # Get indeces of column names. unlist() is to transform list to a vector
  cols_to_remove <- unlist(lapply(
    montly_data_cols_to_remove_names,
    grep,
    colnames(data_by_month)
  ))
  data_by_month <- data_by_month[, -cols_to_remove]
  
  # Get other metadata
  long_desc <- data_by_month[1,]
  short_desc <- data_by_month[2,]
  tcodes <- tcode <- data_by_month[4,]
  include <- data_by_month[6,]
  catcode <- data_by_month[7,]
  scale <- data_by_month[9,]
  names <- colnames(data_by_month)
  
  # Exclude the metadata(first 10 rows of the data set)
  data_by_month <- data_by_month[10:nrow(data_by_month), ]
  
  # create Time-Series Object
  data_by_month <- ts(data.matrix(data_by_month),
       start = c(1959, 1),
       frequency = 12)
  
  # Create quarterly data
  data_by_month <-
    aggregate(data_by_month, nfrequency = 4, FUN = mean)
  rawdata <- data_by_month
  
  idx <- order(as.numeric(catcode))
  names <- names[idx]
  include <- as.numeric(include[idx])
  tcodes <- as.numeric(tcodes[idx])
  rawdata <- rawdata[1:nrow(rawdata), idx]
  colnames(rawdata) <- names
  short_desc <- as.character(short_desc[idx])
  long_desc <- long_desc[idx]
  long_desc <- capitalize(trim(tolower(as.character(long_desc))))
  rawdata <- ts(rawdata, start = c(1959, 1), freq = 4)
  
  return(
    list(
      include = include,
      rawdata = rawdata,
      tcodes = tcodes,
      shortname = short_desc,
      longnames = long_desc
    )
  )
}

WORKING_DATA <- create_time_series()
save(WORKING_DATA, file = "./data/data/WORKING_DATA.rda")
