get_percentage_of_number <- function(percentage, number){
  return(round(percentage / number, digits = 2))
}

string_date_to_vector_date <- function(string_date){
  
  # parse string to number
  result <- as.numeric(
    # split the string by -, get only numbers. [[1]] extracts the year, month, day
    strsplit(string_date, "-")[[1]]
  )[-3] # this removes the day. So, we'll get c(1971, 1) and not c(1971, 10, 1)
  
  return(result)
}