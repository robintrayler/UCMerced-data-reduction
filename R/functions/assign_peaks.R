assign_peaks <- function(data, retention_time) {
  
  # error checking ------------------------------------------------------------
  # check for start and end columns in data 
  if(
    !(any(names(data) %in% 'start') & 
      any(names(data) %in% 'end'))
  ) {
    stop('Error: data must contain peak "start" and "end" columns')
  }
  
  # check that column names are correct
  if(any(
    !(names(retention_time) %in% c('peak_name', 'Rt')
    )
  )
  ) {
    stop('Error: retention_time column names must be exactly: \n 
         "Rt" and "peak_name"')
  }
  
  # check for missing retention times
  if(
    any(
      is.na(retention_time$Rt)
    )
  ) {
    warning('Some retention times are missing. \n 
            This may lead to mis-identified peaks')
  }
  
  # check for identical retention times 
  if(
    !(length(unique(retention_time$Rt)) == length(retention_time$Rt))
  ) {
    stop("Error: Identical retention times are not allowed")
  }
  
  # build a string for parsing ------------------------------------------------
  time <- retention_times %>% 
    str_glue_data("start < {Rt} & end > {Rt} ~ '{peak_name}'")
  
  # add the names -------------------------------------------------------------
  data <- data %>% 
    mutate(peak_name = case_when(!!!map(time, rlang::parse_expr)))
  
  return(data)
}



