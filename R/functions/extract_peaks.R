extract_peaks <- function(data, 
                          rentention_time) {
  data %>% 
    filter(start < rentention_time & rentention_time < end) %>% 
    return()
}