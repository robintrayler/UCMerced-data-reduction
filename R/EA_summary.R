# load required packages ------------------------------------------------------
library(tidyverse)
library(viridis)
library(lubridate)
theme_set(theme_minimal())
# define Z score function -----------------------------------------------------
zscore <- function(x){
  Z <- abs(mean(x, na.rm = T) - x) / sd(x, na.rm = T)
  return(Z)
}

# define file path ------------------------------------------------------------
file_path <- "~/Box Sync/Data Repository/EA/Corrected/2021/"

# reference materials to check ------------------------------------------------
standards <- c('USGS 40', 
               'USGS 41a', 
               'costech acetanilide',
               'EA sed',
               'peach',
               'Peach', 
               'MB squid',
               'Mb squid')

colors <- viridis(n = length(standards) + 1, end = 0.8, option = 'D')
names(colors) <- c(standards, TRUE)
# get a list of all corrected data files --------------------------------------
files <- list.files(file_path, 
                    pattern = '_corrected.csv' , 
                    full.names = TRUE, 
                    recursive = TRUE)

read_corrected <- function(file_path) {
  date <- ymd(str_sub(basename(file_path), start = 3, end = 10))
  file_path %>% read_csv() %>% 
    select(Identifier_1, 
           sample, 
           d13C_corrected, 
           d15N_corrected, 
           N_wt_percent, 
           C_wt_percent) %>% 
    filter(sample %in% standards) %>% 
    add_column(date = date) %>% 
    return()
}

# read in all the files 
storage <- files %>% 
  map(read_corrected) %>% 
  reduce(rbind) %>% 
  mutate(C_wt_percent = as.numeric(C_wt_percent),
         N_wt_percent = as.numeric(N_wt_percent))

# fix some miss named samples 
storage <- storage %>% 
  mutate(sample = case_when(sample == 'Peach' ~ 'peach', 
                            sample == 'Mb squid' ~'MB squid',
                            TRUE ~ sample)) %>% 
  group_by(sample) %>% 
  mutate(d13C_z = zscore(d13C_corrected),
         d15N_z = zscore(d15N_corrected)) %>% 
  filter(d13C_z < 2) %>% 
  filter(d15N_z < 2) %>% 
  ungroup()



storage %>%  ggplot(mapping = aes(x = date, 
                                  y = d13C_corrected,
                                  color = sample)) + 
  geom_point() + 
  theme(legend.position = 'top') + 
  facet_wrap(~sample, scales = "free_y")





storage %>%  ggplot(mapping = aes(x = date, 
                                  y = d13C_corrected,
                                  color = sample)) + 
  geom_point() + 
  theme(legend.position = 'top')  + geom_smooth() + 
  ylim(c(-30, -17.5))



storage %>% 
  group_by(sample) %>% 
  summarise(d13C_mean = mean(d13C_corrected, na.rm = TRUE),
            d13C_sd = sd(d13C_corrected, na.rm = TRUE),
            d15N_mean = mean(d15N_corrected, na.rm = TRUE),
            d15N_sd = sd(d15N_corrected, na.rm = TRUE), 
            C__mean = mean(C_wt_percent, na.rm  = TRUE),
            C__sd = sd(C_wt_percent, na.rm  = TRUE),
            N__mean = mean(N_wt_percent, na.rm  = TRUE),
            N__sd = sd(N_wt_percent, na.rm  = TRUE),
            n = n()) 



