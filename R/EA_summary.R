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
file_path <- "~/Box Sync/Data Repository/EA/Corrected/"

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





storage %>% 
  group_by(sample) %>% 
  mutate(d13C_dev = d13C_corrected - mean(d13C_corrected),
         d15N_dev = d15N_corrected - mean(d15N_corrected)) %>% 
  ggplot(mapping = aes(x = d13C_dev,
                       fill = sample),
         ) + 
  geom_density(color = NA) + 
  facet_wrap(~sample, scales = 'free+y') + 
  xlim(-1, 1)



storage %>%  
  group_by(sample) %>% 
  mutate(d13C_dev = d13C_corrected - mean(d13C_corrected),
         d15N_dev = d15N_corrected - mean(d15N_corrected)) %>% 
  ggplot(mapping = aes(x = date, 
                       y = d13C_dev,
                       color = sample)) + 
  geom_jitter(alpha = 0.35,
              size = 3) + 
  theme(legend.position = 'top')  + 
  scale_color_brewer(palette = 'Dark2') + 
  ylim(-2, 2) + 
  ylab(expression(delta^13*C[centered])) + 
  xlab('Date')



x <- storage %>% 
  
  group_by(sample) %>% 
  summarise(d13C_mean = mean(d13C_corrected, na.rm = TRUE),
            d13C_sd = sd(d13C_corrected, na.rm = TRUE),
            d15N_mean = mean(d15N_corrected, na.rm = TRUE),
            d15N_sd = sd(d15N_corrected, na.rm = TRUE), 
            C_mean = mean(C_wt_percent, na.rm  = TRUE),
            C_sd = sd(C_wt_percent, na.rm  = TRUE),
            N_mean = mean(N_wt_percent, na.rm  = TRUE),
            N_sd = sd(N_wt_percent, na.rm  = TRUE),
            n = n()) |> 
  mutate_if(is.numeric, round, 2)

storage %>%  
  group_by(sample) %>% 
  # filter(sample != 'EA sed') |>
  mutate(C_wt_dev = C_wt_percent - mean(C_wt_percent), 
         N_wt_dev = N_wt_percent - mean(N_wt_percent), 
         d13C_dev = d13C_corrected - mean(d13C_corrected, na.rm = TRUE),
         d15N_dev = d15N_corrected - mean(d15N_corrected, na.rm = TRUE)) %>% 
  ungroup() %>%
  summarize(d13C_sd = sd(d13C_dev,na.rm = TRUE),
            d15N_sd = sd(d15N_dev,na.rm = TRUE),
            C_sd = sd(C_wt_dev,na.rm = TRUE),
            N_sd = sd(N_wt_dev,na.rm = TRUE))



storage %>% 
  filter(!(sample %in% c('MB squid', 'peach'))) %>% 
  group_by(sample) %>% 
  summarize(C_wt = mean(C_wt_percent, na.rm = TRUE),
            C_sd = sd(C_wt_percent, na.rm = TRUE)) %>% 
  mutate(C_error = C_sd / C_wt * 100) %>% 
  ungroup( ) %>% 
  pull(C_error) %>% mean()
