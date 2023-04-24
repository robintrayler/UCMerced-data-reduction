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
file_path <- "~/Box Sync/Data Repository/TCEA/Corrected/2022"

# reference materials to check ------------------------------------------------
standards <- tribble(~sample,      ~d18O_corrected,
                     'USGS 80',   13.1,
                     'USGS 81',   35.4)
                     # 'IAEA 601',  23.3)
                     # 'alfa asar',   NA)


colors <- viridis(n = nrow(standards) + 1, end = 0.8, option = 'D')
names(colors) <- c(standards$sample, TRUE)

# outlier exclusion -----------------------------------------------------------
# exclude = 2 # exclude analyses that fall more that 2 standard deviations from the mean
# whole analyses are excluded if ANY of d13C, d15N, N%, C% are outliers

files <- list.files(file_path, 
                    pattern = '_corrected.csv' , 
                    full.names = TRUE, 
                    recursive = TRUE)


storage <- read_csv(file = files[1]) %>% 
  
  select(Identifier_1, 
         sample, 
         d18O_corrected) %>% 
  mutate(date = ymd(str_sub(basename(files[1]), start = 5, end = 12)))




for(i in 2:length(files)) {
  storage <- read_csv(file = files[i], col_names = T) %>% 
    select(Identifier_1, sample, 
           d18O_corrected) %>% 
    mutate(date = ymd(str_sub(basename(files[i]), start = 5, end = 12))) %>% 
    full_join(storage)
}

storage <- storage %>% 
  filter(sample %in% standards$sample) %>% 
  group_by(sample) %>% 
  mutate(z_score_O = zscore(d18O_corrected)) %>% 
  ungroup() %>% 
  group_by(sample) %>% 
  mutate(d18O_centered = d18O_corrected - mean(d18O_corrected, na.rm = TRUE))

storage %>% 
  group_by(sample) %>% 
  summarise(d18O_mean = mean(d18O_corrected),
            d18O_sd = sd(d18O_corrected),
            n = n()) %>% 
  knitr::kable()


storage %>% 
  ggplot(mapping = aes(x = date,
                                 y = d18O_corrected,
                                 color = sample)) + 
  geom_point() + 
  facet_wrap(~sample, scale = 'free_y') + 
  geom_hline(data = standards,
             mapping = aes(yintercept = d18O_corrected))




