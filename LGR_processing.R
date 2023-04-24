library(tidyverse)
standards <- tribble(~sample,   ~d2H_true,   ~d18O_true,
                     'LGR 1',  -154.1,  -19.57,
                     'LGR 2',  -117.0,  -15.55,
                     'LGR 3',  -79.0,   -11.54,
                     'LGR 4',  -43.6,   -7.14,
                     'LGR 5',  -9.8,    -2.96)

standards_used <- c('LGR 1', 'LGR 3', 'LGR 5')

discard <- 3

# load the raw data 
data <- read_csv(file = '~/Desktop/h2o_19791231_019.txt',
                 skip = 1)

# find where the data ends
index <- 
  str_starts(data$`Inj#`, 
             pattern = "//ADVANCED SETTINGS") %>% which()

# slice out just the data and rename
data <- data %>% 
  slice(1:(index - 1)) %>% 
  select(inj_no = `Inj#`,
         sample = Sample_name,
         sample_no = `Sample_S/N`,
         time = `Time_sec`,
         d2H_measured  = `raw_delta_D/H`,
         d18O_measured = `raw_delta_O18/O16`) %>% 
  mutate(inj_no = as.numeric(inj_no)) %>% 
  group_by(sample) %>% 
  mutate(index = seq_along(sample)) %>% 
  filter(index > discard) %>% 
  ungroup() %>% 
  full_join(standards, 
            by = 'sample') %>% 
  group_by(sample) %>% 
  mutate(d18O_dev = d18O_measured - mean(d18O_measured),
         d2H_dev = d2H_measured - mean(d2H_measured)) %>% 
  ungroup()


# Oxygen ----------------------------------------------------------------------
oxy_drift <- data %>% 
  filter(sample %in% standards_used) %>% 
  lm(d18O_dev ~ inj_no, data = . )
hyd_drift <- data %>% 
  filter(sample %in% standards_used) %>% 
  lm(d2H_dev ~ inj_no, data = . )

data <- data %>% 
  mutate(d18O_measured = d18O_measured - predict(oxy_drift, newdata = data),
         d2H_measured = d2H_measured - predict(hyd_drift, newdata = data))
 


 
# standard correction ---------------------------------------------------------
data <- data %>% 
  group_by(sample_no) %>% 
  summarize(d2H_mean = mean(d2H_measured),
            d2H_sd = sd(d2H_measured),
            d18O_mean = mean(d18O_measured),
            d18O_sd = sd(d18O_measured),
            d2H_true = mean(d2H_true),
            d18O_true = mean(d18O_true),
            sample = unique(sample))

H_std_fit <- 
  data %>% 
  filter(sample %in% standards_used) %>% 
  lm(d2H_true ~ d2H_mean, data = . )

O_std_fit <- 
  data %>% 
  filter(sample %in% standards_used) %>% 
  lm(d18O_true ~ d18O_mean, data = . )
  
data <- data %>% 
  mutate(d18O_corrected = predict(O_std_fit, newdata = data),
         d2H_corrected = predict(H_std_fit, newdata = data))
 
  
data %>% 
  group_by(sample) %>% 
  summarize(d2H_mean = mean(d2H_corrected), 
            d2H_sd = sd(d2H_corrected),
            d18O_mean = mean(d18O_corrected),
            d18O_sd = sd(d18O_corrected))

data %>% 
  write_csv(file = '~/Desktop/bottled_water_isotopes.csv')

