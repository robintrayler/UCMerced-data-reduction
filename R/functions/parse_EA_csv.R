parse_EA_csv <- function(file_path) {
  data <- read_csv(file = file_path,
                   col_types = cols()) %>% 
    select(identifier_1 = `Identifier 1`,
           sample = `Identifier 2`,
           amount = Amount,
           seq_nr = Row,
           peak_nr = `Peak Nr.`,
           retention_time = Rt,
           end = End,
           start = Start,
           area_28 = `Area 28`,
           area_44 = `Area 44`,
           d13C_measured = `d 13C/12C`,
           d15N_measured = `d 15N/14N`) %>% 
    return()
}
