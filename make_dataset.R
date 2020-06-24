#packages
library(tidyverse)
library(rlang)

#create patients dataset, n=300
ds_structure <- read_tsv("dataset_structure.tsv")
n_patients <- 300
n_patients_groups <- round(ds_structure$prop * n_patients)
new_patients <- ds_structure[
  rep(seq_len(nrow(ds_structure)), n_patients_groups), ]
new_patients$id <- seq_len(nrow(new_patients))

#generate SLEs on case day
set.seed(20)
case_day <- sapply(new_patients$group_exp_chance_case_day, 
  rbinom, n = 1, size = 1)
new_patients$case_day <- case_day

#generate SLEs on control days
set.seed(20)
number_of_new_days <- 7
new_days <- t(sapply(new_patients$group_exp_chance_control_day, 
  rbinom, n = number_of_new_days, size = 1))
colnames(new_days) <- colnames(
  new_days, 
  do.NULL = FALSE, 
  prefix = "control_day")
new_patients <- (cbind(new_patients, new_days))

#remove unneccessary variables
new_patients <- new_patients %>%
select(-group_exp_chance_case_day, -group_exp_chance_control_day, -n, -prop) %>%
select(id, everything())

#make long dataset with exposures and outcomes for each day
new_patients_long <- new_patients %>%
pivot_longer( 
  cols = c(case_day, starts_with("control_day")),
  names_to = "day",
  values_to = "exposure") %>%
mutate(outcome = day == "case_day") 
new_patients_long$exposure <- new_patients_long$exposure %>% 
recode(`0` = FALSE, `1` = TRUE) 

#report fequencies
lapply(new_patients, count, x = new_patients)

#save dataset
write_tsv(new_patients_long, "dataset_.tsv")
