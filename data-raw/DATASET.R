## code to prepare `DATASET` dataset goes here
library(tidyverse)

load("data-raw/sysdata.rda")

temp <- fda_ddi_table %>%
  pivot_longer(cols = seq(2, 11), names_to = "CLASS", values_to = "VALUE") %>%
  filter(str_trim(VALUE) != "") %>%
  as.data.frame()

targets <- c("1A2", "2B6", "2D6", "2C8", "2C9", "2C19", "3A4",
             "OATP1B1", "OATP1B3", "OAT1", "OAT3", "P-gp", "BCRP",
             "MATE1", "MATE2-K")

fdi_clinical_ddi_drug_list <- temp %>%
  rename(drug = "Drug or Other Substance") %>%
  mutate(type = str_extract(VALUE, "inducer|inhibitor|substrate")) %>%
  mutate(qualifier = str_extract(VALUE, "moderate|strong|weak|sensitive")) %>%
  mutate(target = str_extract_all(VALUE, paste(targets, collapse = "|"))) %>%
  as_tibble() %>%
  unnest(target) %>%
  select(drug, type, qualifier, target)

usethis::use_data(fdi_clinical_ddi_drug_list, overwrite = TRUE)
