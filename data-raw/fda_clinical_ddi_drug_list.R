library(tidyverse)

fda_ddi_table <- read_csv("data-raw/fda_ddi_table.csv")

temp <- fda_ddi_table %>%
  pivot_longer(cols = seq(2, 11), names_to = "CLASS", values_to = "VALUE") %>%
  filter(str_trim(VALUE) != "") %>%
  as.data.frame()

targets <- c("1A2", "2B6", "2D6", "2C8", "2C9", "2C19", "3A4",
             "OATP1B1", "OATP1B3", "OAT1", "OAT3", "P-gp", "BCRP",
             "MATE1", "MATE2-K")

fda_clinical_ddi_drug_list <- temp %>%
  rename(DRUG = "Drug or Other Substance") %>%
  mutate(TYPE = str_extract(VALUE, "inducer|inhibitor|substrate")) %>%
  mutate(QUALIFIER = str_extract(VALUE, "moderate|strong|weak|sensitive")) %>%
  mutate(TARGET = str_extract_all(VALUE, paste(targets, collapse = "|"))) %>%
  as_tibble() %>%
  unnest(TARGET) %>%
  select(DRUG, TYPE, QUALIFIER, TARGET) %>%
  mutate(DRUG = toupper(DRUG))

usethis::use_data(fda_clinical_ddi_drug_list, overwrite = TRUE)
