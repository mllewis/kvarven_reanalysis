# read tables from supplementary materials
library(tidyverse)
library(tabulizer)
library(janitor)

meta_data <- tibble(page = c(4:6, 11:14),
                    table_name = c("original_studies",
                                   "original_MA",
                                   "replication_studies",
                                   "RE_MA",
                                   "PET-PEESE_MA",
                                   "PSM_MA",
                                   "TF_MA"))

save_supp_data <- function(page_num, meta_data){
  this_table_name <- filter(meta_data, page == page_num)  %>%
    pull(table_name)
  tab <- extract_tables("kvaren_supp.pdf",
                        pages = page_num, 
                        output = "data.frame") %>%
    pluck(1) %>%
    filter(X != "") %>%
    mutate(table_name = this_table_name) %>%
    select(table_name, everything())
  
  write_csv(tab, paste0("data/", this_table_name, ".csv"))
  
}

walk(meta_data$page, save_supp_data, meta_data)

all_files <- list.files("data/tidy/", full.names = T) %>%
  map_df(read_csv) %>%
  clean_names() %>%
  select(-converted) %>%
  mutate(ss = str_replace_all(ss, "N = ", "")) %>%
  separate(ss, c("n", "k"), sep = ",") %>%
  mutate(k = str_trim(k))  %>%
  separate(ci, c("ci_lower", "ci_upper"), sep = ", ")   %>%
  mutate(k = as.numeric(k),
         n = as.numeric(n),
         ci_lower = as.numeric(ci_lower),
         ci_upper = as.numeric(ci_upper))

write_csv(all_files, "data/tidy_kvaren.csv")


```