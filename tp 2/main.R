require(tidyverse)
library(tidyr)
require(purrr)
library(dplyr)

library(stringr)

path <- 'Documents/uba/modelado_estadistico/'
titles_train <- read.csv(paste(path,'titles_train.csv', sep = ''))

# ejercicio 2a

country_score_df <- titles_train %>% 
  select(imdb_id, imdb_score, production_countries) %>% 
  mutate(country = str_remove_all(production_countries, "\\[|\\]|'")) %>% 
  separate_rows(country, sep = ",\\s*") %>% 
  filter(country != "") %>% 
  group_by(imdb_id) %>% 
  mutate(w = 1 / n()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols     = c(imdb_id, imdb_score),  # the two columns that uniquely ID a film
    names_from  = country,                 # one column per country code
    values_from = w,                       # numbers, not lists
    values_fn   = sum,                     # sums duplicates -> single numeric
    values_fill = 0                        # 0 for countries a film lacks
  ) %>% 
  select(-imdb_id)

country_score_df

# modelo sin intercept
fit_without_intercept <- lm(imdb_score ~ . - 1, data = country_score_df)
