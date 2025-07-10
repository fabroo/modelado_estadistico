require(tidyverse)
library(tidyr)
require(purrr)
library(dplyr)
require(lme4)
library(stringr)
library(splines)
library(mgcv)

path <- 'Documents/uba/modelado_estadistico/'
titles_train <- read.csv(paste(path,'titles_train.csv', sep = ''))

nrow(titles_train %>% filter(str_detect(production_countries, 'LK')))


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
    id_cols     = c(imdb_id, imdb_score),
    names_from  = country,
    values_from = w,
    values_fn   = sum,
    values_fill = 0
  ) %>% 
  select(-imdb_id)

country_score_df

# modelo sin intercept
fit_without_intercept <- lm(imdb_score ~ . - 1, data = country_score_df)

# 2b

df_largo <- titles_train %>% 
  select(imdb_id, imdb_score, production_countries) %>% 
  mutate(country = str_remove_all(production_countries, "\\[|\\]|'")) %>% 
  separate_rows(country, sep = ",\\s*") %>% 
  filter(country != "") %>% 
  group_by(imdb_id) %>% 
  mutate(w = 1 / n()) %>% 
  ungroup() %>% 
  select(-c(imdb_id, production_countries))


df_largo

fit_random_intercept <- lmer(imdb_score ~ 1 + (1 | country),
            data    = df_largo,
            weights = w)
summary(fit_random_intercept)


fe_df <- broom::tidy(fit_without_intercept) |>
  transmute(
    country = term,          
    mean_fe = estimate
  )

mu  <- fixef(fit_random_intercept)[1]
u   <- ranef(fit_random_intercept)$country[, 1]

re_df <- tibble(
  country = rownames(ranef(fit_random_intercept)$country),  # aquí
  mean_re = mu + u
)

plot_df <- full_join(fe_df, re_df, by = "country") |>
  mutate(order_fe = mean_fe) |>
  pivot_longer(c(mean_fe, mean_re),
               names_to  = "model",
               values_to = "mean") |>
  mutate(model = recode(model,
                        mean_fe = "Fijos",
                        mean_re = "Aleatorios"))

countries <- unique(plot_df$country)
counts_country <- tibble(country = countries) %>%
  mutate(
    n_titles = map_int(country, ~ 
                         titles_train %>% 
                         filter(str_detect(production_countries, fixed(.x))) %>% 
                         nrow()
    )
  )

plot_with_countries <- plot_df %>%
  left_join(counts_country, by = "country")


ggplot(plot_df2,
       aes(x = mean,
           y = reorder(country, order_fe),
           colour = model,
           shape  = model,
           size   = n_titles)) +
  geom_point(position = position_dodge(width = .3)) +
  geom_vline(xintercept = mu, linetype = "dashed") +
  scale_size_continuous(name = "Nº títulos", range = c(1, 5)) +
  labs(x = "Puntaje IMDb promedio estimado",
       y = NULL,
       colour = "Modelo",
       shape  = "Modelo",
       title = "Efectos de país: fijo vs. aleatorio") +
  theme_minimal() +
  theme(
    axis.text.y  = element_text(size = 6),
    axis.ticks.y = element_blank(),
    plot.margin  = margin(5, 5, 5, 40)
  )


# ejercicio 3
# considero la popularidad como la cantidad de votos que tiene y no los votos
popularity_df <- titles_train %>% 
  mutate(imdb_votes = as.numeric(imdb_votes)) %>%
  filter(!is.na(imdb_votes)) %>%
  filter(imdb_votes < 700000) %>% # CONSIDERAR DEJAR ESTOS OUTLIERS
  select(release_year, imdb_votes)

ggplot(popularity_df,
       aes(x = release_year, y = imdb_votes)) +
  geom_point(alpha = 0.6) +            # dispersión básica
  scale_y_continuous(labels = scales::comma) +   # miles con coma
  labs(
    x = "Año de estreno",
    y = "Votos en IMDb",
    title = "Popularidad (nº de votos) según año de lanzamiento"
  ) +
  theme_minimal()

# analisis de outliers
mas_populares <- popularity_df %>% filter(imdb_votes > 1000000)
mas_populares

ks <- c(1,2,3,5,10,20, 48,50)
year_grid <- seq(min(popularity_df$release_year),
                 max(popularity_df$release_year), length.out = 300)

pred_df <- do.call(rbind, lapply(ks, function(k){
  p <- seq(0,1,length.out=k+2)[-c(1,k+2)]
  fit <- lm(imdb_votes ~ bs(release_year,
                            knots = quantile(popularity_df$release_year, p)),
            data = popularity_df)
  data.frame(release_year = year_grid,
             pred = predict(fit, newdata = data.frame(release_year = year_grid)),
             k = factor(k))
}))

ggplot(popularity_df, aes(release_year, imdb_votes)) +
  geom_point(colour="grey60", alpha=.5, size=.7) +
  geom_line(data=pred_df, aes(y=pred, colour=k), linewidth=1) +
  scale_colour_viridis_d(name="k") +
  coord_cartesian(ylim = c(0, 1.e6)) +
  labs(x="Año de estreno", y="Votos en IMDb") +
  theme_minimal()


# ejericio 4
# hecho en el informe


# ejercicio 5

names(titles_train)
titles_train

unique(titles_train$genres)

age_cert_significativos <- c("G", "PG", "PG-13", "R", "TV-G", "TV-PG")
generos_significativos <- c("action", "animation", "comedy", "documentation",
                   "drama", "horror", "romance", "scifi", "thriller")
nice_df <- titles_train %>% 
  mutate(
    age_certification = if_else(age_certification %in% age_cert_significativos,
                                age_certification, "other"),
    genres_clean      = str_replace_all(genres, "\\[|\\]|'", "") %>% str_squish()
  ) %>% 
  mutate(
    !!! set_names(
      lapply(generos_significativos, function(g)
        expr(str_detect(genres_clean, !!paste0("\\b", g, "\\b")))
      ),
      paste0("is_", generos_significativos)
    )
  ) %>% 
  filter(imdb_votes < 1000000) %>%
  mutate(
  pc_clean = str_remove_all(production_countries, "\\[|\\]|'") %>% 
    str_squish(),
  primary_pc = str_extract(pc_clean, "^[A-Z]{2}")
  ) %>% 
  mutate(
    primary_pc = fct_lump_n(primary_pc, n = 10, other_level = "OTHER")
  )

unique(nice_df$primary_pc)

# PRIMER MODELO
candidate_1 <- lm(imdb_score ~ 
                    log1p(imdb_votes)
                  + runtime
                  + release_year
                  + type
                  + is_action        
                  + is_animation     
                  + is_comedy        
                  + is_documentation 
                  + is_drama         
                  + is_horror        
                  + is_romance       
                  + is_scifi         
                  + is_thriller  
                  + primary_pc
                  + age_certification, data = nice_df)

summary(candidate_1)

mse_1 <- mean(resid(candidate_1)^2)
mse1

# SEGUNDO MODELO
fixed  <- "log1p(imdb_votes) + runtime + (1 + log1p(imdb_votes) | primary_pc) + (1|release_year)"
fixed2 <- paste0(fixed,
                 " + is_action + is_animation + is_comedy + is_documentation",
                 " + is_drama + is_horror + is_romance + is_scifi + is_thriller",
                 " + age_certification")

candidate_2 <- lmer(
  formula = as.formula(paste("imdb_score ~", fixed2)),
  data    = nice_df,
  REML    = TRUE
)
summary(candidate_2)

mse_2 <- mean(resid(candidate_2)^2)
mse_2

# TERCER MODELO

# Generative Additive Model con k=20 spline on release_year
candidate_3 <- gam(
  imdb_score ~ s(log1p(imdb_votes), k = 30)
  + s(runtime,           k = 10) +
  + s(release_year, k = 30)
  + type
  + is_action + is_animation + is_comedy + is_documentation
  + is_drama  + is_horror    + is_romance     + is_scifi 
  + is_thriller
  + primary_pc
  + age_certification,
  data   = nice_df,
  method = "REML"
)

summary(candidate_3)

mse_3 <- mean(residuals(candidate_3)^2)
mse_3

my_list <- list(
  row1 = c("Modelo 1 (lm FE)", "RE Model",    "Splines"),
  row2 = c(              mse_1,        mse_2,        mse_3)
)

print(my_list)





