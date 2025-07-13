require(tidyverse)
library(tidyr)
require(purrr)
library(dplyr)
require(lme4)
library(lme4)
library(stringr)
library(splines)
library(mgcv)
library(broom)
library(ggplot2)

library(tidytext)
library(stopwords)

set.seed(42)

path <- ''
titles_train <- read.csv(paste(path,'titles_train.csv', sep = ''))

nrow(titles_train %>% filter(str_detect(production_countries, 'LK')))

# EJERCICIO 1 ==================================================================

df_genres <- titles_train %>%
  mutate(genres = str_remove_all(genres, "\\[|\\]|'")) %>%
  separate_rows(genres, sep = ",\\s*") %>%
  filter(genres != "")

# Puntaje promedio por género -------------------------------------------------
genre_scores <- df_genres %>%
  group_by(genres) %>%
  summarise(mean_score = mean(imdb_score, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(mean_score))

# Mostrar géneros con mayor puntaje promedio
genre_scores %>% filter(n >= 10) %>% head(10)

ggplot(genre_scores %>% filter(n >= 10), aes(x = reorder(genres, mean_score), y = mean_score)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Puntaje IMDb promedio por género (con al menos 10 títulos)",
       x = "Género",
       y = "Puntaje promedio") +
  theme_minimal()


# Palabras asociacas a mayor/menor puntaje ---------------------------------------

title_words <- titles_train %>%
  unnest_tokens(word, title) %>%
  filter(!word %in% stopwords::stopwords("en")) 

# Promedio de puntaje por palabra
word_scores <- title_words %>%
  group_by(word) %>%
  summarise(mean_score = mean(imdb_score, na.rm = TRUE),
            n = n()) %>%
  filter(n >= 5) %>%  # Filtrar palabras que aparecen en al menos 5 títulos
  arrange(desc(mean_score))

# Palabras con mayor puntaje promedio
word_scores %>%
  select(word, mean_score) %>%
  head(10)

# Palabras con menor puntaje promedio
word_scores %>%
  select(word, mean_score) %>%
  tail(10)

# Cantidad de peliculas por pais -----------------------------------

df_countries <- titles_train %>%
  mutate(
    country = str_remove_all(production_countries, "\\[|\\]|'") 
  ) %>%
  separate_rows(country, sep = ",\\s*") %>%
  filter(country != "") %>%
  group_by(country) %>%
  summarise(n_movies = n()) %>%
  arrange(desc(n_movies))

top_5 <- df_countries %>%
  arrange(desc(n_movies)) %>%
  slice_head(n = 5)

countries_1 <- df_countries %>%
  filter(n_movies ==1)

cat("### Top 5 países con más películas\n")
print(top_5)

cat("\n\n### 5 países con menos películas\n")
print(countries_1)

# Distribucion de los puntajes ------------------------------------

ggplot(titles_train, aes(x = imdb_score)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribución del puntaje IMDb",
       x = "Puntaje",
       y = "Cantidad de películas") +
  theme_minimal()




# EJERCICIO 2 ==================================================================

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


ggplot(plot_with_countries,
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



# EJERCICIO 3 ==================================================================
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


# EJERCICIO 4 ==================================================================
# hecho en el informe


# EJERCICIO 5 ==================================================================

names(titles_train)
titles_train

unique(titles_train$genres)

map_primary_pc <- function(pais_vector, top_paises) {
  niveles <- if ("OTHER" %in% top_paises) {
    top_paises
  } else {
    c(top_paises, "OTHER")
  }
  
  pais_vector_mapped <- ifelse(pais_vector %in% top_paises, pais_vector, "OTHER")
  factor(pais_vector_mapped, levels = niveles)
}

preprocesar_titles <- function(df, top_paises = NULL) {
  df <- df %>% 
    mutate(
      age_certification = if_else(age_certification %in% age_cert_significativos,
                                  age_certification, "other"),
      genres_clean = str_replace_all(genres, "\\[|\\]|'", "") %>% str_squish()
    ) %>% 
    mutate(
      !!! set_names(
        lapply(generos_significativos, function(g)
          expr(str_detect(genres_clean, !!paste0("\\b", g, "\\b")))
        ),
        paste0("is_", generos_significativos)
      )
    ) %>% 
    filter(imdb_votes < 1000000 | is.na(imdb_votes)) %>%
    mutate(
      pc_clean = str_remove_all(production_countries, "\\[|\\]|'") %>% str_squish(),
      primary_pc_raw = str_extract(pc_clean, "^[A-Z]{2}")
    )
  
  if (is.null(top_paises)) {
    # Para entrenamiento, agrupo con fct_lump_n
    df <- df %>%
      mutate(primary_pc = fct_lump_n(primary_pc_raw, n = 10, other_level = "OTHER"))
  } else {
    # Para test, mapeo según niveles fijos
    df <- df %>%
      mutate(primary_pc = map_primary_pc(primary_pc_raw, top_paises))
  }
  
  return(df)
}


nice_df <- preprocesar_titles(titles_train)

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
mse_1

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


# EJERCICIO 6 ==================================================================

titles_test <- read.csv("titles_test.csv")

niveles_train <- levels(nice_df$primary_pc)

titles_test_df <- preprocesar_titles(titles_test,top_paises = niveles_train)

# Modelo con splines
pred_gam <- predict(candidate_3, newdata = titles_test_df)

write.table(pred_gam, file = "predicciones.csv", row.names = FALSE, col.names = FALSE, sep = ",")

