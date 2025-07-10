require(tidyverse)
library(tidyr)
require(purrr)
library(dplyr)
require(lme4)
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

ggplot(plot_df,
       aes(mean,
           y = reorder(country, order_fe),
           colour = model,
           shape  = model)) +
  geom_point(position = position_dodge(width = .3), size = 2) +
  geom_vline(xintercept = mu, linetype = "dashed") +
  labs(x = "Puntaje IMDb promedio estimado",
       y = NULL,
       colour = "Modelo",
       shape  = "Modelo",
       title = "Efectos de país: fijo vs. aleatorio") +
  theme_minimal()


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

library(splines)

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
  coord_cartesian(ylim = c(0, 1.5e6)) +
  labs(x="Año de estreno", y="Votos en IMDb") +
  theme_minimal()


# ejericio 4
# https://documentation.sas.com/doc/en/statug/15.2/statug_causalgraph_details02.htm

# Dado el DAG de causalidad, vemos que el nodo Pais apunta a Comedia por lo que podria estar interfiriendo en la relacion causal
# Esto lo podemos interpretar como: depende el pais, el nivel de comedia puede ser mayor o menor y esto influiria en el Score.
# Pero entonces vemos que el Pais seria condicionante (luego si condicionamos por pais ya podriamos estimar el efecto causal).

# Demo:

# For a set of treatment variables X and a set of outcome variables Y, a set of observed variables Z is a valid adjustment set if all the following conditions are present:

# 1.  Z = {pais} bloquea TODOS los caminos no-causales de X a Y [solo hay 1 que es pais -> comedia -> score]
# 2. para todo z en Z, z no pertenece o desciende de un camino causal de X a Y
# 3. ningun z en Z desciende de alguna variable de un camino causal (pais es confounder asi que no desciende de nada)