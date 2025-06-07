# Paquetes ------------------------------------------------------------

library(tidyverse)
library(MASS)
library(dplyr)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(ggplot2)

load("workspace.RData")
ls()

# Configuraciones iniciales ------------------------------------------

set.seed(42)

path <- "./data.csv"

dataset <- read_table(
  path,
  col_types = cols(.default = col_character())
) %>%
  mutate(
    Q43 = as.numeric(Q43),
    Q9 = as.numeric(Q9),
    age  = as.numeric(age)
  ) %>%
  filter(!is.na(Q43), Q43 > 0, !is.na(Q9), Q9 > 0, !is.na(age),  age <= 100) %>%
  mutate(
    Q43 = as.ordered(Q43),
    Q9 = as.ordered(Q9),
    age = as.numeric(age)
  ) %>%
  mutate(
    is_train = sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.8, 0.2))
  )


# Ejercicio 1 ----------------------------------------------------------------

train <- dataset %>% filter(is_train)
test  <- dataset %>% filter(!is_train)

# Ejercicio 5 ----------------------------------------------------------------

model <- polr(Q43 ~ age, data = train, Hess = TRUE)

summary(model)

table(train$Q43)
prop.table(table(train$Q43))

predictions_ordinal <- predict(model, newdata = test)
confusion_matrix <- table(
  Predicho = predictions_ordinal,
  Observado = test$Q43
)

print(confusion_matrix)

accuracy <- mean(
  as.character(predictions_ordinal) == as.character(test$Q43)
)
cat("Accuracy:", round(accuracy, 3), "\n")


# Ejercicio 6 -----------------------------------------------------------------

# Estimar P({persona de 25 a;os este al menos de acuerdo con "me gustan las armas" (Q9)})

poblacion <- dataset %>%
  mutate(
    Q9 = as.numeric(Q9),
    age  = as.numeric(age)
  ) %>%
  filter(age == 25)


de_acuerdo <- poblacion %>%
  filter(Q9 > 3, Q9 < 6)

# Por definicion de estimador (equivalente al estimador de Momentos?)
cat("Proba estimada = ", round(nrow(de_acuerdo)/ nrow(poblacion), 2))

# Estimador con Regresion Ordinal
model_Q9 <- polr(Q9 ~ age, data = train, Hess = TRUE)

test_Q9 <- data.frame(age = 25)
probs_25 <- predict(model_Q9, newdata = test_Q9, type = "probs")

print(probs_25)

p_al_menos_de_acuerdo <- probs_25["4"] + probs_25["5"]
print(p_al_menos_de_acuerdo)


# Ejercicio 7 ----------------------------------------------------------------

loss <- function(y_vector, y_vector_sombrero){
  L <- 0
  n <- length(y_vector)
  for (i in 1:n){
    L <- L + abs(y_vector[i] - y_vector_sombrero[i])
  }
  
  return(L / n)
}

# Ejercicio 8 ----------------------------------------------------------------

train_linear <- train %>%
  mutate(
    Q43 = as.numeric(Q43)
  )
    
linear_model <- lm(Q43 ~ age, data = train_linear)

# el parametro activation para elegir entre hacer un clipeo de valores > 5 o < 1 o fitear en una sigmoidea escalada y desplazada
predict_helper <- function(prediction, activation) {
  preds <- as.numeric(prediction)
  
  mapped <- preds
  mapped <- pmax(pmin(mapped, 5), 1) 
  
  if(activation == 'sigmoid'){
    mapped <- 1 + 4 * plogis(preds)  
  }
  
  frac <- mapped - floor(mapped)
  
  res <- ifelse(frac > 0.5,
                ceiling(mapped),
                floor(mapped))
  return(res)
}

# Predicciones raw del modelo
predictions_lm <- predict(linear_model, newdata = test)

# fiteo entre [1, 5] (ambos metodos)
predictions_lm_sigmoid <- predict_helper(predictions_lm,'sigmoid')
predictions_lm_truncated <- predict_helper(predictions_lm,'truncated')

confusion_matrix_lm_sigmoid <- table(
  Predicho = predictions_lm_sigmoid,
  Observado = test$Q43
)

confusion_matrix_lm_truncated <- table(
  Predicho = predictions_lm_truncated,
  Observado = test$Q43
)

print('Matriz confusion LM Sigmoid')
print(confusion_matrix_lm_sigmoid)

print('Matriz confusion LM truncated')
print(confusion_matrix_lm_truncated)

accuracy_lm_sigmoid <- mean(
  as.character(predictions_lm_sigmoid) == as.character(test$Q43)
)
cat("Accuracy LM Sigmoid:", round(accuracy_lm_sigmoid, 3), "\n")

accuracy_lm_truncated <- mean(
  as.character(predictions_lm_truncated) == as.character(test$Q43)
)
cat("Accuracy LM truncated:", round(accuracy_lm_truncated, 3), "\n")


# Ejercicio 9 ----------------------------------------------------------------

loss_ordinal <- loss(as.numeric(predictions_ordinal), as.numeric(test$Q43))
loss_linear_sigmoid <- loss(predictions_lm_sigmoid, as.numeric(test$Q43))
loss_linear_truncated <- loss(predictions_lm_truncated, as.numeric(test$Q43))

loss_df <- tibble(
  model = c("Ordinal", "Linear Sigmoid", 'Linear Truncated'),
  loss  = c(loss_ordinal, loss_linear_sigmoid, loss_linear_truncated)
)

print(loss_df)


# Ejercicio 10 ---------------------------------------------------------------

# Primera version: algoritmo sampling
# Modelo 1: (R2 ~ 0.8)
modelo_location_alta <- stan_polr(Q43 ~ age,
                  data = train,
                  prior = R2(location = 0.8, what='mean'),
                  seed=42,
                  chains=2,
                  iter=1000
                  )

# Modelo 2: (R2 ~ 0.2)
modelo_location_media <- stan_polr(Q43 ~ age,
                  data = train,
                  prior = R2(location = 0.2, what='mean'),
                  seed=42,
                  chains=2,
                  iter=1000)

# Modelo 3: (uniforme por defecto)
modelo_prior_uniforme <- stan_polr(Q43 ~ age,
                  data = train,
                  prior = NULL,  # prior uniforme
                  seed = 42,
                  chains=2,
                  iter=1000)

summary(modelo_location_alta)
summary(modelo_location_media)
summary(modelo_prior_uniforme)

post_modelo_location_alta <- as.data.frame(modelo_location_alta)$age
post_modelo_location_media <- as.data.frame(modelo_location_media)$age
post_modelo_prior_uniforme <- as.data.frame(modelo_prior_uniforme)$age

# Combino todo en un dataframe para visualizacion
df_plot <- data.frame(
  beta_age = c(post_modelo_location_alta, post_modelo_location_media, post_modelo_prior_uniforme),
  modelo = rep(
    c("Prior Fuerte (R2=0.8)", "Prior Regularizador (R2=0.2)", "Prior Vaga (Uniforme)"), 
    each = length(post_modelo_location_alta)
  )
)

# Gráfico de densidades superpuestas
ggplot(df_plot, aes(x = beta_age, fill = modelo)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Efecto de la Prior en la Distribución a Posteriori de β (age). Modelo con algoritmo sampling.",
    subtitle = "Comparación de las creencias finales del modelo sobre el efecto de la edad",
    x = "Valor del coeficiente β (age)",
    y = "Densidad a Posteriori",
    fill = "Modelo (Elección de la Prior)"
  ) +
  theme_minimal()


# Segunda version: algoritmo con fullrank
# Modelo 1: (R2 ~ 0.8) con fullrank
modelo_location_alta_fullrank <- stan_polr(Q43 ~ age,
                           data = train,
                           prior = R2(location = 0.8, what='mean'),
                           seed=42,
                           algorithm = "fullrank"
)

# Modelo 2:  (R2 ~ 0.2) con fullrank
modelo_location_media_fullrank <- stan_polr(Q43 ~ age,
                  data = train,
                  prior = R2(location = 0.2, what='mean'),
                  seed=42,
                  algorithm = 'fullrank')

# Modelo 3: (uniforme por defecto) con fullrank
modelo_prior_uniforme_fullrank <- stan_polr(Q43 ~ age,
                  data = train,
                  prior = NULL,  # prior uniforme
                  seed = 42,
                  algorithm = 'fullrank')


post_modelo_location_alta_fullrank <- as.data.frame(modelo_location_alta_fullrank)$age
post_modelo_location_media_fullrank <- as.data.frame(modelo_location_media_fullrank)$age
post_modelo_location_uniforme_fullrank <- as.data.frame(modelo_prior_uniforme_fullrank)$age


df_plot_fullrank <- data.frame(
  beta_age = c(post_modelo_location_alta_fullrank, post_modelo_location_media_fullrank, post_modelo_location_uniforme_fullrank),
  modelo = rep(
    c("Prior Fuerte (R2=0.8)", "Prior Regularizador (R2=0.2)", "Prior Vaga (Uniforme)"), 
    each = length(post_modelo_location_alta) # Repite cada etiqueta el número de muestras que haya
  )
)


ggplot(df_plot_fullrank, aes(x = beta_age, fill = modelo)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Efecto de la Prior en la Distribución a Posteriori de β (age). Modelo con algoritmo fullrank",
    subtitle = "Comparación de las creencias finales del modelo sobre el efecto de la edad",
    x = "Valor del coeficiente β (age)",
    y = "Densidad a Posteriori",
    fill = "Modelo (Elección de la Prior)"
  ) +
  theme_minimal()


# Tercera version: como la edad se mueve en un rango de [13,100], consideramos que
# puede haber sobrerepresentacion de los valores. Como una estrategia, pasamos
# menos data con la misma distribucion.
set.seed(123)
train_subsampleado <- train[sample(nrow(train), 50000), ]

modelo_location_alta_subsamp_fullrank <- stan_polr(Q43 ~ age,
                                                   data = train_subsampleado,
                                                   prior = R2(location = 0.8, what='mean'),
                                                   seed=42,
                                                   algorithm = 'fullrank'
)



modelo_location_media_subsamp_fullrank <- stan_polr(Q43 ~ age,
                                            data = train_subsampleado,
                                            prior = R2(location = 0.01, what='mean'),
                                            seed=42,
                                            algorithm = 'fullrank')

modelo_prior_uniforme_subsamp_fullrank <- stan_polr(Q43 ~ age,
                                            data = train_subsampleado,
                                            prior = NULL,  # prior uniforme
                                            seed = 42,
                                            algorithm = 'fullrank')

post_modelo_location_alta_subsamp_fullrank <- as.data.frame(modelo_location_alta_subsamp_fullrank)$age
post_modelo_location_media_subsamp_fullrank <- as.data.frame(modelo_location_media_subsamp_fullrank)$age
post_modelo_location_uniforme_subsamp_fullrank <- as.data.frame(modelo_prior_uniforme_subsamp_fullrank)$age


df_plot_fullrank_sumsamp <- data.frame(
  beta_age = c(post_modelo_location_alta_subsamp_fullrank,
               post_modelo_location_media_subsamp_fullrank, post_modelo_location_uniforme_subsamp_fullrank),
  modelo = rep(
    c("Prior Fuerte (R2=0.8)","Prior Regularizador (R2=0.2)", "Prior Vaga (Uniforme)"), 
    each = length(post_modelo_location_alta_subsamp_fullrank) # Repite cada etiqueta el número de muestras que haya
  )
)

ggplot(df_plot_fullrank_sumsamp, aes(x = beta_age, fill = modelo)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Efecto de la Prior en la Distribución a Posteriori de β (age). Modelo con algoritmo fullrank",
    subtitle = "Comparación de las creencias finales del modelo sobre el efecto de la edad",
    x = "Valor del coeficiente β (age)",
    y = "Densidad a Posteriori",
    fill = "Modelo (Elección de la Prior)"
  ) +
  theme_minimal()



