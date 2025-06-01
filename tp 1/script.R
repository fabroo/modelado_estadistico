library(tidyverse)
library(MASS)
library(dplyr)

set.seed(42)

path <- "./Documents/uba/modelado estadistico/tp 1/data.csv"

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

train <- dataset %>% filter(is_train)
test  <- dataset %>% filter(!is_train)

# ejericio 5
model <- polr(Q43 ~ age, data = train, Hess = TRUE)

summary(model)

predictions_ordinal <- predict(model, newdata = test)
conf_mat <- table(
  Predicho = predictions_ordinal,
  Observado = test$Q43
)

print(conf_mat)

accuracy <- mean(
  as.character(predictions_ordinal) == as.character(test$Q43)
)
cat("Accuracy:", round(accuracy, 3), "\n")

# ejercicio 6

# estimar P({persona de 25 a;os este al menos de acuerdo con "me gustan las armas" (Q9)})

poblacion <- dataset %>%
  mutate(
    Q9 = as.numeric(Q9),
    age  = as.numeric(age)
  ) %>%
  filter(age == 25)

de_acuerdo$Q9

de_acuerdo <- poblacion %>%
  filter(Q9 > 3, Q9 < 6)

cat("Proba estimada = ", round(nrow(de_acuerdo)/ nrow(poblacion), 2))

# o sino
model_Q9 <- polr(Q9 ~ age, data = train, Hess = TRUE)

test_Q9 <- data.frame(age = 25)
probs_25 <- predict(model_Q9, newdata = test_Q9, type = "probs")

print(probs_25)

p_al_menos_de_acuerdo <- probs_25["4"] + probs_25["5"]
print(p_al_menos_de_acuerdo)

# pregunta 7
loss <- function(y_vector, y_vector_sombrero){
  L <- 0
  n <- length(y_vector)
  for (i in 1:n){
    L <- L + abs(y_vector[i] - y_vector_sombrero[i])
  }
  
  return(L / n)
}

# pregunta 8
train_linear <- train %>%
  mutate(
    Q43 = as.numeric(Q43)
  )
    
linear_model <- lm(Q43 ~ age, data = train_linear)

test_lm <- data.frame(age = 25)
pred_continuo <- predict(linear_model, newdata = test_lm)

predict_helper <- function(prediction) {
  preds <- as.numeric(prediction)
  
  mapped <- 1 + 4 * plogis(preds)
  
  frac <- mapped - floor(mapped)
  
  res <- ifelse(frac > 0.5,
                ceiling(mapped),
                floor(mapped))
  return(res)
}

predictions_lm <- predict(linear_model, newdata = test)
predictions_lm <- predict_helper(predictions_lm)
conf_mat_lm <- table(
  Predicho = predictions_lm,
  Observado = test$Q43
)

print(conf_mat_lm)

accuracy_lm <- mean(
  as.character(predictions_lm) == as.character(test$Q43)
)
cat("Accuracy:", round(accuracy_lm, 3), "\n")

# pregunta 9

loss_ordinal <- loss(as.numeric(predictions_ordinal), as.numeric(test$Q43))
loss_linear <- loss(predictions_lm, as.numeric(test$Q43))

loss_df <- tibble(
  model = c("Ordinal", "Linear"),
  loss  = c(loss_ordinal, loss_linear)
)

print(loss_df)

