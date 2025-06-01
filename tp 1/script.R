path <- './Documents/uba/modelado estadistico/tp 1/data.csv'
dataset <- read.table(path, sep = "", header = TRUE)

head(dataset)
columns <- names(dataset)
columns


set.seed(42)

# 1. separar en train y test
sample <- sample(c(TRUE, FALSE), nrow(dataset), replace=TRUE, prob=c(0.8,0.2))
train  <- dataset[sample, ]
test   <- dataset[!sample, ]

# 2. elegir una pregunta Q: Q43	I think a natural disaster would be kind of exciting.

# 3. Supongamos que queremos modelar la respuesta Q en funci ́on de la edad y el g ́enero. ¿Cúal
# ser ́ıa el problema te ́orico de usar una regresi ́on lineal para esto? ¿Cu ́al ser ́ıa el problema de usar
# una regresi ́on multinomial en este problema?

# Primero que nada, el tipo de supuestos que asume la regresion lineal para funcionar.
# En particular que la variable independiente [Nivel de acuerdo con Q] es una combinacion lineal del genero y la edad
# Esto le permite al modelo retornar valores como 2.5, PI, etc, es decir, variables no categoricas {1..5}.


# En el caso de la regresion multinomial, por motivos que aun no puedo entender (leer https://stats.oarc.ucla.edu/other/mult-pkg/whatstat/what-is-the-difference-between-categorical-ordinal-and-interval-variables/)
# el modelo pide/asume que los targets posibles {1..5} representas valores no "ordenables" es decir que no se le puede asignar un orden
# cuando en realidad sabemos que si (el mismo nivel de satisfaccion lo ordena). Luego no podemos aplicar el modelo


