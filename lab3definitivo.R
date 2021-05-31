library(maxent)
#install.packages("tm")
library(tm)

data(iris)
attach(iris)
x <- subset(iris,select = -Species)
y <- Species
f <- tune.maxent(x,y,nfold=3,showall=TRUE,verbose=TRUE)
#install.packages("pkgbuild")
#pkgbuild::check_build_tools()


# Eficiencia accudracy (los mas altos)
# Regularizador indica el nivel de ajuste de los moelos
# use_gsd: para grandes cantidades de datos
# set_hellout: cuantos se quedan afuera del entrenamiento

model <- maxent(x,y,l1_regularizer = 0.0, l2_regularizer = 0.0, use_sgd = TRUE, set_heldout = 0, verbose = FALSE)
result <- predict(model,x[100:150,])
print(result)

#Aplicando sobre texto
#Aplicando sobre texto
#Aplicando sobre texto
#Aplicando sobre texto
#Aplicando sobre texto
#Aplicando sobre texto
#Aplicando sobre texto

data <- read.csv("/Users/matiascoronado/Downloads/datasetcagon.csv", fill=TRUE, header=TRUE, sep=",", row.names=NULL)

#Conjunto de textos relacionados a cada categoria.
corpus <- Corpus(VectorSource(data$Text))
print(corpus)
#summary(corpus)
#inspect(corpus)
#corpus[[1]]$content


###Pre-procesamiento

#Pasar todos los elementos a minusculas
corpus <- tm_map(corpus, content_transformer(tolower))
#Remover puntuacion (, ; .)
corpus <- tm_map(corpus, content_transformer(removePunctuation))
#Remover numeros.
corpus <- tm_map(corpus, content_transformer(removeNumbers))
#Eliminar salto de linea.
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, '\n')
#Elimianar stopwords.
corpus <- tm_map(corpus, content_transformer(removeWords),stopwords("english"))
#Eliminar espacios en blanco.
corpus <- tm_map(corpus, stripWhitespace)

#### Aplicando el metodo
# Ver filtros persoalizados para analizar bigramas (ejm. cuenta corriente)
matrix <- DocumentTermMatrix(corpus)
sparse <- as.compressed.matrix(matrix) 

# Relacion de datos 1/3.
f <- tune.maxent(sparse[1:3500,],data$Rating[1:3500],nfold = 3, showall = TRUE, verbose = TRUE)
print(f)

model <- maxent(sparse[1:3500,],data$Rating[1:3500],l1_regularizer = 0.0, l2_regularizer = 1.0, use_sgd = FALSE, set_heldout = 0, verbose = FALSE)

results <- predict(model,sparse[3501:5006,])

#Probabilidad de que alguno de estos casos pertenezcan a una de las categorias (columnas?)

#Hay que identificar cuales osn las etiquetas (label) relevantes





