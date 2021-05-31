library(maxent)
install.packages("tm")
library(tm)
library(dismo)
data(iris)
attach(iris)
x <- subset(iris,select = -Species)
y <- Species
f <- tune.maxent(x,y,nfold=3,showall=TRUE,verbose=TRUE)

install.packages("pkgbuild")
pkgbuild::check_build_tools()


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


data <- read.csv("/Users/matiascoronado/Downloads/NYTimes.csv.gz")

corpus <- Corpus(VectorSource(data$Title[1:150]))
print(corpus)
summary(corpus)
inspect(corpus)

corpus[[1]]$content

#### Pre procesamiento -- ver contexto del problema


#Remover puntuacion (, ; .)
corpus <- tm_map(corpus, content_transformer(removePunctuation))

#Elimianar stopwords
corpus <- tm_map(corpus, content_transformer(removeWords),stopwords("english"))

#content_transformer(tolower)
#content_transformer(removeNumbers)
#eliminar espacios en blanco

corpus <- tm_map(corpus, stripWhitespace)


#### Aplicando el metodo

# Ver filtros persoalizados para analizar bigramas (ejm. cuenta corriente)

matrix <- DocumentTermMatrix(corpus)
sparse <- as.compressed.matrix(matrix) 

f <- tune.maxent(sparse[1:100,],data$Topic.Code[1:100],nfold = 3, showall = TRUE, verbose = TRUE)
print(f)

model <- maxent(sparse[1:100,],data$Topic.Code[1:100],l1_regularizer = 0.2, l2_regularizer = 0.0, use_sgd = FALSE, set_heldout = 0, verbose = FALSE)


results <- predict(model,sparse[101:120,])

#Probabilidad de que alguno de estos casos pertenezcan a una de las categorias (columnas?)

#Hay que identificar cuales osn las etiquetas (label) relevantes


