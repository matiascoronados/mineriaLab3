library(maxent)
library(tm)

data <- as.data.frame(read.csv("/Users/matiascoronado/Downloads/datasetcagon.csv", fill=TRUE, header=TRUE, sep=",", row.names=NULL))
set.seed(60)
rows <- sample(nrow(data))
data <- data[rows,]
data$Rating <- round(data$Rating*10)
dataA <- data
dataA$Author <- factor(dataA$Author)

#Conjunto de textos relacionados a cada categoria.
corpus <- Corpus(VectorSource(data$Text))
print(corpus)
#summary(corpus)
#inspect(corpus)
corpus[[1]]$content


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

results <- as.data.frame(predict(model,sparse[3501:5006,]))


pcntjAcept <- 1.0

#Mayores a 0.7 son resuperados con exito.
copia <- results
df <- data.frame(copia$labels)

for(i in 1:nrow(copia)) {
  df$recuperado[i] <- 0
  if(copia$labels[i]==0){
    df$relevante[i] <- 0
    if(copia$'0'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  }
  else if(copia$labels[i]==1){
    df$relevante[i] <- 0
    if(copia$'1'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  }
  else if(copia$labels[i]==2){
    df$relevante[i] <- 0
    if(copia$'2'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  } 
  else if(copia$labels[i]==3){
    df$relevante[i] <- 1
    if(copia$'3'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  } 
  else if(copia$labels[i]==4){
    df$relevante[i] <- 0
    if(copia$'4'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  } 
  else if(copia$labels[i]==5){
    df$relevante[i] <- 0
    if(copia$'5'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  }
  else if(copia$labels[i]==6){
    df$relevante[i] <- 1
    if(copia$'6'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  }
  else if(copia$labels[i]==7){
    df$relevante[i] <- 1
    if(copia$'7'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  }  
  else if(copia$labels[i]==8){
    df$relevante[i] <- 1
    if(copia$'8'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  }
  else if(copia$labels[i]==9){
    df$relevante[i] <- 1
    if(copia$'9'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  }
  else{
    df$relevante[i] <- 1
    if(copia$'10'[i] >= pcntjAcept){
      df$recuperado[i] <- 1
    }
  }  
}



recuperado <- sum(df$recuperado)
noRecuperado <- nrow(df) - recuperado

VP <- 0
FP <- 0
FN <- 0
VN <- 0

for(i in 1:nrow(df)){
  if(df$recuperado[i] == 1 && df$relevante[i] == 1){
    VP <- VP + 1
  }else if(df$recuperado[i] == 1 && df$relevante[i] == 0){
    FP <- FP + 1
  }else if(df$recuperado[i] == 0 && df$relevante[i] == 1){
    FN <- FN + 1
  }else{
    VN <- VN + 1
  }
}

recuperado
noRecuperado
VP
FP
FN
VN

#              | Relevantes | No relevantes
#-------------------------------------------
# Recuperado   |     617   |      488
# No recuperado|     245    |      156

precicion = VP / (VP + FP)
#          = 617 / 617 + 488
recall = VP / (VP + FN)
#          = 617 / 617 + 245
calculoF1 <- 2*precicion*recall/(precicion + recall)

#Probabilidad de que alguno de estos casos pertenezcan a una de las categorias (columnas?)
precicion
recall
calculoF1
#ROC


#Hay que identificar cuales osn las etiquetas (label) relevantes

