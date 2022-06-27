# Skrypt umozliwiajacy dokonanie wyboru najelpszych atrybutow poprzez opakowanie algorytmu selekcji - metoda forward
# Autor: Mateusz Hryciow, nr indeksu: 283365
data <- read.delim("~/spliceDTrainKIS.dat", header = TRUE)
data_labels = data[seq(1,nrow(data),2), ]
data_DNA = data[seq(2,nrow(data),2), ]

library(stringr)
library(rpart)
library(rpart.plot)
library(MASS)
library(mlr)
library(FSelector)
library(caret)
source("selectFeatureWrapper.R")
set.seed(1)

# Stworzenie df zawierajacego informacje o kolejnych nukleotydach w sekwencji
DNA_len = nchar(data_DNA[1])
DNA = data.frame()
for (i in 1:length(data_DNA))
{
  dat <- data.frame(Class = data_labels[i], str_split_fixed(data_DNA[i],"",DNA_len))
  DNA <- rbind(DNA,dat)
}

# Usuwanie rzedow z blednymi danymi
DNA_letters = c("A","C","G","T")
for (i in 1:nrow(DNA))
{
  if (!all(unique(c(DNA[i,2:ncol(DNA)])) %in% DNA_letters))
  {
    DNA <- DNA[-c(i),]
  }
}


DNA_len = ncol(DNA)

# Tworzenie atrybutow bedacych parami 
for ( i in 2:(DNA_len-1) )
{
  coli = colnames(DNA)[i]
  for (j in (i+1):DNA_len)
  {
    colj = colnames(DNA)[j]
    name = paste(coli,"_",colj,sep="")
    DNA[[name]] <- paste(DNA[[coli]],DNA[[colj]])
  }
}


# Tworzenie atrybutow bedacych trojkami
for ( i in 2:(DNA_len-2) )
{
  coli = colnames(DNA)[i]
  for ( j in (i+1):(DNA_len-1))
  {
    colj = colnames(DNA)[j]
    for ( k in (j+1):(DNA_len))
    {
      colk = colnames(DNA)[k]
      name = paste(coli,"_",colj,"_",colk,sep="")
      DNA[[name]] <- paste(DNA[[coli]],DNA[[colj]],DNA[[colk]])
    }
  }
}

# k-krotna walidacja (k=10)
folds <- createFolds(DNA$Class, k = 10)


# Wybranie pierwszego atrybutu dla najwiekszego zysku informacji ("information gain")
ig=information.gain(Class~. ,DNA)
feat1=row.names(ig)[which.max(ig$attr_importance)]
features <- c(feat1)
accuracy <- c()
# Iterowanie w celu okreslenia kolejnych atrybutow, metoda forward
for (i in 1:10) {
  results <- selectFeatureWrapper(DNA, folds, features)
  print(results$feature)
  features <- c(features, results$feature)
  print(features)
  accuracy <- c(accuracy, results$accuracy)
}



# Okreslenie macierzy pomylek
for (j in 1:length(folds))
{
  train_data <- DNA[-c(folds[[j]]) ,c('Class',features)]
  test_data <- DNA[c(folds[[j]]) ,c(features)]
  test_data_class <- DNA[c(folds[[j]]) ,c('Class')]
  tree <- rpart(Class ~., data = train_data, method = 'class')
  pred <- predict(tree, test_data, type = 'class')
  if (j == 1)
  {
    conf_matrix = table(pred,test_data_class)
  } else {
    conf_matrix = conf_matrix + table(pred,test_data_class)
  }
  
}
print(conf_matrix)