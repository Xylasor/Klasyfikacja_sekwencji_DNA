# Funkcja pozwalajaca wybrac paramert maksymalizujacy precyzcje klasyfikacji drzewa binarnego
# Na wejsiu pobiera:
# - data - dataframe z danymi do klasyfikacji
# - folds - indeksy rzedow do k-krotnej walidacji krzyzowej
# - features - dotychczasowy zbior atrybutow
# Funkcja zwraca w zmiennej return list najelpszy atrybut oraz uzyskana dla neigo wartosc precyzji 
# Autor: Mateusz Hryciow, nr indeksu: 283365
selectFeatureWrapper <- function(data, folds, features)
{
  best_accuracy <- -Inf
  best_feature <- NULL
  class_feature <- colnames(data)[1]
  for (i in 2:ncol(data))
  {
    cur_feature <- colnames(data)[i]
    #print(cur_feature)
    if(!cur_feature %in% features)
    {
      accuracy = 0
      #print('--------------------')
      for (j in 1:length(folds))
      {
        train_data <- data[-c(folds[[j]]) ,c(class_feature, features, cur_feature)]
        test_data <- data[c(folds[[j]]) ,c(features, cur_feature)]
        test_data_class <- data[c(folds[[j]]) ,c(class_feature)]
        tree <- rpart(Class ~., data = train_data, method = 'class')
        pred <- predict(tree, test_data, type = 'class')
        accuracy = accuracy + sum(pred == test_data_class) / length(test_data_class)
        #print(accuracy)
      }
      accuracy = accuracy/length(folds)
      if (accuracy > best_accuracy)
      {
        best_accuracy = accuracy
        best_feature = cur_feature
        return_list <- list("feature"=best_feature, "accuracy"=best_accuracy)
      }
    }
  }
  return(return_list)
}