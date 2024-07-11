
########################### SVM



library(tidyverse)
library(splitstackshape)
library(caret)
library(randomForest)


setwd("C:/Users/ENRIQUE/OneDrive/Escritorio/Uni/4º/TFG")
datos_final<- read.csv("datos_final.csv", header=TRUE)

datos_final <- datos_final[, -1]

#Modelos


library(fastDummies)
library(dplyr)
library(tidyverse) # For data manipulation and visualization
library(purrr)     # For functional programming
library(skimr)
library(ggplot2)
library(tidyr)
library(dummy)
library(caret)
library(randomForest)
library(rpart)
library(randomForest)
library(caret)
library(e1071)

datos_final_2 = datos_final[,-c(2:13)]

edad_strat = c()
for(i in c(1:nrow(datos_final))){
  edad_strat = c(edad_strat,0)
  for(j in c(2:13)){
    if(datos_final[i,j] == 1)
      edad_strat[i] = j-1
  }
}

edad_strat

datos_final_2$EDADS = edad_strat

library(dplyr)

summary_data <- datos_final_2 %>%
  group_by(respuesta) %>%
  summarise(count = n())

print(summary_data)




set.seed(1)
training_stratified <- datos_final_2 %>% stratified(., group = "EDADS", size = 0.80) # Muestra 80% / 20%
table(datos_final_2$EDADS)/nrow(datos_final_2)
table(training_stratified$EDADS)/nrow(training_stratified)

set.seed(1)
folds <- createFolds(factor(training_stratified$EDADS), k = 10, list = FALSE)

training_stratified = as.data.frame(training_stratified)
training_indices <- as.numeric(rownames(training_stratified))
testing_stratified <- datos_final_2[-training_indices, ]

cat("Training set distribution:\n")
print(table(training_stratified$EDADS) / nrow(training_stratified))
cat("Testing set distribution:\n")
print(table(testing_stratified$EDADS) / nrow(testing_stratified))



library(class)
library(FNN)


library(class)
library(caret)
library(fastDummies)

# Define values for k (number of neighbors) to be tested


# Assuming `training_stratified` is your dataset and `folds` contains the fold indices for cross-validation
str(training_stratified)

training_stratified_knn = training_stratified
training_stratified_knn[["respuesta"]] <- as.factor(training_stratified_knn[["respuesta"]])
levels <- levels(training_stratified_knn[["respuesta"]])
training_stratified_knn[["respuesta"]] <- as.numeric(training_stratified_knn[["respuesta"]])

category_mapping <- data.frame(Category = levels, Numerical = 1:length(levels))
print("Category to numerical mapping:")
print(category_mapping)


str(training_stratified_knn)




################################## SVM

exponents <- seq(-5, 5, by = 1)
c_values <- 2^exponents 
print(c_values)


# kernel lineal

best_svml_accuracy = 0
best_svml_recall=0
set.seed(1)
for (c in c_values){
  mean_accuracy = 0
  accuracies = c()
  mean_recall= 0
  recalls= c()
  for(folds_index in c(1:10)){
    
    svm_model <- svm(as.factor(training_stratified_knn$respuesta) ~ ., data = training_stratified_knn, kernel = "linear", cost = c_values)
    
    prediction = predict(svm_model, training_stratified_knn[c(which(folds==folds_index)),-c(ncol(training_stratified_knn)-1)])
    real = as.factor(training_stratified_knn[c(which(folds==folds_index)),c(ncol(training_stratified_knn)-1)])
    (conf_table = table(real,prediction))
    (accuracy = sum(conf_table[1,1],conf_table[2,2],conf_table[2,3],conf_table[3,2],conf_table[3,3])/sum(conf_table))
    recall<- (conf_table[1,1])/(conf_table[1,1]+conf_table[1,3]+conf_table[1,2])
    accuracies = c(accuracies,accuracy)
    recalls = c(recalls, recall)
    
  }
  if (mean(accuracies) > best_svml_accuracy){
    best_c_acc = c
    best_svml_accuracy = mean(accuracies)
    
    
  }
  
  if (mean(recalls) > best_svml_recall){
    best_c_rec = c
    best_svml_recall = mean (recalls)
  }
}

cat("Best c in terms of accuracy:", best_c_acc, "\n")
cat("Best accuracy:", best_svml_accuracy, "\n")

cat("Best c in terms of recall:", best_c_rec, "\n")
cat("Best recall:", best_svml_recall, "\n")



# Falta validar el modelo en el conjunto de test. Para esto es necesario calcular la metrica que queramos, y con los mejores parametros meterlos
#en testing stratified (creandolo previamente) y sacar las metricas q nos da

final_svml <-svm(as.factor(training_stratified_knn$respuesta) ~ ., data = training_stratified_knn, kernel = "linear", cost = best_c_acc)



testing_predictions_svml <- predict(final_svml, testing_stratified[, -c(ncol(testing_stratified)-1)])
testing_real <- as.factor(testing_stratified[, ncol(testing_stratified)-1])
testing_conf_table <- table(testing_real, testing_predictions_svml)

testing_svml_accuracy <- sum(testing_conf_table[1,1],testing_conf_table[2,2],testing_conf_table[2,3],testing_conf_table[3,2],testing_conf_table[3,3]) / sum(testing_conf_table)
cat("Testing svml accuracy:", testing_svml_accuracy, "\n")


testing_svml_recall<-(testing_conf_table[1,1])/(testing_conf_table[1,1]+testing_conf_table[1,3]+testing_conf_table[1,2])

cat("Testing svml recall:", testing_svml_recall, "\n")















# kernel radial

gamma_values= 2^exponents

best_svmr_accuracy = 0
best_svmr_recall=0
set.seed(1)
for (c in c_values){
  for (gamma in gamma_values){
    mean_accuracy = 0
    accuracies = c()
    mean_recall= 0
    recalls= c()
    for(folds_index in c(1:10)){
      
      svm_model <- svm(as.factor(training_stratified_knn$respuesta) ~ ., data = training_stratified_knn, kernel = "radial", cost = c_values, gamma= gamma_values)
      
      prediction = predict(svm_model, training_stratified_knn[c(which(folds==folds_index)),-c(ncol(training_stratified_knn)-1)])
      real = as.factor(training_stratified_knn[c(which(folds==folds_index)),c(ncol(training_stratified_knn)-1)])
      (conf_table = table(real,prediction))
      (accuracy = sum(diag(conf_table))/sum(conf_table))
      (recall= (conf_table[1,1])/(conf_table[1,1]+conf_table[1,3]))
      
      accuracies = c(accuracies,accuracy)
      recalls = c( recalls, recall)
      
    }
    if (mean(accuracies) > best_svmr_accuracy){
      best_c_acc = c
      best_gamma_acc = gamma
      best_svmr_accuracy = mean(accuracies)
      
      
    }
    
    if (mean(recalls) > best_svmr_recall){
      best_c_rec = c
      best_gamma_rec = gamma
      best_svm_recall = mean (recalls)
    }
  }
}

cat("Best c in terms of accuracy:", best_c_acc, "\n")
cat("Best gamma in terms of accuracy:", best_gamma_acc, "\n")
cat("Best accuracy:", best_svmr_accuracy, "\n")

cat("Best c in terms of recall:", best_c_rec, "\n")
cat("Best gamma in terms of recall:", best_gamma_rec, "\n")
cat("Best recall:", best_svm_recall, "\n")



# Falta validar el modelo en el conjunto de test. Para esto es necesario calcular la metrica que queramos, y con los mejores parametros meterlos
#en testing stratified (creandolo previamente) y sacar las metricas q nos da

final_svmr_acc <- svm(as.factor(training_stratified_knn$respuesta) ~ ., data = training_stratified_knn, kernel = "radial", cost = best_c_acc, gamma = best_gamma_acc)


testing_predictions_acc <- predict(final_svmr_acc, testing_stratified[, -c(ncol(testing_stratified)-1)])
testing_real_acc <- as.factor(testing_stratified[, ncol(testing_stratified)-1])
testing_conf_table_acc <- table(testing_real_acc, testing_predictions_acc)
testing_svmr_accuracy <- sum(testing_conf_table_acc[1,1],testing_conf_table_acc[2,2],testing_conf_table_acc[2,3],testing_conf_table_acc[3,2],testing_conf_table_acc[3,3]) / sum(testing_conf_table_acc)

cat("Testing accuracy:", testing_svmr_accuracy, "\n")


final_svmr_rec <- svm(as.factor(training_stratified_knn$respuesta) ~ ., data = training_stratified_knn, kernel = "radial", cost = best_c_rec, gamma = best_gamma_rec)


testing_predictions_rec <- predict(final_svmr_rec, testing_stratified[, -c(ncol(testing_stratified)-1)])
testing_real_rec <- as.factor(testing_stratified[, ncol(testing_stratified)-1])
testing_conf_table_rec <- table(testing_real_rec, testing_predictions_rec)
testing_svm_recall<-(testing_conf_table_rec[1,1])/(testing_conf_table_rec[1,1]+testing_conf_table_rec[1,3]+testing_conf_table_rec[1,2])

cat("Testing rf recall:", testing_svm_recall, "\n")






######################################## REDES NEURONALES



library(dplyr)
library(neuralnet)
# Assuming training_stratified_knn is your dataset
# Select every column except 'respuesta'
selected_data <- training_stratified_knn[, names(training_stratified_knn) != "respuesta"]


# Create the formula dynamically
predictor_vars <- names(selected_data)
formula <- as.formula(paste("respuesta ~", paste(predictor_vars, collapse = " + ")))

training_stratified_knn$respuesta = as.factor(training_stratified$respuesta)

hidden_values = c(5, c(5,3),c(10,5,3))
threshold = 0.1
my_vector <- seq(0.01, 0.1, by = 0.01)
learningrate_values = my_vector
act_values = c("logistic","tanh")

best_nn_accuracy = 0
best_nn_recall=0
set.seed(1)
folds <- createFolds(factor(training_stratified$EDADS), k = 10, list = FALSE)
for (hidden in hidden_values){
  for (threshold in threshold) {
    for(learningrate in learningrate_values){
      for(act in act_values){
        mean_accuracy = 0
        accuracies = c()
        mean_recall= 0
        recalls= c()
        for(folds_index in c(1:10)){
          nn = neuralnet(formula, data= training_stratified_knn,
                         hidden = hidden ,
                         threshold = threshold,
                         learningrate = learningrate,
                         act.fct= act)
          
          
          prediction = predict(nn,training_stratified[c(which(folds==folds_index)),-c(ncol(training_stratified)-1)])
          colnames(prediction) = c("Negative","Neutral","Positive")
          
          predicted_classes <- apply(prediction, 1, function(x) {
            colnames(prediction)[which.max(x)]
          })
          
          
          real = as.factor(training_stratified[c(which(folds==folds_index)),c(ncol(training_stratified)-1)])
          (conf_table = table(as.factor(real),as.factor(predicted_classes)))
          (accuracy = sum(diag(conf_table))/sum(conf_table))
          if(dim(conf_table)[1] == 3 & dim(conf_table)[2] == 3){
            (recall= (conf_table[1,1])/(conf_table[1,1]+(conf_table[1,3]+conf_table[1,2])))
          }else if(dim(conf_table)[1]==3 & dim(conf_table)[2]==1) {
            recall= (conf_table[1,1])/conf_table[1,1]
          }else{ 
            (recall= (conf_table[1,1])/(conf_table[1,1]+(conf_table[1,2])))
          }
          
          accuracies = c(accuracies,accuracy)
          recalls = c( recalls, recall)
          
        }
        if (mean(accuracies) > best_nn_accuracy){
          best_hidden_acc = hidden
          best_threshold_acc = threshold
          best_learningrate_acc = learningrate
          best_act_acc = act
          best_nn_accuracy = mean(accuracies)
          
          
        }
        
        if (mean(recalls) > best_nn_recall){
          best_hidden_rec = hidden
          best_threshold_rec = threshold
          best_learningrate_rec = learningrate
          best_act_rec = act
          best_nn_recall = mean (recalls)
        }
        
      }
    }
  }
}

cat("Best hidden in terms of accuracy:", best_hidden_acc, "\n")
cat("Best threshold in terms of accuracy:", best_threshold_acc, "\n")
cat("Best learningrate in terms of accuracy:", best_learningrate_acc, "\n")
cat("Best act in terms of accuracy:", best_act_acc, "\n")
cat("Best accuracy:", best_nn_accuracy, "\n")

cat("Best hidden in terms of recall:", best_hidden_rec, "\n")
cat("Best threshold in terms of recall:", best_threshold_rec, "\n")
cat("Best learningrate in terms of recall:", best_learningrate_rec, "\n")
cat("Best act in terms of accuracy:", best_act_rec, "\n")
cat("Best recall:", best_nn_recall, "\n")



set.seed(1)
final_nn_acc <- neuralnet(formula, data= training_stratified_knn,
                          hidden = best_hidden_acc ,
                          threshold = best_threshold_acc,
                          learningrate = best_learningrate_acc,
                          act.fct= best_act_acc)


# Assuming 'final_nn_acc' is already defined

# Make predictions on testing data
testing_predictions_acc_prob <- predict(final_nn_acc, testing_stratified[, -c(ncol(testing_stratified)-1)])
colnames(testing_predictions_acc_prob) <- c("Negative", "Neutral", "Positive")

# Extract predicted classes
testing_predictions_acc <- apply(testing_predictions_acc_prob, 1, function(x) {
  colnames(testing_predictions_acc_prob)[which.max(x)]
})

# Convert actual classes to factor
testing_real_acc <- as.factor(testing_stratified[, ncol(testing_stratified)-1])

# Create confusion table
testing_conf_table_acc <- table(testing_real_acc, testing_predictions_acc)

# Calculate testing accuracy
testing_accuracy <- sum(diag(testing_conf_table_acc)) / sum(testing_conf_table_acc)

cat("Testing accuracy:", testing_accuracy, "\n")













set.seed(1)
final_nn_rec <- neuralnet(formula, data= training_stratified_knn,
                          hidden = best_hidden_rec ,
                          threshold = best_threshold_rec,
                          learningrate = best_learningrate_rec,
                          act.fct= best_act_rec)


# Assuming 'final_nn_acc' is already defined

# Make predictions on testing data
testing_predictions_rec_prob <- predict(final_nn_rec, testing_stratified[, -c(ncol(testing_stratified)-1)])
colnames(testing_predictions_rec_prob) <- c("Negative", "Neutral", "Positive")

# Extract predicted classes
testing_predictions_rec <- apply(testing_predictions_rec_prob, 1, function(x) {
  colnames(testing_predictions_rec_prob)[which.max(x)]
})

# Convert actual classes to factor
testing_real_rec <- as.factor(testing_stratified[, ncol(testing_stratified)-1])

# Create confusion table
testing_conf_table_rec <- table(testing_real_rec, testing_predictions_rec)

# Calculate testing accuracy
testing_recall <- (conf_table[1,1])/(conf_table[1,1]+(conf_table[1,2]))

cat("Testing recall:", testing_recall, "\n")


