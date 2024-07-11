
library(tidyverse)
library(splitstackshape)
library(caret)
library(randomForest)


setwd("C:/Users/ENRIQUE/OneDrive/Escritorio/Uni/4º/TFG")
datos_final<- read.csv("datos_final.csv", header=TRUE)

datos_final <- datos_final[, -1]

#Modelos

#######################3 RF Y ARBOLES
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


mtry_values = c( round(sqrt(ncol(training_stratified)-1),0), round((ncol(training_stratified)-1)/2,0), round((ncol(training_stratified)-1)/3,0) ) 
ntree_values = c(100,200,500)
nodesize_values = c(1,2,5,10)

best_rf_accuracy = 0
best_rf_recall=0
set.seed(1)
for (mtry in mtry_values){
  for (ntree in ntree_values) {
    for(nodesize in nodesize_values){
      mean_accuracy = 0
      accuracies = c()
      mean_recall= 0
      recalls= c()
      for(folds_index in c(1:10)){
        rf = randomForest(training_stratified[-c(which(folds==folds_index)),-c(ncol(training_stratified)-1)],as.factor(training_stratified[-c(which(folds==folds_index)),c(ncol(training_stratified)-1)]) ,
                   mtry = mtry ,
                   ntree = ntree,
                   nodesize = nodesize)
        
        
        prediction = predict(rf,training_stratified[c(which(folds==folds_index)),-c(ncol(training_stratified)-1)])
        real = as.factor(training_stratified[c(which(folds==folds_index)),c(ncol(training_stratified)-1)])
        (conf_table = table(real,prediction))
        (accuracy = sum(conf_table[1,1],conf_table[2,2],conf_table[2,3],conf_table[3,2],conf_table[3,3])/(sum(conf_table)))
        (recall= (conf_table[1,1])/(conf_table[1,1]+conf_table[1,3]+conf_table[1,2]))
        
        accuracies = c(accuracies,accuracy)
        recalls = c( recalls, recall)
                    
      }
      if (mean(accuracies) > best_rf_accuracy){
        best_mtry_acc = mtry
        best_ntree_acc = ntree
        best_nodesize_acc = nodesize
        best_rf_accuracy = mean(accuracies)
        
        
      }
      
      if (mean(recalls) > best_rf_recall){
        best_mtry_rec = mtry
        best_ntree_rec = ntree
        best_nodesize_rec = nodesize
        best_rf_recall = mean (recalls)
      }
      
    }
  }
}

cat("Best mtry in terms of accuracy:", best_mtry_acc, "\n")
cat("Best ntree in terms of accuracy:", best_ntree_acc, "\n")
cat("Best nodesize in terms of accuracy:", best_nodesize_acc, "\n")
cat("Best accuracy:", best_rf_accuracy, "\n")

cat("Best mtry in terms of recall:", best_mtry_rec, "\n")
cat("Best ntree in terms of recall:", best_ntree_rec, "\n")
cat("Best nodesize in terms of recall:", best_nodesize_rec, "\n")
cat("Best recall:", best_rf_recall, "\n")

conf_table
# Falta validar el modelo en el conjunto de test. Para esto es necesario calcular la metrica que queramos, y con los mejores parametros meterlos
#en testing stratified (creandolo previamente) y sacar las metricas q nos da

final_rf_acc <- randomForest(
  training_stratified[, -c(ncol(training_stratified)-1)],
  as.factor(training_stratified[, ncol(training_stratified)-1]),
  mtry = best_mtry_acc,
  ntree = best_ntree_acc,
  nodesize = best_nodesize_acc
)


testing_predictions_acc <- predict(final_rf_acc, testing_stratified[, -c(ncol(testing_stratified)-1)])
testing_real_acc <- as.factor(testing_stratified[, ncol(testing_stratified)-1])
testing_conf_table_acc <- table(testing_real_acc, testing_predictions_acc)
testing_accuracy <- sum(testing_conf_table_acc[1,1],testing_conf_table_acc[2,2],testing_conf_table_acc[2,3],testing_conf_table_acc[3,2],testing_conf_table_acc[3,3]) / sum(testing_conf_table_acc)

cat("Testing accuracy:", testing_accuracy, "\n")


final_rf_rec <- randomForest(
  training_stratified[, -c(ncol(training_stratified)-1)],
  as.factor(training_stratified[, ncol(training_stratified)-1]),
  mtry = best_mtry_rec,
  ntree = best_ntree_rec,
  nodesize = best_nodesize_rec
)


testing_predictions_rec <- predict(final_rf_rec, testing_stratified[, -c(ncol(testing_stratified)-1)])
testing_real_rec <- as.factor(testing_stratified[, ncol(testing_stratified)-1])
testing_conf_table_rec <- table(testing_real_rec, testing_predictions_rec)
testing_recall<-(testing_conf_table_rec[1,1])/(testing_conf_table_rec[1,1]+testing_conf_table_rec[1,3]+testing_conf_table_rec[1,2])

cat("Testing rf recall:", testing_recall, "\n")





########################################### KNN
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



set.seed(1)
folds = createFolds(training_stratified_knn[, ncol(training_stratified_knn)-1], k = 10, list = TRUE)

neighbours_values = c(3, 5, 7, 9, 11)

best_accuracy_knn = 0
best_recall_knn = 0
set.seed(1)
for (k in neighbours_values) {
  mean_accuracy = 0
  accuracies = c()
  mean_recall = 0
  recalls = c()
  
  for (folds_index in 1:10) {
    train_indices = unlist(folds[-folds_index])
    test_indices = unlist(folds[folds_index])
    
    train_data = training_stratified_knn[train_indices,]
    test_data = training_stratified_knn[test_indices,]
    
    train_x = train_data[, -ncol(train_data)-1]
    train_y = as.factor(train_data[, ncol(train_data)-1])
    test_x = test_data[, -ncol(test_data)-1]
    test_y = as.factor(test_data[, ncol(test_data)-1])
    
    
    # Applying KNN
    prediction = knn(train = train_x, test = test_x, cl = train_y, k = k)
    attributes(prediction)$nn.dist = NULL
    attributes(prediction)$nn.index=NULL
    # Evaluate the predictions
    conf_table = table(test_y, prediction)
    if(dim(conf_table)[1] == 3 & dim(conf_table)[2] == 3){
      accuracy =  sum(conf_table[1,1],conf_table[2,2],conf_table[2,3],conf_table[3,2],conf_table[3,3])/(sum(conf_table))
      recall= (conf_table[1,1])/(conf_table[1,1]+conf_table[1,3]+conf_table[1,2])
    }else{
      accuracy =  sum(conf_table[1,1],conf_table[2,2])/(sum(conf_table))
      recall= (conf_table[1,1])/(conf_table[1,1]+conf_table[1,2])
    }
    
    accuracies = c(accuracies, accuracy)
    recalls = c(recalls, recall)
  }
  
  if (mean(accuracies) > best_accuracy_knn) {
    best_k_acc = k
    best_accuracy_knn = mean(accuracies)
  }
  
  if (mean(recalls) > best_recall_knn) {
    best_k_rec = k
    best_recall_knn = mean(recalls)
  }
}

# Output the best hyperparameters
cat("Best Accuracy Parameters: k =", best_k_acc, ", Accuracy =", best_accuracy_knn, "\n")
cat("Best Recall Parameters: k =", best_k_rec, ", Recall =", best_recall_knn, "\n")


# Falta validar el modelo en el conjunto de test. Para esto es necesario calcular la metrica que queramos, y con los mejores parametros meterlos
#en testing stratified (creandolo previamente) y sacar las metricas q nos da

final_knn_acc <- knn(train = train_x, test = test_x, cl = train_y, k = best_k_acc)
attributes(prediction)$nn.dist = NULL
attributes(prediction)$nn.index=NULL
testing_conf_table_acc <- table(test_y, final_knn_acc)
if(dim(testing_conf_table_acc)[1] == 3 & dim(testing_conf_table_acc)[2] == 3){
  testing_knn_accuracy =  sum(testing_conf_table_acc[1,1],testing_conf_table_acc[2,2],testing_conf_table_acc[2,3],testing_conf_table_acc[3,2],testing_conf_table_acc[3,3])/(sum(testing_conf_table_acc))
  testing_recall= (testing_conf_table_acc[1,1])/(testing_conf_table_acc[1,1]+testing_conf_table_acc[1,3]+testing_conf_table_acc[1,2])
}else{
  testing_knn_accuracy =  sum(testing_conf_table_acc[1,1],testing_conf_table_acc[2,2])/(sum(testing_conf_table_acc))
  testing_recall= (testing_conf_table_acc[1,1])/(testing_conf_table_acc[1,1]+testing_conf_table_acc[1,2])
}
cat("Testing accuracy:", testing_knn_accuracy, "\n")
cat("Testing rf recall:", testing_recall, "\n")






##################################### REG LOGISTICA ########################################

# Load necessary libraries
library(MASS)
library(nnet)
# Load a dataset (for example, Pima Indians Diabetes dataset)



set.seed(1)
# Fit the logistic regression model
reg_log<- multinom(training_stratified$respuesta ~ ., data = training_stratified)


final_reg_log<- multinom(training_stratified$respuesta ~ ., data = training_stratified)

testing_predictions_reg_log <- predict(final_reg_log, testing_stratified[, -c(ncol(testing_stratified)-1)])
testing_real_reg_log <- as.factor(testing_stratified[, ncol(testing_stratified)-1])
testing_conf_table_reg_log <- table(testing_real_reg_log, testing_predictions_reg_log)
print(testing_conf_table_reg_log)


testing_reg_log_accuracy <- sum(testing_conf_table_reg_log[1,1],testing_conf_table_reg_log[2,2],testing_conf_table_reg_log[2,3],testing_conf_table_reg_log[3,3]) / sum(testing_conf_table_reg_log)
cat("Testing accuracy:", testing_reg_log_accuracy, "\n")



testing_reg_log_recall<-(testing_conf_table_reg_log[1,1])/(testing_conf_table_reg_log[1,1]+(testing_conf_table_reg_log[1,3]+testing_conf_table_reg_log[1,2]))
cat("Testing accuracy:", testing_reg_log_recall, "\n")










################################################ SVM #######################################################


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










################################## REDES NEURONALES




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


testing_predictions_acc <- predict(final_nn_acc, testing_stratified[, -c(ncol(testing_stratified)-1)])
testing_real_acc <- as.factor(testing_stratified[, ncol(testing_stratified)-1])
testing_conf_table_acc <- table(testing_real_acc, testing_predictions_acc)
testing_accuracy <- sum(testing_conf_table_acc[1,1],testing_conf_table_acc[2,2],testing_conf_table_acc[2,3],testing_conf_table_acc[3,2],testing_conf_table_acc[3,3]) / (sum(testing_conf_table_acc))

cat("Testing accuracy:", testing_accuracy, "\n")



print(testing_real_acc)
View(testing_real_acc)

View(testing_predictions_acc)


final_rf_rec <- randomForest(
  training_stratified[, -c(ncol(training_stratified)-1)],
  as.factor(training_stratified[, ncol(training_stratified)-1]),
  mtry = best_mtry_rec,
  ntree = best_ntree_rec,
  nodesize = best_nodesize_rec
)


testing_predictions_rec <- predict(final_rf_rec, testing_stratified[, -c(ncol(testing_stratified)-1)])
testing_real_rec <- as.factor(testing_stratified[, ncol(testing_stratified)-1])
testing_conf_table_rec <- table(testing_real_rec, testing_predictions_rec)
testing_recall<-(testing_conf_table_rec[1,1])/(testing_conf_table_rec[1,1]+testing_conf_table_rec[1,3]+testing_conf_table_rec[1,2])

cat("Testing rf recall:", testing_recall, "\n")










