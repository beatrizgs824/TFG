


#################################################   EXPLORATORY DATA ANALYSIS ############################################################


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
library(gplots)
library(reshape2)

setwd("C:/Users/ENRIQUE/OneDrive/Escritorio/Uni/4º/TFG")
data <- read.csv("TFG_2_nuevo.csv", header=TRUE, stringsAsFactors = TRUE) #antes de correr esto nos metemos en spreadsheets - borramos columna I - 
#nos aseguramos de que todas las preguntas aparezcan en la misma linea (no como punto y aparte). Luego tambien en spreadsheets tengo que reemplazar
#<= por <= en caso de que haya. Muy importante introducir esta fórmula:
#Sustituir el prácticamente todos los días por prácticamente todos los días (>11)
View(data)
#Empezamos eliminando las columnas innecesarias (Timestamp y correo electrónico de los participantes, aparte de las dos primeras filas que
#están vacías)
data<- data[,-c(1,28)]



#HACIENDOLO UNO A UNO

names(data)[1] <- "Sexo"
names(data)[2] <- "Edad"
names(data)[3] <- "Tres_RRSS"
names(data)[4] <- "Bucle"
names(data)[5] <- "Mente_En_Blanco"
names(data)[6] <- "Irritabilidad"
names(data)[7] <- "Preocupacion_Suceso_Terrible"
names(data)[8] <- "Tristeza"
names(data)[9] <- "Cansancio"
names(data)[10] <- "Comer_Poco"
names(data)[11] <- "Comer_Exceso"
names(data)[12] <- "Fracaso"
names(data)[13] <- "Dificultad_Concentrarse"
names(data)[14] <- "Pensamientos_Suicidas"
names(data)[15] <- "Tiempo_Sueño"
names(data)[16] <- "Duracion_Sueño"
names(data)[17] <- "Trastorno_Alimenticio"
names(data)[18] <- "Comparacion_Fisica"
names(data)[19] <- "Satisfaccion_Conductas_Alimenticias"
names(data)[20] <- "Peso_Autoimagen"
names(data)[21] <- "Promedio_Diario_Movil"
names(data)[22] <- "Nocion_Tiempo"
names(data)[23] <- "Necesidad_Urgente"
names(data)[24] <- "Desbloqueo_Movil_Constante"
names(data)[25] <- "Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil"
names(data)[26] <- "%De_Tiempo_De_Pantalla_Con_El_Movil"






cat_vars <- c("Sexo","Edad","Bucle","Mente_En_Blanco","Irritabilidad","Preocupacion_Suceso_Terrible","Tristeza","Cansancio",
              "Comer_Poco","Comer_Exceso","Fracaso","Dificultad_Concentrarse","Pensamientos_Suicidas","Tiempo_Sueño","Duracion_Sueño","Trastorno_Alimenticio",
              "Comparacion_Fisica","Satisfaccion_Conductas_Alimenticias","Peso_Autoimagen","Promedio_Diario_Movil","Nocion_Tiempo","Necesidad_Urgente",
              "Desbloqueo_Movil_Constante","%De_Tiempo_De_Pantalla_Con_El_Movil","Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil")




library(dplyr)

# Assuming your dataset is named 'your_dataset'



respuesta = c()
for(j in c(1:nrow(data))){
  cadena_rota = strsplit(as.character(data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil)[j], ", ", fixed = FALSE)
  emocion = c()
  for(i in c(1:length(cadena_rota[[1]]))){
    if(cadena_rota[[1]][i] %in% c("Feliz", "Sereno/Tranquilo", "Realizado", "Productivo", "Satisfecho", "Capacitado", "Orgulloso")){
      emocion = c(emocion,"Positive")
    }else{
      emocion = c(emocion,"Negative")
    }
  }
  if(length(unique(emocion)) == 2){
    respuesta_aux = "Neutral"
  }else{
    if(unique(emocion) == "Positive"){
      respuesta_aux = "Positive"
    }else{
      respuesta_aux = "Negative"
    }
  }
  respuesta = c(respuesta,respuesta_aux)
}



data$"Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil" = as.factor(respuesta)






results <- list()

for (i in 1:(length(cat_vars) - 1)) {
  for (j in (i+1):length(cat_vars)) {
    result <- chisq.test(data[[cat_vars[i]]], data[[cat_vars[j]]])
    
    results[[paste(cat_vars[i], cat_vars[j], sep = "_vs_")]] <- result
    print(i)
    print(j)
  }
}

for (key in names(results)) {
  print(paste("Chi-square test result for", key))
  print(results[[key]])
}




results_df <- data.frame(Comparación = character(),
                         p_value = numeric(),
                         stringsAsFactors = FALSE)

for (i in 1:(length(cat_vars) - 1)) {
  for (j in (i+1):length(cat_vars)) {
    result <- chisq.test(data[[cat_vars[i]]], data[[cat_vars[j]]])
    comparison <- paste(cat_vars[i], cat_vars[j], sep = "_vs_")
    p_value <- result$p.value
    results_df <- rbind(results_df, data.frame(Comparación = comparison, p_value = p_value))
  }
}

print(results_df)
significant_results <- subset(results_df, p_value > 0.05 | p_value > 0.01)
print(significant_results)



cramer_results <- list()
for (i in 1:(length(cat_vars) - 1)) {
  for (j in (i+1):length(cat_vars)) {
    chi_sq_result <- chisq.test(data[[cat_vars[i]]], data[[cat_vars[j]]])
    n <- sum(chi_sq_result$observed)  # Total number of observations
    phi <- sqrt(chi_sq_result$statistic / n)  # Phi coefficient (effect size)
    cramers_v <- phi / sqrt(min(nrow(chi_sq_result$observed) - 1, ncol(chi_sq_result$observed) - 1))  # Cramér's V
    comparison <- paste(cat_vars[i], cat_vars[j], sep = "_vs_")
    cramer_results[[comparison]] <- cramers_v
  }
}

length(cramer_results)
matrizresultados = matrix(0,nrow=24,24)
colnames(matrizresultados) = cat_vars[-length(cat_vars)]
rownames(matrizresultados) = cat_vars[-1]
matrizresultados
dim(matrizresultados)
contador =1
for(i in c(1:24)){
  for(j in c(i:24)){
    print("Entra")
    matrizresultados[j,i] = as.numeric(cramer_results[contador])
    print("Entra2")
    contador = contador + 1
    print(contador)
  }
}
View(matrizresultados)
dim(matrizresultados)
length(cat_vars)
matrizresultados = rbind(0,matrizresultados)
matrizresultados = cbind(matrizresultados,0)
dim(matrizresultados)
length(cat_vars)
colnames(matrizresultados) = cat_vars
rownames(matrizresultados) = cat_vars
View(matrizresultados)
diag(matrizresultados) = 0.5
matrizresultados = t(matrizresultados) + matrizresultados
View(matrizresultados)


# MAPA DE CALOR 


# Zero out the lower triangular part to only show the upper triangular part
matrizresultados[lower.tri(matrizresultados)] <- NA

# Melt the matrix into long format
melted_matriz <- melt(matrizresultados, na.rm = TRUE)


library(ggplot2)


# Create the heatmap
ggplot(data = melted_matriz, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  labs(title = "Correlación entre Variables medida con la V de Cramer",
       x = "Variable", y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





















