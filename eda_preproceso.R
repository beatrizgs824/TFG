

############################## TFG ############################################################################################3

# Se cargan las librerías necesarias y los datos

library(fastDummies)
library(dplyr)
library(tidyverse) 
library(purrr)     
library(skimr)
library(ggplot2)
library(tidyr)
library(dummy)
library(caret)
library(randomForest)
library(rpart)
library(gplots)
library(reshape2)
library(gridExtra)

setwd("C:/Users/ENRIQUE/OneDrive/Escritorio/Uni/4º/TFG")
data <- read.csv("TFG_2_nuevo.csv", header=TRUE, stringsAsFactors = TRUE) 
View(data)

#Empezamos eliminando las columnas innecesarias (Timestamp y correo electrónico de los participantes, aparte de las dos primeras filas que
#están vacías)

data<- data[,-c(1,28)]


#################################################   EXPLORATORY DATA ANALYSIS ############################################################


# Empezamos viendo la dimensión de los datos, su naturaleza y un resumen

dim(data)
str(data)
summary(data)


##### VISUALIZACIÓN

### BAR PLOTS

# Se realizan barplots, tablas de contingencia y mosaic plots de las de las distintas variables

sex_freq <- table(data$Indique.el.sexo.que.le.fue.asignado.al.nacer)
#View(sex_freq)
sex_df <- as.data.frame(sex_freq)
names(sex_df) <- c("Sex", "Frequency")

abs_frec_sex<-ggplot(sex_df, aes(x = Sex, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Bar Plot Frecuencias Absolutas del Sexo",           # Title of the plot
       x = "Sexo",                             # Label for the x-axis
       y = "Frecuencia Absoluta") +                      # Label for the y-axis
  theme_minimal()                              # Optional: Use a minimal theme



sex_rel_frec<-sex_freq / sum(sex_freq)

# Plot with relative frequencies
rel_freq_sex <- ggplot(sex_df, aes(x = Sex, y = sex_rel_frec)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Bar Plot Frecuencias Relativas del Sexo", 
       x = "Sexo", 
       y = "Frecuencia Relativa") +
  theme_minimal()

grid.arrange(abs_frec_sex,rel_freq_sex, nrow= 2)


## SOCIAL MEDIA


apps_df <- data %>%
  separate_rows(Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga., sep = ",\\s*")


# Counting the occurrences of each app
app_counts <- apps_df %>%
  count(Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga.)


# Creating a bar plot
frec_abs_count<-ggplot(app_counts, aes(x = Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga., y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Set color to skyblue
  labs(x = "Redes Sociales", y = "Frecuencia Absoluta", title = "Frecuencia Absoluta de las Apps de Redes Sociales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability

frec_abs_count


total_count <- nrow(data)


# Calculate relative frequencies
app_counts_rel <- app_counts %>%
  mutate(relative_freq = n / total_count)

# Create a bar plot with relative frequencies
frec_rel_counts<-ggplot(app_counts_rel, aes(x = Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga., y = relative_freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Redes Sociales", y = "Frecuencia Relativa", title = "Frecuencia Relativa de las Apps de Redes Sociales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

frec_rel_counts


grid.arrange(frec_abs_count,frec_rel_counts,nrow=2)






VariableRedAux = c()
VariableEdadAux = c()
for(ind in c(1:nrow(data))){
  VariableRedAux = c(VariableRedAux,unlist(strsplit(as.character(data$Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga.[ind]),split=", ")))
  lengRed = length(unlist(strsplit(as.character(data$Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga.[ind]),split=", ")))
  VariableEdadAux = c(VariableEdadAux,rep(data$Indique.su.edad[ind],lengRed))
}

dataframeAux = data.frame(VariableRedAux,VariableEdadAux, y = rep(1,length(VariableEdadAux)))

ggplot(dataframeAux,aes(x = VariableEdadAux,fill=VariableRedAux,y=y))


# Create a frequency table
frequency_table <- table(dataframeAux$VariableRedAux, dataframeAux$VariableEdadAux)
frequency_relative_table <- table(dataframeAux$VariableRedAux, dataframeAux$VariableEdadAux)/nrow(data)

# Convert the frequency table to a dataframe
frequency_df <- as.data.frame(frequency_table)
frequency_relative_df <- as.data.frame(frequency_relative_table)

# View the first few rows of the frequency dataframe
head(frequency_df)

# Load ggplot2 library
library(ggplot2)


# Plot the data using ggplot2 with stacked bars
ggplot(frequency_df, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Edad", y = "Frecuencia Absoluta", fill = "RRSS más usadas") +
  theme_minimal()

proporciones_rel<-ggplot(frequency_df, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Edad", y = "Proporciones", fill = "RRSS más usadas") +
  theme_minimal()

proporciones_rel

frecuencias_rel <- ggplot(frequency_relative_df, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Edad", y = "Frecuencias", fill = "RRSS más usadas") +
  theme_minimal() +
  guides(fill = "none")  # Remove the legend for fill aesthetic

# Print the plot
print(frecuencias_rel)


grid.arrange(proporciones_rel, frecuencias_rel, nrow = 2)




length(app_counts)
length(age_freq)
# hacer esto con sexo y comparaciones fisicas negativas
# sexo y sentimientos a la hora de estar en redes sociales
# edad y sentimientos a la hora de estar en redes sociales 

## AGE

age_freq <-table(data$Indique.su.edad)
age_df<- as.data.frame(age_freq)
names(age_df) <- c("Age", "Frequency")


# Create the barplot
abs_frec_age<-ggplot(data = age_df, aes(x = Age, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Rangos de Edad", y = "Frecuencia Absoluta", title = "Bar Plot Frecuencias Absolutas de la Edad")+
  theme_minimal()


age_rel_frec<-age_freq / sum(age_freq)


# Plot with relative frequencies
rel_freq_age <- ggplot(age_df, aes(x = Age, y = age_rel_frec)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Bar Plot Frecuencias Relativas de la Edad", 
       x = "Rangos de Edad", 
       y = "Frecuencia Relativa") +
  theme_minimal()

grid.arrange(abs_frec_age,rel_freq_age, nrow= 2)






## PREGUNTAS ANSIEDAD Y DEPRESIÓN

###1

bucle_preocupacion_freq <-table(data$Entrar.en.un.bucle.de.preocupación.por.algo.en.concreto)
bucle_preocupacion_df<- as.data.frame(bucle_preocupacion_freq)[1:length(bucle_preocupacion_freq),]
names(bucle_preocupacion_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = bucle_preocupacion_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Mental Loop", y = "Frequency", title = "Distribution of Mental Loop")


###2


mente_blanco_freq <-table(data$Tener.problemas.para.dejar.la.mente.en.blanco)
mente_blanco_df<- as.data.frame(mente_blanco_freq)[1:length(mente_blanco_freq),]
names(mente_blanco_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = mente_blanco_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Mente en Blanco", y = "Frequency", title = "Distribution of Mente en Blanco")


## 3

mente_blanco_freq <-table(data$Tener.problemas.para.dejar.la.mente.en.blanco)
mente_blanco_df<- as.data.frame(mente_blanco_freq)[1:length(mente_blanco_freq),]
names(mente_blanco_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = mente_blanco_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Mente en Blanco", y = "Frequency", title = "Distribución de Dificultad para dejar la mente en blacno")



## 4

irritable_freq <-table(data$Molestarse.o.volverse.fácilmente.irritable)
irritable_df<- as.data.frame(irritable_freq)[1:length(irritable_freq),]
names(irritable_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = irritable_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Exceso de irritabilidad", y = "Frequency", title = "Distribución de fácilmente irritable")


## 5

suceso_terrible_freq <-table(data$Preocuparse.porque.algo.terrible.vaya.a.pasar)
suceso_terrible_df<- as.data.frame(suceso_terrible_freq)[1:length(suceso_terrible_freq),]
names(suceso_terrible_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = suceso_terrible_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Pensamiento de suceso terrible", y = "Frequency", title = "Distribución de Preocuparse porque algo terrible vaya a pasar")



## 6

tristeza_freq <-table(data$Sentirse.triste)
tristeza_df<- as.data.frame(tristeza_freq)[1:length(tristeza_freq),]
names(tristeza_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = tristeza_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Sentimiento de tristeza", y = "Frequency", title = "Distribución de Sentimiento de Tristeza")



## 7

cansancio_freq <-table(data$Sentirse.cansado.o.con.poca.energía)
cansancio_df<- as.data.frame(cansancio_freq)[1:length(cansancio_freq),]
names(cansancio_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = cansancio_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Sentirse cansado o con poca energía", y = "Frequency", title = "Distribución de Sentirse cansado o con poca energía")


## 8

comer_poco_freq <-table(data$Comer.poco)
comer_poco_df<- as.data.frame(comer_poco_freq)[1:length(comer_poco_freq),]
names(comer_poco_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = comer_poco_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Comer poco", y = "Frequency", title = "Distribución de Comer poco")


## 9


comer_en_exceso_freq <-table(data$Comer.en.exceso..especialmente.alimentos.poco.saludables.)
comer_en_exceso_df<- as.data.frame(comer_en_exceso_freq)[1:length(comer_en_exceso_freq),]
names(comer_en_exceso_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = comer_en_exceso_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Comer en exceso", y = "Frequency", title = "Distribución de Comer en exceso")


## 10


fracaso_freq <-table(data$Sentirse.mal.con.uno.mismo..o.que.es.un.fracaso.y.ha.decepcionado.a.su.familia.)
fracaso_df<- as.data.frame(fracaso_freq)[1:length(fracaso_freq),]
names(fracaso_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = fracaso_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Sentir fracaso", y = "Frequency", title = "Distribución de Sentir fracaso")


## 11

dificultad_concentrarse_freq <-table(data$Dificultad.para.concentrarse.en.las.cosas..como.leer.o.hacer.tarea.escolar.laboral.)
dificultad_concentrarse_df<- as.data.frame(dificultad_concentrarse_freq)[1:length(dificultad_concentrarse_freq),]
names(dificultad_concentrarse_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = dificultad_concentrarse_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Dificultad para concentrarse", y = "Frequency", title = "Distribución de Dificultad para concentrarse")



## 12


pensamientos_suicidas_freq <-table(data$Pensamientos.suicidas)
pensamientos_suicidas_df<- as.data.frame(pensamientos_suicidas_freq)[1:length(pensamientos_suicidas_freq),]
names(pensamientos_suicidas_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = pensamientos_suicidas_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Pensamientos suicidas", y = "Frequency", title = "Distribución de Pensamientos suicidas")


## 13

tiempo_dormir_freq <-table(data$X.Cuánto.tiempo.le.cuesta.dormirse.tras.meterse.en.la.cama.)
tiempo_dormir_df<- as.data.frame(tiempo_dormir_freq)[1:length(tiempo_dormir_freq),]
names(tiempo_dormir_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = tiempo_dormir_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Tiempor que tarda en dormirse", y = "Frequency", title = "Distribución de Tiempo que tarda en dormirse")


## 14

duracion_sueño_freq <-table(data$X.Cuál.es.su.duración.total.del.sueño)
duracion_sueño_df<- as.data.frame(duracion_sueño_freq)[1:length(duracion_sueño_freq),]
names(duracion_sueño_df) <- c("Días", "Frequency")


# Create the barplot
ggplot(data = duracion_sueño_df, aes(x = Días, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Duración del sueño", y = "Frequency", title = "Distribución de Duración del Sueño")


## 15

trastorno_alimenticio_freq <-table(data$X.Ha.sufrido.o.sufre.de.algún.trastorno.de.alimentación.)
trastorno_alimenticio_df<- as.data.frame(trastorno_alimenticio_freq)[1:length(trastorno_alimenticio_freq),]
names(trastorno_alimenticio_df) <- c("Respu", "Frequency")


# Create the barplot
ggplot(data = trastorno_alimenticio_df, aes(x = Respu, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Trastorno alimenticio", y = "Frequency", title = "Distribución de Trastorno Alimenticio")


## 16

comparaciones_fisicas_freq <-table(data$X.Se.compara.de.manera.negativa.físicamente.con.los.cuerpos.de.la.gente.que.ve.en.redes.sociales.)
comparaciones_fisicas_df<- as.data.frame(comparaciones_fisicas_freq)[1:length(comparaciones_fisicas_freq),]
names(comparaciones_fisicas_df) <- c("Respu", "Frequency")


# Create the barplot
ggplot(data = comparaciones_fisicas_df, aes(x = Respu, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Comparaciones físicas", y = "Frequency", title = "Distribución de Comparaciones Físicas")


## 17

conductas_alimenticias_freq <-table(data$X.Está.satisfecho.a.con.sus.conductas.alimenticias.)
conductas_alimenticias_df<- as.data.frame(conductas_alimenticias_freq)[1:length(conductas_alimenticias_freq),]
names(conductas_alimenticias_df) <- c("Respu", "Frequency")


# Create the barplot
ggplot(data = conductas_alimenticias_df, aes(x = Respu, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Conductas alimenticias", y = "Frequency", title = "Distribución de Conductas alimenticias")


## 18

impacto_peso_freq <-table(data$X.Afecta.su.peso.a.la.manera.en.la.que.se.siente.con.usted.mismo.)
impacto_peso_df<- as.data.frame(impacto_peso_freq)[1:length(impacto_peso_freq),]
names(impacto_peso_df) <- c("Respu", "Frequency")


# Create the barplot
ggplot(data = impacto_peso_df, aes(x = Respu, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Afecta tu peso sobre tu estado de ánimo", y = "Frequency", title = "Distribución de Impacto del Peso")

## 19

promedio_diario_telefono_freq <-table(data$X.Cuál.es.su.promedio.diario.con.su.teléfono.móvil...Compruebe.el.tiempo.de.hoy.en..Ajustes.....Bienestar.Digital..o..en.caso.de.tener.iPhone..el.promedio.exacto..)
promedio_diario_telefono_df<- as.data.frame(promedio_diario_telefono_freq)[1:length(promedio_diario_telefono_freq),]
names(promedio_diario_telefono_df) <- c("Respu", "Frequency")


# Create the barplot
ggplot(data = promedio_diario_telefono_df, aes(x = Respu, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Promedio diario teléfono", y = "Frecuencia", title = "Distribución de Promedio diario con el teléfono")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 20

nocion_tiempo_freq <-table(data$X.Siente.que.pierde.la.noción.del.tiempo.cuando.usa.las.redes.sociales.)
nocion_tiempo_df<- as.data.frame(nocion_tiempo_freq)[1:length(nocion_tiempo_freq),]
names(nocion_tiempo_df) <- c("Respu", "Frequency")


# Create the barplot
ggplot(data = nocion_tiempo_df, aes(x = Respu, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Pérdida de noción del tiempo", y = "Frequency", title = "Distribución de Pérdida de noción del tiempo")


## 21

necesidad_urgente_freq <-table(data$Cuando.su.teléfono.suena.o.vibra...sientes.la.necesidad.urgente.de.comprobar.si.tiene.WhatsApps..mensajes.directos.de.Instagram..Tiktok.o.cualquier.otra.red.social.inmediatamente.)
necesidad_urgente_df<- as.data.frame(necesidad_urgente_freq)[1:length(necesidad_urgente_freq),]
names(necesidad_urgente_df) <- c("Respu", "Frequency")


# Create the barplot
ggplot(data = necesidad_urgente_df, aes(x = Respu, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Necesidad urgente de coger el teléfono cada vez que suena", y = "Frequency", title = "Distribución de Necesidad urgente")


## 22

inconsciencia_freq <-table(data$X.Se.encuentra.entrando.a.sus.redes.sociales.de.manera.inconsciente.más.de.10.veces.al.día...En..Ajustes.....Bienestar.Digital..se.puede.comprobar.el.número.de.veces.que.se.desbloquea.el.teléfono..)
inconsciencia_df<- as.data.frame(inconsciencia_freq)[1:length(inconsciencia_freq),]
names(inconsciencia_df) <- c("Respu", "Frequency")


# Create the barplot
ggplot(data = inconsciencia_df, aes(x = Respu, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Inconsciencia a la hora de coger el móvil", y = "Frequency", title = "Distribución de Inconsciencia")


## 23


mayor_parte_df <- data %>%
  separate_rows(X.Cómo.se.siente.cuando.dedica.la.mayor.parte.de.su.día.a.las.redes.sociales., sep = ",\\s*")


# Counting the occurrences of each app
mayor_parte_counts <- mayor_parte_df %>%
  count(X.Cómo.se.siente.cuando.dedica.la.mayor.parte.de.su.día.a.las.redes.sociales.)



# Creating a bar plot
frec_abs_mayor<- ggplot(mayor_parte_counts, aes(x = X.Cómo.se.siente.cuando.dedica.la.mayor.parte.de.su.día.a.las.redes.sociales., y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Set color to skyblue
  labs(x = "Emociones Por Excesivo Uso De las RRSS", y = "Frecuencia Absoluta", title = "Frecuencia Relativa de Emociones Por Excesivo Uso De las RRSS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability

frec_abs_mayor

total_mayor<-nrow(data)

mayor_counts_rel<-mayor_parte_counts%>%
  mutate(relative_freq=n/total_mayor)


frec_rel_mayor<-ggplot(mayor_counts_rel, aes(x = X.Cómo.se.siente.cuando.dedica.la.mayor.parte.de.su.día.a.las.redes.sociales., y = relative_freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Emociones Por Excesivo Uso De las RRSS", y = "Frecuencia Relativa", title = "Frecuencia Relativa de Emociones Por Excesivo Uso De las RRSS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

frec_rel_mayor

grid.arrange(frec_abs_mayor,frec_rel_mayor,nrow=2)

apps_df <- data %>%
  separate_rows(Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga., sep = ",\\s*")


# Counting the occurrences of each app
app_counts <- apps_df %>%
  count(Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga.)


# Creating a bar plot
frec_abs_count<-ggplot(app_counts, aes(x = Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga., y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Set color to skyblue
  labs(x = "Redes Sociales", y = "Frecuencia Absoluta", title = "Frecuencia Absoluta de las Apps de Redes Sociales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability

frec_abs_count
total_count <- sum(app_counts$n)

# Calculate relative frequencies
app_counts_rel <- app_counts %>%
  mutate(relative_freq = n / total_count)

# Create a bar plot with relative frequencies
frec_rel_counts<-ggplot(app_counts_rel, aes(x = Indique..de.la.siguiente.lista..qué.3.redes.sociales.son.las.que.más.utiliza..si.ni.siquiera.tiene.3.indique.las.que.tenga., y = relative_freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Redes Sociales", y = "Frecuencia Relativa", title = "Frecuencia Relativa de las Apps de Redes Sociales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

frec_rel_counts


grid.arrange(frec_abs_count,frec_rel_counts,nrow=2)














## 24

ochenta_freq <-table(data$X.Diría.que.el.80..del.tiempo.que.está.con.el.móvil..es.para.utilizar.las.redes.sociales.)
ochenta_df<- as.data.frame(ochenta_freq)[1:length(inconsciencia_freq),]
names(ochenta_df) <- c("Respu", "Frequency")


# Create the barplot
ggplot(data = ochenta_df, aes(x = Respu, y= Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Dedica más del 80% de su tiempo con el móvil a las rrss", y = "Frequency", title = "Distribución de Tiempo dedicado a rrss 
       de tiempo total dedicado a móvil")


#CORRELACIONES


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






# CORRELACIONES ENTRE RRSS MÁS USADAS Y EMOCIONES



# Load the data
# Assuming your data is stored in a dataframe called "data"

# Create dummy variables for WhatsApp and Feliz
whatsapp_dummy <- sapply(data$Tres_RRSS, function(x) as.integer("WhatsApp" %in% unlist(strsplit(as.character(x), ", "))))
feliz_dummy <- sapply(data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil, function(x) as.integer("Feliz" %in% unlist(strsplit(as.character(x), ", "))))

# Create contingency table
contingency_table <- table(whatsapp_dummy, feliz_dummy)

# Add row and column names
dimnames(contingency_table) <- list(c("No WhatsApp", "WhatsApp"), c("Not Happy", "Happy"))

# Print contingency table
print(contingency_table)




# Assuming your data is stored in a dataframe called "data"

# Count occurrences of "WhatsApp" in a specific column
whatsapp_count <- table(unlist(strsplit(as.character(data$Tres_RRSS), ", ")))["WhatsApp"]

# Print the count
print(whatsapp_count)



# Assuming your data is stored in a dataframe called "data"

# Count rows with both "WhatsApp" in Tres_RRSS and "Feliz" in Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil
count_both <- sum(grepl("WhatsApp", data$Tres_RRSS) & grepl("Feliz", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))

# Print the count
print(count_both)



#improductivo, frustrado, insatisfecho, sereno/tranquilo

# WhatsApp and sereno
sereno_dummy <- sapply(data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil, function(x) as.integer("Sereno/Tranquilo" %in% unlist(strsplit(as.character(x), ", "))))
contingency_table_2 <- table(whatsapp_dummy, sereno_dummy)
dimnames(contingency_table_2) <- list(c("No WhatsApp", "WhatsApp"), c("Not Tranquilo", "Tranquilo"))
print(contingency_table_2)


count_both <- sum(grepl("WhatsApp", data$Tres_RRSS) & grepl("Sereno/Tranquilo", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# WhatsApp and improductivo

improductivo_dummy<-sapply(data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil, function(x) as.integer("Improductivo" %in% unlist(strsplit(as.character(x), ", "))))
contingency_table_3 <- table(whatsapp_dummy, improductivo_dummy)
dimnames(contingency_table_3) <- list(c("No WhatsApp", "WhatsApp"), c("Not Improductivo", "Improductivo"))
print(contingency_table_3)


count_both <- sum(grepl("WhatsApp", data$Tres_RRSS) & grepl("Improductivo", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# WhatsApp and frustrado

frustrado_dummy<-sapply(data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil, function(x) as.integer("Frustrado" %in% unlist(strsplit(as.character(x), ", "))))
contingency_table_4 <- table(whatsapp_dummy, frustrado_dummy)
dimnames(contingency_table_4) <- list(c("No WhatsApp", "WhatsApp"), c("Not Frustrado", "Frustrado"))
print(contingency_table_4)


count_both <- sum(grepl("WhatsApp", data$Tres_RRSS) & grepl("Frustrado", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# WhatsApp and insatisfecho


insatisfecho_dummy<-sapply(data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil, function(x) as.integer("Insatisfecho" %in% unlist(strsplit(as.character(x), ", "))))
contingency_table_5 <- table(whatsapp_dummy, insatisfecho_dummy)
dimnames(contingency_table_5) <- list(c("No WhatsApp", "WhatsApp"), c("Not Insatisfecho", "Insatisfecho"))
print(contingency_table_5)


count_both <- sum(grepl("WhatsApp", data$Tres_RRSS) & grepl("Insatisfecho", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Instagram and Feliz

instagram_dummy <- sapply(data$Tres_RRSS, function(x) as.integer("Instagram" %in% unlist(strsplit(as.character(x), ", "))))

contingency_table_6 <- table(instagram_dummy, feliz_dummy)
dimnames(contingency_table_6) <- list(c("No Instagram", "Instagram"), c("Not Happy", "Happy"))
print(contingency_table_6)


instagram_count <- table(unlist(strsplit(as.character(data$Tres_RRSS), ", ")))["Instagram"]
print(instagram_count)
count_both <- sum(grepl("Instagram", data$Tres_RRSS) & grepl("Feliz", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Instagram and Sereno

contingency_table_7<- table(instagram_dummy, sereno_dummy)
dimnames(contingency_table_7) <- list(c("No Instagram", "Instagram"), c("Not Sereno", "Sereno"))
print(contingency_table_7)
count_both <- sum(grepl("Instagram", data$Tres_RRSS) & grepl("Sereno/Tranquilo", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Instagram and Improductivo

contingency_table_8<- table(instagram_dummy, improductivo_dummy)
dimnames(contingency_table_8) <- list(c("No Instagram", "Instagram"), c("Not Improductivo", "Improductivo"))
print(contingency_table_8)
count_both <- sum(grepl("Instagram", data$Tres_RRSS) & grepl("Improductivo", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Instagram and frustrado

contingency_table_9<- table(instagram_dummy, frustrado_dummy)
dimnames(contingency_table_9) <- list(c("No Instagram", "Instagram"), c("Not Frustrado", "Frustrado"))
print(contingency_table_9)


count_both <- sum(grepl("Instagram", data$Tres_RRSS) & grepl("Frustrado", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Instagram and insatisfecho

contingency_table_10 <- table(instagram_dummy, insatisfecho_dummy)
dimnames(contingency_table_10) <- list(c("No Instagram", "Instagram"), c("Not Insatisfecho", "Insatisfecho"))
print(contingency_table_10)


count_both <- sum(grepl("Instagram", data$Tres_RRSS) & grepl("Insatisfecho", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# YouTube and Feliz

youtube_dummy <- sapply(data$Tres_RRSS, function(x) as.integer("Youtube" %in% unlist(strsplit(as.character(x), ", "))))

contingency_table_11<- table(youtube_dummy, feliz_dummy)
dimnames(contingency_table_11) <- list(c("No Youtube", "Youtube"), c("Not Happy", "Happy"))
print(contingency_table_11)


youtube_count <- table(unlist(strsplit(as.character(data$Tres_RRSS), ", ")))["Youtube"]
print(youtube_count)
count_both <- sum(grepl("Youtube", data$Tres_RRSS) & grepl("Feliz", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Youtube and Sereno

contingency_table_12<- table(youtube_dummy, sereno_dummy)
dimnames(contingency_table_12) <- list(c("No Youtube", "Youtube"), c("Not Sereno", "Sereno"))
print(contingency_table_12)
count_both <- sum(grepl("Youtube", data$Tres_RRSS) & grepl("Sereno/Tranquilo", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Youtube and improductivo

contingency_table_13<- table(youtube_dummy, improductivo_dummy)
dimnames(contingency_table_13) <- list(c("No Youtube", "Youtube"), c("Not Improductivo", "Improductivo"))
print(contingency_table_13)
count_both <- sum(grepl("Youtube", data$Tres_RRSS) & grepl("Improductivo", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Youtube and frustrado

contingency_table_14<- table(youtube_dummy, frustrado_dummy)
dimnames(contingency_table_14) <- list(c("No Youtube", "Youtube"), c("Not Frustrado", "Frustrado"))
print(contingency_table_14)


count_both <- sum(grepl("Youtube", data$Tres_RRSS) & grepl("Frustrado", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)

# Youtube and insatisfecho

contingency_table_16 <- table(youtube_dummy, insatisfecho_dummy)
dimnames(contingency_table_16) <- list(c("No Youtube", "Youtube"), c("Not Insatisfecho", "Insatisfecho"))
print(contingency_table_16)


count_both <- sum(grepl("Youtube", data$Tres_RRSS) & grepl("Insatisfecho", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Tiktok and Feliz


tiktok_dummy <- sapply(data$Tres_RRSS, function(x) as.integer("Tiktok" %in% unlist(strsplit(as.character(x), ", "))))

contingency_table_17<- table(tiktok_dummy, feliz_dummy)
dimnames(contingency_table_17) <- list(c("No Tiktok", "Tiktok"), c("Not Happy", "Happy"))
print(contingency_table_17)


tiktok_count <- table(unlist(strsplit(as.character(data$Tres_RRSS), ", ")))["Tiktok"]
print(tiktok_count)
count_both <- sum(grepl("Tiktok", data$Tres_RRSS) & grepl("Feliz", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)

#Tiktok and sereno

contingency_table_18<- table(tiktok_dummy, sereno_dummy)
dimnames(contingency_table_18) <- list(c("No Tiktok", "Tiktok"), c("Not Sereno", "Sereno"))
print(contingency_table_18)
count_both <- sum(grepl("Tiktok", data$Tres_RRSS) & grepl("Sereno/Tranquilo", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)

# Tiktok and improductivo

contingency_table_19<- table(tiktok_dummy, improductivo_dummy)
dimnames(contingency_table_19) <- list(c("No Tiktok", "Tiktok"), c("Not Improductivo", "Improductivo"))
print(contingency_table_19)
count_both <- sum(grepl("Tiktok", data$Tres_RRSS) & grepl("Improductivo", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Tiktok and frustrado

contingency_table_20<- table(tiktok_dummy, frustrado_dummy)
dimnames(contingency_table_20) <- list(c("No Tiktok", "Tiktok"), c("Not Frustrado", "Frustrado"))
print(contingency_table_20)


count_both <- sum(grepl("Tiktok", data$Tres_RRSS) & grepl("Frustrado", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


# Tiktok and insatisfecho


contingency_table_21 <- table(tiktok_dummy, insatisfecho_dummy)
dimnames(contingency_table_21) <- list(c("No Tiktok", "Tiktok"), c("Not Insatisfecho", "Insatisfecho"))
print(contingency_table_21)


count_both <- sum(grepl("Tiktok", data$Tres_RRSS) & grepl("Insatisfecho", data$Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil))
print(count_both)


#################################################### PREPROCESO ##################################################







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




#Para las de escala Likert hay que hacer dummy encoding (fem/masc, todas las de sí no y todas las que sean escala likert de 4 respuestas)

library(fastDummies)

dummy_df <- dummy_cols(data, select_columns = c("Sexo","Edad","Bucle", "Mente_En_Blanco","Irritabilidad",
                                                "Preocupacion_Suceso_Terrible","Tristeza","Cansancio",
                                                "Comer_Poco","Comer_Exceso","Fracaso","Dificultad_Concentrarse",
                                                "Pensamientos_Suicidas","Tiempo_Sueño","Duracion_Sueño",
                                                "Trastorno_Alimenticio","Comparacion_Fisica","Satisfaccion_Conductas_Alimenticias",
                                                "Peso_Autoimagen","Nocion_Tiempo","Necesidad_Urgente",
                                                "Desbloqueo_Movil_Constante","%De_Tiempo_De_Pantalla_Con_El_Movil","Promedio_Diario_Movil"), remove_first_dummy = TRUE,remove_selected_columns = TRUE)

# View the dummy encoded data
print(dummy_df)
dummy_df<-dummy_df[,-c(1,2)]
#me queda emociones, rrss. emociones lo usare como variable respuesta


#one hot de rrss
data_transformed <- data %>%
  separate_rows(Tres_RRSS, sep = ", ") %>%
  mutate(dummy = 1) %>%
  pivot_wider(names_from = Tres_RRSS, values_from = dummy, values_fill = list(dummy = 0))

colnames(data_transformed)

data_transformed<-data_transformed[,-c(1:25)]


#añadimos una columna con emociones positivas, negativas o neutras




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
respuesta
j=1


datos_final<-cbind(dummy_df,data_transformed,respuesta)
write.csv(datos_final,file="datos_final.csv")





#data <- data[, !names(data) %in% "Emociones_Cuando_Hay_Excesivo_Uso_Del_Movil"]

#datos_final_2<-cbind(data,respuesta)




datos_pca=datos_final

datos_pca[["respuesta"]] <- as.factor(datos_pca[["respuesta"]])
levels <- levels(datos_pca[["respuesta"]])
datos_pca[["respuesta"]] <- as.numeric(datos_pca[["respuesta"]])

category_mapping <- data.frame(Category = levels, Numerical = 1:length(levels))
print("Category to numerical mapping:")
print(category_mapping)


str(datos_pca)

pca_result <- prcomp(datos_pca, center = TRUE, scale. = TRUE)

# Print PCA summary
print(summary(pca_result))
View(summary(pca_result))

print(pca_result)
# Plot the PCA results
pca_data <- as.data.frame(pca_result$x)
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(title = "PCA Result", x = "Principal Component 1", y = "Principal Component 2")



library(ggplot2)

# Assuming datos_final is your original data frame
datos_pca <- datos_final

# Convert 'respuesta' to factor and keep the original levels
datos_pca[["respuesta"]] <- as.factor(datos_pca[["respuesta"]])
levels <- levels(datos_pca[["respuesta"]])

# Convert 'respuesta' to numeric for PCA computation
numeric_respuesta <- as.numeric(datos_pca[["respuesta"]])

# Store the category mapping
category_mapping <- data.frame(Category = levels, Numerical = 1:length(levels))
print("Category to numerical mapping:")
print(category_mapping)

# Attach the numeric response to the data frame for PCA
datos_pca[["numeric_respuesta"]] <- numeric_respuesta

# Remove the original 'respuesta' column before PCA
datos_pca <- datos_pca[, !names(datos_pca) %in% "respuesta"]

str(datos_pca)

# Perform PCA
pca_result <- prcomp(datos_pca, center = TRUE, scale. = TRUE)

# Print PCA summary
print(summary(pca_result))
# View(summary(pca_result))

print(pca_result)

# Convert PCA results to data frame
pca_data <- as.data.frame(pca_result$x)

# Add the original categorical labels to the PCA result data frame
pca_data[["respuesta"]] <- datos_final[["respuesta"]]

# Plot the PCA results with labels
pc1_2<-ggplot(pca_data, aes(x = PC1, y = PC2, color = respuesta)) +
  geom_point() +
  labs(title = "PCA Result (PC1 vs PC2)", x = "Principal Component 1", y = "Principal Component 2") +
  scale_color_manual(values = c("Positive" = "blue", "Negative" = "red", "Neutral" = "green"))



# Plot the PCA results with labels for PC1 and PC3
pc1_3<-ggplot(pca_data, aes(x = PC1, y = PC3, color = respuesta)) +
  geom_point() +
  labs(title = "PCA Result (PC1 vs PC3)", x = "Principal Component 1", y = "Principal Component 3") +
  scale_color_manual(values = c("Positive" = "blue", "Negative" = "red", "Neutral" = "green"))


pc2_3<-ggplot(pca_data, aes(x = PC2, y = PC3, color = respuesta)) +
  geom_point() +
  labs(title = "PCA Result (PC2 vs PC3)", x = "Principal Component 2", y = "Principal Component 3") +
  scale_color_manual(values = c("Positive" = "blue", "Negative" = "red", "Neutral" = "green"))




pc2_4<-ggplot(pca_data, aes(x = PC2, y = PC4, color = respuesta)) +
  geom_point() +
  labs(title = "PCA Result (PC2 vs PC4)", x = "Principal Component 2", y = "Principal Component 3") +
  scale_color_manual(values = c("Positive" = "blue", "Negative" = "red", "Neutral" = "green"))


grid.arrange(pc1_2,pc1_3,pc2_3,pc2_4, nrow=2, ncol=2)

help("grid.arrange")


scores <- pca_result$x
print("Scores:")
View(scores)


loadings <- pca_result$rotation
print("Loadings:")
print(loadings)
View(loadings)



loadings_pc1 <- abs(loadings[, 1])

# Find the indices of the top 5 highest values
top_indices <- order(loadings_pc1, decreasing = TRUE)[1:5]

# Retrieve the top 5 highest values and their corresponding variables
top_loadings <- loadings[top_indices, 1]

# Print the top 5 highest loadings and their corresponding variables (if needed)
top_loadings


print(top_loadings)
View(top_loadings)



loadings_pc2 <- abs(loadings[, 2])

# Find the indices of the top 5 highest values
top_indices <- order(loadings_pc2, decreasing = TRUE)[1:5]

# Retrieve the top 5 highest values and their corresponding variables
top_loadings <- loadings[top_indices, 2]

# Print the top 5 highest loadings and their corresponding variables (if needed)
top_loadings




loadings_pc3 <- abs(loadings[, 3])

# Find the indices of the top 5 highest values
top_indices <- order(loadings_pc3, decreasing = TRUE)[1:5]

# Retrieve the top 5 highest values and their corresponding variables
top_loadings <- loadings[top_indices, 3]

# Print the top 5 highest loadings and their corresponding variables (if needed)
top_loadings


loadings_pc4 <- abs(loadings[, 4])

# Find the indices of the top 5 highest values
top_indices <- order(loadings_pc4, decreasing = TRUE)[1:5]

# Retrieve the top 5 highest values and their corresponding variables
top_loadings <- loadings[top_indices, 4]

# Print the top 5 highest loadings and their corresponding variables (if needed)
top_loadings








# Function to get top n absolute values
top_abs_values <- function(vec, n = 5) {
  abs_sorted <- sort(abs(vec), decreasing = TRUE)  # Sort absolute values
  top_values <- head(abs_sorted, n)  # Take top n values
  return(top_values)
}

# Apply the function to each column
top_five_abs <- apply(loadings, 2, top_abs_values)

# Print the top five absolute values for each column
print(top_five_abs)









#Extract variance explained by each principal component
variance_explained <- pca_result$sdev^2
prop_var_explained <- variance_explained / sum(variance_explained)

# Number of principal components
num_components <- length(variance_explained)

# Create scree plot
plot(1:num_components, prop_var_explained, 
     type = "b", pch = 19, col = "steelblue",
     xlab = "Componente Principal", ylab = "Proporción de la Varianza Explicada",
     main = "Scree Plot Del PCA")

# Add gridlines for clarity
abline(h = 0, col = "gray", lty = 3)
abline(v = 0, col = "gray", lty = 3)











