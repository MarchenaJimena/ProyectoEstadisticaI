library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rcompanion)

#Leer la tabla

datos <- read.csv("C:\\Users\\andre\\OneDrive\\Escritorio\\ESTADISTICA\\heart_2020_cleaned.csv", sep = ",", dec = ".")

#Agregar una columna CardVasc que combina las variables Heartdiseas y Stroke

datos <- datos %>%
  mutate(CardVasc = ifelse(HeartDisease ==
                             "Yes"| Stroke == "Yes", "Yes", "No"))

#Definir los factores que se van a analizar

variables_de_interes <- c("Smoking", "AlcoholDrinking",
                          "PhysicalHealth", "Sex","AgeCategory")
test <- function(var){
  #Construyendo las tablas de contingencia con respecto a las variables que
  # corresponden a enfermedades cardiovasculares
  tabla_heart <- table(datos$HeartDisease, datos[[var]])
  tabla_stroke <- table(datos$Stroke, datos[[var]])
  tabla_cardvasc <- table(datos$CardVasc, datos[[var]])
  
  cat("\nTabla de contingencia para HeartDisease y", var, ":\n")
  print(tabla_heart)
  chi2_heart <- chisq.test(tabla_heart)
  cramerv_heart <- cramerV(tabla_heart)
  print(chi2_heart)
  print(cramerv_heart)
  
  cat("\nTabla de contingencia para Stroke y", var, ":\n")
  print(tabla_stroke)
  chi2_stroke <- chisq.test(tabla_stroke)
  cramerv_stroke <- cramerV(tabla_stroke)
  print(chi2_stroke)
  print(cramerv_stroke)
  
  cat("\nTabla de contingencia para CardVasc y", var, ":\n")
  print(tabla_cardvasc)
  chi2_cardvasc <- chisq.test(tabla_cardvasc)
  cramerv_cardvasc <- cramerV(tabla_cardvasc)
  print(chi2_cardvasc)
  print(cramerv_cardvasc)
}

for (variable in variables_de_interes) {
  test(variable)}
