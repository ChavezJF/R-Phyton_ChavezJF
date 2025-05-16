#EJERCICIOS R PARA DUMMIES

#Tabla con 10 filas y 4 columnas: Mes, Sexo, Talla y Peso

Mes <- c("Enero", "Enero", "Febrero", "Febrero", "Marzo", "Marzo", "Abril", "Abril", "Mayo", "Mayo")
Sexo <- c("Macho", "Hembra", "Macho", "Hembra", "Macho", "Hembra", "Macho", "Hembra", "Macho", "Hembra")
Talla <- c(120, 110, 114, 104, 125, 105, 115, 110, 120, 105)
Peso <- c(.90, .85, .87, .83, .95, .84, .92, .86, .91, .83)

tabla <- data.frame(Mes, Sexo, Talla, Peso)
tabla

#Nueva columna: Talla vs Peso

tabla$TvsP <- tabla$Talla * tabla$Peso
tabla

#Nueva columna: logaritmo base 10 de las tallas

tabla$LogT <- log10(tabla$Talla)
tabla

#Nueva columna: logaritmo base 10 de los pesos

tabla$LogP <- log10(tabla$Peso)
tabla

#Indicar talla promedio de todos los datos
mean(tabla$Talla)

#Indicar desviaciÃ³n estÃ¡ndar de las tallas
sd(tabla$Talla)

#Calculos adicionales para Peso
mean(tabla$Peso)
sd(tabla$Peso)


#1 Importar base de datos existente

setwd("C:/Users/biolc/OneDrive/Documentos/INAPESCA 2023/Curso R")

OM <- read.table("Omaya.txt", header = TRUE)
OM

#Adicionalmente cambiÃ© encabezados

colnames(OM) <- c("LOC", "SEX", "LM", "Pg", "EM")
head(OM)

#2 Sacar todas las operaciones por columnas posibles
summary(OM)

mean(OM$LM)
mean(OM$Pg)
mean(OM$EM)
sd(OM$LM)
sd(OM$Pg)
sd(OM$EM)
median(OM$EM)
median(OM$Pg)
median(OM$EM)
sqrt(OM$LM)
sqrt(OM$Pg)
sqrt(OM$EM)
sum(OM$LM)
sum(OM$Pg)
sum(OM$EM)
cumsum(OM$LM)
cumsum(OM$Pg)
cumsum(OM$EM)
prod(OM$LM)
prod(OM$Pg)
prod(OM$EM)
min(OM$LM)
min(OM$Pg)
min(OM$EM)
max(OM$LM)
max(OM$Pg)
max(OM$EM)
range(OM$LM)
range(OM$Pg)
range(OM$EM)
length(OM$LM)
length(OM$Pg)
length(OM$EM)
cor(OM$LM, OM$Pg, method = "spearman")
cor(OM$LM, OM$Pg, method = "pearson")
cor(OM$LM, OM$Pg, method = "kendall")
cov(OM$LM, OM$Pg, method = "spearman")
cov(OM$LM, OM$Pg, method = "pearson")
cov(OM$LM, OM$Pg, method = "kendall")

#3 Calcular los cuartiles 0.25 y 0.75 de una columna que elijan respecto a factores
#ElegÃ­ 2 ejemplos: Longitud manto vs localidad y Peso vs sexo

tapply(OM$LM, OM$LOC, quantile, probs = c(0.25, 0.75))
tapply(OM$Pg, OM$SEX, quantile, probs = c(0.25, 0.75))


#4 Realizar un remuestreo con reemplazo del 50% de la base de datos total

sample(OM, size = 9, replace = TRUE)
length(OM$LM)


#1 Importar base de datos
setwd("C:/Users/biolc/OneDrive/Documentos/INAPESCA 2023/Curso R")

OM <- read.csv("Omaya.csv", header = T)
head(OM)

colnames(OM) <- c("LOC", "SEX", "LM", "Pg", "EM")


#2 hacer una grÃ¡fica de dispersiÃ³n (x, y)
OM1 <- tapply(OM$LM, OM$Pg, mean)
plot(OM1, type = "p")


#3 hacer un grÃ¡fico de lÃ­neas
OM2 <- tapply(OM$LM, OM$Pg, mean)
plot(OM2, type = "l")

#4 hacer un grÃ¡fico de barras
plot(OM2, type = "h")

#5 hacer un boxplots
boxplot(OM$Pg ~ OM$LOC)

#INSTRUCCIONES
#1 Hacer 4 figuras de diferentes tipos con sus propios datos
#2 Todas las figuras deben ser hechas en ggplot
#3 Unir todas las figuras en una sola hoja

OM <- read.csv("Omaya.csv", header = T)
head(OM)

colnames(OM) <- c("LOC", "SEX", "LM", "Pg", "EM")
head(OM)

library(ggplot2)
library(ggpubr)
library(gridExtra)

#1 Linea
ggplot(OM, aes(x = LM, y = Pg)) + geom_line(col = "#01665e", pch = 16, cex = 1.5) +
  xlab("Longitud del manto (mm)") + ylab("Peso eviscerado (g)")
#2 puntos
ggplot(OM, aes(x = LM, y = Pg)) + geom_point(col = "black", fill = "#5ab4ac", pch = 21, cex = 4)  +
  xlab("Longitud del manto (mm)") + ylab("Peso eviscerado (g)")


#3 histograma
ggplot(OM, aes(x = LM)) + geom_histogram(fill = "#92c5de", col = "black") +
  xlab("Longitud del manto (mm)") + ylab("NÂ° de organismos")

#4 boxplots
ggplot(OM, aes(x = factor(LOC), y = Pg)) + geom_boxplot(fill = "#66c2a5") +
  xlab("Localidad)") + ylab("Peso eviscerado (g)")
