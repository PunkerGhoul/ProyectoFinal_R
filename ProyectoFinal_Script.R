#rm(list=ls());

## Librerías
library(moments)
  
## Funciones que se usarán para los procedimientos
  
### Medidas de tendencia central
suma = function(x, y) {
  return(x + y)
}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
znormalizar <- function(x) {
  return((x - mean(x)) / sd(x))
}
mgeometrica <- function(x) {
  n = length(x)
  producto = prod(x)
  return(producto ** (1 / n))
}
mponderada <- function(x, f) {
  if(length(x) == length(f)) {
    if(sum(f) == 1) return(x%*%f)
    else return("El vector de probabilidad debe sumar 1.0")
  }
  return("Los vectores deben tener el mismo tamaño")
}
marmonica <- function(x) { #También se puede usar 1/mean(1/x)
  n = length(x)
  sumarecip = sum(1 / x)
  return(n / sumarecip)
}
### Medidas de variación
rango <- function(v) {
  return(max(mydata.filtered.estrato) - min(mydata.filtered.estrato))
}
### Estadística descriptiva
estdescript = function(dataset) {
  print(paste("Media Aritmética:", mean(dataset)))
  print(paste("Mediana:", median(dataset)))
  print(paste("Moda:", getmode(dataset)))
  print(paste("Media Geométrica:", mgeometrica(dataset)))
  print(paste("Media Armónica:", marmonica(dataset)))
  print(paste("Rango:", rango(dataset)))
  print(paste("Rango Intercuartílico:", IQR(dataset)))
  print(paste("Varianza:", var(dataset)))
  print(paste("Desviación Estándar:", sd(dataset)))
  print(paste("Coeficiente de Asimetría:", skewness(dataset)))
  print(paste("Curtosis:", kurtosis(dataset)))
}

# MINTIC 2022 - Análisis

## Importar DataSet
mydata <- read.csv2(file = "../Misi_n_TIC_2020_100_mil_programadores_CSV.csv", sep = ",", encoding = "UTF-8")
#View(mydata)
names(mydata)

## Exploración del DataSet y gráficos

### Filtrar DataSet según requerimientos, Jóvenes de estrato bajo o medio
mydata.filtered <- subset(mydata, (mydata$EDAD > 13 & mydata$EDAD < 29) & (mydata$ESTRATO_SOCIAL < 4))
mydata.filtered.edad <- mydata.filtered$EDAD
mydata.filtered.estrato <- as.integer(mydata.filtered$ESTRATO_SOCIAL)
mydata.filtered.ubicacion <- paste(mydata$MUNICIPIO_NOMBRE, mydata$DEPARTAME_NOMBRE, sep = ', ')
mydata.filtered.ubicacion.general <- mydata$DEPARTAME_NOMBRE

summary(mydata.filtered.edad, main = "Edad Aspirantes MINTIC 2022")
boxplot(mydata.filtered.edad, main = "Edad Aspirantes MINTIC 2022")

summary(mydata.filtered.estrato, main = "Estrato Aspirantes MINTIC 2022")
boxplot(mydata.filtered.estrato, main = "Estrato Aspirantes MINTIC 2022")

### Cálculo, gráficos y diagramas - EDAD
perc.edad = NULL
vals.edad = table(mydata.filtered.edad)
for (i in 1:length(vals.edad)) {
  perc.edad = round(cbind(perc.edad, 100 * vals.edad[[i]] / sum(vals.edad)), 1)
}
labels.edad = paste(names(vals.edad),"AÑOS = ",perc.edad,"%")
pie(vals.edad, labels = labels.edad, main = "Edad Aspirantes MINTIC 2022")
barplot(vals.edad, main = "Edad Aspirantes MINTIC 2022")
stem(mydata.filtered.edad, scale = 1, width = 20, atom = 0.01)
estdescript(mydata.filtered.edad)
print("Normalización entre el máximo y mínimo:")
xnormal.edad = normalize(mydata.filtered.edad)
print("Normalización con la Distribución Normal:")
xnormz.edad = znormalizar(mydata.filtered.edad)

### Cálculo, gráficos y diagramas - ESTRATO
perc.estrato = NULL
vals.estrato = table(mydata.filtered.estrato)
for (i in 1:length(vals.estrato)) {
  perc.estrato = round(cbind(perc.estrato, 100 * vals.estrato[[i]] / sum(vals.estrato)), 1)
}
labels.estrato = paste("Estrato ", names(vals.estrato),"= ",perc.estrato,"%")
pie(vals.estrato, labels = labels.estrato, main = "Estrato Aspirantes MINTIC 2022")
barplot(vals.estrato, main = "Estrato Aspirantes MINTIC 2022")
stem(mydata.filtered.estrato, scale = 0.2, width = 20, atom = 0.01)
estdescript(mydata.filtered.estrato)
print("Normalización entre el máximo y mínimo:")
xnormal.estrato = normalize(mydata.filtered.estrato)
print("Normalización con la Distribución Normal:")
xnormz.estrato = znormalizar(mydata.filtered.estrato)

### Tabla de Frecuencias - EDAD
frelativas.edad <- round(prop.table(vals.edad) * 100, 3)
fabsol.acum.edad <- cumsum(vals.edad)
frelat.acum.edad <- cumsum(frelativas.edad)
tabla_frecs.edad <- cbind(vals.edad, fabsol.acum.edad, frelativas.edad, frelat.acum.edad)
colnames(tabla_frecs.edad) <- c("n", "N", "f", "F")
tabla_frecs.edad

### Tabla de Frecuencias - ESTRATO
frelativas.estrato <- round(prop.table(vals.estrato) * 100, 3)
fabsol.acum.estrato <- cumsum(vals.estrato)
frelat.acum.estrato <- cumsum(frelativas.estrato)
tabla_frecs.estrato <- cbind(vals.edad, fabsol.acum.estrato, frelativas.estrato, frelat.acum.estrato)
colnames(tabla_frecs.estrato) <- c("n", "N", "f", "F")
tabla_frecs.estrato

### Tabla con Datos Agrupados - EDAD
n.edad <- length(mydata.filtered.edad)
nclases.edad <- round(1 + log10(n.edad) / log10(2))
frecuencias.edad <- hist(mydata.filtered.edad, breaks = nclases.edad, right = FALSE, plot = F)
marcas_clase.edad <- frecuencias.edad$mids
fabsltas.edad <- frecuencias.edad$counts
freltvas.edad <- (round(fabsltas.edad / sum(fabsltas.edad), 3)) * 100
abs_acum.edad <- cumsum(fabsltas.edad)
rel_acum.edad <- cumsum(freltvas.edad)
tabla_dtsagr.edad <- cbind(marcas_clase.edad, fabsltas.edad, abs_acum.edad, freltvas.edad, rel_acum.edad)
colnames(tabla_dtsagr.edad) <- c("Mc", "n", "N", "f", "F")
rownames(tabla_dtsagr.edad) <- c("[14, 14.6)", "[14.6, 15.6)", "[15.6, 16.6)", "[16.6, 17.6)", "[17.6, 18.6)",
                                 "[18.6, 19.6)", "[19.6, 20.6)", "[20.6, 21.6)", "[21.6, 22.6)",
                                 "[22.6, 23.6)", "[23.6, 24.6)", "[24.6, 25.6)", "[25.6, 26.6)", "[26.6, 28]")
tabla_dtsagr.edad
hist(mydata.filtered.edad, breaks = nclases.edad, right = FALSE, plot = T)
sum(frecuencias.edad$counts, 2)
frecuencias_Comp.edad <- hist(mydata.filtered.edad, breaks = "Sturges", right = FALSE, plot = F) #IDENTICO

### Tabla con Datos Agrupados - ESTRATO
n.estrato <- length(mydata.filtered.estrato)
nclases.estrato <- round(1 + log10(n.estrato) / log10(2))
frecuencias.estrato <- hist(mydata.filtered.estrato, breaks = nclases.estrato, right = FALSE, plot = F)
marcas_clase.estrato <- frecuencias.estrato$mids
fabsltas.estrato <- frecuencias.estrato$counts
freltvas.estrato <- (round(fabsltas.estrato / sum(fabsltas.estrato), 3)) * 100
abs_acum.estrato <- cumsum(fabsltas.estrato)
rel_acum.estrato <- cumsum(freltvas.estrato)
tabla_dtsagr.estrato <- cbind(marcas_clase.estrato, fabsltas.estrato,
                              abs_acum.estrato, freltvas.estrato, rel_acum.estrato)
colnames(tabla_dtsagr.estrato) <- c("Mc", "n", "N", "f", "F")
rownames(tabla_dtsagr.estrato) <- c("[1, 1.06)", "[1.06, 1.16)", "[1.16, 1.26)", "[1.26, 1.36)",
                                    "[1.36, 1.46)", "[1.46, 1.56)", "[1.56, 1.66)", "[1.66, 1.76)",
                                    "[1.76, 1.86)", "[1.86, 1.96)", "[1.96, 2.06)", "[2.06, 2.16)",
                                    "[2.16, 2.26)", "[2.26, 2.36)", "[2.36, 2.46)", "[2.46, 2.56)",
                                    "[2.56, 2.66)", "[2.66, 2.76)", "[2.76, 2.86)", "[2.86, 3]")
tabla_dtsagr.estrato
hist(mydata.filtered.estrato, breaks = nclases.estrato, right = FALSE, plot = T)
sum(frecuencias.estrato$counts, 2)
frecuencias_Comp.estrato <- hist(mydata.filtered.estrato, breaks = "Sturges", right = FALSE, plot = F) #IDENTICO

### Cálculo, gráficos y diagramas - UBICACIÓN
perc.ubicacion = NULL
vals.ubicacion = table(mydata.filtered.ubicacion)
for (i in 1:length(vals.ubicacion)) {
  perc.ubicacion = round(cbind(perc.ubicacion, 100 * vals.ubicacion[[i]] / sum(vals.ubicacion)), 1)
}
labels.ubicacion = paste(names(vals.ubicacion)," = ", perc.ubicacion,"%")
pie(vals.ubicacion, labels = labels.ubicacion, main = "Ubicaciones Aspirantes MINTIC 2022", radius = 1.2, cex = 0.6)
par(mar = c(4,15,1,1)) # BLTR
barplot(vals.ubicacion, main = "Ubicaciones Aspirantes MINTIC 2022", horiz = T,
        las = 2, names.arg = labels.ubicacion, width = 0.2)
table(factor(sample(mydata.filtered.ubicacion)))

perc.ubicacion.dep = NULL
vals.ubicacion.dep = table(mydata.filtered.ubicacion.general)
for (i in 1:length(vals.ubicacion.dep)) {
  perc.ubicacion.dep = round(cbind(perc.ubicacion.dep, 100 * vals.ubicacion.dep[[i]] / sum(vals.ubicacion.dep)), 1)
}
labels.ubicacion.dep = paste(names(vals.ubicacion.dep)," = ", perc.ubicacion.dep,"%")
pie(vals.ubicacion.dep, labels = labels.ubicacion.dep,
    main = "Ubicaciones (Departamentos) Aspirantes MINTIC 2022", radius = 1.2, cex = 0.6)
par(mar = c(4,15,1,1)) # BLTR
barplot(vals.ubicacion.dep, main = "Ubicaciones (Departamentos) Aspirantes MINTIC 2022",
        horiz = T, las = 2, names.arg = labels.ubicacion.dep, width = 0.2)
table(factor(sample(mydata.filtered.ubicacion.general)))

