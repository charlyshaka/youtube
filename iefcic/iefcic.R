## Los archivos que se van a usar fueron descargados de www.datos.gov.co
## Luego fueron extraidos de forma local en formato .txt y .sav
## En el video https://www.youtube.com/watch?v=6ULCP7NsITY se explica paso a paso
## la obtención de datos
## librerias requeridas
library(haven)
archivo <- file.choose() ## seleccionar archivo a cargar
df <- read_sav(archivo) ## leer el archivo seleccionado previamente
df <- read_sav("~/Desktop/IEFIC_2015.sav") ## leer mi archivo preseleccionado

df[df == "NaN"] <- "NA" ## Convertir todos los NaN a NA

library(data.table) ## Libreria requerida
dt <- data.table(df) ## conviertiendo al formato datatable

## Imformación sobre productos financieros
dfinanc <- dt[, c(195:201), with = FALSE] ## seleccionando lo que tiene que ver con productos financieros
dfinanc <- sapply(dfinanc, FUN = function(x) {x <- as.numeric(x)}) ## pasando character a numeric
dfinanc <- as.data.table(dfinanc) ## convirtiendo de tipo matrix a datatable

summary(dfinanc) ## estadísticas básicas de las variables

nrow(dfinanc[is.na(P2966)]) ## cuantos no perciben ingresos por el instrumento financiero o no tienen
nrow(dfinanc[!is.na(P2966)]) ## cuantos perciben algo por el instrumento financiero o lo tienen

nrow(dfinanc[!is.na(P2966)]) / nrow(dfinanc[is.na(P2966)]) ## porcentaje de los que perciben o tienen

library(ggplot2) ## libreria de graficos
dfinanc2 <- dfinanc ## haciendo copia de los datos
dfinanc2[is.na(dfinanc2)] <- 0 ## convirtiendo a cero todos los NAs

## Plot de ingresos por el instrumento financiero
ggplot(data = dfinanc2, aes(x = seq(1:nrow(dfinanc2)), y = log(P2966))) + geom_point()

## plot de la distribucion de ingresos por instrumento financiero
ggplot(data = dfinanc2, aes(x = log(P2966))) + geom_density()

## Correlaciones 
pairs(dfinanc2)
cor(dfinanc2)
library(corrplot)
corrplot(cor(dfinanc2))
corrplot(cor(dfinanc2), method = "number")
corrplot.mixed(cor(dfinanc2), lower="number", upper="circle")


ggplot(data = dfinanc2, aes(x = P2971, y = P2972)) + geom_point()

ggplot(data = dfinanc2, aes(x = P2967, y = P2970)) + geom_point()
