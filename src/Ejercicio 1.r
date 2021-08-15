MediaAritmetica <- function(x) 
{
  return (sum(x) / length(x))
}

Moda <- function(x)
{
  set <- unique(x)
  freq <- tabulate(match(x, set))
  m <- max(freq)
  return (set[freq == m])
}

Mediana <- function(x)
{
  return (median(x))
}

DesviacionEstandar <- function(x)
{
  return (sd(x))
}

Varianza <- function(x)
{
  return (DesviacionEstandar(x) ^ 2)
}

CoeficienteVariacion <- function(x)
{
  return (DesviacionEstandar(x) / MediaAritmetica(x))
}

InfoEstadisticosDescriptivos <- function(text, x)
{
  print(text)
  print(summary(x))
  print(paste("Media Aritmética = ", MediaAritmetica(x)))
  print(paste("Moda = ", Moda(x)))
  print(paste("Mediana = ", Mediana(x)))
  print(paste("Varianza = ", Varianza(x)))
  print(paste("Desviación Estándar = ", DesviacionEstandar(x)))
  print(paste("Coeficiente de Variación = ", CoeficienteVariacion(x)))
  Quartiles(x)
}

Quartiles <- function(x)
{
  quantile(x)
}

Histograma <- function(name, abcisa, x)
{
  hist(x, ylab = "qwe", xlab = abcisa, main = name)
}

CajaBigote <- function(name, x)
{
  boxplot(x, main = name)
}

Graficos <- function(name, abcisa, x)
{
  Histograma(name, abcisa, x)
  CajaBigote(name, x)
}

# Importamos la tabla
data <- read.table(file.choose(), header = TRUE)
d <- read.csv(header = T)


# Imprimimos los estadísticos descriptivos de las variables escogidas
InfoEstadisticosDescriptivos("Estadísticos descriptivos de la variable aleatoria Popularidad", data$Popularity)
InfoEstadisticosDescriptivos("Estadísticos descriptivos de la variable aleatoria Acústica", data$Acousticness)
InfoEstadisticosDescriptivos("Estadísticos descriptivos de la variable aleatoria Bailabilidad", data$Danceability)

# Ploteamos los gráficos de las variables aleatorias escogidas
Graficos("Observación Popularidad", "Values", data$Popularity)
Graficos("Observación Acústica", "Values", data$Acousticness)
Graficos("Observación Bailabilidad", "Values", data$Danceability)

print(data$Track_Name[1])
for(i in data$Track_Name)
{
  print(i)
}