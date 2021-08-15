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
  print(paste('Media Aritmética = ', MediaAritmetica(x)))
  #print(paste('Moda = ', Moda(x)))
  print(paste('Mediana = ', Mediana(x)))
  print(paste('Varianza = ', Varianza(x)))
  print(paste('Desviación Estándar = ', DesviacionEstandar(x)))
  print(paste('Coeficiente de Variación = ', CoeficienteVariacion(x)))
  Quartiles(x)
}

Quartiles <- function(x)
{
  quantile(x)
}

Histograma <- function(name, abcisas, x)
{
  hist(x, main = name, xlab = abcisas)
}

CajaBigote <- function(name, x)
{
  boxplot(x, main = name)
}

Graficos <- function(name, abcisas, x)
{
  Histograma(name, abcisas, x)
  CajaBigote(name, x)
}

GenerarMuestras <- function(poblacion, n, reemplazamiento)
{
  muestra <- sample(x = poblacion, size = n, replace = reemplazamiento)
  return (muestra)
}

IntervaloConfianzaMedia <- function(sample, alpha, varianza = NULL)
{
  n = length(sample)
  i = 1 - alpha
  sqrt_n = sqrt(n)
  x = mean(sample)
  z = qnorm(i)
  
  if (!is.null(varianza))
  {
    e = z * (varianza / sqrt_n)
    return (c(x - e, x + e))
  }
  else if(n <= 30)
  {
    e = qt(i, n - 1) * (Varianza(sample) / sqrt_n)
    return (c(x - e, x + e))
  }
  else
  {
    e = z * (Varianza(sample) / sqrt_n)
    return (c(x - e, x + e))
  }
}

IntervaloConfianzaVarianza <- function(x, alpha)
{
  i = 1 - alpha / 2
  v = Varianza(x)
  n = length(x)
  e1 = qchisq(i, n - 1)
  e2 = qchisq(alpha / 2, n - 1)
  
  return (c(((n - 1) * v) / e1 , ((n - 1) * v) / e2 ))
}

IntervalosConfianza <- function(text, x)
{
  print(text)
  print("Intervalo de Confianza para la Media:")
  print(IntervaloConfianzaMedia(x, 0.95))
  print("Intervalo de Confianza para la Varianza:")
  print(IntervaloConfianzaVarianza(x, 0.95))
}

# Simulando una población normal de tamaño 500
print("Población con distribución normal de tamaño 500")
data <- rnorm(500)

# Calculando los estadísticos descriptivos de la población
InfoEstadisticosDescriptivos("Estadísticos descriptivos de la población", data)

# Calculando los intervalos de confianza de la población
IntervalosConfianza("Intervalos de confianza de la población", data)

# Ploteando los gráficos de la población
Graficos("Población Normal (500)", "Values", data)

# Generando las 8 muestras orientadas
muestra_reemp_300 <- GenerarMuestras(data, 300, TRUE)
muestra_reemp_100 <- GenerarMuestras(data, 100, TRUE)
muestra_reemp_30 <- GenerarMuestras(data, 30, TRUE)
muestra_reemp_20 <- GenerarMuestras(data, 20, TRUE)
muestra_300 <- GenerarMuestras(data, 300, FALSE)
muestra_100 <- GenerarMuestras(data, 100, FALSE)
muestra_30 <- GenerarMuestras(data, 30, FALSE)
muestra_20 <- GenerarMuestras(data, 20, FALSE)

# Calculando los estadísticos descriptivos de las muestras
InfoEstadisticosDescriptivos("Estadísticos descriptivos de una muestra con reemplazamiento de tamaño 300", muestra_reemp_300)
InfoEstadisticosDescriptivos("Estadísticos descriptivos de una muestra con reemplazamiento de tamaño 100", muestra_reemp_100)
InfoEstadisticosDescriptivos("Estadísticos descriptivos de una muestra con reemplazamiento de tamaño 30", muestra_reemp_30)
InfoEstadisticosDescriptivos("Estadísticos descriptivos de una muestra con reemplazamiento de tamaño 20", muestra_20)
InfoEstadisticosDescriptivos("Estadísticos descriptivos de una muestra sin reemplazamiento de tamaño 300", muestra_300)
InfoEstadisticosDescriptivos("Estadísticos descriptivos de una muestra sin reemplazamiento de tamaño 100", muestra_100)
InfoEstadisticosDescriptivos("Estadísticos descriptivos de una muestra sin reemplazamiento de tamaño 30", muestra_30)
InfoEstadisticosDescriptivos("Estadísticos descriptivos de una muestra sin reemplazamiento de tamaño 20", muestra_20)

# Ploteando los gráficos de las muestras
Graficos("Muestra con Reemplazamiento (300)", "Values", muestra_reemp_300)
Graficos("Muestra con Reemplazamiento (100)", "Values", muestra_reemp_100)
Graficos("Muestra con Reemplazamiento (30)", "Values", muestra_reemp_30)
Graficos("Muestra con Reemplazamiento (20)", "Values", muestra_reemp_20)
Graficos("Muestra sin Reemplazamiento (300)", "Values", muestra_300)
Graficos("Muestra sin Reemplazamiento (100)", "Values", muestra_100)
Graficos("Muestra sin Reemplazamiento (30)", "Values", muestra_30)
Graficos("Muestra sin Reemplazamiento (20)", "Values", muestra_20)

# Calculando los intervalos de confianza para la media y para la varianza
IntervalosConfianza("Intervalos de confianza para una muestra con reemplazamiento de tamaño 300", muestra_reemp_300)
IntervalosConfianza("Intervalos de confianza para una muestra con reemplazamiento de tamaño 100", muestra_reemp_100)
IntervalosConfianza("Intervalos de confianza para una muestra con reemplazamiento de tamaño 30", muestra_reemp_30)
IntervalosConfianza("Intervalos de confianza para una muestra con reemplazamiento de tamaño 20", muestra_reemp_20)
IntervalosConfianza("Intervalos de confianza para una muestra sin reemplazamiento de tamaño 300", muestra_300)
IntervalosConfianza("Intervalos de confianza para una muestra sin reemplazamiento de tamaño 100", muestra_100)
IntervalosConfianza("Intervalos de confianza para una muestra sin reemplazamiento de tamaño 30", muestra_30)
IntervalosConfianza("Intervalos de confianza para una muestra sin reemplazamiento de tamaño 20", muestra_20)

