# Importamos la tabla con los géneros pop y sus derivados
data1 <- read.table(file.choose(), header = T)
# Importamos la tabla con los demás géneros musicales
data2 <- read.table(file.choose(), header = T)

# Prueba de hipótesis para la varianza de igualdad contra diferencia
var.test(data1$Popularity, data2$Popularity)

# Prueba de hipótesis para la media
t.test(x = data1$Popularity, y = data2$Popularity, alternative = "greater", var.equal = FALSE)
