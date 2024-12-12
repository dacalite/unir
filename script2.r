# Script: procesar_numeros.R

# --------------------- Función para leer el archivo ---------------------
leer_numeros <- function(nombre_archivo) {
  # Verificar si el archivo existe
  if (!file.exists(nombre_archivo)) {
    stop("El archivo no existe. Deteniendo la ejecución.")
  }
  # Leer el archivo y convertirlo en un vector de enteros
  numeros <- as.integer(readLines(nombre_archivo))
  return(numeros)
}

# --------------------- Configuración del entorno ---------------------
# Nombre del archivo de entrada y salida
archivo_entrada <- "numeros.txt"
archivo_salida <- "resultados.txt"

# Leer el archivo y obtener el vector de números
numeros <- leer_numeros(archivo_entrada)

# --------------------- Cálculo de estadísticos ---------------------
# Calcular la media, mediana y desviación estándar de los datos
media <- mean(numeros, na.rm = TRUE)
mediana <- median(numeros, na.rm = TRUE)
desviacion <- sd(numeros, na.rm = TRUE)

# Manejo de valores atípicos (alta variabilidad)
if (desviacion > 10) {
  mensaje <- "Se detecta alta variabilidad en los datos (desviación estándar > 10)."
  print(mensaje)
} else {
  mensaje <- "La desviación estándar está dentro de límites aceptables."
  print(mensaje)
}

# --------------------- Uso de sapply() para elevar al cuadrado ---------------------
# Calcular el cuadrado de cada número utilizando sapply()
numeros_cuadrado <- sapply(numeros, function(x) x^2)

# --------------------- Escribir resultados en el archivo ---------------------
# Crear el archivo de salida y escribir la información
con <- file(archivo_salida, "w")  # Abrir archivo para escritura

writeLines("Resultados del Análisis de Numeros", con)
writeLines("---------------------------------", con)
writeLines(paste("Media:", media), con)
writeLines(paste("Mediana:", mediana), con)
writeLines(paste("Desviación estándar:", desviacion), con)

writeLines("---------------------------------", con)
writeLines("Lista de Números al Cuadrado:", con)
writeLines(paste(numeros_cuadrado, collapse = ", "), con)

# Cerrar el archivo
close(con)

print(paste("Resultados guardados en", archivo_salida))
