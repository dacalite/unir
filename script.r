# -----------------------------
# Paso 1: Configuración inicial
# -----------------------------

# Crear vectores
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))

# Crear vector de consumo diario con algunos valores faltantes (NA)
set.seed(123)  # Para reproducibilidad
consumo <- round(runif(20, min = 5, max = 20))  # Consumo entre 5 y 20 kWh
consumo[c(5, 11, 15)] <- NA  # Introducir valores faltantes aleatoriamente

# Costo por kWh para cada tipo de energía
costo_kwh <- c(Renovable = 0.05, No_Renovable = 0.10)

# -----------------------------
# Paso 2: Limpieza de datos
# -----------------------------

# Reemplazar NA en el vector consumo con la mediana del consumo diario de cada tipo
consumo_renovable <- consumo[energia == "Renovable"]
consumo_no_renovable <- consumo[energia == "No Renovable"]

# Calcular la mediana para cada tipo de energía
mediana_renovable <- median(consumo_renovable, na.rm = TRUE)
mediana_no_renovable <- median(consumo_no_renovable, na.rm = TRUE)

# Reemplazar valores NA por la mediana correspondiente
consumo[is.na(consumo) & energia == "Renovable"] <- mediana_renovable
consumo[is.na(consumo) & energia == "No Renovable"] <- mediana_no_renovable

# -----------------------------
# Paso 3: Creación del Dataframe
# -----------------------------

# Crear el dataframe df_consumo
df_consumo <- data.frame(
  tipo_energia = energia,
  consumo_diario = consumo,
  costo_kwh = ifelse(energia == "Renovable", costo_kwh["Renovable"], costo_kwh["No Renovable"])
)

# -----------------------------
# Paso 4: Cálculos
# -----------------------------

# Calcular columna costo_total (consumo * costo por kWh)
df_consumo$costo_total <- df_consumo$consumo_diario * df_consumo$costo_kwh

# Calcular el total de consumo y costo por cada tipo de energía
total_consumo <- tapply(df_consumo$consumo_diario, df_consumo$tipo_energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$tipo_energia, sum)

# Calcular la media del consumo diario para cada tipo de energía
media_consumo <- tapply(df_consumo$consumo_diario, df_consumo$tipo_energia, mean)

# Agregar columna ganancia con un aumento del 10% en el costo_total
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# -----------------------------
# Paso 5: Resumen
# -----------------------------

# Ordenar el dataframe por la columna costo_total en orden descendente
df_consumo <- df_consumo[order(df_consumo$costo_total, decreasing = TRUE), ]

# Extraer las 3 filas con el mayor costo_total en un nuevo dataframe top_3_costos
top_3_costos <- head(df_consumo, 3)

# Crear la lista resumen_energia con todos los elementos solicitados
resumen_energia <- list(
  dataframe_ordenado = df_consumo,
  total_consumo_por_tipo = total_consumo,
  costo_total_por_tipo = total_costo,
  top_3_costos = top_3_costos
)

# Mostrar la lista resumen_energia
print(resumen_energia)
