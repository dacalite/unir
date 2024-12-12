# 1. Cargar las librerías y los datos
# Instalar y cargar las librerías necesarias
install.packages(c("dplyr", "tidyr"))
library(dplyr)
library(tidyr)

# Cargar el dataset mtcars y convertirlo en dataframe
data(mtcars)
df <- as.data.frame(mtcars)

# 2. Selección de columnas y filtrado de filas
# Seleccionar las columnas mpg, cyl, hp y gear y filtrar filas donde cyl > 4
df <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)

# Verificar el dataframe después de la selección y filtrado
print("Data después de seleccionar y filtrar:")
print(df)

# 3. Ordenación y renombrado de columnas
# Ordenar por hp de forma descendente y renombrar las columnas
df <- df %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)

# Verificar el dataframe después de ordenar y renombrar
print("Data después de ordenar y renombrar columnas:")
print(df)

# 4. Creación de nuevas columnas y agregación de datos
# Crear una columna llamada eficiencia (consumo / potencia)
df <- df %>%
  mutate(eficiencia = consumo / potencia)

# Agrupar por número de cilindros y calcular el consumo medio y la potencia máxima
df_resumen <- df %>%
  group_by(cyl) %>%
  summarise(consumo_medio = mean(consumo, na.rm = TRUE),
            potencia_maxima = max(potencia, na.rm = TRUE))

# Verificar el resumen de datos agrupados
print("Resumen de consumo medio y potencia por cilindros:")
print(df_resumen)

# 5. Creación del segundo dataframe y unión de dataframes
# Crear el dataframe con gear y tipo_transmision
gear_transmision <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

# Realizar el left_join para combinar el dataframe principal con el tipo de transmisión
df <- df %>%
  left_join(gear_transmision, by = "gear")

# Verificar el dataframe después del join
print("Data después del left_join con tipo_transmision:")
print(df)

# 6. Transformación de formatos
# Transformar el dataframe a formato largo utilizando pivot_longer()
df_long <- df %>%
  pivot_longer(
    cols = c(consumo, potencia, eficiencia),
    names_to = "medida",
    values_to = "valor"
  )

# Agrupar por claves (cyl, gear, tipo_transmision, medida) para identificar duplicados
df_long_resumen <- df_long %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor_promedio = mean(valor, na.rm = TRUE))

# Transformar de nuevo a formato ancho con pivot_wider utilizando mean() para manejar duplicados
df_wide <- df_long_resumen %>%
  pivot_wider(
    names_from = medida,
    values_from = valor_promedio
  )

# Verificar el dataframe final en formato ancho
print("Data final en formato ancho después de pivot_wider:")
print(df_wide)

# 7. Verificación final
print("Todos los resultados han sido verificados en cada etapa.")
