# Cargar las librerías necesarias
library(dplyr)
library(tidyr)

# Cargar el dataset mtcars y convertirlo en un dataframe
df <- as.data.frame(mtcars)

# 1. Selección de columnas y filtrado de filas
df_filtrado <- df %>%
  select(mpg, cyl, hp, gear) %>%   # Seleccionar las columnas mpg, cyl, hp y gear
  filter(cyl > 4)                 # Filtrar las filas donde el número de cilindros (cyl) sea mayor a 4

# 2. Ordenación y renombrado de columnas
df_ordenado <- df_filtrado %>%
  arrange(desc(hp)) %>%                     # Ordenar por hp de forma descendente
  rename(consumo = mpg, potencia = hp)      # Renombrar mpg a consumo y hp a potencia

# 3. Creación de nuevas columnas y agregación de datos
df_con_eficiencia <- df_ordenado %>%
  mutate(eficiencia = consumo / potencia) %>%  # Crear la columna eficiencia
  group_by(cyl) %>%                            # Agrupar por el número de cilindros (cyl)
  summarise(consumo_medio = mean(consumo),     # Calcular el consumo medio
            potencia_maxima = max(potencia))  # Calcular la potencia máxima

# 4. Creación del segundo dataframe y unión de dataframes
df_tipo_transmision <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

# 5. Transformación de formatos
# De formato ancho a largo
df_largo <- df_unido %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia), 
               names_to = "medida", 
               values_to = "valor")

# Manejo de duplicados y transformación de vuelta a formato ancho
df_ancho <- df_largo %>%
  group_by(cyl, gear, tipo_transmision, medida) %>% 
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") %>%  # Manejo de duplicados con mean()
  pivot_wider(names_from = medida, values_from = valor)              # Transformar de vuelta a formato ancho

# 6. Verificación: imprimir los dataframes resultantes
print("Filtrado:")
print(df_filtrado)
print("Ordenado:")
print(df_ordenado)
print("Con eficiencia:")
print(df_con_eficiencia)
print("Unido:")
print(df_unido)
print("Formato largo:")
print(df_largo)

