# --- Script de Análisis de Participación Política ---
# Proyecto: Relación entre factores sociales y participación en jóvenes

# 1. Cargar los datos
# Asegúrate de que el archivo CSV esté en la misma carpeta que este script
datos <- read.csv("encuesta_estres.csv")

# 2. Renombrar columnas para facilitar el análisis estadístico
colnames(datos) <- c("fecha", "estres", "sueno", "estudio", "redes", "materias", "ejercicio", "apoyo", "cafeina", "edad")

# 3. Mostrar un resumen de los datos para verificar que todo esté correcto
summary(datos)

# 4. Construcción del Modelo de Regresión Lineal
# Analizamos cómo influyen las variables en la participación
modelo <- lm(estres ~ sueno + estudio + redes + materias + ejercicio + apoyo + cafeina + edad, data = datos)

# 5. Ver resultados del modelo
summary(modelo)

# 6. Generar Matriz de Correción (solo variables numéricas)
matriz_cor <- cor(datos[, 2:10])
print(matriz_cor)

# 7. Gráfica de dispersión básica (Ejemplo: Interés vs Participación)
plot(datos$sueno, datos$estres, 
     main="Relación entre horas de sueño y nivel de estrés",
     xlab="Horas de sueño", 
     ylab="Nivel de estrés",
     pch=19)

abline(lm(estres ~ sueno, data = datos))

