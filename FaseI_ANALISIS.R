install.packages("tidyverse")
library(tidyverse)
library(dplyr)

goodreadsdatos <- read_csv("C:\\Users\\alici\\Desktop\\UNI\\ANALISIS ESTADISTICO AVANZADO\\PRÁCTICA FINAL\\FASE I\\dataset\\books.csv")

#1 ANÁLISIS BÁSICO DEL DATASET

  #1.1. Conocer las dimensiones y el tamaño
dim(goodreadsdatos)

  #1.2. Tipos variables
str(goodreadsdatos)

  #1.3. Valores fatantes (y eliminarlos)
sapply(goodreadsdatos, function(x) sum(is.na(x))) #así se detectan
goodreadsdatos <- goodreadsdatos %>%
  filter(!is.na(average_rating) & !is.na(num_pages)) #así se eliminan

  #1.4. Datos duplicados
sum(duplicated(goodreadsdatos))

  #1.5. Cambiar el formato de las fechas
goodreadsdatos$publication_date <- as.Date(goodreadsdatos$publication_date, format = "%m/%d/%Y")
str(goodreadsdatos$publication_date) #str y head para confirmar
head(goodreadsdatos$publication_date)

  #1.6. Añadir columna "year_published"
goodreadsdatos$year_published <- format(goodreadsdatos$publication_date, "%Y")
str(goodreadsdatos$year_published)

  #1.7. Convertir "year_published" a valor numérico
goodreadsdatos$year_published <- as.numeric(goodreadsdatos$year_published)
str(goodreadsdatos$year_published)
head(goodreadsdatos$year_published)

  #1.8. Eliminar columnas que no voy a utilizar (isbn, isbn13, publication_date, bookID)
goodreadsdatos <- goodreadsdatos %>%
  select(-isbn, -isbn13, -publication_date, -bookID)
head(goodreadsdatos)
colnames(goodreadsdatos)

#2. ANÁLISIS UNIVARIABLE

  #2.1. Variables cualitativas

library(ggplot2)
paleta_barbie <- c(
  "#F4C2C2", "#F8A1D4", "#F7A8B8", "#D28BC1", "#E7A8C8", 
  "#FF9B70", "#F59B42", "#FF6EB1", "#FF9F94", "#F94F77", 
  "#D670A2", "#FF1493", "#FF70A6", "#FF6347", "#E80082", 
  "#F1A7C9", "#F6B8D1", "#F2D8C9", "#F59B42", "#D397C9", 
  "#B8D8D8", "#3F9D9D", "#A6C9E2", "#70D1D1", "#A1D3D3",
  "#FF9BFF", "#FFD700", "#FAD02E", "#FFB3E6", "#E7A8C8"
)
paleta_barbie

    #2.1.1. Idiomas (frecuencia y gráficos)
frecuencia_idiomas <- goodreadsdatos %>%
  count(language_code) %>%
  mutate(relative_freq = n / sum(n)) %>%
  arrange(desc(n))
print(frecuencia_idiomas)

top_idiomas <- goodreadsdatos %>%
  count(language_code) %>%
  arrange(desc(n)) %>%
  top_n(8, n)

ggplot(top_idiomas, aes(x = reorder(language_code, -n), y = n, fill = language_code)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 8 idiomas con más libros escritos", x = "Idiomas (language_code)", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = paleta_barbie) 

    #2.1.2. Editoriales (frecuencias y gráficos)
frecuencia_editoriales <- goodreadsdatos %>%
  count(publisher) %>%
  mutate(relative_freq = n / sum(n)) %>%
  arrange(desc(n))
print(frecuencia_editoriales)

top_editoriales <- goodreadsdatos %>%
  count(publisher) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

ggplot(top_editoriales, aes(x = reorder(publisher, -n), y = n, fill = publisher)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 editoriales con más publicaciones", x = "Editoriales (publisher)", y = "Libros publicados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = paleta_barbie) 

    #2.1.3. Autores (frecuencias y gráficos)
frecuencia_autores <- goodreadsdatos %>%
  count(authors) %>%
  mutate(relative_freq = n / sum(n)) %>%
  arrange(desc(n))
print(frecuencia_autores)

top_autores <- goodreadsdatos %>%
  count(authors) %>%
  arrange(desc(n)) %>%
  top_n(20, n)

ggplot(top_autores, aes(x = reorder(authors, -n), y = n, fill = authors)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 autores con más libros", x = "Autores (authors)", y = "Libros publicados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = paleta_barbie) 

  #2.2. Variables cuantitativas

install.packages("e1071")
library(e1071)

    #2.2.1. Puntuaciones 
puntuaciones <- goodreadsdatos$average_rating

#tabla de frecuencias
frecuencia_puntuaciones <- goodreadsdatos %>%
  count(average_rating) %>%
  mutate(relative_freq = n / sum(n)) %>%
  arrange(desc(n))
print(frecuencia_puntuaciones)

      #2.2.1.1. Medidas de localización (moda, mediana, media y cuartiles)
moda_puntuaciones <- as.numeric(names(sort(table(puntuaciones), decreasing = TRUE)[1]))
media_puntuaciones <- mean(puntuaciones, na.rm = TRUE)
mediana_puntuaciones <- median(puntuaciones, na.rm = TRUE)
cuartiles_puntuaciones <- quantile(puntuaciones, probs = c(0.25, 0.75), na.rm = TRUE)

      #2.2.1.2. Medidas de dispersión (rango, iqr, sd y cv)
rango_puntuaciones <- range(puntuaciones, na.rm = TRUE)
iqr_puntuaciones <- IQR(puntuaciones, na.rm = TRUE)
sd_puntuaciones <- sd(puntuaciones, na.rm = TRUE)
cv_puntuaciones <- (sd_puntuaciones / media_puntuaciones) * 100

      #2.2.1.3. Medidas de forma (asimetría y curtosis)
asimetria_puntuaciones <- skewness(puntuaciones, na.rm = TRUE)
curtosis_puntuaciones <- kurtosis(puntuaciones, na.rm = TRUE)

      #2.2.1.4. Gráficos (boxplot, barras, dispersión)
ggplot(goodreadsdatos, aes(x = puntuaciones, fill = as.factor(puntuaciones))) +  # Mapeo horizontal
  geom_boxplot(fill = "#FF1493", color = "#A6C9E2") +
  labs(title = "Boxplot de las puntuaciones medias", x = "Puntuaciones medias (average_rating)", y = "Valores") +
  theme_minimal() +
  coord_flip() 

ggplot(goodreadsdatos, aes(x = puntuaciones)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#FF1493", color = "black") +
  geom_density(color = "#A6C9E2", size = 1) +
  labs(title = "Histograma con Curva de Densidad de las puntuaciones promedio",
       x = "Promedio de puntuaciones (average_rating)", y = "Densidad") +
  theme_minimal()

    #2.2.2. Páginas 
paginas <- goodreadsdatos$num_pages

#tabla de frecuencias
frecuencia_paginas <- goodreadsdatos %>%
  count(num_pages) %>%
  mutate(relative_freq = n / sum(n)) %>%
  arrange(desc(n))
print(frecuencia_paginas)

      #2.2.2.1. Medidas de localización (moda, mediana, media y cuartiles)
moda_paginas <- as.numeric(names(sort(table(paginas), decreasing = TRUE)[1]))
media_paginas <- mean(paginas, na.rm = TRUE)
mediana_paginas <- median(paginas, na.rm = TRUE)
cuartiles_paginas <- quantile(paginas, probs = c(0.25, 0.75), na.rm = TRUE)

      #2.2.2.2. Medidas de dispersión (rango, iqr, sd y cv)
rango_paginas <- range(paginas, na.rm = TRUE)
iqr_paginas <- IQR(paginas, na.rm = TRUE)
sd_paginas <- sd(paginas, na.rm = TRUE)
cv_paginas <- (sd_paginas / media_paginas) * 100

      #2.2.2.3. Medidas de forma (asimetría y curtosis)
asimetria_paginas <- skewness(paginas, na.rm = TRUE)
curtosis_paginas <- kurtosis(paginas, na.rm = TRUE)

      #2.2.2.4. Gráficos (boxplot, barras, dispersión)
ggplot(goodreadsdatos, aes(x = paginas, fill = as.factor(paginas))) +  # Mapeo horizontal
  geom_boxplot(fill = "#FF1493", color = "black") +
  labs(title = "Boxplot del número de páginas", x = "Páginas (num_pages)", y = "Valores") +
  theme_minimal() +
  coord_flip() 

ggplot(goodreadsdatos, aes(x = paginas)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#FF1493", color = "black") +
  geom_density(color = "#A6C9E2", size = 1) +
  labs(title = "Histograma con Curva de Densidad del número de páginas",
       x = "Nº de páginas", y = "Densidad") +
  theme_minimal()

    #2.2.3. nº de puntuaciones 
n_puntuaciones <- goodreadsdatos$ratings_count

#tabla de frecuencias
frecuencia_npuntuaciones <- goodreadsdatos %>%
  count(ratings_count) %>%
  mutate(relative_freq = n / sum(n)) %>%
  arrange(desc(n))
print(frecuencia_npuntuaciones)

      #2.2.3.1. Medidas de localización (moda, mediana, media y cuartiles)
moda_n_puntuaciones <- as.numeric(names(sort(table(n_puntuaciones), decreasing = TRUE)[1]))
media_n_puntuaciones <- mean(n_puntuaciones, na.rm = TRUE)
mediana_n_puntuaciones <- median(n_puntuaciones, na.rm = TRUE)
cuartiles_n_puntuaciones <- quantile(n_puntuaciones, probs = c(0.25, 0.75), na.rm = TRUE)

      #2.2.3.2. Medidas de dispersión (rango, iqr, sd y cv)
rango_n_puntuaciones <- range(n_puntuaciones, na.rm = TRUE)
iqr_n_puntuaciones <- IQR(n_puntuaciones, na.rm = TRUE)
sd_n_puntuaciones <- sd(n_puntuaciones, na.rm = TRUE)
cv_n_puntuaciones <- (sd_n_puntuaciones / media_n_puntuaciones) * 100

      #2.2.3.3. Medidas de forma (asimetría y curtosis)
asimetria_n_puntuaciones <- skewness(n_puntuaciones, na.rm = TRUE)
curtosis_n_puntuaciones <- kurtosis(n_puntuaciones, na.rm = TRUE)

      #2.2.3.4. Gráficos (boxplot, barras, dispersión)
ggplot(goodreadsdatos, aes(x = n_puntuaciones, fill = as.factor(n_puntuaciones))) +  # Mapeo horizontal
  geom_boxplot(fill = "#FF1493", color = "black") +
  labs(title = "Boxplot del número de puntuaciones", x = "Nº de puntuaciones (ratings_count)", y = "Valores") +
  theme_minimal() +
  coord_flip() 

ggplot(goodreadsdatos, aes(x = n_puntuaciones)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#FF1493", color = "black") +
  geom_density(color = "#A6C9E2", size = 1) +
  labs(title = "Histograma con Curva de Densidad del número de páginas",
       x = "Nº de páginas", y = "Densidad") +
  theme_minimal()

    #2.2.4. Reseñas 
resenas <- goodreadsdatos$text_reviews_count

#tabla de frecuencias
frecuencia_resenas <- goodreadsdatos %>%
  count(text_reviews_count) %>%
  mutate(relative_freq = n / sum(n)) %>%
  arrange(desc(n))
print(frecuencia_resenas)

      #2.2.4.1. Medidas de localización (moda, mediana, media y cuartiles)
moda_resenas <- as.numeric(names(sort(table(resenas), decreasing = TRUE)[1]))
media_resenas <- mean(resenas, na.rm = TRUE)
mediana_resenas <- median(resenas, na.rm = TRUE)
cuartiles_resenas <- quantile(resenas, probs = c(0.25, 0.75), na.rm = TRUE)

      #2.2.4.2. Medidas de dispersión (rango, iqr, sd y cv)
rango_resenas <- range(resenas, na.rm = TRUE)
iqr_resenas <- IQR(resenas, na.rm = TRUE)
sd_resenas <- sd(resenas, na.rm = TRUE)
cv_resenas <- (sd_resenas / media_resenas) * 100

      #2.2.4.3. Medidas de forma (asimetría y curtosis)
asimetria_resenas <- skewness(resenas, na.rm = TRUE)
curtosis_resenas <- kurtosis(resenas, na.rm = TRUE)

      #2.2.4.4. Gráficos (boxplot, barras, dispersión)
ggplot(goodreadsdatos, aes(x = resenas, fill = as.factor(resenas))) +  # Mapeo horizontal
  geom_boxplot(fill = "#FF1493", color = "black") +
  labs(title = "Boxplot del número de reseñas", x = "Nº de reseñas (text_reviews_count)", y = "Valores") +
  theme_minimal() +
  coord_flip() 

ggplot(goodreadsdatos, aes(x = resenas)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#FF1493", color = "black") +
  geom_density(color = "#A6C9E2", size = 1) +
  labs(title = "Histograma con Curva de Densidad del número de reseñas",
       x = "Nº de reseñas", y = "Densidad") +
  theme_minimal()

    #2.2.5. Año 
ano <- goodreadsdatos$year_published

#tabla de frecuencias
frecuencia_anos <- goodreadsdatos %>%
  count(year_published) %>%
  mutate(relative_freq = n / sum(n)) %>%
  arrange(desc(n))
print(frecuencia_anos)

      #2.2.5.1. Medidas de localización (moda, mediana, media y cuartiles)
moda_ano <- as.numeric(names(sort(table(ano), decreasing = TRUE)[1]))
media_ano <- mean(ano, na.rm = TRUE)
mediana_ano <- median(ano, na.rm = TRUE)
cuartiles_ano <- quantile(ano, probs = c(0.25, 0.75), na.rm = TRUE)

      #2.2.5.2. Medidas de dispersión (rango, iqr, sd y cv)
rango_ano <- range(ano, na.rm = TRUE)
iqr_ano <- IQR(ano, na.rm = TRUE)
sd_ano <- sd(ano, na.rm = TRUE)
cv_ano <- (sd_ano / media_ano) * 100

      #2.2.5.3. Medidas de forma (asimetría y curtosis)
asimetria_ano <- skewness(ano, na.rm = TRUE)
curtosis_ano <- kurtosis(ano, na.rm = TRUE)

      #2.2.5.4. Gráficos (boxplot, barras, dispersión)
ggplot(goodreadsdatos, aes(x = ano, fill = as.factor(ano))) +  # Mapeo horizontal
  geom_boxplot(fill = "#FF1493", color = "black") +
  labs(title = "Boxplot del año de publicación", x = "Año de publicación (year_published)", y = "Valores") +
  theme_minimal() +
  coord_flip() 

ggplot(goodreadsdatos, aes(x = ano)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#FF1493", color = "black") +
  geom_density(color = "#A6C9E2", size = 1) +
  labs(title = "Histograma con Curva de Densidad del año de publicación",
       x = "Año", y = "Densidad") +
  theme_minimal()


#3. ANÁLISIS BIVARIANTE

  #3.1. Variables cualitativas (tabla de contingencia autores/editoriales)
top_20_autores <- top_autores$authors
top_8_idiomas <- top_idiomas$language_code

goodreads_top_autores_idiomas <- goodreadsdatos %>%
  filter(authors %in% top_20_autores & language_code %in% top_8_idiomas)

tabla_contingencia <- table(goodreads_top_autores_idiomas$authors, 
                            goodreads_top_autores_idiomas$language_code)
tabla_contingencia

tabla_df <- as.data.frame(tabla_contingencia)
head(tabla_df)

    #3.1.1. Gráficos (barras apiladas y mapa de calor)
ggplot(tabla_df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución de Libros por autor e idioma",
       x = "Autores (authors)", y = "Nº de libros", fill = "Idioma (language_code)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = paleta_barbie)

ggplot(tabla_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#A6C9E2", high = "#FF1493") +
  labs(title = "Mapa de calor de la distribución de libros por autor e idioma",
       x = "Autores (authors)", y = "Idiomas (language_code)", fill = "Nº de libros") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  #3.2. Variables cuantitativas

    #3.2.2. 1er análisis (year_published y average_rating)
      
      #3.2.2.1. Covarianza
covarianza1 <- cov(goodreadsdatos$average_rating, goodreadsdatos$year_published, use = "complete.obs")

      #3.2.2.2. Coeficiente de correlación
correlacion1 <- cor(goodreadsdatos$average_rating, goodreadsdatos$year_published, use = "complete.obs")
correlacion1

      #3.2.2.3. Coeficiente de determinación
determinacion1 <- (correlacion1^2)*100
determinacion1

      #3.2.2.4. Gráfico de dispersión y recta de regresión
ggplot(goodreadsdatos, aes(x = year_published, y = average_rating)) +
  geom_point(alpha = 0.5, color = "#FF1493") +
  geom_smooth(method = "lm", color = "#A6C9E2", se = FALSE) +  # Agregar la recta de regresión
  labs(title = "Relación entre Año de Publicación y Calificación Promedio",
       x = "Año de Publicación (year_published)", y = "Calificación Promedio (average_rating)") +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", round(determinacion1, 2)),
           hjust = 1.1, vjust = 1.5, color = "black", size = 5) +
  theme_minimal()

    #3.2.3. 2º análisis (average_rating / ratings_count)

      #3.2.3.1.
covarianza2 <- cov(goodreadsdatos$average_rating, goodreadsdatos$ratings_count, use = "complete.obs")

      #3.2.3.2. Coeficiente de correlación
correlacion2 <- cor(goodreadsdatos$average_rating, goodreadsdatos$ratings_count, use = "complete.obs")
correlacion2

      #3.2.2.3. Coeficiente de determinación
determinacion2 <- (correlacion2^2)*100
determinacion2

      #3.2.2.4. Gráfico de dispersión y recta de regresión
ggplot(goodreadsdatos, aes(x = ratings_count, y = average_rating)) +
  geom_point(alpha = 0.5, color = "#FF1493") +
  geom_smooth(method = "lm", color = "#A6C9E2", se = FALSE) +  # Agregar la recta de regresión
  labs(title = "Relación entre la cantidad de votos y la calificación promedio",
       x = "Nº de votaciones (ratings_count)", y = "Calificación promedio (average_rating)") +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", round(determinacion2, 2)),
           hjust = 1.1, vjust = 1.5, color = "black", size = 5) +
  theme_minimal()