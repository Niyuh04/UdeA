install.packages("openxlsx")
install.packages("stats")
install.packages("cluster")
install.packages("factoextra")
install.packages("dendextend")
install.packages("ggdendro")
library(ggdendro)
library(dendextend)
library(factoextra)
library(stats)
library(cluster)
library(openxlsx)

database<-read.csv("https://raw.githubusercontent.com/Niyuh04/UdeA/main/Cursos/Bioestadistica2/bases%20de%20datos/caracteristicas%20de%20vinos.csv")
datos <- scale(database[, -1])

summary(datos[, -1])

#Paso 1 Calcular las distancias y su método de aglomeración

dist_euclidiana_simple <- hclust(dist(datos), method = "single")
dist_euclidiana_centroide <- hclust(dist(datos), method = "centroid")
dist_mahalanobis_simple <- hclust(daisy(datos), method = "single")
dist_mahalanobis_centroide <- hclust(daisy(datos), method = "centroid")

# Paso 2 Calculo coeficiente de correlación cofenético

# Para distancia euclidiana y método simple
dis_euc_sim <- cophenetic(dist_euclidiana_simple)  # Distancia cofenética 
print(cor(as.vector(dist(datos)), as.vector(dis_euc_sim))) # Calculo el coeficiente de correlación cofenético con las distancias normales y las distancias cofenéticas convertidas en vector

# Para distancia euclidiana y método de centroides
dis_euc_cent <- cophenetic(dist_euclidiana_centroide) 
print(cor(as.vector(dist(datos)), as.vector(dis_euc_cent)))

# Para distancia Mahalanobis y método simple
dis_mah_sim <- cophenetic(dist_mahalanobis_simple) 
print(cor(as.vector(daisy(datos)), as.vector(dis_mah_sim)))

#Para distancia Mahalanobis y método centroides
dis_mah_cent <- cophenetic(dist_mahalanobis_centroide) 
print(cor(as.vector(daisy(datos)), as.vector(dis_mah_cent)))

#Paso 3 Graficar



colores <- c(
  "#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", 
  "#911EB4", "#46F0F0", "#F032E6", "#BCF60C", "#FABEBE", 
  "#008080", "#E6BEFF", "#9A6324", "#FFFAC8", "#800000", 
  "#AA6E28", "#FFD8B1", "#000075", "#808000", "#AaffC3", 
  "#808080", "#FFFFFF", "#000000", "#C0C0C0", "#FF0000", 
  "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF",
  "#6A3D9A", "#FF4500", "#DA70D6", "#DDA0DD", "#98FB98",
  "#8A2BE2", "#F0E68C", "#F5DEB3", "#BDB76B", "#D2B48C",
  "#5F9EA0", "#7FFF00", "#FF6347", "#FF1493", "#B0E0E6",
  "#8B008B", "#556B2F", "#ADFF2F", "#D2691E", "#5F9EA0",
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
  "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
  "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
  "#E5C494", "#B3B3CC", "#CCEBC5", "#FFED6F", "#AEAEAE",
  "#DDAE5C", "#58508D", "#BC5090", "#FF6361", "#AA675D",
  "#D9BF77", "#96C7D6", "#7A9D6F", "#343C3D", "#8496A3",
  "#D3CCE3", "#5C5F58", "#00A4A6", "#E88873", "#B8B894",
  "#4B4E6D", "#2B2E2E", "#7B8D8E", "#506C7F", "#95A3A4",
  "#FF5733", "#4B0082", "#00CED1", "#FFA500", "#228B22",
  "#8B4513", "#8A2BE2", "#2E8B57", "#D2691E", "#9932CC"
)

# para distancia euclidiana + método de aglomeración de enlace simple con 7,30,50 y 100 grupos

fviz_dend(as.dendrogram(dist_euclidiana_simple), 
          k = 7,  # Número de grupos
          k_colors = colores,
          rect = TRUE,
          cex = 0.6,
          k_label = TRUE,
          rect_width = 0.5,
          rect_fill = TRUE,
          main = "Distancia euclidiana + Enlace simple con 7 grupos",
          ylab = "Eje de disimilitud")

fviz_dend(as.dendrogram(dist_euclidiana_simple), 
          k = 30,  # Número de grupos
          k_colors = colores,
          rect = TRUE,
          cex = 0.6,
          k_label = TRUE,
          rect_width = 0.5,
          rect_fill = TRUE,
          main = "Distancia euclidiana + Enlace simple con 30 grupos",
          ylab = "Eje de disimilitud")

fviz_dend(as.dendrogram(dist_euclidiana_simple), 
          k = 50,  # Número de grupos
          k_colors = colores,
          rect = TRUE,
          cex = 0.6,
          k_label = TRUE,
          rect_width = 0.5,
          rect_fill = TRUE,
          main = "Distancia euclidiana + Enlace simple con 50 grupos",
          ylab = "Eje de disimilitud")

fviz_dend(as.dendrogram(dist_euclidiana_simple), 
          k = 100,  # Número de grupos
          k_colors = colores,
          rect = TRUE,
          cex = 0.6,
          k_label = TRUE,
          rect_width = 0.5,
          rect_fill = TRUE,
          main = "Distancia euclidiana + Enlace simple con 100 grupos",
          ylab = "Eje de disimilitud")

length(colores)




