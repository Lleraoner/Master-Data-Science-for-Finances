set.seed(123)
library(memisc)
library(dplyr)
library(factoextra)
library(cluster)
library(clustertend)
library(FactoMineR)




ruta <-
  'D:/aaaCUNEF/tecnicas de agrupacion y reduccion de la dimension/LOScochesdeljefe/tterReno.sav'
coches <- as.data.frame(as.data.set(spss.system.file(file = ruta)))


coches <- na.omit(coches)



##TEnemos que escalar todas las variables, el problema es que no todas son numericas, hay algunas de tipo factor entonces le decimos que scale todas las que son iguales a integer y num.

perfomScaling <-  T
if (perfomScaling) {
  for (i in names(coches)) {
    if (class(coches[, i]) == 'integer' |
        class(coches[, i]) == 'numeric') {
      coches[, i] = scale(coches[, i])
    }
  }
  
}




head(coches, 3)


str(coches)

##Ahora ponemos la marca como indice
coches <-
  data.frame(coches[, -1], row.names = make.names(coches[, 1], unique = T))


##Elegimos las columnas numericas.
columnasnum <-
  c(
    'pvp',
    'cc',
    'potencia',
    'rpm',
    'peso',
    'cons90',
    'cons120',
    'consurb',
    'velocida',
    'acelerac'
  )
##Creamos un nuevo dataframe, con las columnas buenas.

cochesescalados <- subset(coches, select = columnasnum)
##ACP
acp <- prcomp(cochesescalados)
analisiscomponentes <- PCA(cochesescalados)
analisiscomponentes$eig
analisiscomponentes$var$contrib
pc <- princomp(cochesescalados)
plot(pc)
summary(pc)
pc$center

columnasnum <-
  c('potencia', 'rpm', 'peso', 'consurb', 'velocida', 'acelerac')
cochesescalados <- subset(coches, select = columnasnum)

##ANALISIS CLUSTER
hopkins(cochesescalados[, -1], n = nrow(cochesescalados) - 1)


qdist <- get_dist(cochesescalados, stand = T, method = 'pearson')
qdist
str(qdist)
##Realizamos la representación gráfica.
fviz_dist(qdist, lab_size = 5)
##cambiamos la representación
fviz_dist(
  qdist,
  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),
  lab_size = 5
)



##Realizamos este plot, para observar cual es el número óptimo de clusteres
fviz_nbclust(
  x = cochesescalados,
  FUNcluster = kmeans,
  method = "wss",
  k.max = 15,
  diss = get_dist(cochesescalados, method = "euclidean"),
  nstart = 50
)
##Observamos que el numero óptimo es el 2, pero nosotros vamos a elegir 4 clusteres por que divirlo en 2 no tiene sentido.


fviz_nbclust(cochesescalados, kmeans, method = "silhouette") +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x = "Número k de clusters", y = "Anchura del perfil promedio")

##Ahora vamos a realizar una comparativa de los clusteres
k2 <- kmeans(cochesescalados, centers = 2, nstart = 25)
k3 <- kmeans(cochesescalados, centers = 3, nstart = 25)
k4 <- kmeans(cochesescalados, centers = 4, nstart = 25)
k5 <- kmeans(cochesescalados, centers = 5, nstart = 25)
k6 <- kmeans(cochesescalados, centers = 6, nstart = 25)
k7 <- kmeans(cochesescalados, centers = 7, nstart = 25)
k8 <- kmeans(cochesescalados, centers = 8, nstart = 25)
p1 <-
  fviz_cluster(k2, geom = 'point', data = cochesescalados) + ggtitle('K = 2')
p2 <-
  fviz_cluster(k3, geom = 'point', data = cochesescalados) + ggtitle('K = 3')
p3 <-
  fviz_cluster(k4, geom = 'point', data = cochesescalados) + ggtitle('K = 4')
p4 <-
  fviz_cluster(k5, geom = 'point', data = cochesescalados) + ggtitle('K = 5')
p5 <-
  fviz_cluster(k6, geom = 'point', data = cochesescalados) + ggtitle('K = 6')



library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, nrow = 2)

##Con los 4 clusteres observamos el dendograma
fviz_dend(hclust(dist(cochesescalados)),
          k = 4,
          cex = 0.5,
          main = "Dendrograma")


##Ahora analizamos la correlación entre las variables
cochescorr <- cor(cochesescalados, method = 'pearson')
round(cochescorr, 3)

##Y ahora la convertimos en la matriz de distancias
dist.cor <- as.dist(1 - cochescorr)
round(as.matrix(dist.cor), 2)

daisy(cochescorr,
      metric = c('euclidean', 'manhattan', 'gower'),
      stand = F)##Stand lo que hace es standarizar las variables.

coches.eclust <-
  eclust(
    cochesescalados,
    FUNcluster = 'kmeans',
    stand = T,
    hc_metric = 'euclidean',
    nstart = 25,
    k = 4
  )





km_clusters <- kmeans(x = cochesescalados,
                      centers = 4,
                      nstart = 25)
fviz_cluster(
  object = km_clusters,
  data = cochesescalados,
  show.clust.cent = TRUE,
  ellipse.type = "convex",
  star.plot = TRUE,
  repel = TRUE
) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")




coches.eclust <-
  eclust(
    cochesescalados,
    FUNcluster = "kmeans",
    stand = TRUE,
    hc_metric = "euclidean",
    nstart = 25,
    k = 5
  )

numoptimocluster <- coches.eclust$nbclust
