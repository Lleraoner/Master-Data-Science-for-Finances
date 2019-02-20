##ANACOR


library(factoextra)
library(FactoMineR)


peliculas <-
  matrix(c(70, 45, 30, 0, 35, 0, 45, 30, 80, 5, 0, 0, 30, 20, 10),
         nrow = 5,
         ncol = 3)
rownames(peliculas) <-
  c("Terror", "Comedia", "Drama", "Accion", "Otras")
colnames(peliculas) <- c("<25", "25-50", ">50")
peliculas
test.ind = chisq.test(peliculas)
test.ind

library(ggplot2)
library(gplots)
tabla.peliculas = as.table(as.matrix(peliculas))
tabla.peliculas
balloonplot(
  t(tabla.peliculas),
  main = 'Peliculas',
  xlab = 'Edades',
  ylab = 'Generos',
  label = TRUE,
  show.margins = FALSE
)
library(ggpubr)
ggballoonplot(peliculas, fill = "value", show.label = TRUE) +
  scale_fill_viridis_c(option = "C")

library("graphics")
mosaicplot(
  tabla.peliculas,
  shade = TRUE,
  las = 1,
  main = "Peliculas",
  margin = 0.3
)


library("vcd")
assoc(
  head(tabla.peliculas),
  shade = T,
  las = 3,
  main = "Relacion entre edades y generos"
)

peliculas.ca = CA(tabla.peliculas, graph = FALSE)
peliculas.ca

summary(peliculas.ca, nb.dec = 2, ncp = 2)
peliculas.ca$eig

eig = get_eigenvalue(peliculas.ca)
traza = sum(eig[1:2])
coef.cor = sqrt(traza)
coef.cor

chi2 = traza * sum(as.matrix(peliculas))
chi2

gdl = (nrow(peliculas) - 1) * (ncol(peliculas) - 1)
gdl

p.val = pchisq(chi2, df = gdl, lower.tail = FALSE)
p.val

round(peliculas.ca$eig, 2)


fviz_screeplot(peliculas.ca) +
  geom_hline(yintercept = 25,
             linetype = 3,
             color = "black")

plot(
  peliculas.ca,
  axes = c(1, 2),
  col.row = "blue",
  col.col = "red",
  title = "Relación entre generos y edades"
)

fviz_ca_biplot(peliculas.ca) +
  ggtitle("Relación entre generos y edades") +
  theme_minimal()

filas = get_ca_row(peliculas.ca)
filas
filas$coord[, 1:2]

fviz_ca_row(peliculas.ca, col.row = "steelblue", shape.row = 15) +
  ggtitle("Puntuaciones de fila")

filas$contrib
library(corrplot)
corrplot(filas$contrib, is.corr = FALSE)

fviz_contrib(peliculas.ca, choice = "row", axes = 1) +
  ggtitle("Contribución de las categorías de fila a la explicación de la Dim 1")

fviz_contrib(peliculas.ca, choice = "row", axes = 2) +
  ggtitle("Contribución de las categorías de fila a la explicación de la Dim 2")

round(filas$cos2, 4)

corrplot(filas$cos2, is.corr = FALSE)

fviz_ca_biplot(peliculas.ca
               , map = "rowprincipal", arrow = c(TRUE, TRUE))

fviz_ca_biplot(peliculas.ca, map = "colgreen",
               arrow = c(TRUE, FALSE)) +
  ggtitle("Contribución de los géneros a las dimensiones")

fviz_ca_biplot(peliculas.ca, map = "rowgreen",
               arrow = c(FALSE, TRUE)) +
  ggtitle("Contribución de las edades a las dimensiones")
