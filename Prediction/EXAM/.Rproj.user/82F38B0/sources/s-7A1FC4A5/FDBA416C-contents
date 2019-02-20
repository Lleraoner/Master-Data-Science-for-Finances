

# Paquetes

library(dplyr)
library(psych)
library(GGally)
library(corrplot)
library(ggpubr)
library(leaps)
library(car)
library(Jmisc)
library(gvlma)
library(caret)
library(ROCR)
library(glmnet)
library(kableExtra)
library(splines)
library(gam)

################################################################# TAREAS ################################################################# 

################ 1) Parseado #########################
#########################

datos <- read.csv('EGT2010_2017.csv') # carga de datos

str(datos) # comprobar carga



################ 2) Tareas #########################
#########################

# seleccion variables enunciado

datos_sel <- datos[, c(81:93, 3, 4, 6, 21, 72, 112, 110, 109, 80, 151, 146)] 

# eliminar NAs conforme enunciado

datos_sel_clean <- na.omit(datos_sel) 

# vision global de los datos

summary(datos_sel_clean) 

# dividimos el dataset conforme al enunciado. df.1 obs de 2010 a 2014 y df.2 de 2015 a 2017.

df.1 <- subset(datos_sel_clean,datos_sel_clean$AÑO <= 2014)
df.2 <- subset(datos_sel_clean,datos_sel_clean$AÑO > 2014)

# asignamos semilla
set.seed(1234)

# Seleccionamos 150 observaciones de cada muestra.

df.1_sample <- df.1[sample(nrow(df.1), 150),]
df.2_sample <- df.2[sample(nrow(df.2), 150),]

# Eliminamos la variables creadas anteriormente excepto las dos ultimas muestras.

rm(datos,datos_sel, datos_sel_clean,df.1,df.2)

################################################################# EXAMEN ################################################################# 

################ 3) Tratamiento de variables #########################
#########################

#########################
## Muestra 2010 a 2014 será muestra_1
#########################
# Index

rownames(df.1_sample) <- make.names(df.1_sample$ID)
muestra_1 <- df.1_sample[,-14]

# colnames AÑO como YEAR

colnames(muestra_1)[23] <- "YEAR"

# tratamiento de variables

muestra_1 <- muestra_1 %>%
 mutate(YEAR = as.factor(YEAR)) ## convertimos year a factor.

#########################
## Muestra 2015 a 2017 será muestra_2
#########################
# Index

rownames(df.2_sample) <- make.names(df.2_sample$ID)
muestra_2 <- df.2_sample[,-14]

# colnames AÑO como YEAR

colnames(muestra_2)[23] <- "YEAR"

# tratamiento de variables

muestra_2 <- muestra_2 %>%
  mutate(YEAR = as.factor(YEAR)) ## convertimos year a factor.


################ 4) TARGET #########################
#########################

#########################
## Muestra 2010 a 2014 será variables_1
#########################

variables_1 <- muestra_1

target <- variables_1$VALORACION_MEDIO_AMBIENTE <- as.factor(ifelse(variables_1$VALORACION_MEDIO_AMBIENTE <=8,0,1))

table(variables_1$VALORACION_MEDIO_AMBIENTE)

Nvariables_1 <- variables_1 %>%
  select_if(~!is.factor(.x))
  
Fvariables_1 <-  select_if(variables_1, is.factor)

  
#########################
## Muestra 2015 a 2017 será variables_2
#########################

variables_2 <- muestra_2

target_test <- variables_2$VALORACION_MEDIO_AMBIENTE <- as.factor(ifelse(variables_2$VALORACION_MEDIO_AMBIENTE <=8,0,1))

table(variables_2$VALORACION_MEDIO_AMBIENTE)


Nvariables_2 <- variables_2 %>%
  select_if(~!is.factor(.x))

Fvariables_2 <-  select_if(variables_2, is.factor)


################ 5) ANALISIS EXPLORATORIO #########################
#########################


# Correlaciones

Nvariables_1_cor <- cor(Nvariables_1)

corrplot(Nvariables_1_cor,
         tl.col="black", tl.cex=0.7, tl.srt=45) 

Nvariables_2_cor <- cor(Nvariables_2)

corrplot(Nvariables_2_cor, tl.col="black", tl.cex=0.7, tl.srt=45) 


# Histogramas

## todos 1

multi.hist(x = Nvariables_1, dcol = c("blue","red"), dlty = c("dotted", "solid"))

## todos 2

multi.hist(x = Nvariables_2, dcol = c("blue","red"), dlty = c("dotted", "solid"))

# relaciones entre variables

variables_chart <- muestra_1 %>%
  mutate(val_alojamiento = (rowSums(.[2:4])/3)) %>%
  mutate(val_restauracion = (rowSums(.[10:13])/4)) %>%
  dplyr::select(c(VALORACION_MEDIO_AMBIENTE, PAIS_RESID_AGRUP,SEXO,NOCHES_PERNOCTADAS,val_alojamiento, VALORACION_CLIMA:VALORACION_LIMPIEZA, val_restauracion)) %>%  ## seleccionamos variables
  select_if(~!is.factor(.x))

ggpairs(variables_chart,
        colour = variables_chart$VALORACION_MEDIO_AMBIENTE > 8,
        lower = list(continuous='points'),
        axisLabels = 'none',
        upper = list(continuous='blank'))

## Valoraciones


## Limpieza
### 1

box_status <- ggplot(variables_1, aes(VALORACION_MEDIO_AMBIENTE, VALORACION_LIMPIEZA))
box_status + geom_boxplot(aes(fill = VALORACION_LIMPIEZA)) +
  theme(axis.text.x = element_blank()) + labs(
    title = "Valoración Medio Ambiente en Función de Valoración Limpieza",
    x = "Valoración Medio Ambiente",
    y = "Valoración Limpieza")    + facet_wrap(~ PAIS_RESID_AGRUP)

### 2 
box_status <- ggplot(variables_2, aes(VALORACION_MEDIO_AMBIENTE, VALORACION_LIMPIEZA))
box_status + geom_boxplot(aes(fill = VALORACION_LIMPIEZA)) +
  theme(axis.text.x = element_blank()) + labs(
    title = "Valoración Medio Ambiente en Función de Valoración Limpieza",
    x = "Valoración Medio Ambiente",
    y = "Valoración Limpieza")    + facet_wrap(~ PAIS_RESID_AGRUP)


## Paisajes
### 1

box_status <- ggplot(variables_1, aes(VALORACION_MEDIO_AMBIENTE, VALORACION_PAISAJES))
box_status + geom_boxplot(aes(fill = VALORACION_PAISAJES)) +
  theme(axis.text.x = element_blank()) + labs(
    title = "Valoración Medio Ambiente en Función de Valoración Limpieza",
    x = "Valoración Medio Ambiente",
    y = "Valoración Limpieza")  + facet_wrap(~ ESTANCIA_MAYOR_ISLA_G2)

### 2 
box_status <- ggplot(variables_2, aes(VALORACION_MEDIO_AMBIENTE, VALORACION_PAISAJES))
box_status + geom_boxplot(aes(fill = VALORACION_PAISAJES)) +
  theme(axis.text.x = element_blank()) + labs(
    title = "Valoración Medio Ambiente en Función de Valoración Limpieza",
    x = "Valoración Medio Ambiente",
    y = "Valoración Limpieza") + facet_wrap(~ ESTANCIA_MAYOR_ISLA_G2)



## Paisajes en funcion de la isla de estancia por cada rango de ingresos
### 1


box_status <- ggplot(variables_1, aes(VALORACION_MEDIO_AMBIENTE, VALORACION_PAISAJES))
box_status + geom_boxplot(aes(fill = VALORACION_PAISAJES)) +
  theme(axis.text.x = element_blank()) + labs(
    title = "Valoración Medio Ambiente en Función de Valoración Paisaje",
    x = "Valoración Medio Ambiente",
    y = "Valoración Paisaje") + facet_wrap(~ SEXO)

### 2 
box_status <- ggplot(variables_2, aes(VALORACION_MEDIO_AMBIENTE, VALORACION_PAISAJES))
box_status + geom_boxplot(aes(fill = VALORACION_PAISAJES)) +
  theme(axis.text.x = element_blank()) + labs(
    title = "Valoración Medio Ambiente en Función de Valoración Paisaje",
    x = "Valoración Medio Ambiente",
    y = "Valoración Paisaje") + facet_wrap(~ SEXO)


# Matrices de correspondencia

# estancia

estancia <- as.data.frame(table(variables_1$VALORACION_MEDIO_AMBIENTE, variables_1$ESTANCIA_MAYOR_ISLA_G2))
ggballoonplot(estancia, fill = "value",main ="ESTANCIA_MAYOR_ISLA_G2",show.label = TRUE,ggtheme=theme_bw())+
  scale_fill_viridis_c(option = "D")


estancia_2 <- as.data.frame(table(variables_2$VALORACION_MEDIO_AMBIENTE, variables_2$ESTANCIA_MAYOR_ISLA_G2))
ggballoonplot(estancia_2, fill = "value",main ="ESTANCIA_MAYOR_ISLA_G2",show.label = TRUE,ggtheme=theme_bw())+
  scale_fill_viridis_c(option = "D")

# aeropuerto

aeropuerto <- as.data.frame(table(variables_1$VALORACION_MEDIO_AMBIENTE, variables_1$AEROPUERTO))
ggballoonplot(aeropuerto, fill = "value",main ="AEROPUERTO",show.label = TRUE,ggtheme=theme_bw())+
  scale_fill_viridis_c(option = "D")


aeropuerto_2 <- as.data.frame(table(variables_2$VALORACION_MEDIO_AMBIENTE, variables_2$AEROPUERTO))
ggballoonplot(aeropuerto_2, fill = "value",main ="AEROPUERTO",show.label = TRUE,ggtheme=theme_bw())+
  scale_fill_viridis_c(option = "D")


################################################################# LINEAL ################################################################# 

# si tuvieramos que unir ambas muestras, el mejor sitio es este.

################ 1) SELECCION VARIABLES LINEAL #########################
#########################

lm_variables <- muestra_1

################ 2) MODELO LINEAL #########################
#########################

lm_v1 <- lm(data = muestra_1, formula = VALORACION_MEDIO_AMBIENTE ~ .)
summary(lm_v1)

################ 3) STEP #########################
#########################

# A) BOTH STEPWISE

stepAIC(lm_v1, direction="both")

lm_v2 <- lm(formula = VALORACION_MEDIO_AMBIENTE ~ VALORACION_ALOJ + VALORACION_PAISAJES + 
     VALORACION_TRANQUILIDAD + VALORACION_LIMPIEZA + VALORACION_OFERTA_GASTR_LOC + 
     PAIS_RESID_AGRUP + NOCHES_PERNOCTADAS + SEXO, data = muestra_1)
summary(lm_v2)

################ 4) ANALISIS #########################
#########################

# NORMALIDAD

qqPlot(lm_v2, id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

# NORMALIDAD RESIDUOS

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(lm_v2)


# PRUEBA DE NORMALIDAD SAPHIRO - WILK

vResid=resid(lm_v2)

shapiro.test(vResid)

# COMPRUEBO LA LINEALIDAD

crPlots(lm_v2)

# HOMOCEDASTICIDAD

ncvTest(lm_v2)

# VALIDACION GLOBAL

gvmodel <- gvlma(lm_v2)
summary(gvmodel)

# MULTICOLINEALIDAD

# todos los valores estan cercanos a 1, lo que nos indica que que no existe una correlacion relevante entres estas variables.


vif(lm_v2)

# OUTLIERS

outlierTest(lm_v2)

# Cooks Distance D
# identify D values > 4/(n-k-1) 


cutoff <- 4/(length(muestra_1)-length(lm_v2$coefficients)-2)
plot(lm_v2, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


# represento la distancia de cook

influencePlot(lm_v2, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )


# Posibles outliers
outliers <- muestra_1[c(48,67,98,103,140),]

################ 5) PREDICT #########################
#########################

target_test_lm <- muestra_2 %>%
  dplyr::select(VALORACION_MEDIO_AMBIENTE, VALORACION_ALOJ, VALORACION_PAISAJES , 
           VALORACION_TRANQUILIDAD , VALORACION_LIMPIEZA , VALORACION_OFERTA_GASTR_LOC , 
           PAIS_RESID_AGRUP , NOCHES_PERNOCTADAS , SEXO)


predictions <- lm_v2 %>% predict(target_test_lm)

final_predictions <- table(round(predictions,0),target_test_lm$VALORACION_MEDIO_AMBIENTE)


################################################################# LOGISTICA ################################################################# 


################ 1) SELECCION VARIABLES LOGISTICA #########################
#########################

lg_muestra <- cbind(target,muestra_1[,-7]) # sustituimos la variable MEDIO_AMBIENTE por target (MEDIO AMBIENTE binario)


# entorno junto

lg_variables_prueba <- lg_muestra %>%
  mutate(val_alojamiento = (rowSums(.[2:4])/3)) %>%
  mutate(val_entorno = (rowSums(.[c(5,6,8,9,10)])/5)) %>%
  mutate(val_restauracion = (rowSums(.[10:13])/4)) %>%
  dplyr::select(c(PAIS_RESID_AGRUP,SEXO,NOCHES_PERNOCTADAS,val_alojamiento, val_entorno, val_restauracion))  ## seleccionamos variables



# entorno por separado (mejor AIC)
lg_variables <- lg_muestra %>%
  mutate(val_alojamiento = (rowSums(.[2:4])/3)) %>%
  mutate(val_restauracion = (rowSums(.[10:13])/4)) %>%
  dplyr::select(c(target, PAIS_RESID_AGRUP,SEXO,NOCHES_PERNOCTADAS,val_alojamiento, VALORACION_CLIMA:VALORACION_LIMPIEZA, val_restauracion))  ## seleccionamos variables


######### TEST ##########

lg_muestra_test <- cbind(target_test,muestra_2[,-7]) # sustituimos la variable MEDIO_AMBIENTE por target (MEDIO AMBIENTE binario)


# variables lg seleccionadas

lg_variables_test <- lg_muestra_test %>%
  mutate(val_alojamiento = (rowSums(.[2:4])/3)) %>%
  mutate(val_restauracion = (rowSums(.[10:13])/4)) %>%
  dplyr::select(c(target_test, PAIS_RESID_AGRUP,SEXO,NOCHES_PERNOCTADAS,val_alojamiento, VALORACION_CLIMA:VALORACION_LIMPIEZA, val_restauracion))  ## seleccionamos variables


################ 2) LOGISTICA #########################
#########################


lg_V1 <- glm(target ~ .,data = lg_variables, family = "binomial"(link = 'logit'))
summary(lg_V1)

family = "binomial"(link = 'logit')

################ 3) PREDICCION, MC y CURVA ROC #########################
#########################


# PROBABILITY PREDICT

test_predictions <- predict(lg_V1,newdata = lg_variables_test ,type="response")

table(round(test_predictions,1)) # valoraciones predichas por debajo de ocho y por encima de ocho

test_predictions <- ifelse(test_predictions <= 0.5,0,1) # cutoff

# Confusion matrix
confusion<-confusionMatrix(as.factor(target_test),as.factor(test_predictions))

# ROC curve

pred <- prediction(test_predictions, target_test)
perf <- performance(pred, "tpr", "fpr")
roc<-plot(perf, colorize=TRUE)

################ 4) LASSO, RIDGE, ELASTICNET #########################
#########################

# A) Seleccionar método de regularización

lasso <- glmnet(x = train_x, y = train$loan_status, family = "binomial", alpha = 1)

plot(lasso, xvar = "lambda")

# Iteramos sobre alpha y guardamos los resultados para cada uno.

set.seed(123)

train_x<-model.matrix(target ~ .,lg_variables)[, -1]

# Cross validation - numero de muestras (folds)
fold_id <- sample(1:5, size = length(target), replace=TRUE)

# Tibble para la iteracion, probaremos por cada 0.1
tuning_grid <- tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA)

# iteramos alpha

for(i in seq_along(tuning_grid$alpha)) {
  
  # fit CV model for each alpha value
  fit <- cv.glmnet(x=train_x, y = target , family = "binomial", alpha = tuning_grid$alpha[i],type.measure = "deviance", paralle = TRUE,foldid = fold_id)
  
  # extract MSE and lambda values
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

# comparamos errores para cada alpha

tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>%
  ggplot(aes(alpha, mse_min)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = 0.25) +
  ggtitle("MSE ± one standard error")

# alpha igual a 1, aplicamos LASSO

# B) LASSO

# sin cv

lasso <- glmnet(x = train_x, y = target, family = "binomial", alpha = 1)

plot(lasso, xvar = "lambda")

# con cv

lassocv <- cv.glmnet(x = train_x, y = target, family = "binomial", alpha = 1, nfolds = 8)

plot(lassocv, xvar = "lambda")
abline(v = log(lassocv$lambda.1se), col = "red", lty = "dashed")


# Representamos las variables seleccionadas por el modelo

coef(lassocv, s = "lambda.1se") %>%
  broom::tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Influential variables") +
  xlab("Coefficient") +
  ylab(NULL)


################################################################# NO LINEALES ################################################################# 

###################
# Modelo polinomico, un predictor
###################

modelo_poli4 <- lm(muestra_1$VALORACION_MEDIO_AMBIENTE ~ poly(muestra_1$VALORACION_LIMPIEZA, 2), data = muestra_1) # No ajusta bien el cuadrático
summary(modelo_poli4)

predicciones <- predict(modelo_poli4, newdata = muestra_2, se.fit = TRUE,
                        level = 0.95)

final_predictions <- table(round(predicciones$fit,0),muestra_2$VALORACION_MEDIO_AMBIENTE)

###################
# Modelo polinomico logit
###################

lg_poly <- glm(target  ~ poly(val_alojamiento + VALORACION_PAISAJES + 
                                VALORACION_TRANQUILIDAD + VALORACION_LIMPIEZA + val_restauracion + NOCHES_PERNOCTADAS, 2), family = "binomial",
               data = lg_variables)
summary(lg_poly)

predicciones <- predict(lg_poly, newdata = lg_variables_test, se.fit = TRUE,
                        level = 0.95)

table(round(predicciones$fit,1)) # valoraciones predichas por debajo de ocho y por encima de ocho

test_predictions <- ifelse(predicciones$fit <= 0,0,1) # cutoff

# Confusion matrix
confusion<-confusionMatrix(as.factor(target_test),as.factor(test_predictions))
confusion

###################
# Step functions
###################

# VALORACION_LIMPIEZA
modelo_step_fun <- lm(VALORACION_MEDIO_AMBIENTE ~ cut(VALORACION_LIMPIEZA, 4), data = muestra_1)
summary(modelo_step_fun)

predicciones_step <- predict(modelo_step_fun, newdata = muestra_2, se.fit = TRUE,
                             level = 0.95)
# YEAR 

modelo_step_fun <- lm(VALORACION_MEDIO_AMBIENTE ~ cut(as.numeric(YEAR), 4), data = muestra_1)
summary(modelo_step_fun)

muestra_year_test <- muestra_2 %>%
  mutate(YEAR = as.numeric(YEAR))

predicciones_step <- predict(modelo_step_fun, newdata = muestra_year_test, se.fit = TRUE,
                             level = 0.95)



###################
# Splines regression
###################

# LIMPIEZA

modelo_splines <- lm(VALORACION_MEDIO_AMBIENTE ~ bs(VALORACION_LIMPIEZA, knots = c(8), degree = 3),
                     data = muestra_1)

summary(modelo_splines)

predicciones <- predict(modelo_splines, newdata = muestra_2, se.fit = TRUE,
                        level = 0.95)

final_predictions <- table(round(predicciones$fit,0),muestra_2$VALORACION_MEDIO_AMBIENTE)

# PAISAJE

modelo_splines <- lm(VALORACION_MEDIO_AMBIENTE ~ bs(VALORACION_PAISAJES, knots = c(8), degree = 3),
                     data = muestra_1)

summary(modelo_splines)

predicciones <- predict(modelo_splines, newdata = muestra_2, se.fit = TRUE,
                        level = 0.95)

final_predictions <- table(round(predicciones$fit,0),muestra_2$VALORACION_MEDIO_AMBIENTE)

###################
# Smoothing Splines
###################

# VALORACION_LIMPIEZA

modelo_smooth_splines <- smooth.spline(muestra_1$VALORACION_MEDIO_AMBIENTE ~ muestra_1$VALORACION_LIMPIEZA, cv = F)
modelo_smooth_splines$df

modelo_smooth_splines$lambda

modelo_smooth_splines$spar

predicciones <- predict(modelo_smooth_splines, newdata = muestra_2)

plot(x = muestra_1$VALORACION_LIMPIEZA, y = muestra_1$VALORACION_MEDIO_AMBIENTE, pch = 20, col = "darkgrey")
title("Smooth Spline df = 2.48, lambda = 0.31")
lines(x = predicciones$x, predicciones$y, col = "red", lwd = 2)

###################
# GAM lineal
###################

# modelo
# s = smooth splines
m_1 <- gam(VALORACION_MEDIO_AMBIENTE ~  s(VALORACION_LIMPIEZA, 3) + SEXO, data = muestra_1)
m_2 <- gam(VALORACION_MEDIO_AMBIENTE ~ VALORACION_PAISAJES + s(VALORACION_LIMPIEZA, 3) + SEXO, data = muestra_1)
m_3 <- gam(VALORACION_MEDIO_AMBIENTE ~ s(VALORACION_PAISAJES, 2) + s(VALORACION_LIMPIEZA, 3) + SEXO, data = muestra_1)
m_4 <- gam(VALORACION_MEDIO_AMBIENTE ~ s(VALORACION_PAISAJES, 2) + s(VALORACION_LIMPIEZA, 2) + SEXO, data = muestra_1)
m_5 <- gam(VALORACION_MEDIO_AMBIENTE ~ VALORACION_PAISAJES + VALORACION_LIMPIEZA + SEXO, data = muestra_1)
# método anova para objetos de tipo gam
anova(object = m_1, m_2, m_3,m_4,m_5, test = "F")


gam_model <- gam(VALORACION_MEDIO_AMBIENTE ~ VALORACION_PAISAJES + s(VALORACION_LIMPIEZA, 3) + SEXO, data = muestra_1)
summary(gam_model)
plot(gam_model, se = TRUE, col = "red")


###################
# GAM logistico
###################

modelo_gam_logit <- gam(target ~ VALORACION_PAISAJES + s(VALORACION_LIMPIEZA, 3) + s(val_alojamiento,2),
                        family = "binomial", data = lg_variables)
summary(modelo_gam_logit)


plot(modelo_gam_logit, se = TRUE, col = "green")

# Predicción

test_predictions <- predict(modelo_gam_logit,newdata = lg_variables_test ,type="response")

table(round(test_predictions,1)) # valoraciones predichas por debajo de ocho y por encima de ocho

test_predictions <- ifelse(test_predictions <= 0.5,0,1) # cutoff

# Confusion matrix
confusion<-confusionMatrix(as.factor(target_test),as.factor(test_predictions))
confusion$overall


# ROC curve

pred <- prediction(test_predictions, target_test)
perf <- performance(pred, "tpr", "fpr")
roc<-plot(perf, colorize=TRUE)



