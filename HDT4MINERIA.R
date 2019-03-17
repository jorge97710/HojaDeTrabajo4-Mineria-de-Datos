library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
#Modelo de Regresión lineal
porcentaje<-0.7

perros5 <- read.csv("train.csv")

prueba <- perros5[,3:15]
prueba[14] <- perros5[24]


datos<-prueba
set.seed(123)

datos$y<- as.numeric(datos$AdoptionSpeed)
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]


#-------------------------------------------------
# Regresión Lineal Simple 
#-------------------------------------------------


fitLMPW<-lm(AdoptionSpeed~Age, data = train)

#Estimar el lenght del pétalo a partir de su width
#-------
predL<-predict(fitLMPW, newdata = test)
#Verificando la predicción
resultados<-data.frame(test$AdoptionSpeed,predL)
resultados$variacion<-abs(resultados$test.AdoptionSpeed-resultados$predL)
#Sería de determinar cual es el umbral de variación permitido

#Predecir la clase de la flor por la longitud del pétalo
fitLMSpByPL<-lm(y~AdoptionSpeed, data = train)
summary(fitLMSpByPL)
# Multiple R-squared:  0.905,	Adjusted R-squared:  0.9041

#El modelo explica los datos en un 90% la predicción debe ser buena

predMSpByPL<-predict(fitLMSpByPL,newdata = test)
resultados1<-data.frame(test$y,round(predMSpByPL,0))
names(resultados1)<-c("real","prediccion")


confusionMatrix(resultados1$real,resultados1$prediccion)
#Otra Vez me da problemas, intento la misma solucion que use la vez pasada


u <- union(resultados1$real, resultados1$prediccion)
t <- table(factor(resultados1$real, u), factor(resultados1$prediccion, u))
confusionMatrix(t)

#Otra Vez me da problemas, intento la misma solucion que use la vez pasada
#Accuracy : 1

#-------------------------------------------------
# Regresión Lineal Múltiple 
#-------------------------------------------------

fitLM<-lm(AdoptionSpeed~ Age + Breed1 + Breed2 + Gender + Color1 + Color2 + Color3 + MaturitySize + FurLength + Vaccinated + Dewormed + Sterilized + Health , data = train)

summary(fitLM)
#El modelo se ajusta perfectamente a los datos
#Multiple R-squared:      1,	Adjusted R-squared:      1
#Advertencia que pone R:
# Warning message:
#   In summary.lm(fitLM) : essentially perfect fit: summary may be unreliable


predicted<-predict(fitLM,newdata = test)

test$prediccion <- predicted

cfm<-confusionMatrix(test$AdoptionSpeed,test$prediccion)
cfm
#Accuracy : 1 
#HAY SUBREAJUSTE. Esto se debe a que hay multicolinealidad en las variables participantes en el modelo


#--------------------------------HASTA ACA LLEGUE--------------

cor(datos$AdoptionSpeed,datos$Age, method = "spearman")
#La correlación es del 93% con Spearman porque las variables no siguen una distrbución normal
#Esta correlación tan fuerte está interfiriendo en el modelo. 
#Quitar la variable Petal.Width 

#ESTO SE REPITE PARA TODAS 


fitLM1<-lm(y ~ AdoptionSpeed + Age, data = train)

summary(fitLM1)
# Call:
#   lm(formula = y ~ Sepal.Length + Petal.Length, data = train)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.61519 -0.17330  0.01859  0.15534  0.53081 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.92146    0.26913   3.424  0.00089 ***
#   Sepal.Length -0.13138    0.06094  -2.156  0.03344 *  
#   Petal.Length  0.48667    0.02831  17.190  < 2e-16 ***
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2477 on 102 degrees of freedom
# Multiple R-squared:  0.9092,	Adjusted R-squared:  0.9074 
# F-statistic: 510.6 on 2 and 102 DF,  p-value: < 2.2e-16

#Ambos parámetros son significativos por lo que aportan al modelo
#El modelo describe el  90% de los datos por lo que la predicción debe ser buena

pred<-predict(fitLM1,newdata = test)
test$prediccionModeloAjustado<-round(pred,0)

cfm1<-confusionMatrix(test$AdoptionSpeed, test$prediccionModeloAjustado)
#Accuracy : 0.9333 


#Otra Vez me da problemas, intento la misma solucion que use la vez pasada


a <- union(test$AdoptionSpeed, test$prediccionModeloAjustado)
s <- table(factor(test$AdoptionSpeed, a), factor(test$prediccionModeloAjustado, a))
confusionMatrix(s)

#Otra Vez me da problemas, intento la misma solucion que use la vez pasada
