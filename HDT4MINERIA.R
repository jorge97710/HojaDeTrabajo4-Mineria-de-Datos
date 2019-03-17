library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n?mero de clusters ?ptimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
#Modelo de Regresi?n lineal
porcentaje<-0.7

perros5 <- read.csv("train.csv")

prueba <- perros5[,3:15]
prueba[14] <- perros5[24]


datos<-prueba
set.seed(123)

datos$y<- datos$AdoptionSpeed
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]


#-------------------------------------------------
# Regresi?n Lineal Simple 
#-------------------------------------------------


fitLMPW<-lm(y~Age, data = train)

#Estimar el lenght del p?talo a partir de su width
#-------
predL<-predict(fitLMPW, newdata = test)
#Verificando la predicci?n
resultados<-data.frame(test$AdoptionSpeed,predL)
resultados$variacion<-abs(resultados$test.AdoptionSpeed-resultados$predL)
#Ser?a de determinar cual es el umbral de variaci?n permitido

#Predecir la clase de la flor por la longitud del p?talo
fitLMSpByPL<-lm(y~AdoptionSpeed, data = train)
summary(fitLMSpByPL)
# Multiple R-squared:  0.905,	Adjusted R-squared:  0.9041

#El modelo explica los datos en un 90% la predicci?n debe ser buena

predMSpByPL<-predict(fitLMSpByPL,newdata = test)
resultados1<-data.frame(test$y,round(predMSpByPL,0))
names(resultados1)<-c("real","prediccion")


confusionMatrix(as.factor(resultados1$real), as.factor(resultados1$prediccion))
#Otra Vez me da problemas, intento la misma solucion que use la vez pasada


u <- union(resultados1$real, resultados1$prediccion)
t <- table(factor(resultados1$real, u), factor(resultados1$prediccion, u))
confusionMatrix(t)

#Otra Vez me da problemas, intento la misma solucion que use la vez pasada
#Accuracy : 1

#-------------------------------------------------
# Regresi?n Lineal M?ltiple 
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

#ESTO NO ME SALE
cfm<-confusionMatrix( as.factor(test$AdoptionSpeed), as.factor(test$prediccion))
cfm
#ESTO NO ME SALE
#Accuracy : 1 
#HAY SUBREAJUSTE. Esto se debe a que hay multicolinealidad en las variables participantes en el modelo



cor(datos$AdoptionSpeed,datos$Age, method = "spearman")
#La correlaci?n es del 93% con Spearman porque las variables no siguen una distrbuci?n normal
#Esta correlaci?n tan fuerte est? interfiriendo en el modelo. 
#Quitar la variable Petal.Width 

#ESTO SE REPITE PARA TODAS 


fitLM1<-lm(y ~ Sterilized + Age, data = train)

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

#Ambos par?metros son significativos por lo que aportan al modelo
#El modelo describe el  90% de los datos por lo que la predicci?n debe ser buena

pred<-predict(fitLM1,newdata = test)
test$prediccionModeloAjustado<-round(pred,0)

cfm1<-confusionMatrix( as.factor(test$AdoptionSpeed), as.factor(test$prediccionModeloAjustado))
#Accuracy : 0.9333 


#Otra Vez me da problemas, intento la misma solucion que use la vez pasada


a <- union(test$AdoptionSpeed, test$prediccionModeloAjustado)
s <- table(factor(test$AdoptionSpeed, a), factor(test$prediccionModeloAjustado, a))
confusionMatrix(s)

#Otra Vez me da problemas, intento la misma solucion que use la vez pasada
#Breed1 
cor(datos$AdoptionSpeed,datos$Breed1, method = "spearman")
#Breed2
cor(datos$AdoptionSpeed,datos$Breed2, method = "spearman")
cor(datos$AdoptionSpeed,datos$Breed2, method = "spearman")
cor(datos$AdoptionSpeed,datos$Gender, method = "spearman")
cor(datos$AdoptionSpeed,datos$Color1, method = "spearman")
cor(datos$AdoptionSpeed,datos$Color2, method = "spearman")
cor(datos$AdoptionSpeed,datos$Color3, method = "spearman")
cor(datos$AdoptionSpeed,datos$MaturitySize, method = "spearman")
cor(datos$AdoptionSpeed,datos$FurLength, method = "spearman")
cor(datos$AdoptionSpeed,datos$Vaccinated, method = "spearman")
cor(datos$AdoptionSpeed,datos$Dewormed, method = "spearman")
cor(datos$AdoptionSpeed,datos$Sterilized, method = "spearman")
cor(datos$AdoptionSpeed,datos$Health, method = "spearman")
cor(datos$AdoptionSpeed,datos$y, method = "spearman") 