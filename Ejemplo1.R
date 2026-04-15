

### ejemplo 1 plsglm.
### 15 abril 2026

library(chemometrics)
library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(car)
library(e1071)
library(xtable)
library(plsRglm)
library(MASS)
library(FactoMineR)
library(factoextra)

data("Pima.tr")

summary(Pima.tr)

table(Pima.tr$type)/200

resPCA = PCA(Pima.tr[,-8])
fviz_pca_biplot(resPCA, habillage = Pima.tr$type)

### regresion logistica

modGLM <- glm(type~., 
              family = binomial(link = "logit"),
              data=Pima.tr)

modGLM$aic
BIC(modGLM)
car::vif(modGLM)

summary(modGLM)

y <- ifelse(Pima.tr$type == "Yes", 1, 0)
yAjustado.GLM  <- ifelse(modGLM$fitted.values > 0.5,1,0)

## matriz de confusión.

ww1 <- table(y, yAjustado.GLM)

sum(diag(ww1))/sum(ww1) ## porcentaje de clasificacion ok.

### PLS-GLM.

y <- ifelse(Pima.tr$type == "Yes", 1, 0)
X <- Pima.tr[,-8]

modPLS.Binom <- plsRglm(y,X, 3,modele="pls-glm-logistic")
modPLS.Binom$CoeffC

modPLS.Binom$AIC
modPLS.Binom$BIC

### Determinación de H.

aa <- seq(1,7)
AIC1 <- numeric(7)
BIC1 <- numeric(7)

for(i in 1:length(aa)){
  
  modPLS.gamma <- plsRglm(y,X, aa[i],
                          modele="pls-glm-logistic")
  AIC1 = modPLS.gamma$AIC
  BIC1 = modPLS.gamma$BIC
}

par(mfrow=c(1,2))
plot(aa, AIC1[,-8],ylab="AIC", xlab="# Components",
     pch=18,type="b",
     col="blue")
plot(aa, BIC1[,-8],ylab="BIC", xlab="# Components",
     pch=18,type="b",col="blue")



### matriz de confusion PLS-GLM

yAjustado  <- ifelse(modPLS.Binom$YChapeau > 0.5,1,0)

ww <- table(y,yAjustado) ## matriz de confusion

sum(diag(ww))/sum(ww) ## porcentaje de clasificacion ok.


