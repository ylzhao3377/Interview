library(car)
library(faraway)
library(glmnet)
mydata <- savings
head(mydata)
pairs(mydata)

fit <- lm(sr~., data=mydata)
summary(fit)
apply(is.na(mydata),2,sum)

#######  Diagnostics  ############
rstudent(fit)
vif(fit)
outlierTest(fit)
qqPlot(fit, main = "QQ Plot")
leveragePlots(fit)
plot(fit, which = 4, cook.levels=1)
influencePlot(fit,	id.method="identify")
dfbetaPlots(fit)
fit.AIC <- step(lm(sr ~ 1, data = mydata), scope = list(upper = fit, lower = ~1), 
                direction = 'forward')
fit.lasso <- cv.glmnet(as.matrix(mydata[,-1]),mydata[,1], alpha = 1)
best.lambda <- fit.lasso$lambda.min
lasso.predict <- predict(fit.lasso, newx = as.matrix(mydata[,-1]), 
                                s =best.lambda, type = "response")
lasso.coef <- predict(fit.lasso,newx = as.matrix(mydata[,-1]), 
                      s =best.lambda, type = "coefficients")
lasso.coef
plot(fit.lasso)

confint.lm(fit.AIC)
x <- data.frame(t(apply(mydata[,-1], 2, mean)))
fitted(fit.AIC)
residuals(fit.AIC)
anova(fit.AIC)

predict.lm(fit,x,interval="confidence")
predict.lm(fit,x,interval="prediction")

