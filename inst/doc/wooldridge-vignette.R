## ---- echo = TRUE, eval = TRUE, warning=FALSE----------------------------
library(wooldridge)

## ---- echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE----------------
library(stargazer)

## ------------------------------------------------------------------------
data(wage1)

## ------------------------------------------------------------------------
log_wage_model <- lm(lwage ~ educ, data = wage1)

## ---- results = 'asis', warning=FALSE, message=FALSE---------------------
stargazer(log_wage_model, single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
hourly_wage_model <- lm(lwage ~ educ + exper + tenure, data = wage1)

## ---- results = 'asis', warning=FALSE, message=FALSE---------------------
stargazer(hourly_wage_model,  single.row = TRUE, header = FALSE)

## ---- echo = TRUE, eval = TRUE, warning=FALSE----------------------------
data("jtrain")

## ------------------------------------------------------------------------
index <- jtrain$year == 1987 & jtrain$union == 0

## ------------------------------------------------------------------------
jtrain_1987_nonunion <- jtrain[index,]

## ------------------------------------------------------------------------
linear_model <- lm(lscrap ~ hrsemp + lsales + lemploy, data = jtrain_1987_nonunion)

## ---- results = 'asis', warning=FALSE, message=FALSE---------------------
stargazer(linear_model,  single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
data(crime1)

## ---- tidy = TRUE--------------------------------------------------------
restricted_model <- lm(narr86 ~ pcnv + ptime86 + qemp86, data = crime1)

## ------------------------------------------------------------------------
restricted_model_u <- restricted_model$residuals

## ---- tidy = TRUE--------------------------------------------------------
LM_u_model <- lm(restricted_model_u ~ pcnv + ptime86 + qemp86 + avgsen + tottime, data = crime1)

summary(LM_u_model)$r.square

## ------------------------------------------------------------------------
LM_test <- nobs(LM_u_model) * 0.0015
LM_test

## ------------------------------------------------------------------------
qchisq(1 - 0.10, 2)

## ------------------------------------------------------------------------
1-pchisq(LM_test, 2)

## ---- tidy = TRUE--------------------------------------------------------
housing_standard <- lm(scale(price)~0+scale(nox)+scale(crime)+scale(rooms)+scale(dist) + scale(stratio), data = hprice2)

## ---- results = 'asis', warning=FALSE, message=FALSE---------------------
stargazer(housing_standard,  single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
housing_interactive <- lm(lprice ~ lnox + log(dist) + rooms+I(rooms^2) + stratio, data = hprice2)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(housing_standard,  echo=FALSE, housing_interactive, single.row = TRUE, header = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  ?hprice1

## ---- tidy=TRUE----------------------------------------------------------
housing_qualitative <- lm(lprice ~ llotsize + lsqrft + bdrms + colonial, data = hprice1)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(housing_qualitative,  single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
data("gpa1")
gpa1$parcoll <- as.integer(gpa1$fathcoll==1 | gpa1$mothcoll)

## ------------------------------------------------------------------------
GPA_OLS <- lm(PC ~ hsGPA + ACT + parcoll, data = gpa1)

## ------------------------------------------------------------------------
weights <- GPA_OLS$fitted.values * (1-GPA_OLS$fitted.values)

GPA_WLS <- lm(PC ~ hsGPA + ACT + parcoll, data = gpa1, weights = 1/weights)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(GPA_OLS, GPA_WLS,  single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
data(rdchem)
 
all_rdchem <- lm(rdintens ~ sales + profmarg, data = rdchem)

## ---- tidy=TRUE----------------------------------------------------------
plot(rdintens ~ sales, pch = 21, bg = "lightblue", data = rdchem,
     main = "FIGURE 9.1: Scatterplot of R&D intensity against firm sales",
     xlab = "firm sales (in millions of dollars)",
     ylab = "R&D as a percentage of sales")

## ------------------------------------------------------------------------
smallest_rdchem <- lm(rdintens ~ sales + profmarg, data = rdchem, 
                      subset = (sales < max(sales)))

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(all_rdchem, smallest_rdchem,  single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
data("intdef")

tbill_model <- lm(i3 ~ inf + def, data = intdef)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(tbill_model, single.row = TRUE, header = FALSE)

## ---- tidy=TRUE----------------------------------------------------------
data("barium")
barium_imports <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, data = barium)

## ---- tidy=TRUE----------------------------------------------------------
barium_seasonal <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6 + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec, data = barium)

barium_anova <- anova(barium_imports, barium_seasonal)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(barium_imports, barium_seasonal,  single.row = TRUE, header = FALSE)

stargazer(barium_anova,  single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
data("earns")

wage_time <- lm(lhrwage ~ loutphr + t, data = earns)

## ------------------------------------------------------------------------
wage_diff <- lm(diff(lhrwage) ~ diff(loutphr), data = earns)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(wage_time, wage_diff,  single.row = TRUE, header = FALSE)

## ---- tidy=TRUE----------------------------------------------------------
data("barium")
barium_model <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, data = barium)

## ---- tidy=TRUE----------------------------------------------------------
library(prais)
barium_prais_winsten <- prais.winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, data = barium)

## ------------------------------------------------------------------------
barium_model
barium_prais_winsten

## ------------------------------------------------------------------------
data("nyse")
 
return_AR1 <-lm(return ~ return_1, data = nyse)

## ------------------------------------------------------------------------
return_mu <- residuals(return_AR1)

mu2_hat_model <- lm(return_mu^2 ~ return_1, data = return_AR1$model)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(return_AR1, mu2_hat_model,  single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
mu2_hat  <- return_mu[-1]^2

mu2_hat_1 <- return_mu[-NROW(return_mu)]^2

arch_model <- lm(mu2_hat ~ mu2_hat_1)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(arch_model, single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
data("traffic1")
DD_model <- lm(cdthrte ~ copen + cadmn, data = traffic1)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(DD_model,  single.row = TRUE, header = FALSE)

## ---- tidy=TRUE----------------------------------------------------------
library(plm)
data("jtrain")
scrap_panel <- plm(lscrap ~ d88 + d89 + grant + grant_1, data = jtrain,
            index = c('fcode','year'), model = 'within', effect ='individual')

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(scrap_panel,  single.row = TRUE, header = FALSE)

## ---- message=FALSE------------------------------------------------------
data("mroz")
wage_educ_model <- lm(lwage ~ educ, data = mroz)

## ------------------------------------------------------------------------
fatheduc_model <- lm(educ ~ fatheduc, data = mroz, subset = (inlf==1))

## ---- message=FALSE------------------------------------------------------
library("AER")
wage_educ_IV <- ivreg(lwage ~ educ | fatheduc, data = mroz)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(wage_educ_model, fatheduc_model, wage_educ_IV, single.row = TRUE, header = FALSE)

## ---- warning=FALSE------------------------------------------------------
data("wage2")
 
educ_sibs_model <- lm(educ ~ sibs, data = wage2)

## ---- message=FALSE------------------------------------------------------
library("AER")

educ_sibs_IV <- ivreg(lwage ~ educ | sibs, data = wage2)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(educ_sibs_model, educ_sibs_IV, wage_educ_IV,  single.row = TRUE, header = FALSE)

## ---- tidy=TRUE----------------------------------------------------------
data("mroz")
wage_educ_exper_IV <- ivreg(lwage ~ educ + exper + expersq | exper + expersq + motheduc + fatheduc, data = mroz)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE, echo=FALSE----
stargazer(wage_educ_exper_IV,  single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
data("openness")
 
open_model <-lm(open ~ lpcinc + lland, data = openness)

## ------------------------------------------------------------------------
library(AER)

inflation_IV <- ivreg(inf ~ open + lpcinc | lpcinc + lland, data = openness)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(open_model, inflation_IV,  single.row = TRUE, header = FALSE)

## ---- tidy=TRUE, warning=FALSE-------------------------------------------
data("crime1")

formula <- (narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 + inc86 + black + hispan + born60)

econ_crime_model <- lm(formula, data = crime1)

econ_crim_poisson <- glm(formula, data = crime1, family=poisson)

## ---- results = 'asis', warning=FALSE, message=FALSE, tidy=TRUE----------
stargazer(econ_crime_model, econ_crim_poisson,  single.row = TRUE, header = FALSE)

## ------------------------------------------------------------------------
data("phillips")

unem_AR1 <- lm(unem ~ unem_1, data = phillips, subset = (year <= 1996))

unem_inf_VAR1 <- lm(unem ~ unem_1 + inf_1, data = phillips, subset = (year <= 1996))

## ---- results = 'asis', warning=FALSE, message=FALSE, echo=FALSE---------
stargazer(unem_AR1, unem_inf_VAR1,  single.row = TRUE, header = FALSE)

