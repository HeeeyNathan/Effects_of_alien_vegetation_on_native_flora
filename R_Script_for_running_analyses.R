### Read in data
veg_comm <- read.csv("Laurynas_vegetation_data.csv", h = T, sep = ",", row.names = c(1) ,stringsAsFactors = FALSE)
veg_comm_simp <- read.csv("Laurynas_vegetation_data_simplified.csv", h = T, sep = ",", row.names = c(1) ,stringsAsFactors = FALSE)

### Check data distribution (normality)
library(psych)
library(rcompanion)
# Linaria loeselii
dotchart(veg_comm_simp$Linaria_loeselii_rare,
         ylab = "site", xlab = "percentage in transect", 
         main = "Cleveland dotplot")
boxplot(veg_comm_simp$Linaria_loeselii_rare)
boxplot(veg_comm_simp$Linaria_loeselii_rare ~ veg_comm_simp$year)
plotNormalHistogram(veg_comm_simp$Linaria_loeselii_rare)
plotNormalDensity(veg_comm_simp$Linaria_loeselii_rare)
qqnorm(veg_comm_simp$Linaria_loeselii_rare, ylab = "Sample Quantiles for Linaria loeselii")
qqline(veg_comm_simp$Linaria_loeselii_rare, col = "red")
shapiro.test(veg_comm_simp$Linaria_loeselii_rare) # data are highly skewed
# Agropyron dasyanthum
dotchart(veg_comm_simp$Agropyron_dasyanthum_alien,
         ylab = "Site", xlab = "percentage in transect", 
         main = "Cleveland dotplot")
boxplot(veg_comm_simp$Agropyron_dasyanthum_alien)
boxplot(veg_comm_simp$Agropyron_dasyanthum_alien ~ veg_comm_simp$year)
plotNormalHistogram(veg_comm_simp$Agropyron_dasyanthum_alien)
plotNormalDensity(veg_comm_simp$Agropyron_dasyanthum_alien)
qqnorm(veg_comm_simp$Agropyron_dasyanthum_alien, ylab = "Sample Quantiles for Agropyron dasyanthum")
qqline(veg_comm_simp$Agropyron_dasyanthum_alien, col = "red")
shapiro.test(veg_comm_simp$Agropyron_dasyanthum_alien) # data are highly skewed
# Corispermum leptopterum
dotchart(veg_comm_simp$Corispermum_leptopterum_alien,
         ylab = "Site", xlab = "percentage in transect", 
         main = "Cleveland dotplot")
boxplot(veg_comm_simp$Corispermum_leptopterum_alien)
boxplot(veg_comm_simp$Corispermum_leptopterum_alien ~ veg_comm_simp$year)
plotNormalHistogram(veg_comm_simp$Corispermum_leptopterum_alien)
plotNormalDensity(veg_comm_simp$Corispermum_leptopterum_alien)
qqnorm(veg_comm_simp$Corispermum_leptopterum_alien, ylab = "Sample Quantiles for Corispermum leptopterum")
qqline(veg_comm_simp$Corispermum_leptopterum_alien, col = "red")
shapiro.test(veg_comm_simp$Corispermum_leptopterum_alien) # data are highly skewed
# Corispermum palassii
dotchart(veg_comm_simp$Corispermum_palassii_alien,
         ylab = "Year", xlab = "percentage in transect", 
         main = "Cleveland dotplot")
boxplot(veg_comm_simp$Corispermum_palassii_alien)
boxplot(veg_comm_simp$Corispermum_palassii_alien ~ veg_comm_simp$year)
plotNormalHistogram(veg_comm_simp$Corispermum_palassii_alien)
plotNormalDensity(veg_comm_simp$Corispermum_palassii_alien)
qqnorm(veg_comm_simp$Corispermum_palassii_alien, ylab = "Sample Quantiles for Corispermum palassii")
qqline(veg_comm_simp$Corispermum_palassii_alien, col = "red")
shapiro.test(veg_comm_simp$Corispermum_palassii_alien) # data are highly skewed
# Gypsophila paniculata
dotchart(veg_comm_simp$Gypsophila_paniculata_alien,
         ylab = "Year", xlab = "percentage in transect", 
         main = "Cleveland dotplot")
boxplot(veg_comm_simp$Gypsophila_paniculata_alien)
boxplot(veg_comm_simp$Gypsophila_paniculata_alien ~ veg_comm_simp$year)
plotNormalHistogram(veg_comm_simp$Gypsophila_paniculata_alien)
plotNormalDensity(veg_comm_simp$Gypsophila_paniculata_alien)
qqnorm(veg_comm_simp$Gypsophila_paniculata_alien, ylab = "Sample Quantiles for Gypsophila paniculata")
qqline(veg_comm_simp$Gypsophila_paniculata_alien, col = "red")
shapiro.test(veg_comm_simp$Gypsophila_paniculata_alien) # data are highly skewed

### Check env data for collinearity
source("HighstatLibV10.R") # pair plots as per Zuur et al. (2018)
pairs(veg_comm_simp[ , c(4, 37:39)], lower.panel = panel.smooth, upper.panel = panel.cor, 
      diag.panel = panel.hist, main = "Pearson Correlation Matrix")
# Check for big correlations
Corrs <- cor(veg_comm_simp[ , c(4, 37:39)], method = "spearman")
BigCorrs <- which(Corrs > 0.7 & Corrs < 1, arr.ind = TRUE)
# pairs(Corrs[ , unique(rownames(Corrs))], lower.panel = panel.smooth, upper.panel = panel.cor, 
#       diag.panel = panel.hist, main = "Pearson Correlation Matrix")

### Quick and dirty models
## Basic linear model (regression)
lm1 <- lm(Linaria_loeselii_rare ~ 
            Agropyron_dasyanthum_alien +
            Corispermum_leptopterum_alien +
            Corispermum_palassii_alien +
            Gypsophila_paniculata_alien +
            year, data = veg_comm_simp)
summary(lm1)

lm2 <- lm(Linaria_loeselii_rare ~ 
            alien_sp_comb +
            native_sp_comb +
            native_rare_sp_comb, data = veg_comm_simp)
summary(lm2)

plot(veg_comm_simp$Linaria_loeselii_rare ~ veg_comm_simp$alien_sp_comb)
plot(veg_comm_simp$Linaria_loeselii_rare ~ veg_comm_simp$native_sp_comb)
plot(veg_comm_simp$Linaria_loeselii_rare ~ veg_comm_simp$native_rare_sp_comb)

library("car")
scatterplot(Linaria_loeselii_rare ~ alien_sp_comb, data = veg_comm_simp)
scatterplot(Linaria_loeselii_rare ~ native_sp_comb, data = veg_comm_simp)
scatterplot(Linaria_loeselii_rare ~ native_rare_sp_comb, data = veg_comm_simp)

summary(lm3 <- lm(Linaria_loeselii_rare ~ alien_sp_comb, data = veg_comm_simp))
summary(lm4 <- lm(Linaria_loeselii_rare ~ native_sp_comb, data = veg_comm_simp))
summary(lm4 <- lm(Linaria_loeselii_rare ~ native_rare_sp_comb, data = veg_comm_simp))

## Mixed effect model
library(lme4) # mixed effects model
library(MuMIn) # dredge function
lmer1 <- lmer(Linaria_loeselii_rare ~ 
                Agropyron_dasyanthum_alien +
                Corispermum_leptopterum_alien +
                Corispermum_palassii_alien +
                Gypsophila_paniculata_alien +
                year + (1|site), data = veg_comm_simp)
summary(lmer1)

lmer2 <- lmer(Linaria_loeselii_rare ~ 
                alien_sp_comb +
                native_sp_comb +
                moss_lichen_comb
                (1|site), data = veg_comm_simp)
summary(lmer2)

lmer2_E = resid(lmer2, type = "response")
lmer2_Fit = fitted(lmer2)
op = par(mfrow = c(2, 2))
plot(x = lmer2_Fit, y = lmer2_E,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Linaria_loeselii_rare residuals vs fitted values")
hist(lmer2_E, nclass = 15)
qqnorm(lmer2_E)
qqline(lmer2_E)
acf(lmer2_E)
par(op)
#Step10: Present the relevant summary of the final model
summary(lmer2)
#plot explanatory variables vs residuals
plot(veg_comm_simp$alien_sp_comb, lmer2_E, xlab = "alien_sp_comb", ylab = "Residuals")
plot(veg_comm_simp$native_sp_comb, lmer2_E, xlab = "native_sp_comb", ylab = "Residuals")
plot(veg_comm_simp$moss_lichen_comb, lmer2_E, xlab = "moss_lichen_comb", ylab = "Residuals")
#create graph of the model fit
op = par(mfrow = c(1, 1))
plot(predict(lmer2) ~ alien_sp_comb, data = veg_comm_simp, xlab = "alien_sp_comb", 
     ylab = "Linaria_loeselii_rare", main = "Model of the model")
abline(lm(Linaria_loeselii_rare ~ alien_sp_comb, data = veg_comm_simp), col = "blue")
plot(predict(lmer2) ~ native_sp_comb, data = veg_comm_simp, xlab = "native_sp_comb", 
     ylab = "Linaria_loeselii_rare", main = "Model of the model")
plot(predict(lmer2) ~ moss_lichen_comb, data = veg_comm_simp, xlab = "moss_lichen_comb", 
     ylab = "Linaria_loeselii_rare", main = "Model of the model")
par(op)





# Example from Burnham and Anderson (2002), page 100:
# prevent fitting sub-models to different datasets
options(na.action = "na.fail")
lmer1_dd <- dredge(lmer1) #checks all possible models, if the model is complex, this may take awhile
lmer1_dd #get "w" weights from here (full model list) not next line (the subset)
(lmer1_dd_red <- subset(lmer1_dd, delta < 2)) #look only at models with delta AIC <2
# Model average models with delta AICc < 2
model.avg(lmer1_dd, subset = delta < 2)
# or as a 95% confidence set:
model.avg(lmer1_dd, subset = cumsum(weight) <= .95)
# get averaged coefficients
#'Best model
summary(get.models(lmer1_dd, 1)[[1]]) #change the first "1" to look at second model (2), third model (3) etc
anova(get.models(lmer1_dd, 1)[[1]])
# RIVs- Relative Importance Values- another model averaging approach but to get one value for driver variables
importance(lmer1_dd)
# Get model R sq
r.squaredGLMM(get.models(lmer1_dd, 1)[[1]]) #gives marginal(m) and conditional(c) R^2
# marginal is proportion variance explained by fixed factors alone
# conditional is proportion explained by both fixed and random factors

## Mixed effect model for nested data
# test
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE, dec = ".")
# Step 1 - linear model on the data from each beach
Beta <- vector(length = 9)
for(i in 1:9){
  Mi <- summary(lm(Richness ~ NAP,
                   subset = (Beach == i), data = RIKZ))
  Beta[i] <- Mi$coefficients[2, 1]
}
Beta # view the output

## Same as above, but clear code
install.packages("nlme")
library(nlme)
# Step 1 - linear model on the data from each beach
Beta1 <- lmList(Richness ~ NAP | Beach, data = RIKZ)
summary(Beta1)
# standardized residuals versus fitted values by beach
plot(Beta1, resid(., type = "pool") ~ fitted(.) | Beach, abline = 0, id = 0.05)
# box-plots of residuals by Beach
plot(Beta1, Beach ~ resid(.))
# observed versus fitted values by Beach
plot(Beta1, Richness ~ fitted(.) | Beach, abline = c(0,1))

# Step 2 - estimates from step 1 are modelled as a function of exposure

