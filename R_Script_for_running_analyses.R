### Read in data
veg_comm <- read.csv("Laurynas_vegetation_data.csv", h = T, sep = ",", row.names = c(1) ,stringsAsFactors = FALSE)

### Check data distribution
library(psych)
library(rcompanion)

#Test bitoic data for normality - ORIGINAL DATA USED
#Tabund
boxplot(veg_comm$Linaria_loeselii_rare)
boxplot(veg_comm$Linaria_loeselii_rare ~ veg_comm$year)
plotNormalHistogram(veg_comm$Linaria_loeselii_rare)
plotNormalDensity(veg_comm$Linaria_loeselii_rare)
qqnorm(veg_comm$Linaria_loeselii_rare, ylab = "Sample Quantiles for Linaria loeselii")
qqline(veg_comm$Linaria_loeselii_rare, col = "red")
shapiro.test(veg_comm$Linaria_loeselii_rare) # data are highly skewed

source("HighstatLibV10.R") # pair plots as per Zuur et al. (2018)
# Check env data for collinearity
pairs(veg_comm[ , c(7, 5, 36:39)], lower.panel = panel.smooth, upper.panel = panel.cor, 
      diag.panel = panel.hist, main = "Pearson Correlation Matrix")
# Check for big correlations
Corrs <- cor(veg_comm[ , c(7, 5, 36:39)], method = "spearman")
BigCorrs <- which(Corrs > 0.7 & Corrs < 1, arr.ind = TRUE)
# pairs(ger_env_sel[ , unique(rownames(ger_BigCorrs))], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = "Pearson Correlation Matrix")

# Basic linear model (regression)
lm1 <- lm(Linaria_loeselii_rare ~ 
            Agropyron_dasyanthum_alien +
            Corispermum_leptopterum_alien +
            Corispermum_palassii_alien +
            Gypsophila_paniculata_alien +
            year_code, data = veg_comm)
summary(lm1)

# Mixed effect model
library(lme4) # mixed effects model
library(MuMIn) # dredge function

lmer1 <- lmer(Linaria_loeselii_rare ~ 
                Agropyron_dasyanthum_alien +
                Corispermum_leptopterum_alien +
                Corispermum_palassii_alien +
                Gypsophila_paniculata_alien +
                year_code + (1|site_transect), data = veg_com)
summary(lmer1)

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
