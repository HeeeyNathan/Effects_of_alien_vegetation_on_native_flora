veg_comm <- read.csv("Laurynas_vegetation_data.csv", h = T, sep = ",", row.names = c(1) ,stringsAsFactors = FALSE)

plot(veg_comm$year, veg_comm$Linaria_loeselii_rare)
plot(veg_comm$Alien_sp_comb_. ~ veg_comm$native_sp_comb_.)
plot(veg_comm$Linaria_loeselii_rare ~ veg_comm$native_sp_comb_.)

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
