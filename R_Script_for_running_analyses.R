veg_comm <- read.csv("Laurynas_vegetation_data.csv", h = T, sep = ",", row.names = c(1) ,stringsAsFactors = FALSE)
veg_comm$Site <- as.factor(veg_comm$Site) # changed the site name to a factor
veg_comm$Year <- as.factor(veg_comm$Year) # changed the year name to a factor

plot(veg_comm$Year, veg_comm$Linaria_loeselii_rare)
plot(veg_comm$Alien_sp_comb_. ~ veg_comm$native_sp_comb_.)
plot(veg_comm$Linaria_loeselii_rare ~ veg_comm$native_sp_comb_.)

lm1 <- lm(Linaria_loeselii_rare ~ 
            Agropyron_dasyanthum_alien +
            Corispermum_leptopterum_alien +
            Corispermum_palassii_alien +
            Gypsophila_paniculata_alien +
            Year, data = veg_comm)
summary(lm1)

install.packages("lme4")
library(lme4)

lmer1 <- lmer(Linaria_loeselii_rare ~ 
                Agropyron_dasyanthum_alien +
                Corispermum_leptopterum_alien +
                Corispermum_palassii_alien +
                Gypsophila_paniculata_alien +
                (Year | Site), data = veg_comm)
summary(lmer1)
