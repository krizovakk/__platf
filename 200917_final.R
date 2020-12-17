# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("corrgram")

require(tidyverse)
require(readxl)
require(reshape2)
require(corrgram)

rplatf <- read_excel("data/200416_final.xlsx") #rapeseed, 16th April 2020; platform+spad results; n=948
rlab <-  read_excel("data/200416_lab.xls") #rapeseed, 16th April 2020; lab results; n=90

# final table -------------------------------------------------------------

rplatf <- rplatf %>% 
  mutate(spadeq = (99*rplatf$spad)/(144-rplatf$spad)) # SPAD value equation -> chlor. content
rplatf$i <- paste(rplatf$var, rplatf$plant, rplatf$leaf, sep = "")
rplatfag <- aggregate(rplatf[, 1:39], list(rplatf$i), FUN = median) # 5:39 <-- agg 5th to 39th column
colnames(rplatfag)[1] <- "id"

rmer <- merge(rplatfag, rlab, by = "id") # merged table for rapeseed, n=90
# rmer <- merge(rplatfag, rlab, by.x = "id", by.y = "id") # merged table for rapeseed, n=90
rmer <- rmer %>%
  select(id, spad, spadeq, everything()) # moves the "spadeq" column to the front

# CORR for PLATF and SPAD data only------------------------------------------------------

#install.packages("corrplot")
# install.packages("RColorBrewer")

require(corrplot)
require(RColorBrewer)

rcorm_sel <- rmer %>% 
  select(spad, R, G, B, NRI, NGI, NBI, hue, saturation, brightness, Y, Cb, Cr, 
         GMR, GDR, GDB, RDB, VI, `(R-B)/(R+B)`, DGCI, ExG, ExR, `ExG-ExR`) #selected indices

colnames(rcorm_sel)[1] <- "SPAD"
colnames(rcorm_sel)[18] <- "NDI" #rename collumns only for visualization in corrplot

cm <- cor(rcorm_sel) # creates correlation matrix from rcorm_sel dataframe

res1 <- cor.mtest(rcorm_sel, conf.level = .95) # significance test, to add signif. labels into a plot

# corrplot(cm, p.mat = res1$p, method = "color" , type = "upper",
#          sig.level = c(.001, .01, .05), pch.cex = .9,
#          insig = "label_sig", pch.col = "white", order = "original") #funkcni

corrplot(cm, p.mat = res1$p, method = "color" , type = "upper",
         sig.level = c(.001, .01, .05), pch.cex = .8,
         insig = "label_sig", pch.col = "white", order = "original", tl.col = "black", tl.srt = 60, 
         col = brewer.pal(n = 10, name = "PiYG")) #col = gray.colors(8), #PRGn

# corrplot(cm, p.mat = res1$p, insig = "label_sig",
#          sig.level = c(.001, .01, .05), pch.cex = .7, pch.col = "white", type = "lower")

# corrplot(cm, p.mat = res1$p, method = "color",
#          insig = "label_sig", pch.col = "black", pch.cex = .9, type = "upper", col = gray.colors(4), tl.col = "black")

corrgram(cm, lower.panel=panel.conf, upper.panel=NULL) # corelogram based on p-value and confidence intervals only

# CORR siginificant -------------------------------------------------------

sig <- rmer %>% 
  select(spad, R, G, NRI, NGI, hue, brightness, Y, Cb, Cr, 
          GMR, GDR, GDB, RDB, VI, ExG, ExR, `ExG-ExR`)

colnames(sig)[1] <- "SPAD"
colnames(sig)[7] <- "bright"

  
cm_sig <- cor(sig)

corrgram(cm_sig, lower.panel=panel.conf, upper.panel=NULL, cex.labels = 1.4, font.labels = 3) #numbers

res1b <- cor.mtest(cm_sig, conf.level = .95) # significance test, to add signif. labels into a plot

corrplot(cm_sig, p.mat = res1b$p, method = "color" , type = "upper", 
         sig.level = c(.001, .01, .05), pch.cex = 1,
         insig = "label_sig", pch.col = "red", order = "original", col = gray.colors(4), tl.col = "black", tl.srt = 60) #shades

# corrplot(cm_sig, p.mat = res1b$p, method = "color" , type = "upper", pch.cex = 1.2,
#          insig = "label_sig", pch.col = "red", order = "original", col = gray.colors(8), tl.col = "black")

# ggplots ------------------------------------------------------------------

#install.packages("Hmisc")
require(Hmisc)
# 
# ggplot(rmer, aes(var, spad))+
#   stat_summary(fun.data = "mean_cl_normal",
#                geom = "errorbar",
#                width = 0.2)+
#   stat_summary(fun.y = "mean", geom = "point", size = 3) # plot showing spad values with errorbars
# 
# ggplot(rmer, aes(var, spadeq))+
#   stat_summary(fun.data = "mean_cl_normal",
#                geom = "errorbar",
#                width = 0.2)+
#   stat_summary(fun.y = "mean", geom = "point", size = 3) 

rmer$var <- factor(rmer$var)

ggplot(rmer, aes(var, spad))+
  geom_boxplot()+
  labs(x = "treatment", y = "SPAD value")+
  theme_classic(base_size = 25)
ggsave("R_spad_var.png", path = "plots", height = 7, width = 13, dpi = 300)


# ggplot - kalibracni krivka, R2 vepsane do grafu
# install.packages("ggpmisc")
library(ggpmisc)

## ExG

my.formula <- y ~ x
ggplot(rmer, aes(ExG, spad))+
  labs(x = "ExG", y = "SPAD value")+
  geom_smooth(method=lm, se=FALSE, color = "darkgrey")+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               size = 8, label.x = "left", parse = TRUE)+
  geom_point(size = 2)+
  theme_classic(base_size = 25)
ggsave("SPAD_ExG.png", path = "plots", height = 6, width = 10, dpi = 300)

## ExR

ggplot(rmer, aes(ExR, spad))+
  labs(x = "ExR", y = "SPAD value")+
  geom_smooth(method=lm, se=FALSE, color = "darkgrey")+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               size = 8, label.x = "right", parse = TRUE)+
  geom_point(size = 2)+
  theme_classic(base_size = 25)
ggsave("SPAD_ExR.png", path = "plots", height = 6, width = 10, dpi = 300)

## ExG-ExR

ggplot(rmer, aes(`ExG-ExR`, spad))+
  labs(x = "ExG-ExR", y = "SPAD value")+
  geom_smooth(method=lm, se=FALSE, color = "darkgrey")+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               size = 8, label.x = "left", parse = TRUE)+
  geom_point(size = 2)+
  theme_classic(base_size = 25)
ggsave("SPAD_ExG-ExR.png", path = "plots", height = 6, width = 10, dpi = 300)

## SPAD vs. SPADEQ

ggplot(rmer, aes(spad, spadeq))+
  labs(x = "chl", y = "SPAD value")+
  geom_smooth(method=lm, se=FALSE, color = "darkgrey")+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               size = 8, label.x = "left", parse = TRUE)+
  geom_point(size = 2)+
  theme_classic(base_size = 25)


# 1) lm -------------------------------------------------------------------

fullmod <- lm(spad ~ R + G + B + NRI + NGI + NBI + hue + saturation + brightness + Y + Cb + Cr + 
   GMR + GDR + GDB + RDB + VI + `(R-B)/(R+B)` + DGCI + ExG + ExR + `ExG-ExR`, 
   data = rmer) #R2= 0,8049

summary(fullmod)
anova(fullmod)

m1 <-  lm(spad ~ R + G + NRI + NGI + hue + brightness + Y + Cb + Cr + 
            GMR + GDR + GDB + RDB + VI + ExG + ExR + `ExG-ExR`, 
          data = rmer)
summary(m1) #R2= 0,7984

m2 <-  lm(spad ~ NGI + hue + Cb + VI + ExG, 
          data = rmer)
summary(m2) #R2= 0,7992

anova(fullmod, m1, m2)


# 2) lm / single factor ---------------------------------------------------

mod <-lm(spad ~ `ExG-ExR` ,data = rmer)
summary(mod)

# EXPORT data -------------------------------------------------------------

install.packages("writexl")
library(writexl)

# Write the first data set in a new workbook
write_xlsx(rmer,"rmer.xlsx")
