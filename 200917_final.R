# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("corrgram")

require(tidyverse)
require(readxl)
require(reshape2)
require(corrgram)

rplatf <- read_excel("data/200416_platf_spad.xlsx") #rapeseed, 16th April 2020; platform+spad results; n=948
rlab <-  read_excel("data/200416_lab.xls") #rapeseed, 16th April 2020; lab results; n=90

# final table -------------------------------------------------------------

rplatf <- rplatf %>% 
  mutate(spadeq = (99*rplatf$spad)/(144-rplatf$spad)) # SPAD value equation -> chlor. content
rplatf$id <- paste(rplatf$var, rplatf$plant, rplatf$leaf, sep = "")
rplatfag <- aggregate(rplatf[, 1:39], list(rplatf$id), FUN = median) # 5:39 <-- agg 5th to 39th column
colnames(rplatfag)[1] <- "id"
rmer <- merge(rplatfag, rlab, by = "id") # merged table for rapeseed, n=90
rmer <- rmer %>% 
  select(id, spad, spadeq, everything()) # moves the "spadeq" column to the front

# corrgram ------------------------------------------------------

rcorm <- rmer %>%
  select(spad, spadeq, r, g, b, R, G, B, mean_rgb, cmin, cmax, c,
         hue, saturation, brightness, Y, Cb, Cr, GMR, GDR, VI, DGCI, NRI, NGI, ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas,
         cha, chb, chab, car, chacm, chbcm, chabcm, carcm)

corrgram(rcorm, lower.panel=panel.conf, upper.panel=NULL)


# ggplots ------------------------------------------------------------------

#install.packages("Hmisc")
# require(Hmisc)
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
ggsave("R_spad_var.png", path = "plots", height = 5, width = 13, dpi = 300)


ggplot(rmer, aes(spad, hue))+
  geom_point()+
  labs(x = "SPAD value", y = "hue")+
  theme_classic(base_size = 25)

# ggplot - kalibracni krivka, R2 vepsane do grafu
install.packages("ggpmisc")
library(ggpmisc)

my.formula <- y ~ x
ggplot(rmer, aes(NRI, spad))+
  labs(x = "R/(R+G+B)", y = "SPAD value")+
  geom_smooth(method=lm, se=FALSE, color = "darkgrey")+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), size = 8, label.x = "right",
               parse = TRUE)+
  geom_point(size = 2)+
  theme_classic(base_size = 25)
ggsave("SPAD_NRI.png", path = "plots", height = 6, width = 10, dpi = 300)



