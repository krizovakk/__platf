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

# corrgram for PLATF and SPAD data only------------------------------------------------------

rcorm_sel <- rmer %>% 
  select(spad, R, G, B, NRI, NGI, NBI, hue, saturation, brightness, Y, Cb, Cr, 
         GMR, GDR, GDB, RDB, VI, `(R-B)/(R+B)`, DGCI, ExG, ExR, `ExG-ExR`) #selected indices
colnames(rcorm_sel)[1] <- "SPAD"

# cor.test(rcorm_sel$spad, rcorm_sel$R)
# cor.test(rcorm_sel$spad, rcorm_sel$G)
# cor.test(rcorm_sel$spad, rcorm_sel$B)
# cor.test(rcorm_sel$spad, rcorm_sel$NRI)
# cor.test(rcorm_sel$spad, rcorm_sel$NGI)
# cor.test(rcorm_sel$spad, rcorm_sel$NBI)
# cor.test(rcorm_sel$spad, rcorm_sel$hue)
# cor.test(rcorm_sel$spad, rcorm_sel$saturation)
# cor.test(rcorm_sel$spad, rcorm_sel$brightness)
# cor.test(rcorm_sel$spad, rcorm_sel$Y)
# cor.test(rcorm_sel$spad, rcorm_sel$Cb)
# cor.test(rcorm_sel$spad, rcorm_sel$Cr)
# cor.test(rcorm_sel$spad, rcorm_sel$GMR)
# cor.test(rcorm_sel$spad, rcorm_sel$GDR)
# cor.test(rcorm_sel$spad, rcorm_sel$GDB)
# cor.test(rcorm_sel$spad, rcorm_sel$RDB)
# cor.test(rcorm_sel$spad, rcorm_sel$VI)
# cor.test(rcorm_sel$spad, rcorm_sel$`(R-B)/(R+B)`)
# cor.test(rcorm_sel$spad, rcorm_sel$DGCI)
# cor.test(rcorm_sel$spad, rcorm_sel$ExG)
# cor.test(rcorm_sel$spad, rcorm_sel$ExR)
# cor.test(rcorm_sel$spad, rcorm_sel$`ExG-ExR`) # saved in /media/katerina/EXT_KK/TF/2019_IGA_Platformy_Hemisfery/PLATFORMA/200917_final/_wd/corr_coeficients.xlsx

cm <- cor(rcorm_sel) # creates correlation matrix

res1 <- cor.mtest(rcorm_sel, conf.level = .95) # significance test, to add signif. labels into a plot

#install.packages("corrplot")
require(corrplot)
require(RColorBrewer)

par(family="Times New Roman")
corrplot(cm, p.mat = res1$p, method = "color" , type = "upper",
         sig.level = c(.001, .01, .05), pch.cex = .8,
         insig = "label_sig", pch.col = "white", order = "original", 
         col = brewer.pal(n = 10, name = "PiYG"), 
         tl.col = "black", tl.srt = 45)
# saved as png via export, h 950 w 600

# CORR siginificant -------------------------------------------------------

sig <- rmer %>% 
  select(spad, R, G, NRI, NGI, hue, brightness, Y, Cb, Cr, 
          GMR, GDR, GDB, RDB, VI, ExG, ExR, `ExG-ExR`)

colnames(sig)[1] <- "SPAD"
colnames(sig)[7] <- "bright"

  
cm_sig <- cor(sig)

corrgram(cm_sig, lower.panel=panel.conf, upper.panel=NULL, cex.labels = 1.4, font.labels = 3) #numbers

corrplot(cm_sig, p.mat = res1$p, method = "color" , type = "lower",
         sig.level = c(.001, .01, .05), pch.cex = 1,
         insig = "label_sig", pch.col = "white", order = "original", 
         col = gray.colors(8), tl.col = "black") #shades

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
# ggsave("R_spad_var.png", path = "plots", height = 5, width = 13, dpi = 300)


# ggplot - kalibracni krivka, R2 vepsane do grafu
install.packages("ggpmisc")
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

# install.packages("writexl")
library(writexl)

# Write the first data set in a new workbook
write_xlsx(rmer,"rmer.xlsx")


# LED spectrum ------------------------------------------------------------

led <- read_excel("data/led_spectrum.xlsx", sheet = 1)

ptcol <- c("gray25", "gray70")

# summary(led$diff)
# plot(led$diff)
# summary(led$nodiff)
# plot(led$nodiff)

ledl <- led %>% 
  melt(id.vars = "nm", variable.name = "diff", value.name = "intens")

ledl$diff <- factor(ledl$diff, 
                    levels = c("diff", "nodiff"), 
                    labels = c("diffuser", "clear"))

ggplot(ledl, aes(nm, intens, color = diff))+
  geom_point()+
  xlab("Wavelength [nm]")+
  ylab("EMR Intensity")+
  ggtitle("LED Yoldal YZ-WS5N40N")+
  scale_color_manual(values = ptcol)+
  scale_x_continuous(limits = c(380, 820), 
                     breaks = c(400, 450, 500, 550, 600, 650, 700, 750, 800))+
  theme_minimal()+
  theme(legend.title = element_blank())
# ggsave("spec_both.png", path = "plots", height = 5, width = 8, dpi = 300)
ggsave("spec_both_bw.png", path = "plots", height = 5, width = 8, dpi = 300)

# rescale

led_sc <- led %>%
  mutate(diff = diff + 3.01) %>% 
  mutate(nodiff = nodiff + 3.68) %>% 
  mutate(nodiff = nodiff * 1.3838) %>% 
  

# summary(led_sc$diff)  
# summary(led_sc$nodiff)  
  
led_sc$diff <- factor(led_sc$diff, 
                    levels = c("diff", "nodiff"), 
                    labels = c("diffuser", "clear"))

ggplot(led_sc, aes(nm, intens, color = diff))+
  geom_point()+
  xlab("Wavelength [nm]")+
  ylab("EMR Intensity")+
  ggtitle("LED Yoldal YZ-WS5N40N")+
  scale_color_manual(values = ptcol)+
  scale_x_continuous(limits = c(380, 820), 
                     breaks = c(400, 450, 500, 550, 600, 650, 700, 750, 800))+
  theme_minimal()+
  theme(legend.title = element_blank())
# ggsave("spec_both_rescaled.png", path = "plots", height = 5, width = 8, dpi = 300)
ggsave("spec_both_rescaled_bw.png", path = "plots", height = 5, width = 8, dpi = 300)

# one line only - WITH DIFFUSER

led_diff <- led_sc %>% 
  filter(diff == "diffuser")

ggplot(led_diff, aes(nm, intens))+
  geom_point(size = .9)+
  xlab("Wavelength [nm]")+
  ylab("EMR Intensity [arb. u.]")+
  ggtitle("LED Yoldal YZ-WS5N40N")+
  scale_color_manual(values = "grey30")+
  scale_x_continuous(limits = c(380, 820), 
                     breaks = c(400, 450, 500, 550, 600, 650, 700, 750, 800))+
  theme_classic(base_size = 18)+
  theme(text=element_text(family="Times New Roman"))
# ggsave("spec_both_rescaled.png", path = "plots", height = 5, width = 8, dpi = 300)
ggsave("spec_diffuser_bw.png", path = "plots", height = 4, width = 6, dpi = 300)

# final SPAD estimate plot ------------------------------------------------

spadestag <- rplatfag %>% 
  mutate(lm = 89.538+0.475*rplatfag$Cb-0.761*rplatfag$Cr) %>% 
  mutate(glm = 1/(-0.011-0.0003*rplatfag$Cb+0.005*rplatfag$Cr)) %>% 
  select("var", "spad", "lm", "glm")
melt(id.vars = "var", variable.name = "source", value.name = "value")

# install.packages("ggpmisc")
library(ggpmisc)
# install.packages("ggtext")
require(ggtext)

labR <- paste("R^2 == 0.81")
# lb1 <- paste("R^2 == ", round(runif(1),4))

gslm <- ggplot(spadestag, aes(lm, spad))+
  geom_point()+
  labs(x = "SLM estimate", y = "SPAD value")+
  annotate("text", x=37, y=54, family="Times New Roman",
           label= "y = 89.538+0.475Cb-0.761Cr", size = 5)+
  annotate("text", x=37, y=52, family="Times New Roman",
           label= labR, size = 5, parse=TRUE)+
  theme_classic(base_size = 18)+
  theme(text=element_text(family="Times New Roman")) # face="bold"

gslm

gglm <- ggplot(spadestag, aes(glm, spad))+
  geom_point()+
  labs(x = "GLM estimate", y = "SPAD value")+
  annotate("text", x=1.755, y=54, family="Times New Roman",
           label= "y = 1/(-0.011-0.0003Cb+0.005Cr)", size = 5)+
  annotate("text", x=1.755, y=52, family="Times New Roman",
           label= labR, size = 5, parse=TRUE)+
  theme_classic(base_size = 18)+
  theme(text=element_text(family="Times New Roman")) # face="bold"

gglm

# install.packages("ggpubr")
# require(ggpubr)

ggarrange(gslm, gglm + rremove("y.title"), # https://rdrr.io/cran/ggpubr/man/rremove.html
          # labels = c("A", "B"),
          # label.x = 0.2,
          ncol = 2, nrow = 1)

ggsave("SPAD_estimate.png", path = "plots", height = 5, width = 10, dpi = 300)

# estimboth <- spadestag %>% 
#   melt(id.vars = c("var", "spad"), variable.name = "model", value.name = "value")
# 
# ggplot(estimboth, aes(value, spad, shape = model))+
#   geom_point()+
#   labs(x = "SPAD value estimate", y = "SPAD value")+
#   theme_classic(base_size = 20)

#ppt plots

spadestag$var <- factor(spadestag$var)

ggplot(spadestag, aes(var, spad))+
  geom_boxplot()+
  labs(x = "treatment", y = "SPAD value")+
  theme_classic(base_size = 25)
ggsave("R_spad_var.png", path = "plots", height = 5, width = 13, dpi = 300)

ggplot(spadestag, aes(var, lm))+
  geom_boxplot()+
  labs(x = "treatment", y = "SPAD value estimate")+
  theme_classic(base_size = 25)
ggsave("R_spad_estimate_var.png", path = "plots", height = 5, width = 13, dpi = 300)

# final boxplot SPAD vs R2S

spadestagl <- spadestag %>% 
  melt(id.vars = c("var"), variable.name = "device", value.name = "value")
spadestagl$var <- factor(spadestagl$var)

# attempt for color transparency

t_col <- function(color, percent = 50, name = name) {
     #   color = color name
     # percent = % transparency
     #    name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

mycol1<- t_col("deeppink3", perc = 50, name = "lt.pink")
mycol2 <- t_col("forestgreen", perc = 50, name = "lt.green")

ggplot(spadestagl, aes(var, value, fill = device))+
  geom_boxplot()+
  labs(x = "Treatment", y = "SPAD value", fill = "")+
  # scale_fill_manual(values = c("#228B227F", "#CD10767F"), # transparent version
  scale_fill_manual(values = c("springgreen4", "mediumvioletred"), 
                    name = "", labels = c("SPAD-502Plus", "Rasp2SPAD LM"))+
  theme_classic(base_size = 18)+
  theme(legend.position = "bottom",
        text=element_text(family="Times New Roman"))
# ggsave("SPAD_R2S_boxplot.png", path = "plots", height = 5, width = 7, dpi = 300)
# ggsave("SPAD_R2S_boxplot_legend_bottom.png", path = "plots", height = 5, width = 9, dpi = 300)

# indices stats ------------------------------------------------------

rstat <- read_excel("data/200416_rapeseed_rgb_stat.xlsx")

install.packages("psych")
require(psych)

stattab <- describeBy(rstat, rstat$var) #works absolutely perfect :)
write_xlsx(stattab,"stattab.xlsx")
