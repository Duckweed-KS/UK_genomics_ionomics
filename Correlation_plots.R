#linear models for each element between sites and dws
#color them by region 

setwd("C:\\Users\\kelli\\OneDrive - The University of Nottingham\\Nottingham\\Year 2\\Duckweed ionomics analysis\\Lemna correlations water and DW ionomes")

El <- read.csv("Elements_both.csv")

library(dplyr)
library(ggplot2)
library(ggpubr)

names(El)

#plots line plots for ind species and summarises model as intercept and mean_fqfm
El %>% ggplot(aes(Mg, Mg.1, col = El$Loco)) + geom_point() + stat_smooth(method = "lm")
summary(lm(Mg ~ Mg.1, El[El$Loco == "ABE", ])) #**
summary(lm(Mg ~ Mg.1, El[El$Loco == "BFD", ]))
summary(lm(Mg ~ Mg.1, El[El$Loco == "BRI", ]))
summary(lm(Mg ~ Mg.1, El[El$Loco == "COR", ])) #**
summary(lm(Mg ~ Mg.1, El[El$Loco == "ELG", ]))
summary(lm(Mg ~ Mg.1, El[El$Loco == "GLA", ]))
summary(lm(Mg ~ Mg.1, El[El$Loco == "HAS", ]))
summary(lm(Mg ~ Mg.1, El[El$Loco == "HUL", ]))
summary(lm(Mg ~ Mg.1, El[El$Loco == "NEW", ]))
summary(lm(Mg ~ Mg.1, El[El$Loco == "YOR", ]))

as.factor(El$Loco)

El$Loco <- factor(El$Loco, levels = c("ELG", "ABE", "GLA","BFD", "YOR", "HUL", "NEW", "BRI", "HAS", "COR"))
El$Loco
mycolors <- c("#632770", "#2915B4", "#3260B4",
              "#2E4E13", "#2D882D", "#77EE15",
              "#FFA256", "#FF4E32", "#AA3939", "#7D0900") 
names(mycolors) <- levels(El$Loco)

Mg <- ggscatter(
  El, x = "Mg", y = "Mg.1",
  color = "Loco",
  add = "reg.line"
) +
  facet_wrap(~Loco, nrow=2, ncol=5, scales = "free") +
  ylab("Mg site")+ xlab("Mg duckweed")+
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 500) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme_classic() +
  scale_color_manual(values = mycolors)

Mg

#element
B <- ggscatter(
  El, x = "B", y = "B.1",
  color = "Loco",
  add = "reg.line"
) +
  facet_wrap(~Loco, nrow=2, ncol=5, scales = "free") +
  ylab("B site")+ xlab("B duckweed")+
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 60) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme_classic() +
  scale_color_manual(values = mycolors)

B

#element
Na <- ggscatter(
  El, x = "Na", y = "Na.1",
  color = "Loco",
  add = "reg.line"
) +
  facet_wrap(~Loco, nrow=2, ncol=5, scales = "free") +
  ylab("Na site")+ xlab("Na duckweed")+
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 30) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme_classic() +
  scale_color_manual(values = mycolors)

Na

#element
P <- ggscatter(
  El, x = "P", y = "P.1",
  color = "Loco",
  add = "reg.line"
) +
  facet_wrap(~Loco, nrow=2, ncol=5, scales = "free") +
  ylab("P site")+ xlab("P duckweed")+
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 900) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme_classic() +
  scale_color_manual(values = mycolors)

P

#element
S<- ggscatter(
  El, x = "S", y = "S.1",
  color = "Loco",
  add = "reg.line"
) +
  facet_wrap(~Loco, nrow=2, ncol=5, scales = "free") +
  ylab("S site")+ xlab("S duckweed")+
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 600) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme_classic() +
  scale_color_manual(values = mycolors)

S

#element
K <- ggscatter(
  El, x = "K", y = "K.1",
  color = "Loco",
  add = "reg.line"
) +
  facet_wrap(~Loco, nrow=2, ncol=5, scales = "free") +
  ylab("K site")+ xlab("K duckweed")+
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 3200) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme_classic() +
  scale_color_manual(values = mycolors)

K

#element
Ca <- ggscatter(
  El, x = "Ca", y = "Ca.1",
  color = "Loco",
  add = "reg.line"
) +
  facet_wrap(~Loco, nrow=2, ncol=5, scales = "free") +
  ylab("Ca site")+ xlab("Ca duckweed")+
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 2000) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme_classic() +
  scale_color_manual(values = mycolors)

Ca

#element
Mn <- ggscatter(
  El, x = "Mn", y = "Mn.1",
  color = "Loco",
  add = "reg.line"
) +
  facet_wrap(~Loco, nrow=2, ncol=5, scales = "free") +
  ylab("Mn site")+ xlab("Mn duckweed")+
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 300) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme_classic() +
  scale_color_manual(values = mycolors)

Mn

#element
Fe <- ggscatter(
  El, x = "Fe", y = "Fe.1",
  color = "Loco",
  add = "reg.line"
) +
  facet_wrap(~Loco, nrow=2, ncol=5, scales = "free") +
  ylab("Fe site")+ xlab("Fe duckweed")+
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 80) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme_classic() +
  scale_color_manual(values = mycolors)

Fe
