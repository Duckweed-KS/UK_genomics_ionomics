#KES STARTING FROM 

library(readxl)
library(ggplot2)
library(fmsb)
library(ggradar)
library(radarchart)
library(ggforce)
library(ggplot2)

setwd("C:\\Users\\Kellie\\Documents\\R\\DWionome\\MS1 Lemna species group")
ions <- read.csv("DW_Nmedia_tissue_noLOD_zscores.csv")
ions3 <- read.csv("DW_Nmedia_tissue_noLOD_zscores_nosp.csv")
ions3 <- read.csv("DW_Nmedia_tissue_noLOD_zscores_nosp_SITENAMES1.csv")

#ions <- ions2[, 1:12] #unuseful empty cols from end removed
#ions[,-1:3]
#ions2 <- ions[,-1:-3]
#range(ions[, -1:-3]) #minus 3 to 8 set range for all radars

#ions <- ions[nrow(ions):1, ]

#sort by loco
#ions3 <- ions3[order(ions$Loco), ]

#sl <- ions3[1:10, ] #group by rows ie wolffia, wolffiella

#ggradar(sl,
#       grid.min = -3,
#         grid.mid = 0,
#         grid.max = 8,
#         values.radar = c(-3, 0, 8))

#sl <- ions
#sl <- data.frame(sl)
#sl <- sl[, 12:2] # no accession name
#names(sl) #col names, need to specify n media want to use
#sl <- sl[, c(ncol(sl), 1:11)] 
#names(sl)

# apply(sl, 2, min)
#sl <- rbind(unname(apply(sl, 2, min)), unname(apply(sl, 2, max)), sl)

# sl <- rbind(rep(min(sl), 11), rep(max(sl), 11), sl)

#names(sl)

#points(0, 0)
#sl$Mn[1:7]

#slmin <- sl[4:14]

ions <- ions3[,-2]
range(ions3[, -1:-2]) #minus 2 to 9 set range for all radars


ABE <- ggradar(ions[1:11, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "ABE", legend.text.size = 18,
        grid.min = -5,
        grid.mid = 0,
        grid.max = 11,
        values.radar = c(-5, 0, 11)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 4.1)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 8.7)) +
  geom_text(aes(-1.5, 4.0, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 8.5, label = "2"), size = 5)#
ggsave("ABE.pdf", width = 12, height = 12)
ggsave("ABE.png", width = 12, height = 12)

BFD <- ggradar(ions[12:20, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "BFD", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
ggsave("BFD.png", width = 12, height = 12)

BRI <- ggradar(ions[21:33, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "BRI", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
ggsave("BRI.png", width = 12, height = 12)

COR <- ggradar(ions[34:51, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "COR", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
  ggsave("COR.png", width = 12, height = 12)

ELG <- ggradar(ions[52:60, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "ELG", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
  ggsave("ELG.png", width = 12, height = 12)

GLA <- ggradar(ions[61:66, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "GLA", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8.2,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
  ggsave("GLA.png", width = 12, height = 12)

HAS <- ggradar(ions[67:77, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "HAS", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
  ggsave("HAS.png", width = 12, height = 12)

HUL <- ggradar(ions[78:83, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "HUL", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
  ggsave("HUL.png", width = 12, height = 12)

LAN <- ggradar(ions[84:86, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "LAN", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
  ggsave("LAN.png", width = 12, height = 12)
  
MID <- ggradar(ions[87:91, ],
          group.point.size = 3,
          group.line.width = 1,
        plot.title = "MID", legend.text.size = 18,
          grid.min = -3,
          grid.mid = 0,
          grid.max = 8,
          values.radar = c(-3, 0, 8)) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
    geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
    geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
  ggsave("MID.png", width = 12, height = 12)

NEW <- ggradar(ions[92:108, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "NEW", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
  ggsave("NEW.png", width = 12, height = 12)

YOR <- ggradar(ions[109:114, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "YOR", legend.text.size = 18,
        grid.min = -3,
        grid.mid = 0,
        grid.max = 8,
        values.radar = c(-3, 0, 8)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 2.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 6.4)) +
  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 6.3, label = "2"), size = 5)
  ggsave("YOR.png", width = 12, height = 12)
  
  #TRY SAVE AS AUTOMATIC PANEL FIGURE
  #LOOKS BAD
#  install.packages("gridExtra")
#  library(gridExtra)
# tog <-  grid.arrange(ELG, ABE, GLA, LAN,
#               BFD, YOR, HUL,MID,
#               NEW, BRI, COR, HAS,ncol=4, nrow=4)
# ggsave(tog, file="tog.pdf", width = 210, height = 297, units = "mm")
