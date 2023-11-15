#KES STARTING FROM 
#install.packages("devtools")
#install.packages("ggplot2")
#install.packages("readxl")
#devtools::install_github("ricardo-bion/ggradar")
#install.packages("ggradar")
#install.packages("fmsb")
#install.packages("scico")
#install.packages("ggforce")
#install.packages("radarchart")
library(readxl)
library(ggplot2)
library(fmsb)
library(ggradar)
library(radarchart)
library(ggforce)
library(ggplot2)

setwd("C:\\Users\\Kellie\\Documents\\R\\Water ionome radars")
ions3 <- read.csv("Water_ionome_zscores.csv")
ions3 <- read.csv("Water_ionome_zscores_SITENAMES1.csv")
ions3 <- read.csv("Water_ionome_zscores_SITENAMES2.csv")

#ions <- ions2[, 1:12] #unuseful empty cols from end removed
ions <- ions3[,-2]
range(ions3[, -1:-2]) #minus 2 to 9 set range for all radars

# Color for the lines x12 max per group
#lcols <- c("#632770", "#2915B4", "#3260B4", "#17706A",
#           "#2E4E13", "#2D882D", "#77EE15", "#ABF057",
#           "#FFA256", "#FF4E32", "#AA3939", "#7D0900")
#then add this in ggrdar section
#group.colours = lcols,
#LOOK MORE DIFFERENTIATED WHEN DEFAULT SET

#ions <- ions[nrow(ions):1, ]

#sort by loco
#ions <- ions[order(ions$Loco), ]

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

ggradar(ions[1:12, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "ABE", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
#  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
#  geom_text(aes(8, 2, label = "*"), size = 10)
ggsave("ABE.png", width = 12, height = 12)

ggradar(ions[13:19, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "BFD", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
  #  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
  #  geom_text(aes(8, 2, label = "*"), size = 10)
  ggsave("BFD.png", width = 12, height = 12)

ggradar(ions[20:28, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "BRI", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
  #  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
  #  geom_text(aes(8, 2, label = "*"), size = 10)
  ggsave("BRI.png", width = 12, height = 12)

ggradar(ions[29:40, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "COR", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9.003,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
  #  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
  #  geom_text(aes(8, 2, label = "*"), size = 10)
  ggsave("COR.png", width = 12, height = 12)

ggradar(ions[41:52, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "ELG", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
  #  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
  #  geom_text(aes(8, 2, label = "*"), size = 10)
  ggsave("ELG.png", width = 12, height = 12)

ggradar(ions[53:64, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "GLA", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
  #  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
  #  geom_text(aes(8, 2, label = "*"), size = 10)
  ggsave("GLA.png", width = 12, height = 12)

ggradar(ions[65:73, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "HAS", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
  #  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
  #  geom_text(aes(8, 2, label = "*"), size = 10)
  ggsave("HAS.png", width = 12, height = 12)

ggradar(ions[74:80, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "HUL", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
  #  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
  #  geom_text(aes(8, 2, label = "*"), size = 10)
  ggsave("HUL.png", width = 12, height = 12)

ggradar(ions[81:91, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "NEW", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
  #  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
  #  geom_text(aes(8, 2, label = "*"), size = 10)
  ggsave("NEW.png", width = 12, height = 12)

ggradar(ions[92:97, ],
        group.point.size = 3,
        group.line.width = 1,
        plot.title = "YOR", legend.text.size = 18,
        grid.min = -2,
        grid.mid = 0,
        grid.max = 9,
        values.radar = c(-2, 0, 9)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.7)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5.4)) +
  #  geom_text(aes(-1.5, 2.3, label = "-2"), size = 5) +
  geom_text(aes(-1.4, 5.3, label = "2"), size = 5) +
  #  geom_text(aes(8, 2, label = "*"), size = 10)
  ggsave("YOR.png", width = 12, height = 12)
