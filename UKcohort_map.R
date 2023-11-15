#install.packages(c("cowplot", "googleway", "ggrepel", 
#"ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
#install.packages("rgeos")

#remove.packages(c("ggplot2", "data.table"))
#install.packages('Rcpp', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('data.table', dependencies = TRUE)

library(ggplot2)
theme_set(theme_bw())
library("sf")
setwd("C:\\Users\\kelli\\OneDrive\\Documents\\Duckweed\\UK maps")

#ALL DAT
dat <- readxl::read_xlsx("All_latlong_reduced.xlsx", sheet = 1)

library("rnaturalearth")
library("rnaturalearthdata")

library("maps")
library("mapdata")

#Option 2
library(ggplot2)
library(maps)
#Data
worldmap = map_data('world')

str(dat)

#dat <- dat[1:24, 1:5]
dat <- data.frame(dat)
str(dat)
dat
dat$Lat <- as.numeric(dat$Lat)
dat$Long <- as.numeric(dat$Long)

#by place coloring
dat$Place <- factor(dat$Place, levels = c("ELG", "ABE", "GLA", "LAN","BFD", "YOR", "HUL", "MID", "BRI", "NEW", "COR", "HAS"))
dat$Place

#produces rainbow colored plot, trying to color blue to red
#none of these methods working
#my_pal <- scico::scico(length(unique(dat$Place)), palette = "roma")
#scale_color_manual(values=mycolors) +
#mycolors = c("ELG" = "blue", "ABE"="blue", "GLA"="blue",
#             "LANC"= "green", "BFD" = "green", "YOR"="yellow",
#             "HUL"="gold", "MID"="pink", "BRI" = "orange",
#             "NEW" ="orange", "COR"="red", "HAS"="red")
#create vector of colors
#by place coloring
#change order alphabetical, color by cool to warm
colnames(dat) <- c("Site", "Lat", "Long", "Region", "Species")
dat$Region <- factor(dat$Region, levels = c("ELG", "ABE", "GLA", "LAN","BFD", "YOR", "HUL", "MID", "NEW", "BRI", "HAS", "COR"))
dat$Region
mycolors <- c("blue","steelblue","lightblue",
              "cyan","green","darkgreen",
              "yellow","gold","pink",
              "orange","red","darkred")
mycolors <- c("#632770", "#2915B4", "#3260B4", "#17706A",
              "#2E4E13", "#2D882D", "#77EE15", "#ABF057",
              "#FFA256", "#FF4E32", "#AA3939", "#7D0900") #"#16E400",
names(mycolors) <- levels(dat$Region)
mycolors #assigned correctly, done in order n to s
#do colors based on used on all other plots in paper
ggplot() + geom_polygon(data = worldmap, 
                        aes(x = long, 
                            y = lat, 
                            group = group,
                        ),
                        fill = 'lightgrey', 
                        color = 'black') +
  #labs(x = "Longitude", y = "Latitude") + #not doing anything with this line
  coord_fixed(ratio=1.3, xlim = c(-6.5,2.0), 
              ylim = c(50, 58.5)) +
  geom_point(data = dat, aes(x = Long, y = Lat, fill = Region), shape = 21, size = 6) +
  scale_fill_manual(values=mycolors)+
  theme_void() +
  theme(legend.key.size = unit(12,"pt"), legend.title = element_text(size=12), legend.text = element_text(size=12))+
  geom_rect(aes(xmin=0.10, xmax=-2.32, ymin=53.35, ymax=54.18), color="black", lwd=0.5, alpha=0)
#scale_fill_discrete(labels = c(expression(italic("L. minor", "L. minuta", "L. turionifera", "S. polyrhiza")))))
ggsave("UKmap_all_ntoscols.png", dpi = 300, width = 15, height = 15 , units = "cm") 
ggsave("UKmap_all_ntoscols.pdf", dpi = 300, width = 15, height = 15 , units = "cm") 
