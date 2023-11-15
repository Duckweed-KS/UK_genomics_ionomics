setwd("C:\\Users\\kelli\\OneDrive - The University of Nottingham\\Nottingham\\Year 2\\Duckweed ionomics analysis")

library(stringr)

#read in one at a time
sa <- read.csv("Linegraphs_seasonal_K.csv")
sa1 <- read.csv("Linegraphs_seasonal_Ca.csv")
sa2 <- read.csv("Linegraphs_seasonal_Na.csv")
sa3 <- read.csv("Linegraphs_seasonal_S.csv")
sa4 <- read.csv("Linegraphs_seasonal_Mg.csv")
sa5 <- read.csv("Linegraphs_seasonal_Si.csv")
sa6 <- read.csv("Linegraphs_seasonal_P.csv")
sa7 <- read.csv("Linegraphs_seasonal_Fe.csv")
sa8 <- read.csv("Linegraphs_seasonal_Mn.csv")
sa9 <- read.csv("Linegraphs_seasonal_B.csv")
sa10 <- read.csv("Linegraphs_seasonal_Sr.csv")
sa11 <- read.csv("Linegraphs_seasonal_Ba.csv")
sa12 <- read.csv("Linegraphs_seasonal_Al.csv")
sa13 <- read.csv("Linegraphs_seasonal_Cu.csv")
sa14 <- read.csv("Linegraphs_seasonal_Mo.csv")
sa15 <- read.csv("Linegraphs_seasonal_Zn.csv")
sa16 <- read.csv("Linegraphs_seasonal_Ni.csv")
#sa17 <- read.csv("Linegraphs_seasonal_Ti.csv") lod
sa17 <- read.csv("Linegraphs_seasonal_Rb.csv")
sa18 <- read.csv("Linegraphs_seasonal_Li.csv")
sa19 <- read.csv("Linegraphs_seasonal_As.csv")
#sa21 <- read.csv("Linegraphs_seasonal_Cr.csv") lod
sa20 <- read.csv("Linegraphs_seasonal_Pb.csv")
#sa23 <- read.csv("Linegraphs_seasonal_Sn.csv") lod
sa21 <- read.csv("Linegraphs_seasonal_Co.csv")
sa22 <- read.csv("Linegraphs_seasonal_Cd.csv")

#ca
row.names(sa1) <- sa1$Site
sa1$Site <- NULL
#take col names from first col and give new names
sa1 <- t(sa1) #become a matrix during transposition
sa1
#turn back into dataframe again
sa1 <- data.frame(sa1)
class(sa1)

row.names(sa1)
row.names(sa1) <- str_replace_all(row.names(sa1), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa1)) #make vector sa10me name as row names

library(stringr)

time_vec <- row.names(sa1)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa1$time <- time_vec

str(sa1)

library(tidyr)

Time <- c("Autumn 2020", "Summer 2021", "Autumn 2021", "Spring 2022")

#save as tiff
#tiff("Water_lineplots.tiff", type="cairo", width = 1650, height = 1650) #save as png

#save as svg
#svg(filename="Water_lineplots.svg", 
#    width=50, 
#    height=40, 
#    pointsize=12)

png("Water_lineplots_dwqstd.png", type="cairo", width = 1650, height = 1650) #save as png


par(mfrow = c(5,5)) #no of rows and cols in plot display
#par(mfrow = c(1,1)) #use when testing each graph
#no of rows and cols in plot display
plot(KS02 ~ time, sa1, type = "n",
     ylab = "Ca",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 200000))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa1$time, sa1[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa1$time, sa1[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa1$time, sa1[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa1$time, sa1[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa1$time, sa1[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa1$time, sa1[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa1$time, sa1[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 200000, "Ca", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa1$time[-5], labels = Time)
#xaxp = c("Autumn 2020", "Summer 2021", "Autumn 2021", "Spring 2022")#at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa10df[-ncol(b_sa10df)])) #include all except final column
box()

#axis(2)
#axis(1, at = sa$time, labels = sa$time) #at is where ticks going, labels printed

#plot(1:20, 1:20, type = "n")

#for(i in 1:15){
#  points(i, 10, col = i, pch = 16, cex = 3)
#}
line_colours <- rep(1:8, length.out = ncol(sa1)-1) #8 colors
line_colours
line_type <- rep(1:2, length.out =  ncol(sa1)-1)
line_type

#possibly color by region so 3 colors?

#na
row.names(sa2) <- sa2$Site
sa2$Site <- NULL
#take col names from first col and give new names
sa2 <- t(sa2) #become a matrix during transposition
sa2
#turn back into dataframe again
sa2 <- data.frame(sa2)
class(sa2)

row.names(sa2)
row.names(sa2) <- str_replace_all(row.names(sa2), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa2)) #make vector sa2me name as row names

library(stringr)

time_vec <- row.names(sa2)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa2$time <- time_vec

str(sa2)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa2, type = "n",
     ylab = "Na",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 200500))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa2$time, sa2[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa2$time, sa2[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa2$time, sa2[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa2$time, sa2[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa2$time, sa2[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa2$time, sa2[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa2$time, sa2[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 200500, "Na", cex=2)
abline(h=200000, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa2$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa2df[-ncol(b_sa2df)])) #include all except final column
box()

row.names(sa) <- sa$Site
sa$Site <- NULL
#take col names from first col and give new names
sa <- t(sa) #become a matrix during transposition
sa
#turn back into dataframe again
sa <- data.frame(sa)
class(sa)

row.names(sa)
row.names(sa) <- str_replace_all(row.names(sa), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa)) #make vector same name as row names

library(stringr)

time_vec <- row.names(sa)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa$time <- time_vec

str(sa)

library(tidyr)
plot(KS02 ~ time, sa, type = "n", 
     ylab = "K",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 80000))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa$time, sa[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa$time, sa[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa$time, sa[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa$time, sa[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa$time, sa[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa$time, sa[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa$time, sa[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa$time, sa[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa$time, sa[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa$time, sa[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa$time, sa[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa$time, sa[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa$time, sa[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa$time, sa[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa$time, sa[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa$time, sa[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa$time, sa[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa$time, sa[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa$time, sa[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 80000, "K", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sadf[-ncol(b_sadf)])) #include all except final column
box()

##mg
row.names(sa3) <- sa3$Site
sa3$Site <- NULL
#take col names from first col and give new names
sa3 <- t(sa3) #become a matrix during transposition
sa3
#turn back into dataframe again
sa3 <- data.frame(sa3)
class(sa3)

row.names(sa3)
row.names(sa3) <- str_replace_all(row.names(sa3), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa3)) #make vector sa3me name as row names

library(stringr)

time_vec <- row.names(sa3)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa3$time <- time_vec

str(sa3)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa3, type = "n",
     ylab = "S",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 60000))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa3$time, sa3[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa3$time, sa3[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa3$time, sa3[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa3$time, sa3[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa3$time, sa3[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa3$time, sa3[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa3$time, sa3[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 60000, "S", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa3$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa3df[-ncol(b_sa3df)])) #include all except final column
box()

row.names(sa4) <- sa4$Site
sa4$Site <- NULL
#take col names from first col and give new names
sa4 <- t(sa4) #become a matrix during transposition
sa4
#turn back into dataframe again
sa4 <- data.frame(sa4)
class(sa4)

row.names(sa4)
row.names(sa4) <- str_replace_all(row.names(sa4), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa4)) #make vector sa4me name as row names

library(stringr)

time_vec <- row.names(sa4)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa4$time <- time_vec

str(sa4)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa4, type = "n",
     ylab = "Mg",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 40000))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa4$time, sa4[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa4$time, sa4[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa4$time, sa4[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa4$time, sa4[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa4$time, sa4[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa4$time, sa4[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa4$time, sa4[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 40000, "Mg", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa4$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa4df[-ncol(b_sa4df)])) #include all except final column
box()

row.names(sa5) <- sa5$Site
sa5$Site <- NULL
#take col names from first col and give new names
sa5 <- t(sa5) #become a matrix during transposition
sa5
#turn back into dataframe again
sa5 <- data.frame(sa5)
class(sa5)

row.names(sa5)
row.names(sa5) <- str_replace_all(row.names(sa5), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa5)) #make vector sa5me name as row names

library(stringr)

time_vec <- row.names(sa5)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa5$time <- time_vec

str(sa5)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa5, type = "n",
     ylab = "Si",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 15000))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa5$time, sa5[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa5$time, sa5[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa5$time, sa5[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa5$time, sa5[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa5$time, sa5[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa5$time, sa5[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa5$time, sa5[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 15000, "Si", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa5$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa5df[-ncol(b_sa5df)])) #include all except final column
box()

row.names(sa6) <- sa6$Site
sa6$Site <- NULL
#take col names from first col and give new names
sa6 <- t(sa6) #become a matrix during transposition
sa6
#turn back into dataframe again
sa6 <- data.frame(sa6)
class(sa6)

row.names(sa6)
row.names(sa6) <- str_replace_all(row.names(sa6), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa6)) #make vector sa6me name as row names

library(stringr)

time_vec <- row.names(sa6)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa6$time <- time_vec

str(sa6)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa6, type = "n",
     ylab = "P",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 6000))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa6$time, sa6[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa6$time, sa6[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa6$time, sa6[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa6$time, sa6[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa6$time, sa6[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa6$time, sa6[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa6$time, sa6[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 6000, "P", cex=2) 
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa6$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa6df[-ncol(b_sa6df)])) #include all except final column
box()

row.names(sa7) <- sa7$Site
sa7$Site <- NULL
#take col names from first col and give new names
sa7 <- t(sa7) #become a matrix during transposition
sa7
#turn back into dataframe again
sa7 <- data.frame(sa7)
class(sa7)

row.names(sa7)
row.names(sa7) <- str_replace_all(row.names(sa7), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa7)) #make vector sa7me name as row names

library(stringr)

time_vec <- row.names(sa7)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa7$time <- time_vec

str(sa7)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa7, type = "n",
     ylab = "Fe",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 6000))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa7$time, sa7[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa7$time, sa7[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa7$time, sa7[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa7$time, sa7[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa7$time, sa7[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa7$time, sa7[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa7$time, sa7[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 6000, "Fe", cex=2)
abline(h=200, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa7$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa7df[-ncol(b_sa7df)])) #include all except final column
box()

row.names(sa8) <- sa8$Site
sa8$Site <- NULL
#take col names from first col and give new names
sa8 <- t(sa8) #become a matrix during transposition
sa8
#turn back into dataframe again
sa8 <- data.frame(sa8)
class(sa8)

row.names(sa8)
row.names(sa8) <- str_replace_all(row.names(sa8), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa8)) #make vector sa8me name as row names

library(stringr)

time_vec <- row.names(sa8)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa8$time <- time_vec

str(sa8)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa8, type = "n",
     ylab = "Mn",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 1500))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa8$time, sa8[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa8$time, sa8[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa8$time, sa8[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa8$time, sa8[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa8$time, sa8[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa8$time, sa8[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa8$time, sa8[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 1500, "Mn", cex=2)
abline(h=50, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa8$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa8df[-ncol(b_sa8df)])) #include all except final column
box()

row.names(sa9) <- sa9$Site
sa9$Site <- NULL
#take col names from first col and give new names
sa9 <- t(sa9) #become a matrix during transposition
sa9
#turn back into dataframe again
sa9 <- data.frame(sa9)
class(sa9)

row.names(sa9)
row.names(sa9) <- str_replace_all(row.names(sa9), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa9)) #make vector sa9me name as row names

library(stringr)

time_vec <- row.names(sa9)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa9$time <- time_vec

str(sa9)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa9, type = "n",
     ylab = "B",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 1500))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa9$time, sa9[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa9$time, sa9[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa9$time, sa9[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa9$time, sa9[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa9$time, sa9[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa9$time, sa9[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa9$time, sa9[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 1500, "B", cex=2)
abline(h=1000, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa9$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa9df[-ncol(b_sa9df)])) #include all except final column
box()

row.names(sa10) <- sa10$Site
sa10$Site <- NULL
#take col names from first col and give new names
sa10 <- t(sa10) #become a matrix during transposition
sa10
#turn back into dataframe again
sa10 <- data.frame(sa10)
class(sa10)

row.names(sa10)
row.names(sa10) <- str_replace_all(row.names(sa10), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa10)) #make vector sa10me name as row names

library(stringr)

time_vec <- row.names(sa10)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa10$time <- time_vec

str(sa10)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa10, type = "n",
     ylab = "Sr",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 600))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa10$time, sa10[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa10$time, sa10[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa10$time, sa10[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa10$time, sa10[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa10$time, sa10[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa10$time, sa10[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa10$time, sa10[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 600, "Sr", cex=2) 
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa10$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa10df[-ncol(b_sa10df)])) #include all except final column
box()

row.names(sa11) <- sa11$Site
sa11$Site <- NULL
#take col names from first col and give new names
sa11 <- t(sa11) #become a matrix during transposition
sa11
#turn back into dataframe again
sa11 <- data.frame(sa11)
class(sa11)

row.names(sa11)
row.names(sa11) <- str_replace_all(row.names(sa11), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa11)) #make vector sa11me name as row names

library(stringr)

time_vec <- row.names(sa11)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa11$time <- time_vec

str(sa11)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa11, type = "n",
     ylab = "Ba",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 300))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa11$time, sa11[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa11$time, sa11[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa11$time, sa11[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa11$time, sa11[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa11$time, sa11[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa11$time, sa11[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa11$time, sa11[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 300, "Ba", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa11$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa11df[-ncol(b_sa11df)])) #include all except final column
box()

#al
row.names(sa12) <- sa12$Site
sa12$Site <- NULL
#take col names from first col and give new names
sa12 <- t(sa12) #become a matrix during transposition
sa12
#turn back into dataframe again
sa12 <- data.frame(sa12)
class(sa12)

row.names(sa12)
row.names(sa12) <- str_replace_all(row.names(sa12), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa12)) #make vector sa120me name as row names

library(stringr)

time_vec <- row.names(sa12)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa12$time <- time_vec

str(sa12)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa12, type = "n",
     ylab = "Al",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 250))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa12$time, sa12[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa12$time, sa12[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa12$time, sa12[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa12$time, sa12[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa12$time, sa12[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa12$time, sa12[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa12$time, sa12[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 250, "Al", cex=2)
abline(h=200, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa12$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa120df[-ncol(b_sa120df)])) #include all except final column
box()

#zn
row.names(sa15) <- sa15$Site
sa15$Site <- NULL
#take col names from first col and give new names
sa15 <- t(sa15) #become a matrix during transposition
sa15
#turn back into dataframe again
sa15 <- data.frame(sa15)
class(sa15)

row.names(sa15)
row.names(sa15) <- str_replace_all(row.names(sa15), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa15)) #make vector sa15me name as row names

library(stringr)

time_vec <- row.names(sa15)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa15$time <- time_vec

str(sa15)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa15, type = "n",
     ylab = "Zn",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 50))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa15$time, sa15[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa15$time, sa15[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa15$time, sa15[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa15$time, sa15[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa15$time, sa15[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa15$time, sa15[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa15$time, sa15[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 50, "Zn", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa15$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa15df[-ncol(b_sa15df)])) #include all except final column
box()

#rb
row.names(sa17) <- sa17$Site
sa17$Site <- NULL
#take col names from first col and give new names
sa17 <- t(sa17) #become a matrix during transposition
sa17
#turn back into dataframe again
sa17 <- data.frame(sa17)
class(sa17)

row.names(sa17)
row.names(sa17) <- str_replace_all(row.names(sa17), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa17)) #make vector sa17me name as row names

library(stringr)

time_vec <- row.names(sa17)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa17$time <- time_vec

str(sa17)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa17, type = "n",
     ylab = "Rb",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 20))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa17$time, sa17[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa17$time, sa17[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa17$time, sa17[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa17$time, sa17[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa17$time, sa17[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa17$time, sa17[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa17$time, sa17[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 20, "Rb", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa17$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa17df[-ncol(b_sa17df)])) #include all except final column
box()

#li
row.names(sa18) <- sa18$Site
sa18$Site <- NULL
#take col names from first col and give new names
sa18 <- t(sa18) #become a matrix during transposition
sa18
#turn back into dataframe again
sa18 <- data.frame(sa18)
class(sa18)

row.names(sa18)
row.names(sa18) <- str_replace_all(row.names(sa18), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa18)) #make vector sa18me name as row names

library(stringr)

time_vec <- row.names(sa18)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa18$time <- time_vec

str(sa18)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa18, type = "n",
     ylab = "Li",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 15))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa18$time, sa18[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa18$time, sa18[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa18$time, sa18[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa18$time, sa18[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa18$time, sa18[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa18$time, sa18[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa18$time, sa18[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 15, "Li", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa18$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa18df[-ncol(b_sa18df)])) #include all except final column
box()

#cu
row.names(sa13) <- sa13$Site
sa13$Site <- NULL
#take col names from first col and give new names
sa13 <- t(sa13) #become a matrix during transposition
sa13
#turn back into dataframe again
sa13 <- data.frame(sa13)
class(sa13)

row.names(sa13)
row.names(sa13) <- str_replace_all(row.names(sa13), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa13)) #make vector sa13me name as row names

library(stringr)

time_vec <- row.names(sa13)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa13$time <- time_vec

str(sa13)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa13, type = "n",
     ylab = "Cu",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 10))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa13$time, sa13[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa13$time, sa13[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa13$time, sa13[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa13$time, sa13[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa13$time, sa13[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa13$time, sa13[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa13$time, sa13[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 10, "Cu", cex=2)
abline(h=2, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa13$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa13df[-ncol(b_sa13df)])) #include all except final column
box()

#as
row.names(sa19) <- sa19$Site
sa19$Site <- NULL
#take col names from first col and give new names
sa19 <- t(sa19) #become a matrix during transposition
sa19
#turn back into dataframe again
sa19 <- data.frame(sa19)
class(sa19)

row.names(sa19)
row.names(sa19) <- str_replace_all(row.names(sa19), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa19)) #make vector sa19me name as row names

library(stringr)

time_vec <- row.names(sa19)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa19$time <- time_vec

str(sa19)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa19, type = "n",
     ylab = "As",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 12))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa19$time, sa19[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa19$time, sa19[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa19$time, sa19[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa19$time, sa19[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa19$time, sa19[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa19$time, sa19[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa19$time, sa19[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 12, "As", cex=2)
abline(h=10, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa19$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa19df[-ncol(b_sa19df)])) #include all except final column
box()

#ni
row.names(sa16) <- sa16$Site
sa16$Site <- NULL
#take col names from first col and give new names
sa16 <- t(sa16) #become a matrix during transposition
sa16
#turn back into dataframe again
sa16 <- data.frame(sa16)
class(sa16)

row.names(sa16)
row.names(sa16) <- str_replace_all(row.names(sa16), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa16)) #make vector sa16me name as row names

library(stringr)

time_vec <- row.names(sa16)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa16$time <- time_vec

str(sa16)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa16, type = "n",
     ylab = "Ni",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 22))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa16$time, sa16[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa16$time, sa16[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa16$time, sa16[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa16$time, sa16[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa16$time, sa16[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa16$time, sa16[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa16$time, sa16[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 22, "Ni", cex=2)
abline(h=20, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa16$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa16df[-ncol(b_sa16df)])) #include all except final column
box()

#pb

row.names(sa20) <- sa20$Site
sa20$Site <- NULL
#take col names from first col and give new names
sa20 <- t(sa20) #become a matrix during transposition
sa20
#turn back into dataframe again
sa20 <- data.frame(sa20)
class(sa20)

row.names(sa20)
row.names(sa20) <- str_replace_all(row.names(sa20), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa20)) #make vector sa20me name as row names

library(stringr)

time_vec <- row.names(sa20)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa20$time <- time_vec

str(sa20)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa20, type = "n",
     ylab = "Pb",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 12))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa20$time, sa20[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa20$time, sa20[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa20$time, sa20[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa20$time, sa20[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa20$time, sa20[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa20$time, sa20[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa20$time, sa20[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 12, "Pb", cex=2)
abline(h=10, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa20$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa20df[-ncol(b_sa20df)])) #include all except final column
box()

##mo
row.names(sa14) <- sa14$Site
sa14$Site <- NULL
#take col names from first col and give new names
sa14 <- t(sa14) #become a matrix during transposition
sa14
#turn back into dataframe again
sa14 <- data.frame(sa14)
class(sa14)

row.names(sa14)
row.names(sa14) <- str_replace_all(row.names(sa14), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa14)) #make vector sa14me name as row names

library(stringr)

time_vec <- row.names(sa14)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa14$time <- time_vec

str(sa14)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa14, type = "n",
     ylab = "Mo",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 4))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa14$time, sa14[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa14$time, sa14[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa14$time, sa14[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa14$time, sa14[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa14$time, sa14[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa14$time, sa14[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa14$time, sa14[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 4, "Mo", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa14$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa14df[-ncol(b_sa14df)])) #include all except final column
box()

row.names(sa21) <- sa21$Site
sa21$Site <- NULL
#take col names from first col and give new names
sa21 <- t(sa21) #become a matrix during transposition
sa21
#turn back into dataframe again
sa21 <- data.frame(sa21)
class(sa21)

row.names(sa21)
row.names(sa21) <- str_replace_all(row.names(sa21), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa21)) #make vector sa21me name as row names

library(stringr)

time_vec <- row.names(sa21)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa21$time <- time_vec

str(sa21)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa21, type = "n",
     ylab = "Co",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 3))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa21$time, sa21[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa21$time, sa21[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa21$time, sa21[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa21$time, sa21[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa21$time, sa21[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa21$time, sa21[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa21$time, sa21[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 3, "Co", cex=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa21$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa21df[-ncol(b_sa21df)])) #include all except final column
box()

row.names(sa22) <- sa22$Site
sa22$Site <- NULL
#take col names from first col and give new names
sa22 <- t(sa22) #become a matrix during transposition
sa22
#turn back into dataframe again
sa22 <- data.frame(sa22)
class(sa22)

row.names(sa22)
row.names(sa22) <- str_replace_all(row.names(sa22), "X", "")
#looks for pattern and give replacement
#row names defined col
#null col 

time_vec <- numeric(length = nrow(sa22)) #make vector sa22me name as row names

library(stringr)

time_vec <- row.names(sa22)
time_vec <- as.numeric(time_vec) #coerce to numeric

sa22$time <- time_vec

str(sa22)

library(tidyr)

#no of rows and cols in plot display
plot(KS02 ~ time, sa22, type = "n",
     ylab = "Cd",
     xlab = "",
     axes = F,
     xlim = c(0, 3),
     ylim = c(0, 6))
#rect(xleft = 14, ybottom = -50, xright = 21, ytop = 5000, col ="#00000033", border = "#FFFFFFFF")
for(i in 1:19){
  lines(sa22$time, sa22[, i], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa22$time, sa22[, 2], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 3], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa22$time, sa22[, 4], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 5], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 6], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 7], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 8], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 9], col = "#2E4E13", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 10], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 11], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 12], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 13], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 14], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 15], col = "#77EE15", lty = 1, lwd = 2)
  lines(sa22$time, sa22[, 16], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa22$time, sa22[, 17], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa22$time, sa22[, 18], col = "#2D882D", lty = 2, lwd = 2)
  lines(sa22$time, sa22[, 19], col = "#2D882D", lty = 2, lwd = 2)
}
text(0.1, 6, "Cd", cex=2)
abline(h=5, col="red", lty=2, lwd=2)
#axis(1, las = 1) #hash out so doesnt add x labels as defined
axis(1, at = sa22$time[-5], labels = Time) #at is where ticks going, labels printed
axis(2, las = 1)
#legend("bottomright", pt.cex = 150, cex = 0.3, col = "#00000033", lty = sort(line_type), lwd = 3.5, legend = names(b_sa22df[-ncol(b_sa22df)])) #include all except final column
box()

dev.off()