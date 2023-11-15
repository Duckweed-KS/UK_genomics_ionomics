#script to explore water ionome data

setwd("C:\\Users\\kelli\\OneDrive - The University of Nottingham\\Nottingham\\Year 2\\Duckweed ionomics analysis")

gr <- read.csv("Water_ionome.csv") #added sp loco country
gr <- read.csv("Water_ionome_aut2020change_remLOD+heli_03REPS.csv") #added sp loco country

tail(gr)
head(gr)
names(gr)

sapply(gr, class)

library(dplyr)
library(stringr)
library(ggplot2)

gr$Accession <- gr$Duckweed.collection
gr %>% select(Accession) #just displays them
unique(gr$Accession) #125 #124 when no blank
length(unique(gr$Accession,order = ascending))
length(unique(gr$Species,order = ascending))
length(unique(gr$Loco,order = ascending))

as.factor(gr$Accession)
as.factor(gr$Timepoint)
as.factor(gr$Loco)

names(gr)

#visual of data for N media elements
boxplot(B~Accession,data=gr)
boxplot(K~Accession,data=gr)
boxplot(P~Accession,data=gr)
boxplot(Ca~Accession,data=gr)
boxplot(Mg~Accession,data=gr)
boxplot(S~Accession,data=gr)
boxplot(Na~Accession,data=gr)
boxplot(Mo~Accession,data=gr)
boxplot(Mn~Accession,data=gr)
boxplot(Fe~Accession,data=gr)

#visual trace and heavy metals
boxplot(Li~Accession,data=gr)
boxplot(Ti~Accession,data=gr)
boxplot(Cr~Accession,data=gr)
boxplot(Al~Accession,data=gr)
boxplot(Si~Accession,data=gr)
boxplot(Co~Accession,data=gr)
boxplot(Ni~Accession,data=gr)
boxplot(Cu~Accession,data=gr)
boxplot(Zn~Accession,data=gr)
boxplot(As~Accession,data=gr)
boxplot(Rb~Accession,data=gr)
boxplot(Sr~Accession,data=gr)
boxplot(Cd~Accession,data=gr)
boxplot(Sn~Accession,data=gr)
boxplot(Ba~Accession,data=gr)
boxplot(Pb~Accession,data=gr)

#by loco
boxplot(B~Loco,data=gr)
boxplot(K~Loco,data=gr)
boxplot(P~Loco,data=gr)
boxplot(Ca~Loco,data=gr)
boxplot(Mg~Loco,data=gr)
boxplot(S~Loco,data=gr)
boxplot(Na~Loco,data=gr)
boxplot(Mo~Loco,data=gr)
boxplot(Mn~Loco,data=gr)
boxplot(Fe~Loco,data=gr)

#visual trace and heavy metals
boxplot(Li~Loco,data=gr)
boxplot(Ti~Loco,data=gr)
boxplot(Cr~Loco,data=gr)
boxplot(Al~Loco,data=gr)
boxplot(Si~Loco,data=gr)
boxplot(Co~Loco,data=gr)
boxplot(Ni~Loco,data=gr)
boxplot(Cu~Loco,data=gr)
boxplot(Zn~Loco,data=gr)
boxplot(As~Loco,data=gr)
boxplot(Rb~Loco,data=gr)
boxplot(Sr~Loco,data=gr)
boxplot(Cd~Loco,data=gr)
boxplot(Sn~Loco,data=gr)
boxplot(Ba~Loco,data=gr)
boxplot(Pb~Loco,data=gr)

#sig effect of accession on ions?
aov <- aov(B~Loco*Accession,data=gr)
summary(aov) #*** sig loco, ns access
aov <- aov(K~Loco*Accession,data=gr)
summary(aov) #** sig loco, access *
aov <- aov(P~Loco*Accession,data=gr)
summary(aov) #*** sig loco, access **
aov <- aov(Ca~Loco*Accession,data=gr)
summary(aov) #*** sig,loco ***
aov <- aov(Mg~Loco*Accession,data=gr)
summary(aov) #*** sig loco, *** access
aov <- aov(S~Loco*Accession,data=gr)
summary(aov) #*** sig loco, access ***
aov <- aov(Na~Loco*Accession,data=gr)
summary(aov) #*** sig loco, *** access
aov <- aov(Mo~Loco*Accession,data=gr)
summary(aov) #ns 
aov <- aov(Mn~Loco*Accession,data=gr)
summary(aov) #*** sig loco, access ***
aov <- aov(Fe~Loco*Accession,data=gr)
summary(aov) #*** sig loco, access ***

#sig affect Loco*Accessions on trace?
aov <- aov(Li~Loco*Accession,data=gr)
summary(aov) #*** sig loco, access ***
aov <- aov(Ti~Loco*Accession,data=gr)
summary(aov) #*** loco, access ns
aov <- aov(Cr~Loco*Accession,data=gr)
summary(aov) #loco ns, access **
aov <- aov(Al~Loco*Accession,data=gr)
summary(aov) #*** loco, *** access
aov <- aov(Si~Loco*Accession,data=gr)
summary(aov) #*** loco, *** access
aov <- aov(Co~Loco*Accession,data=gr)
summary(aov) #*** sig loco, access ***
aov <- aov(Ni~Loco*Accession,data=gr)
summary(aov) #*** sig access, loco ***
aov <- aov(Cu~Loco*Accession,data=gr)
summary(aov) #*** loco, access ns
aov <- aov(Zn~Loco*Accession,data=gr)
summary(aov) #*** loco, *** access
aov <- aov(As~Loco*Accession,data=gr)
summary(aov) #*** loco, *** access
aov <- aov(Rb~Loco*Accession,data=gr)
summary(aov) #*** loco, *** access
aov <- aov(Sr~Loco*Accession,data=gr)
summary(aov) #*** loco sig, *** access
aov <- aov(Cd~Loco*Accession,data=gr)
summary(aov) #*** loco sig, access ***
aov <- aov(Sn~Loco*Accession,data=gr)
summary(aov) #* loco, ns access
aov <- aov(Ba~Loco*Accession,data=gr)
summary(aov) #*** loco, *** access
aov <- aov(Pb~Loco*Accession,data=gr)
summary(aov) #*** loco, *** access


#filters to seperate experiments
contam <- gr %>% filter(Timepoint == "Autumn_2020")
contam1 <- gr %>% filter(Timepoint == "Autumn_2021")
contam2 <- gr %>% filter(Timepoint == "Summer_2021")
contam3 <- gr %>% filter(Timepoint == "Spring_2022")
others <- gr %>% filter(Timepoint == "Spring_2021")

core <- rbind(contam, contam1, contam2, contam3)

#to split analysis
gr <- others
gr <- core

#check levels of loco
gr$Loco
gr$Loco <- factor(gr$Loco,levels = c("ABE", "BFD", "BRI", "COR", "ELG",
                                     "GLA", "HAS", "HUL", "NEW", "YOR"))

#could use this to visualise core or others
#visual of data for N media elements
boxplot(B.11_b~Loco,data=gr)
boxplot(K.39_b~Loco,data=gr)
boxplot(P.31_b~Loco,data=gr)
boxplot(Ca.43_b~Loco,data=gr)
boxplot(Mg.24_b~Loco,data=gr)
boxplot(S.34_b~Loco,data=gr)
boxplot(Na.23_b~Loco,data=gr)
boxplot(Mo.98_b~Loco,data=gr)
boxplot(Mn.55_b~Loco,data=gr) #interesting
boxplot(Fe.56_b~Loco,data=gr)

#visual trace and heavy metals
boxplot(Li.7_b~Loco,data=gr)
boxplot(Ti.47_b~Loco,data=gr)
boxplot(Cr.52_b~Loco,data=gr)
boxplot(Al.27_b~Loco,data=gr)
boxplot(Si.29_b~Loco,data=gr)
boxplot(Co.59_b~Loco,data=gr)
boxplot(Ni.60_b~Loco,data=gr)
boxplot(Cu.63_b~Loco,data=gr)
boxplot(Zn.66_b~Loco,data=gr)
boxplot(As.75_b~Loco,data=gr)
boxplot(Rb.85_b~Loco,data=gr)
boxplot(Sr.88_b~Loco,data=gr)
boxplot(Cd.111_b~Loco,data=gr)
boxplot(Sn.120_b~Loco,data=gr)
boxplot(Ba.135_b~Loco,data=gr)
boxplot(Pb.208_b~Loco,data=gr)

#sig effect of Loco on ions?
aov <- aov(B.11_b~Loco,data=gr)
summary(aov) #*** sig
#test to check if NAs included, not
length(unique(gr$Loco,order = ascending))
tuk_out <- TukeyHSD(aov, "Loco", conf.level=.95) 
tuk_out
aov <- aov(K.39_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(P.31_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Ca.43_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Mg.24_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(S.34_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Na.23_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Mo.98_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Mn.55_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Fe.56_b~Loco,data=gr)
summary(aov) #** sig

#sig affect Locos on trace?
aov <- aov(Li.7_b~Loco,data=gr)
summary(aov) #* sig
aov <- aov(Ti.47_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Cr.52_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Al.27_b~Loco,data=gr)
summary(aov) #NS
aov <- aov(Si.29_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Co.59_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Ni.60_b~Loco,data=gr)
summary(aov) #** sig
aov <- aov(Cu.63_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Zn.66_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(As.75_b~Loco,data=gr)
summary(aov) #** sig
aov <- aov(Rb.85_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Sr.88_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Cd.111_b~Loco,data=gr)
summary(aov) #* NS
aov <- aov(Sn.120_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Ba.135_b~Loco,data=gr)
summary(aov) #*** sig
aov <- aov(Pb.208_b~Loco,data=gr)
summary(aov) #NS

#<2e-16 for all ions for accession apart from li not as sig
#for loco most ions ***, fe **, ni **?, cd pb and al NS

#add species and locations, see sig effects
#test loco accession species together?
#formula only works if nested level no eg small to larg sp > lo > acc
#+ variables computes quicker than * variables
aov <- aov(B.11_b~Species+Loco+Accession,data=gr) #all ***
summary(aov) #*** sig
aov <- aov(K.39_b~Species+Loco+Accession,data=gr) #all ***
summary(aov) #*** sig
aov <- aov(P.31_b~Species+Loco+Accession,data=gr) #all ***
summary(aov) #*** sig
aov <- aov(Ca.43_b~Species+Loco+Accession,data=gr) #all ***
summary(aov) #*** sig
aov <- aov(Mg.24_b~Species+Loco+Accession,data=gr) #all ***
summary(aov) #*** sig
aov <- aov(S.34_b~Species+Loco+Accession,data=gr) #all ***
summary(aov) #*** sig
aov <- aov(Na.23_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Mo.98_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Mn.55_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Fe.56_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig

#sig affect Species+Loco+Accessions on trace?
aov <- aov(Li.7_b~Species+Loco+Accession,data=gr)
summary(aov) #loco ** sig, accession * sig
aov <- aov(Ti.47_b~Species+Loco+Accession,data=gr)
summary(aov) #*** loco, ** sp, * access sig
aov <- aov(Cr.52_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Al.27_b~Species+Loco+Accession,data=gr)
summary(aov) #accession *** sig, others not
aov <- aov(Si.29_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Co.59_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Ni.60_b~Species+Loco+Accession,data=gr)
summary(aov) #** sig
aov <- aov(Cu.63_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Zn.66_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(As.75_b~Species+Loco+Accession,data=gr)
summary(aov) #*** loco and access sig, sp not
aov <- aov(Rb.85_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Sr.88_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Cd.111_b~Species+Loco+Accession,data=gr)
summary(aov) #*** loco access sig, sp not sig
aov <- aov(Sn.120_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sp and loco sig, access not sig
aov <- aov(Ba.135_b~Species+Loco+Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Pb.208_b~Species+Loco+Accession,data=gr)
summary(aov) #*** loco and access sig, sp not sig

#visualising data
#pca
ions_all <- gr
str(ions_all)
ions_all$Loco

# turn accession into factor and classify data to include
#ions_all$Loco <- factor(ions_all$Loco, levels=c("ABE", "BFD", "BRI", "COR", "ELG",
#                                                "GLA", "HAS", "HUL", "NEW", "YOR"))
                                                    
library(FactoMineR)
#ions_acc <- ions_all[ ,c(4:30)] # selecting columns from csv
#ions_acc.pca <- PCA(ions_acc, quali.sup=27) #cant exceed max col number

#no LOD, use all data so bfd york hull inc
ions_acc <- ions_all[ ,c(4:26)] # selecting columns from csv
ions_acc.pca <- PCA(ions_acc, quali.sup=23)
print(ions_acc.pca)
head(ions_acc.pca)
print(summary(ions_acc.pca)) #shows pc contirbutions of eigens
#write.csv(ions_acc.pca, ions_app_pca_data.csv) # doesnt work
#18% AND 15% 1 AND 2, 9 AND 7% 3 AND 4
#26 and 14% lod removed, just spring data
#21 and 17% lod remove, all data


#FOR VARIABLE COS 2 GRAPH
#plot cos 2 as bar graph #high = good representation on pc
library(FactoMineR)
library(factoextra)

#basic plot
biplot(ions_acc.pca) # not working

fviz_pca_biplot(ions_acc.pca, repel=TRUE, pointsize=6, pointshape=21, col.var="black", arrowsize=0.6, labelsize=5, col.ind=ions_all$Loco, palette=c("green2", "gold", "red", "blue", "orange",
                                                                                                                                                    "pink", "darkgreen", "purple", "darkred", "brown"), addEllipses=TRUE, ellipse.type="confidence")

fviz_cos2(ions_acc.pca, choice = "var", axes = 1:2) #mg, s, ca, mo, li top 5

fviz_pca_var(ions_acc.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

library(ggplot2)
plot1 <- fviz_pca_var(ions_acc.pca, col.var = "cos2",
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE)
plot1 + xlab("PC1 (21%)") + ylab("PC2 (17%)") +
  ggtitle(NULL)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#save plot 1
ggsave("Water_ionome_biplot_remLOD_alldata_edit.png", dpi = 300, width = 15, height = 15 , units = "cm") 


#do this first to get pc columns, then visualise variable
#problem with column 25 Ba includes NAs so not including it
ions_acc.pca<-prcomp(ions_acc,center=T,scale=T) #list can include mix mat and df

#rem lod all data, remove ba
ions_acc.pca<-prcomp(ions_acc[,1:21,23],center=T,scale=T) #list can include mix mat and df

#rem lod, just spring data
ions_acc.pca<-prcomp(ions_acc[ ,2:24,26],center=T,scale=T) #list can include mix mat and df
str(ions_acc.pca)
mypc <- ions_acc.pca$x #define new variable x = pcs just need to plot these on xy graph

#plot locos
plot(mypc[,1], mypc[,2], col = my_pal,
     las=1, xlab="PC1 (26%)", ylab="PC2 (15%)",
     pch=16, cex=1.5, xlim=c(-30,30), ylim=c(-30,30)) #pcs as columns, produce xy plot, las is rotation of axis numbers, pch plot shape, ylim expand out so legend room
abline(v=0, lty=2, col="lightgrey") #draw line, lty is segmented
abline(h=0, lty=2, col="lightgrey") #0 lines dashed
legend("right", pch=16, col=my_pal, cex=1, c("ABE", "BFD", "BRI", "COR", "ELG",
                                             "GLA", "HAS", "HUL",
                                             "NEW", "YOR")) #customise legend seperately cex=txtsize cols 1-4 stnd, concat order of places as want to display
#too busy
#text(x=mypc[,1],y=mypc[,2], labels =ions_all$Loco, pos=2) #txt and pos to define labels on plot, 2 = top of plot, 1 bottom

#need more than 10 colors as repeating
my_pal <- scico::scico(length(unique(ions_all$Loco)), palette = "lisbon")

#try old MK code
ion <- gr
#working with already blanked data

class(ion)

#rem loq, all remps and rem just ba
sub <- ion[, c(2, 4:24, 26)] #without accessions

#rem loq and other reps
sub <- ion[, c(2, 4:26)] #without accessions
sub <- ion[, c(2, 4:27)] #with accessions

sub <- ion[, c(2, 4:29)] #all rows, just cols range plus one wanted

lapply(sub, as.numeric)
#try manually remove 1st and last and Ba col
sub <- sub[, c(1:25,27)]
View(sub)

library(dplyr)

#Var2 #Loco variables
#sub(1:26)
str(head(sub))
#look for na's
apply(sub,2, function(x) is.na(x)) #all false
sapply(is.finite(sub)) # try to kill infinite vals, not working
indx <- apply(sub, 2, function(x) any(is.na(x) %>% is.infinite(x)))
colnames[indx]
indx # all false
is.infinite(sub) # cant run on lists
class(sub) # object is a df not a list?

#NEW CENTROID PLOTS NEW  COLORING
#no loq all reps, no ba
par(mfrow = c(1, 1))
iris_pca <- prcomp(sub[, 2:23], center = T, scale. = T)

#BEFORE LOQ?
#iris_pca <- prcomp(sub[, 2:26], center = T, scale. = T)

scores <- iris_pca$x
scores <- scores %>% data.frame()

scores$Loco <- sub$Loco
centroids <- scores %>% group_by(Loco) %>% summarise_all("mean")

tiff('Water_loco_PCA_remLOQ_newcolord.tiff', units="in", width=7, height=6, res=300, compression = 'lzw')
plot(scores[, 1], scores[, 2], type = "n",
     las=1, xlab="PC1 (21%)", ylab="PC2 (17%)",
     cex=1.5, xlim=c(-17,17), ylim=c(-17,17))
abline(h = 0, col = "lightgrey", lty = 2)
abline(v = 0, col = "lightgrey", lty = 2)
points(centroids[centroids$Loco == "ELG", 2], centroids[centroids$Loco == "ELG", 3], pch = 3, lwd = 2, cex = 5, col = "#632770")
points(centroids[centroids$Loco == "ABE", 2], centroids[centroids$Loco == "ABE", 3], pch = 3, lwd = 2, cex = 5, col = "#2915B4")
points(centroids[centroids$Loco == "GLA", 2], centroids[centroids$Loco == "GLA", 3], pch = 3, lwd = 2, cex = 5, col = "#3260B4")
points(centroids[centroids$Loco == "BFD", 2], centroids[centroids$Loco == "BFD", 3], pch = 3, lwd = 2, cex = 5, col = "#2E4E13")
points(centroids[centroids$Loco == "YOR", 2], centroids[centroids$Loco == "YOR", 3], pch = 3, lwd = 2, cex = 5, col = "#2D882D")
points(centroids[centroids$Loco == "HUL", 2], centroids[centroids$Loco == "HUL", 3], pch = 3, lwd = 2, cex = 5, col = "#77EE15")
points(centroids[centroids$Loco == "NEW", 2], centroids[centroids$Loco == "NEW", 3], pch = 3, lwd = 2, cex = 5, col = "#FFA256")
points(centroids[centroids$Loco == "BRI", 2], centroids[centroids$Loco == "BRI", 3], pch = 3, lwd = 2, cex = 5, col = "#FF4E32")
points(centroids[centroids$Loco == "HAS", 2], centroids[centroids$Loco == "HAS", 3], pch = 3, lwd = 2, cex = 5, col = "#AA3939")
points(centroids[centroids$Loco == "COR", 2], centroids[centroids$Loco == "COR", 3], pch = 3, lwd = 2, cex = 5, col = "#7D0900")

points(scores[sub$Loco == "ELG", 1], scores[sub$Loco == "ELG", 2], pch= 16, col = "#632770")
points(scores[sub$Loco == "ABE", 1], scores[sub$Loco == "ABE", 2], pch= 16, col = "#2915B4")
points(scores[sub$Loco == "GLA", 1], scores[sub$Loco == "GLA", 2], pch= 17, col = "#3260B4")
points(scores[sub$Loco == "BFD", 1], scores[sub$Loco == "BFD", 2], pch= 16, col = "#2E4E13")
points(scores[sub$Loco == "YOR", 1], scores[sub$Loco == "YOR", 2], pch= 16, col = "#2D882D")
points(scores[sub$Loco == "HUL", 1], scores[sub$Loco == "HUL", 2], pch= 17, col = "#77EE15")
points(scores[sub$Loco == "NEW", 1], scores[sub$Loco == "NEW", 2], pch= 16, col = "#FFA256")
points(scores[sub$Loco == "BRI", 1], scores[sub$Loco == "BRI", 2], pch= 16, col = "#FF4E32")
points(scores[sub$Loco == "HAS", 1], scores[sub$Loco == "HAS", 2], pch= 17, col = "#AA3939")
points(scores[sub$Loco == "COR", 1], scores[sub$Loco == "COR", 2], pch= 16, col = "#7D0900")
legend("topright", inset=.05, y.intersp=0.4, text.width=0.3, pch =c(16,16,17,16,16,17,16,16,17,16), col=c("#632770", "#2915B4", "#3260B4", "#2E4E13", "#2D882D", "#77EE15","#FFA256", "#FF4E32", "#AA3939", "#7D0900"), cex=1.2, c("ELG","ABE","GLA","BFD","YOR", "HUL","NEW","BRI", "HAS", "COR"), bty="n")
#legend("bottomright", pch=16, col=c("black", "red", "blue", "green", "purple", "brown", "orange", "pink", "gold", "grey"), cex=0.5, c("COR", "BRI", "HAS", "NEW", "GLA", "ABE", "ELG", "BFD", "YOR", "HUL"), bty="n")
dev.off()

#ZOOMED IN CENTROID
tiff('Water_loco_PCA_remLOQ_newcolord_zoomed.tiff', units="in", width=7, height=6, res=300, compression = 'lzw')
plot(scores[, 1], scores[, 2], type = "n",
     las=1, xlab="PC1 (21%)", ylab="PC2 (17%)",
     cex=1.5, xlim=c(-3,5), ylim=c(-3,5))
abline(h = 0, col = "lightgrey", lty = 2)
abline(v = 0, col = "lightgrey", lty = 2)
points(centroids[centroids$Loco == "ELG", 2], centroids[centroids$Loco == "ELG", 3], pch = 3, lwd = 2, cex = 5, col = "#632770")
points(centroids[centroids$Loco == "ABE", 2], centroids[centroids$Loco == "ABE", 3], pch = 3, lwd = 2, cex = 5, col = "#2915B4")
points(centroids[centroids$Loco == "GLA", 2], centroids[centroids$Loco == "GLA", 3], pch = 3, lwd = 2, cex = 5, col = "#3260B4")
points(centroids[centroids$Loco == "BFD", 2], centroids[centroids$Loco == "BFD", 3], pch = 3, lwd = 2, cex = 5, col = "#2E4E13")
points(centroids[centroids$Loco == "YOR", 2], centroids[centroids$Loco == "YOR", 3], pch = 3, lwd = 2, cex = 5, col = "#2D882D")
points(centroids[centroids$Loco == "HUL", 2], centroids[centroids$Loco == "HUL", 3], pch = 3, lwd = 2, cex = 5, col = "#77EE15")
points(centroids[centroids$Loco == "NEW", 2], centroids[centroids$Loco == "NEW", 3], pch = 3, lwd = 2, cex = 5, col = "#FFA256")
points(centroids[centroids$Loco == "BRI", 2], centroids[centroids$Loco == "BRI", 3], pch = 3, lwd = 2, cex = 5, col = "#FF4E32")
points(centroids[centroids$Loco == "HAS", 2], centroids[centroids$Loco == "HAS", 3], pch = 3, lwd = 2, cex = 5, col = "#AA3939")
points(centroids[centroids$Loco == "COR", 2], centroids[centroids$Loco == "COR", 3], pch = 3, lwd = 2, cex = 5, col = "#7D0900")

points(scores[sub$Loco == "ELG", 1], scores[sub$Loco == "ELG", 2], pch= 16, col = "#632770")
points(scores[sub$Loco == "ABE", 1], scores[sub$Loco == "ABE", 2], pch= 16, col = "#2915B4")
points(scores[sub$Loco == "GLA", 1], scores[sub$Loco == "GLA", 2], pch= 17, col = "#3260B4")
points(scores[sub$Loco == "BFD", 1], scores[sub$Loco == "BFD", 2], pch= 16, col = "#2E4E13")
points(scores[sub$Loco == "YOR", 1], scores[sub$Loco == "YOR", 2], pch= 16, col = "#2D882D")
points(scores[sub$Loco == "HUL", 1], scores[sub$Loco == "HUL", 2], pch= 17, col = "#77EE15")
points(scores[sub$Loco == "NEW", 1], scores[sub$Loco == "NEW", 2], pch= 16, col = "#FFA256")
points(scores[sub$Loco == "BRI", 1], scores[sub$Loco == "BRI", 2], pch= 16, col = "#FF4E32")
points(scores[sub$Loco == "HAS", 1], scores[sub$Loco == "HAS", 2], pch= 17, col = "#AA3939")
points(scores[sub$Loco == "COR", 1], scores[sub$Loco == "COR", 2], pch= 16, col = "#7D0900")
#legend("topright", inset=.07, y.intersp=1.0, text.width=0.5, pch =c(16,16,17,16,16,17,16,16,17,16), col=c("#632770", "#2915B4", "#3260B4", "#2E4E13", "#2D882D", "#77EE15",                                                                                                      "#FFA256", "#FF4E32", "#AA3939", "#7D0900"), cex=1.2, c("ELG","ABE","GLA","BFD","YOR", "HUL","NEW","BRI", "HAS", "COR"), bty="n")
#legend("bottomright", pch=16, col=c("black", "red", "blue", "green", "purple", "brown", "orange", "pink", "gold", "grey"), cex=0.5, c("COR", "BRI", "HAS", "NEW", "GLA", "ABE", "ELG", "BFD", "YOR", "HUL"), bty="n")
dev.off()

#OLD CENTROID PLOTS OLD COLORING
#no loq all reps, no ba
par(mfrow = c(1, 1))
iris_pca <- prcomp(sub[, 2:23], center = T, scale. = T)

#no loq just spring
par(mfrow = c(1, 1))
iris_pca <- prcomp(sub[, 2:24], center = T, scale. = T)

iris_pca <- prcomp(sub[, 2:26], center = T, scale. = T)

scores <- iris_pca$x
scores <- scores %>% data.frame()

scores$Loco <- sub$Loco
centroids <- scores %>% group_by(Loco) %>% summarise_all("mean")

tiff('Water_loco_PCA_remLOQ_alldata.tiff', units="in", width=7, height=6, res=300, compression = 'lzw')
plot(scores[, 1], scores[, 2], type = "n",
     las=1, xlab="PC1 (21%)", ylab="PC2 (17%)",
     cex=1.5, xlim=c(-17,17), ylim=c(-17,17))
abline(h = 0, col = "lightgrey", lty = 2)
abline(v = 0, col = "lightgrey", lty = 2)

points(centroids[centroids$Loco == "COR", 2], centroids[centroids$Loco == "COR", 3], pch = 3, lwd = 2, cex = 5)
points(centroids[centroids$Loco == "BRI", 2], centroids[centroids$Loco == "BRI", 3], pch = 3, lwd = 2, cex = 5, col = "red")
points(centroids[centroids$Loco == "HAS", 2], centroids[centroids$Loco == "HAS", 3], pch = 3, lwd = 2, cex = 5, col = "blue")
points(centroids[centroids$Loco == "NEW", 2], centroids[centroids$Loco == "NEW", 3], pch = 3, lwd = 2, cex = 5, col = "green")
points(centroids[centroids$Loco == "GLA", 2], centroids[centroids$Loco == "GLA", 3], pch = 3, lwd = 2, cex = 5, col = "purple")
points(centroids[centroids$Loco == "ABE", 2], centroids[centroids$Loco == "ABE", 3], pch = 3, lwd = 2, cex = 5, col = "brown")
points(centroids[centroids$Loco == "ELG", 2], centroids[centroids$Loco == "ELG", 3], pch = 3, lwd = 2, cex = 5, col = "orangered1")
points(centroids[centroids$Loco == "BFD", 2], centroids[centroids$Loco == "BFD", 3], pch = 3, lwd = 2, cex = 5, col = "violetred1")
points(centroids[centroids$Loco == "YOR", 2], centroids[centroids$Loco == "YOR", 3], pch = 3, lwd = 2, cex = 5, col = "gold")
points(centroids[centroids$Loco == "HUL", 2], centroids[centroids$Loco == "HUL", 3], pch = 3, lwd = 2, cex = 5, col = "grey")

points(scores[sub$Loco == "COR", 1], scores[sub$Loco == "COR", 2], pch= 1)
points(scores[sub$Loco == "BRI", 1], scores[sub$Loco == "BRI", 2], pch= 1, col = "red")
points(scores[sub$Loco == "HAS", 1], scores[sub$Loco == "HAS", 2], pch= 1, col = "blue")
points(scores[sub$Loco == "NEW", 1], scores[sub$Loco == "NEW", 2], pch= 1, col = "green")
points(scores[sub$Loco == "GLA", 1], scores[sub$Loco == "GLA", 2], pch= 1, col = "purple")
points(scores[sub$Loco == "ABE", 1], scores[sub$Loco == "ABE", 2], pch= 1, col = "brown")
points(scores[sub$Loco == "ELG", 1], scores[sub$Loco == "ELG", 2], pch= 1, col = "orangered1")
points(scores[sub$Loco == "BFD", 1], scores[sub$Loco == "BFD", 2], pch= 1, col = "violetred1")
points(scores[sub$Loco == "YOR", 1], scores[sub$Loco == "YOR", 2], pch= 1, col = "gold")
points(scores[sub$Loco == "HUL", 1], scores[sub$Loco == "HUL", 2], pch= 1, col = "grey")
legend("topright", inset=.07, y.intersp=1.0, text.width=0.5, pch = 1, col=c("black", "red", "blue", "green", "purple", "brown", "orangered1", "violetred1", "gold", "grey"), cex=1.2, c("COR", "BRI", "HAS", "NEW", "GLA", "ABE", "ELG", "BFD", "YOR", "HUL"), bty="n")
#legend("bottomright", pch=16, col=c("black", "red", "blue", "green", "purple", "brown", "orange", "pink", "gold", "grey"), cex=0.5, c("COR", "BRI", "HAS", "NEW", "GLA", "ABE", "ELG", "BFD", "YOR", "HUL"), bty="n")
dev.off()

#prep for heatmaps and radars
#to get zscores first
#prep
library(dplyr)
#split into meta data and numerical
ions_num <- ions_all %>% dplyr::select(-Duckweed.collection, -Loco, -Timepoint)

#do by index
ions_num <- ions_all[,c(4:26)]

#zscores
#avg ions
ions_num <- as.matrix(ions_num)
avgs <- colMeans(ions_num)
avgs <- as.data.frame(avgs)

ions_num <- as.data.frame(ions_num)

as.matrix(ions_s)
as.data.frame(ions_s)


#try scale data works for z scores?
scaled.dat <- scale(ions_num)
#zscores achieved 2 ways ions_s using func above or
#using raw data
ions_sc <- scale(ions_num,center=T,scale=T)
#same as scaled.dat

#radar plots not available on r 3.6.3

#heat map tutorial
library(RColorBrewer)
#scale numeric data
ions_all <- ions_all[10:61]

#run scaled data
ions_sc <- scale(ions_all[4:26],center=T,scale=T)
ions_sc <- as.matrix(ions_sc)

#assign row names from df
rownames(ions_sc) <- ions_in$Accession
#or
rownames(ions_sc) <- ions_all$Duckweed.collection

#raw data
heatmap(ions_sc)
heatmap(ions_sc, Colv = NA, Rowv = NA, scale="column")
#change color scheme yellow blue
colfunc <- colorRampPalette(c("blue", "yellow"))
heatmap(ions_sc,col=colfunc(11),scale="row")

#looks different when add scale=column, removes branches
#need to add names of accessions to plot, not working?

#summarise means avgs sd max

#summary raw
gr$Accession <- gr$Duckweed.collection
Summary <- gr %>%
  group_by(Accession, Loco) %>%
  summarise(Li_mean = mean(Li), Li_stdev = sd(Li), n= n(), Li_maximum = max(Li),
            B_mean = mean(B), B_stdev = sd(B), B_maximum = max(B),
            Na_mean = mean(Na), Na_stdev = sd(Na), Na_maximum = max(Na),
            Mg_mean = mean(Mg), Mg_stdev = sd(Mg), Mg_maximum = max(Mg),
            Al_mean = mean(Al), Al_stdev = sd(Al), Al_maximum = max(Al),
            Si_mean = mean(Si), Si_stdev = sd(Si), Si_maximum = max(Si),
            P_mean = mean(P), P_stdev = sd(P), P_maximum = max(P),
            S_mean = mean(S), S_stdev = sd(S), S_maximum = max(S),
            K_mean = mean(K), K_stdev = sd(K), K_maximum = max(K),
            Ca_mean = mean(Ca), Ca_stdev = sd(Ca), Ca_maximum = max(Ca),
            #Ti_mean = mean(Ti), Ti_stdev = sd(Ti), Ti_maximum = max(Ti),
            #Cr_mean = mean(Cr), Cr_stdev = sd(Cr), Cr_maximum = max(Cr),
            Mn_mean = mean(Mn), Mn_stdev = sd(Mn), Mn_maximum = max(Mn),
            Fe_mean = mean(Fe), Fe_stdev = sd(Fe), Fe_maximum = max(Fe),
            Co_mean = mean(Co), Co_stdev = sd(Co), Co_maximum = max(Co),
            Ni_mean = mean(Ni), Ni_stdev = sd(Ni), Ni_maximum = max(Ni),
            Cu_mean = mean(Cu), Cu_stdev = sd(Cu), Cu_maximum = max(Cu),
            Zn_mean = mean(Zn), Zn_stdev = sd(Zn), Zn_maximum = max(Zn),
            As_mean = mean(As), As_stdev = sd(As), As_maximum = max(As),
            Rb_mean = mean(Rb), Rb_stdev = sd(Rb), Rb_maximum = max(Rb),
            Sr_mean = mean(Sr), Sr_stdev = sd(Sr), Sr_maximum = max(Sr),
            Mo_mean = mean(Mo), Mo_stdev = sd(Mo), Mo_maximum = max(Mo),
            Cd_mean = mean(Cd), Cd_stdev = sd(Cd), Cd_maximum = max(Cd),
            #Sn_mean = mean(Sn), Sn_stdev = sd(Sn), Sn_maximum = max(Sn),
            #Ba_mean = mean(Ba, na.rm=TRUE), Ba_stdev = sd(Ba, na.rm=TRUE), Ba_maximum = max(Ba, na.rm=TRUE),
            Pb_mean = mean(Pb), Pb_stdev = sd(Pb), Pb_maximum = max(Pb)
  )
Summary

#write.csv(Summary, "Water_ionomics_summary.csv")
#write.csv(Summary, "Water_ionomics_summary_ba_naomit.csv")
write.csv(Summary, "Water_ionomics_summary_remloq_alldata_ba_naomit.csv")

#summary just get means
names(Summary)
avg_cols <- stringr::str_detect(names(Summary), pattern = "mean") %>% which()
#if contain SD read indicies columns
SD_cols <- stringr::str_detect(names(Summary), pattern = "stdev") %>% which()
#if contain SD read indicies columns
max_cols <- stringr::str_detect(names(Summary), pattern = "maximum") %>% which()
#if contain SD read indicies columns

Summary <- Summary[, avg_cols] #just means
#write.csv(Summary, "Water_summary_justmeans.csv")
#write.csv(Summary, "Water_summary_justmeans_banaomit.csv")
write.csv(Summary, "Water_summary_justmeans_banaomit_loqrem_alldata.csv")

#using average per accession means
#use mean data to plot heatmap
#named to can be plotted with neat names, loco and acc added in
#ions_in <- read.csv("Water_summary_justmeans_rname.csv")
#ions_in <- read.csv("Water_summary_justmeans_banaomit_rname.csv")
ions_in <- read.csv("Water_summary_justmeans_banaomit_loqrem_alldata_rname.csv")

#to edit for newest
ions_all <- ions_in[3:24]

#group by ion type
#macro <- ions_in[c(1,4,5:6,8,9:12,15:16)] #> 100 mg/g
#trace <- ions_in[c(1,19:20,24)]
heavy <- ions_in[c(1,3,7,14:21,23,24)]
nmedia <- ions_in[c(1,4,5,6,8:13,22)]

#remove observations = nas in barium?
#nmedia <- nmedia[-c(28:29,31:32,37:38),]

#just compare whats in n media variation as supplied
ions_all <- nmedia[2:11]
ions_sc <- scale(ions_all,center=T,scale=T)
ions_sc <- as.matrix(ions_sc)
rownames(ions_sc) <- nmedia$Accession

#just compare whats in heavy variation as supplied
ions_all <- heavy[2:12]
ions_sc <- scale(ions_all,center=T,scale=T)
ions_sc <- as.matrix(ions_sc)
rownames(ions_sc) <- heavy$Accession

write.csv(ions_sc, "Water_ionome_zscores_noLODba.csv")
write.csv(ions_sc, "Water_heavy_ionome_zscores_noLODba.csv")

heatmap(ions_sc)
heatmap(ions_sc, Colv = NA, Rowv = NA, scale="column")
heatmap(ions_sc, Colv = NA, Rowv = NA, scale="row")
#minimized version
heatmap(ions_sc,col=colfunc(11),scale="row", margins=c(5,10))

pdf("Water_ionome_Nmedia_avgs_heatmap_big_noLODba.pdf", height=20, width=20)
pdf("Water_ionome_Heavy_avgs_heatmap_big_noLODba.pdf", height=20, width=20)
colfunc <- colorRampPalette(c("blue", "yellow"))
heatmap(ions_sc,col=colfunc(11),scale="row", cexCol=4, margins=c(5,10))
dev.off()
#dev.new()
