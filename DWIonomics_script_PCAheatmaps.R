#script to explore duckweed ionomics data 

setwd("C:\\Users\\kelli\\OneDrive - The University of Nottingham\\Nottingham\\Year 2\\Duckweed ionomics analysis")

gr <- read.csv("DW_ionomics_tosummarise.csv")
gr <- read.csv("DW_ionomics_tosummarise_remB.csv") #remove blanks man

#newest version, need to change species from genomics
gr <- read.csv("DW_ionomics_tosummarise_remB_sp_loco.csv") #added sp loco country
#need to remove elements below LOD

tail(gr)
head(gr)
names(gr)

sapply(gr, class)

library(dplyr)
library(stringr)
library(ggplot2)

gr %>% select(Accession) #just displays them
unique(gr$Accession) #125 #124 when no blank
length(unique(gr$Accession,order = ascending))
length(unique(gr$Species,order = ascending))
length(unique(gr$Loco,order = ascending))

as.factor(gr$Accession)
as.factor(gr$Species)
as.factor(gr$Loco)
as.factor(gr$Country)
as.factor(gr$Rep)
as.factor(gr$Run)

names(gr)

#ADD KS TO NAMES MANUALLY
#remove blank row from all manually or
gr <- gr %>% mutate(Li.7_b = (Li.7-0.0025))
gr <- gr %>% mutate(B.11_b = (B.11-12.70425))
gr <- gr %>% mutate(Na.23_b = (Na.23-6.58925))
gr <- gr %>% mutate(Mg.24_b = (Mg.24-1.164))
gr <- gr %>% mutate(Al.27_b = (Al.27-1.2565))
gr <- gr %>% mutate(Si.29_b = (Si.29-73.21325))
gr <- gr %>% mutate(P.31_b = (P.31--2.61775))
gr <- gr %>% mutate(S.34_b = (S.34-174.85775))
gr <- gr %>% mutate(K.39_b = (K.39-20.15225))
gr <- gr %>% mutate(Ca.43_b = (Ca.43-11.41))
gr <- gr %>% mutate(Ti.47_b = (Ti.47-0.006))
gr <- gr %>% mutate(Cr.52_b = (Cr.52--0.0045))
gr <- gr %>% mutate(Mn.55_b = (Mn.55-0.14375))
gr <- gr %>% mutate(Fe.56_b = (Fe.56-0.32275))
gr <- gr %>% mutate(Co.59_b = (Co.59--0.0005))
gr <- gr %>% mutate(Ni.60_b = (Ni.60-0.00225))
gr <- gr %>% mutate(Cu.63_b = (Cu.63-0.03225))
gr <- gr %>% mutate(Zn.66_b = (Zn.66-0.138))
gr <- gr %>% mutate(As.75_b = (As.75-0.0045))
gr <- gr %>% mutate(Rb.85_b = (Rb.85-0.00225))
gr <- gr %>% mutate(Sr.88_b = (Sr.88-0.0035))
gr <- gr %>% mutate(Mo.98_b = (Mo.98-0.05175))
gr <- gr %>% mutate(Cd.111_b = (Cd.111-0.002))
gr <- gr %>% mutate(Sn.120_b = (Sn.120--0.0025))
gr <- gr %>% mutate(Ba.135_b = (Ba.135-0.02))
gr <- gr %>% mutate(Pb.208_b = (Pb.208-0.047))

#subsets filtering
#sort in media
gr_N <- gr %>% group_by(Accession) %>% select(B.11, K.39, P.31, Ca.43, Mg.24, S.34, Na.23, Mo.98, Mn.55, Fe.56)
#trace #heavy metals
gr_tr <- gr %>% group_by(Accession) %>% select(Li.7, Ti.47, Cr.52, Al.27, Si.29, Co.59, Ni.60, Cu.63, Zn.66, As.75, Rb.85, Sr.88, Cd.111, Sn.120, Ba.135, Pb.208)

#visual of data for N media elements
boxplot(B.11_b~Accession,data=gr)
boxplot(K.39_b~Accession,data=gr)
boxplot(P.31_b~Accession,data=gr)
boxplot(Ca.43_b~Accession,data=gr)
boxplot(Mg.24_b~Accession,data=gr)
boxplot(S.34_b~Accession,data=gr)
boxplot(Na.23_b~Accession,data=gr)
boxplot(Mo.98_b~Accession,data=gr)
boxplot(Mn.55_b~Accession,data=gr)
boxplot(Fe.56_b~Accession,data=gr)

#visual trace and heavy metals
boxplot(Li.7_b~Accession,data=gr)
boxplot(Ti.47_b~Accession,data=gr)
boxplot(Cr.52_b~Accession,data=gr)
boxplot(Al.27_b~Accession,data=gr)
boxplot(Si.29_b~Accession,data=gr)
boxplot(Co.59_b~Accession,data=gr)
boxplot(Ni.60_b~Accession,data=gr)
boxplot(Cu.63_b~Accession,data=gr)
boxplot(Zn.66_b~Accession,data=gr)
boxplot(As.75_b~Accession,data=gr)
boxplot(Rb.85_b~Accession,data=gr)
boxplot(Sr.88_b~Accession,data=gr)
boxplot(Cd.111_b~Accession,data=gr)
boxplot(Sn.120_b~Accession,data=gr)
boxplot(Ba.135_b~Accession,data=gr)
boxplot(Pb.208_b~Accession,data=gr)

#sig effect of accession on ions?
aov <- aov(B.11_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(K.39_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(P.31_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Ca.43_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Mg.24_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(S.34_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Na.23_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Mo.98_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Mn.55_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Fe.56_b~Accession,data=gr)
summary(aov) #*** sig

#sig affect accessions on trace?
aov <- aov(Li.7_b~Accession,data=gr)
summary(aov) #* sig
aov <- aov(Ti.47_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Cr.52_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Al.27_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Si.29_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Co.59_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Ni.60_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Cu.63_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Zn.66_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(As.75_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Rb.85_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Sr.88_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Cd.111_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Sn.120_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Ba.135_b~Accession,data=gr)
summary(aov) #*** sig
aov <- aov(Pb.208_b~Accession,data=gr)
summary(aov) #*** sig

#affect of rep?
aov <- aov(B.11_b~Rep,data=gr) #ns
aov <- aov(B.11_b~Run,data=gr) #***
summary(aov) 
aov <- aov(K.39_b~Rep,data=gr) #* #ns when rem rep 4
aov <- aov(K.39_b~Run,data=gr) #***
summary(aov)
aov <- aov(P.31_b~Rep,data=gr)#* #ns rem rep 4
aov <- aov(P.31_b~Run,data=gr)#*
summary(aov)
aov <- aov(Ca.43_b~Rep,data=gr) #ns rem rep 4
aov <- aov(Ca.43_b~Run,data=gr) #***
summary(aov)
aov <- aov(Mg.24_b~Rep,data=gr) #ns rem rep 4
aov <- aov(Mg.24_b~Run,data=gr) #ns
summary(aov)
aov <- aov(S.34_b~Rep,data=gr) #ns
aov <- aov(S.34_b~Run,data=gr) #***
summary(aov)
aov <- aov(Na.23_b~Rep,data=gr) #ns
aov <- aov(Na.23_b~Run,data=gr) #ns
summary(aov)
aov <- aov(Mo.98_b~Rep,data=gr) #ns
aov <- aov(Mo.98_b~Run,data=gr) #***
summary(aov)
aov <- aov(Mn.55_b~Rep,data=gr) #ns
aov <- aov(Mn.55_b~Run,data=gr) #ns
summary(aov)
aov <- aov(Fe.56_b~Rep,data=gr) #*
aov <- aov(Fe.56_b~Run,data=gr) #***
summary(aov) #*** sig

#run
boxplot(B.11_b~Run,data=gr)
boxplot(K.39_b~Run,data=gr)
boxplot(P.31_b~Run,data=gr)
boxplot(Ca.43_b~Run,data=gr)
boxplot(Mg.24_b~Run,data=gr)
boxplot(S.34_b~Run,data=gr)
boxplot(Na.23_b~Run,data=gr)
boxplot(Mo.98_b~Run,data=gr)
boxplot(Mn.55_b~Run,data=gr)
boxplot(Fe.56_b~Run,data=gr)

#visual trace and heavy metals
boxplot(Li.7_b~Run,data=gr)
boxplot(Ti.47_b~Run,data=gr)
boxplot(Cr.52_b~Run,data=gr)
boxplot(Al.27_b~Run,data=gr)
boxplot(Si.29_b~Run,data=gr)
boxplot(Co.59_b~Run,data=gr)
boxplot(Ni.60_b~Run,data=gr)
boxplot(Cu.63_b~Run,data=gr)
boxplot(Zn.66_b~Run,data=gr)
boxplot(As.75_b~Run,data=gr)
boxplot(Rb.85_b~Run,data=gr)
boxplot(Sr.88_b~Run,data=gr)
boxplot(Cd.111_b~Run,data=gr)
boxplot(Sn.120_b~Run,data=gr)
boxplot(Ba.135_b~Run,data=gr)
boxplot(Pb.208_b~Run,data=gr)

#rep
boxplot(B.11_b~Rep,data=gr)
boxplot(K.39_b~Rep,data=gr)
boxplot(P.31_b~Rep,data=gr)
boxplot(Ca.43_b~Rep,data=gr)
boxplot(Mg.24_b~Rep,data=gr)
boxplot(S.34_b~Rep,data=gr)
boxplot(Na.23_b~Rep,data=gr)
boxplot(Mo.98_b~Rep,data=gr)
boxplot(Mn.55_b~Rep,data=gr)
boxplot(Fe.56_b~Rep,data=gr)

#visual trace and heavy metals
boxplot(Li.7_b~Rep,data=gr)
boxplot(Ti.47_b~Rep,data=gr)
boxplot(Cr.52_b~Rep,data=gr)
boxplot(Al.27_b~Rep,data=gr)
boxplot(Si.29_b~Rep,data=gr)
boxplot(Co.59_b~Rep,data=gr)
boxplot(Ni.60_b~Rep,data=gr)
boxplot(Cu.63_b~Rep,data=gr)
boxplot(Zn.66_b~Rep,data=gr)
boxplot(As.75_b~Rep,data=gr)
boxplot(Rb.85_b~Rep,data=gr)
boxplot(Sr.88_b~Rep,data=gr)
boxplot(Cd.111_b~Rep,data=gr)
boxplot(Sn.120_b~Rep,data=gr)
boxplot(Ba.135_b~Rep,data=gr)
boxplot(Pb.208_b~Rep,data=gr)

#graphs and sig LOCO

#possibly remove NEW, SHA, WO as no reps
library(dplyr)
(gr$Loco)
gapminder <- gr %>% filter(Loco != "NCA")
gapminder <- gapminder %>% filter(Loco != "SHA")
gapminder <- gapminder %>% filter(Loco != "WO")
gr <- gapminder

#possibly remove contamination experiment
contam <- gr %>% filter(Accession != "KS13U")
contam <- contam %>% filter(Accession != "KS13C")
contam <- contam %>% filter(Accession != "KS14U")
contam <- contam %>% filter(Accession != "KS14C")
contam <- contam %>% filter(Accession != "KS18U")
contam <- contam %>% filter(Accession != "KS18C")

gr <- contam

#just have contam data
contam1 <- gr %>% filter(Accession == "KS13U")
contam2 <- gr %>% filter(Accession == "KS13C")
contam3 <- gr %>% filter(Accession == "KS14U")
contam4 <- gr %>% filter(Accession == "KS14C")
contam5 <- gr %>% filter(Accession == "KS18U")
contam6 <- gr %>% filter(Accession == "KS18C")
#concat together to add as rows
contam_exp <- rbind(contam1, contam2, contam3, contam4, contam5, contam6)

#check levels of loco
gr$Loco <- factor(gr$Loco,levels = c("ABE", "BFD", "BRI", "COR", "ELG",
                                     "GLA", "HAS", "HUL", "LAN", "MID",
                                     "NEW", "YOR"))
#converts others to NA no longer included on graphs
#are they included in stats?no

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

#remove blank rows
#just doing columns? need to traverse
#gr2 <- subset(gr, select=-c(,344:347))
#gr2 = gr[,grepl("B",names(gr))]

#gr2 <- t(gr)
#gr2 <- subset(gr2, select=-c(344:347))

#turn back
#gr2 <-t(gr2)
#gr <- gr2
#class(gr) #matrix after traversing
#gr <- as.data.frame(gr) #make data frame so can do summaries

#visualising data
#pca
ions_all <- gr

#DO WITH SUMMARY DATA SO NOT 3 REPS EACH DW
#no species column at min until organise by genomics

#KS do blanks then summary blanks on gr then get avgs + accession cols then
#use sumacc to run from here
ions_all <- sumacc

str(ions_all)

names(ions_all) <- c("Accession", "Loco", "Li","B","Na","Mg","Al","Si","P","S",
                   "K","Ca","Ti","Cr","Mn","Fe","Co",
                   "Ni","Cu","Zn","As","Rb","Sr",
                   "Mo","Cd","Sn","Ba","Pb")

# turn accession into factor and classify data to include
ions_all$Loco <- factor(ions_all$Loco, levels=c("ABE", "BFD", "BRI", "COR", "ELG",
                                                "GLA", "HAS", "HUL", "LAN", "MID",
                                                "NEW", "YOR"))

#not being used yet
#ions_all$Species <- factor(ions_all$Species, levels=c("L. minor", "L. minuta", "L. turionifera",
#                                                "L. gibba", "S. polyrhiza"))

library(FactoMineR)
#just using blanked raw data from gr
ions_acc <- ions_all[ ,c(36:61)] # selecting columns from csv

#using avgd data from sumacc rem LOD, using numeric 
ions_acc <- ions_all[ ,c(3:28)] # selecting columns from csv


#change col names in ions_acc raw so plot nice names on biplot
names(ions_acc)
colnames(ions_acc) <- c("Li","B","Na","Mg","Al","Si","P","S",
                        "K","Ca","Ti","Cr","Mn","Fe","Co",
                        "Ni","Cu","Zn","As","Rb","Sr",
                        "Mo","Cd","Sn","Ba","Pb")

#remove columns of elements raw <LOD so not in PCA
#do same for avgd data
ions_accrem <- ions_acc[-c(1,12,16,19,24)]
ions_acc<- ions_accrem

ions_acc.pca <- PCA(ions_acc, quali.sup=21) #cant exceed max col number
print(ions_acc.pca)
#ions_acc.pca <- PCA(ions_acc, quali.sup=26) #cant exceed max col number
print(ions_acc.pca)
head(ions_acc.pca)
print(summary(ions_acc.pca)) #shows pc contirbutions of eigens
#write.csv(ions_acc.pca, ions_app_pca_data.csv) # doesnt work
#26% AND 15% 1 AND 2, 10 AND 6% 3 AND 4
#when removed <LOD = 21 var, 31% and 15%
#33% 18% when avged 115 obs and <LOD


#FOR VARIABLE COS 2 GRAPH
#plot cos 2 as bar graph #high = good representation on pc
library(FactoMineR)
library(factoextra)

#basic plot
biplot(ions_acc.pca)

fviz_pca_biplot(ions_acc.pca, repel=TRUE, pointsize=6, pointshape=21, col.var="black", arrowsize=0.6, labelsize=5, col.ind=ions_all$Species, palette=c("green2", "gold","red","blue","purple"), addEllipses=TRUE, ellipse.type="confidence")

fviz_cos2(ions_acc.pca, choice = "var", axes = 1:2) #K and S biggest

fviz_pca_var(ions_acc.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#try to adjust easthetics
library(ggplot2)
plot1 <- fviz_pca_var(ions_acc.pca, col.var = "cos2",
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE)
plot1 + xlab("PC1 (30%)") + ylab("PC2 (15%)") +
  ggtitle(NULL)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#save plot 1
ggsave("DW_tissue_biplot_NONAVGD115obs_remLOD_gggplot_midlan.png", dpi = 300, width = 15, height = 15 , units = "cm") 

#do this first to get pc columns, then visualise variable
ions_acc.pca<-prcomp(ions_acc[ ,1:21],center=T,scale=T) #list can include mix mat and df
str(ions_acc.pca)
#ions_acc.pca<-prcomp(ions_acc[ ,1:26],center=T,scale=T) #list can include mix mat and df
str(ions_acc.pca)
mypc <- ions_acc.pca$x #define new variable x = pcs just need to plot these on xy graph

#plot locos
plot(mypc[,1], mypc[,2], col = my_pal,
     las=1, xlab="PC1 (30%)", ylab="PC2 (15%)",
     pch=16, cex=1.5, xlim=c(-25,25), ylim=c(-25,25)) #pcs as columns, produce xy plot, las is rotation of axis numbers, pch plot shape, ylim expand out so legend room
abline(v=0, lty=2, col="lightgrey") #draw line, lty is segmented
abline(h=0, lty=2, col="lightgrey") #0 lines dashed
legend("right", pch=16, col=my_pal, cex=1, c("ABE", "BFD", "BRI", "COR", "ELG",
                                               "GLA", "HAS", "HUL", "LAN", "MID",
                                               "NEW", "YOR")) #customise legend seperately cex=txtsize cols 1-4 stnd, concat order of places as want to display
text(x=mypc[,1],y=mypc[,2], labels =ions_all$Accession, cex=0.7, pos=2)
#add labels so know outlying accessions

#need more than 12 colors as repeating
my_pal <- scico::scico(length(unique(ions_all$Loco)), palette = "lisbon")

#species
tiff('DW_tissue_species_PCA.tiff', units="in", width=7, height=6, res=300, compression = 'lzw')
plot(mypc[,1], mypc[,2], col = ions_all$Species,
     las=1, xlab="PC1 (26%)", ylab="PC2 (15%)",
     pch=16, cex=1.5, xlim=c(-15,15), ylim=c(-15,15)) #pcs as columns, produce xy plot, las is rotation of axis numbers, pch plot shape, ylim expand out so legend room
abline(v=0, lty=2, col="lightgrey") #draw line, lty is segmented
abline(h=0, lty=2, col="lightgrey") #0 lines dashed
legend(9,15.5, bty="n", x.intersp=0.45, text.width=0.40, pch=16, col=1:5, cex=1, c("L. minor", "L. minuta", "L. turionifera",
                                           "L. gibba", "S. polyrhiza")) #customise legend seperately cex=txtsize cols 1-4 stnd, concat order of places as want to display
text(x=mypc[,1],y=mypc[,2], labels =ions_all$Accession, cex=0.3, pos=2) #txt and pos to define labels on plot, 2 = top of plot, 1 bottom
dev.off()

#y.intersp=0.05, text.width=1.5,
#species map well clustered doesnt say much

#old MK script for centroids per loco
#input when removed contam and gapminder, locos no reps for
ion <- gr
sub <- ion[, c(5, 10:61)] #all rows, just cols range plus one wanted
#remove not blanked
sub <- ion[, c(5, 36:61)]
#remove <LOD
sub <- sub[, -c(2,13,17,20,25)] #rem li, cr, ni, sn

names(sub) <- c("Loco", "B","Na","Mg","Al","Si","P","S",
                "K","Ca","Ti","Mn","Fe","Co",
                "Cu","Zn","Rb","Sr",
                "Mo","Cd","Ba","Pb")

#OR IF DOING SUMMARY
#ion <- sumacc
#rename again
names(ion) <- c("Accession", "Loco", "Li","B","Na","Mg","Al","Si","P","S",
                "K","Ca","Ti","Cr","Mn","Fe","Co",
                "Ni","Cu","Zn","As","Rb","Sr",
                "Mo","Cd","Sn","Ba","Pb")
#remove not blanked
sub <- ion[, -c(3,14,18,21,26)]
lapply(sub, as.numeric)
#try manually remove 1st and last and Ba col
#sub <- sub[, c(1:25,27)]

View(sub)
par(mfrow = c(1, 1))
#iris_pca <- prcomp(sub[, 2:53], center = T, scale. = T)

#new
iris_pca <- prcomp(sub[, 2:22], center = T, scale. = T)

#avgd data loq rem
iris_pca <- prcomp(sub[, 3:23], center = T, scale. = T)

scores <- iris_pca$x
scores <- scores %>% data.frame()

scores$Loco <- sub$Loco
centroids <- scores %>% group_by(Loco) %>% summarise_all("mean")

#new colors pca
tiff('DW_tissue_loco_PCA_NONavgd115obs_remLOD_newcol+midlan.tiff', units="in", width=7, height=6, res=300, compression = 'lzw')
plot(scores[, 1], scores[, 2], type = "n",
     las=1, xlab="PC1 (30%)", ylab="PC2 (15%)",
     cex=1.5, xlim=c(-30,30), ylim=c(-30,30))
abline(h = 0, col = "lightgrey", lty = 2)
abline(v = 0, col = "lightgrey", lty = 2)
points(centroids[centroids$Loco == "ELG", 2], centroids[centroids$Loco == "ELG", 3], pch = 3, lwd = 2, cex = 5, col = "#632770")
points(centroids[centroids$Loco == "ABE", 2], centroids[centroids$Loco == "ABE", 3], pch = 3, lwd = 2, cex = 5, col = "#2915B4")
points(centroids[centroids$Loco == "GLA", 2], centroids[centroids$Loco == "GLA", 3], pch = 3, lwd = 2, cex = 5, col = "#3260B4")
points(centroids[centroids$Loco == "LAN", 2], centroids[centroids$Loco == "LAN", 3], pch = 3, lwd = 2, cex = 5, col = "#17706A")
points(centroids[centroids$Loco == "BFD", 2], centroids[centroids$Loco == "BFD", 3], pch = 3, lwd = 2, cex = 5, col = "#2E4E13")
points(centroids[centroids$Loco == "YOR", 2], centroids[centroids$Loco == "YOR", 3], pch = 3, lwd = 2, cex = 5, col = "#2D882D")
points(centroids[centroids$Loco == "HUL", 2], centroids[centroids$Loco == "HUL", 3], pch = 3, lwd = 2, cex = 5, col = "#77EE15")
points(centroids[centroids$Loco == "MID", 2], centroids[centroids$Loco == "MID", 3], pch = 3, lwd = 2, cex = 5, col = "#ABF057")
points(centroids[centroids$Loco == "NEW", 2], centroids[centroids$Loco == "NEW", 3], pch = 3, lwd = 2, cex = 5, col = "#FFA256")
points(centroids[centroids$Loco == "BRI", 2], centroids[centroids$Loco == "BRI", 3], pch = 3, lwd = 2, cex = 5, col = "#FF4E32")
points(centroids[centroids$Loco == "HAS", 2], centroids[centroids$Loco == "HAS", 3], pch = 3, lwd = 2, cex = 5, col = "#AA3939")
points(centroids[centroids$Loco == "COR", 2], centroids[centroids$Loco == "COR", 3], pch = 3, lwd = 2, cex = 5, col = "#7D0900")

points(scores[sub$Loco == "ELG", 1], scores[sub$Loco == "ELG", 2], pch= 16, col = "#632770")
points(scores[sub$Loco == "ABE", 1], scores[sub$Loco == "ABE", 2], pch= 16, col = "#2915B4")
points(scores[sub$Loco == "GLA", 1], scores[sub$Loco == "GLA", 2], pch= 17, col = "#3260B4")
points(scores[sub$Loco == "LAN", 1], scores[sub$Loco == "LAN", 2], pch= 16, col = "#17706A")
points(scores[sub$Loco == "BFD", 1], scores[sub$Loco == "BFD", 2], pch= 16, col = "#2E4E13")
points(scores[sub$Loco == "YOR", 1], scores[sub$Loco == "YOR", 2], pch= 16, col = "#2D882D")
points(scores[sub$Loco == "HUL", 1], scores[sub$Loco == "HUL", 2], pch= 17, col = "#77EE15")
points(scores[sub$Loco == "MID", 1], scores[sub$Loco == "MID", 2], pch= 16, col = "#ABF057")
points(scores[sub$Loco == "NEW", 1], scores[sub$Loco == "NEW", 2], pch= 16, col = "#FFA256")
points(scores[sub$Loco == "BRI", 1], scores[sub$Loco == "BRI", 2], pch= 16, col = "#FF4E32")
points(scores[sub$Loco == "HAS", 1], scores[sub$Loco == "HAS", 2], pch= 17, col = "#AA3939")
points(scores[sub$Loco == "COR", 1], scores[sub$Loco == "COR", 2], pch= 16, col = "#7D0900")
legend("topright", inset=.075, y.intersp=0.4, text.width=0.3, pch =c(16,16,17,16,16,16,17,16,16,16,17,16), col=c("#632770", "#2915B4", "#3260B4", "#17706A", "#2E4E13", "#2D882D", "#77EE15","#ABF057","#FFA256", "#FF4E32", "#AA3939", "#7D0900"), cex=1.2, c("ELG","ABE","GLA","LAN","BFD","YOR", "HUL","MID","NEW","BRI", "HAS", "COR"), bty="n")#legend("bottomright", pch=16, col=c("black", "red", "blue", "green", "purple", "brown", "orange", "pink", "gold", "grey"), cex=0.5, c("COR", "BRI", "HAS", "NEW", "GLA", "ABE", "ELG", "BFD", "YOR", "HUL"), bty="n")
dev.off()

#pca zoomed
#new colors pca
tiff('DW_tissue_loco_PCA_NONavgd115obs_remLOD_newcol_zoomed+midlan.tiff', units="in", width=7, height=6, res=300, compression = 'lzw')
plot(scores[, 1], scores[, 2], type = "n",
     las=1, xlab="PC1 (30%)", ylab="PC2 (15%)",
     cex=1.5, xlim=c(-6,6), ylim=c(-6,6))
abline(h = 0, col = "lightgrey", lty = 2)
abline(v = 0, col = "lightgrey", lty = 2)
points(centroids[centroids$Loco == "ELG", 2], centroids[centroids$Loco == "ELG", 3], pch = 3, lwd = 2, cex = 5, col = "#632770")
points(centroids[centroids$Loco == "ABE", 2], centroids[centroids$Loco == "ABE", 3], pch = 3, lwd = 2, cex = 5, col = "#2915B4")
points(centroids[centroids$Loco == "GLA", 2], centroids[centroids$Loco == "GLA", 3], pch = 3, lwd = 2, cex = 5, col = "#3260B4")
points(centroids[centroids$Loco == "LAN", 2], centroids[centroids$Loco == "LAN", 3], pch = 3, lwd = 2, cex = 5, col = "#17706A")
points(centroids[centroids$Loco == "BFD", 2], centroids[centroids$Loco == "BFD", 3], pch = 3, lwd = 2, cex = 5, col = "#2E4E13")
points(centroids[centroids$Loco == "YOR", 2], centroids[centroids$Loco == "YOR", 3], pch = 3, lwd = 2, cex = 5, col = "#2D882D")
points(centroids[centroids$Loco == "HUL", 2], centroids[centroids$Loco == "HUL", 3], pch = 3, lwd = 2, cex = 5, col = "#77EE15")
points(centroids[centroids$Loco == "MID", 2], centroids[centroids$Loco == "MID", 3], pch = 3, lwd = 2, cex = 5, col = "#ABF057")
points(centroids[centroids$Loco == "NEW", 2], centroids[centroids$Loco == "NEW", 3], pch = 3, lwd = 2, cex = 5, col = "#FFA256")
points(centroids[centroids$Loco == "BRI", 2], centroids[centroids$Loco == "BRI", 3], pch = 3, lwd = 2, cex = 5, col = "#FF4E32")
points(centroids[centroids$Loco == "HAS", 2], centroids[centroids$Loco == "HAS", 3], pch = 3, lwd = 2, cex = 5, col = "#AA3939")
points(centroids[centroids$Loco == "COR", 2], centroids[centroids$Loco == "COR", 3], pch = 3, lwd = 2, cex = 5, col = "#7D0900")

points(scores[sub$Loco == "ELG", 1], scores[sub$Loco == "ELG", 2], pch= 16, col = "#632770")
points(scores[sub$Loco == "ABE", 1], scores[sub$Loco == "ABE", 2], pch= 16, col = "#2915B4")
points(scores[sub$Loco == "GLA", 1], scores[sub$Loco == "GLA", 2], pch= 17, col = "#3260B4")
points(scores[sub$Loco == "LAN", 1], scores[sub$Loco == "LAN", 2], pch= 16, col = "#17706A")
points(scores[sub$Loco == "BFD", 1], scores[sub$Loco == "BFD", 2], pch= 16, col = "#2E4E13")
points(scores[sub$Loco == "YOR", 1], scores[sub$Loco == "YOR", 2], pch= 16, col = "#2D882D")
points(scores[sub$Loco == "HUL", 1], scores[sub$Loco == "HUL", 2], pch= 17, col = "#77EE15")
points(scores[sub$Loco == "MID", 1], scores[sub$Loco == "MID", 2], pch= 16, col = "#ABF057")
points(scores[sub$Loco == "NEW", 1], scores[sub$Loco == "NEW", 2], pch= 16, col = "#FFA256")
points(scores[sub$Loco == "BRI", 1], scores[sub$Loco == "BRI", 2], pch= 16, col = "#FF4E32")
points(scores[sub$Loco == "HAS", 1], scores[sub$Loco == "HAS", 2], pch= 17, col = "#AA3939")
points(scores[sub$Loco == "COR", 1], scores[sub$Loco == "COR", 2], pch= 16, col = "#7D0900")
legend("topright", inset=.05, y.intersp=0.7, text.width=0.3, pch =c(16,16,17,16,16,16,17,16,16,16,17,16), col=c("#632770", "#2915B4", "#3260B4", "#17706A", "#2E4E13", "#2D882D", "#77EE15","#ABF057","#FFA256", "#FF4E32", "#AA3939", "#7D0900"), cex=1.2, c("ELG","ABE","GLA","LAN","BFD","YOR", "HUL","MID","NEW","BRI", "HAS", "COR"), bty="n")#legend("bottomright", pch=16, col=c("black", "red", "blue", "green", "purple", "brown", "orange", "pink", "gold", "grey"), cex=0.5, c("COR", "BRI", "HAS", "NEW", "GLA", "ABE", "ELG", "BFD", "YOR", "HUL"), bty="n")
dev.off()
#dev.new()
#old  colors
tiff('DW_tissue_loco_PCA_avgd115obs_remLOD.tiff', units="in", width=7, height=6, res=300, compression = 'lzw')
plot(scores[, 1], scores[, 2], type = "n",
     las=1, xlab="PC1 (33%)", ylab="PC2 (18%)",
     cex=1.5, xlim=c(-20,20), ylim=c(-30,30))
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
points(centroids[centroids$Loco == "LAN", 2], centroids[centroids$Loco == "LAN", 3], pch = 3, lwd = 2, cex = 5, col = "cyan")
points(centroids[centroids$Loco == "MID", 2], centroids[centroids$Loco == "MID", 3], pch = 3, lwd = 2, cex = 5, col = "darkgreen")

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
points(scores[sub$Loco == "LAN", 1], scores[sub$Loco == "LAN", 2], pch= 1, col = "cyan")
points(scores[sub$Loco == "MID", 1], scores[sub$Loco == "MID", 2], pch= 1, col = "darkgreen")
legend("topright", inset=.05, y.intersp=1.0, text.width=0.6, pch = 1, col=c("black", "red", "blue", "green", "purple", "brown", "orangered1", "violetred1", "gold", "grey", "cyan", "darkgreen"), cex=1.2, c("COR", "BRI", "HAS", "NEW", "GLA", "ABE", "ELG", "BFD", "YOR", "HUL", "LAN", "MID"), bty="n")
#legend("bottomright", pch=16, col=c("black", "red", "blue", "green", "purple", "brown", "orange", "pink", "gold", "grey"), cex=0.5, c("COR", "BRI", "HAS", "NEW", "GLA", "ABE", "ELG", "BFD", "YOR", "HUL"), bty="n")
dev.off()

#prep for heatmaps and radars
#to get zscores first
#prep
#split into meta data and numerical
ions_num <- ions_all %>% dplyr::select(-Accession.x, -Accession, -Rep, 
                                       -Species, -Loco, -Country, -Original.tube,
                                       -Full.CORde, -Run)
#removed num non blanked
ions_num <- ions_num[,-c(1:26)]
#zscores
#avg ions
#install.packages("GMCM")
#library(GMCM)
ions_num <- as.matrix(ions_num)
avgs <- colMeans(ions_num)
#SDs <- GMCM:::colSds(ions_num)
avgs <- as.data.frame(avgs)
SDs <- as.data.frame(SDs)
join <- cbind(avgs,SDs)
t(join)
ions_num <- as.data.frame(ions_num)

as.matrix(ions_s)
as.data.frame(ions_s)


#try scale data works for z scores?
scaled.dat <- scale(ions_num)

#zscores achieved 2 ways ions_s using func above or
#using raw data
ions_sc <- scale(ions_num,center=T,scale=T)

#using average per accession means
ions_sc <- scale(Summary_b_s,center=T,scale=T)

#using average per accession means, removed <LOD
ions_sc <- scale(sub[3:23],center=T,scale=T)

#same as scaled.dat

#radar plots not available on r 3.6.3

#heat map tutorial
library(RColorBrewer)
#scale numeric data
ions_all <- ions_all[10:61]
ions_sc <- scale(ions_all,center=T,scale=T)
ions_sc <- as.matrix(ions_sc)

#assign row names from df
rownames(ions_sc) <- ions_in$Accession
#or
rownames(ions_sc) <- ions_all$Accession
#or
rownames(ions_sc) <- sub$Accession
heatmap(ions_sc)
heatmap(ions_sc, Colv = NA, Rowv = NA, scale="column")
#change color scheme yellow blue
colfunc <- colorRampPalette(c("blue", "yellow"))
heatmap(ions_sc,col=colfunc(11),scale="row")

#looks different when add scale=column, removes branches
#need to add names of accessions to plot, not working?

#OLD USE SUB INSTEAD
#use mean data to plot heatmap
#named to can be plotted with neat names
ions_in <- read.csv("DW_ionomics_summary_justmeans.csv")
#created below to treat for ba, added name cols back in manually
ions_in <- read.csv("DW_ionomics_summary_justmeans-ba_omitna.csv")
ions_all <- ions_in[4:29]

#avgd data, raw, no z scores until split out, no LOD inc
#group by ion type
names(sub) <- c("Accession", "Loco", "B","Na","Mg","Al","Si","P","S",
                "K","Ca","Ti","Mn","Fe","Co",
                "Cu","Zn","Rb","Sr",
                "Mo","Cd", "Ba","Pb")

nmedia <- sub[c(1,2,3,4,5,7:11,13,14,20)]
heavy <- sub[c(1,2,6,12,15,16:19,21:23)]

#OLD USE SUB ABOVE INSTEAD
macro <- ions_in[c(1,2,3,5,6,7,9:13,16:17)] #> 100 mg/g
trace <- ions_in[c(1,2,3,20,21,25)]
heavy <- ions_in[c(1,2,3,4,8,14,15,18,19,22:24,26:29)]
nmedia <- ions_in[c(1,2,3,5,6:7,9,10:13,16:17,25)]

#remove observations = rows 124 obs to 118 obs
#not neccessary when using sub?
nmedia <- nmedia[-c(28:29,31:32,37:38),]

#just compare whats in n media variation as supplied
ions_all <- nmedia[3:13]
ions_sc <- scale(ions_all,center=T,scale=T)
ions_sc <- as.matrix(ions_sc)
rownames(ions_sc) <- nmedia$Accession

#compare whats in heavy metals
ions_all <- heavy[3:12]
ions_sc <- scale(ions_all,center=T,scale=T)
ions_sc <- as.matrix(ions_sc)
rownames(ions_sc) <- heavy$Accession

#for saving
#as.data.frame(ions_sc)
#not working to add accessions and loco
#addloco <- cbind(heavy$Accession, heavy$Loco, ions_sc)
write.csv(heavy$Loco, "DW_Locostoaddto_zscores.csv")
write.csv(ions_sc, "DW_Nmedia_tissue_noLOD_zscores.csv")
write.csv(ions_sc, "DW_heavy_tissue_noLOD_zscores.csv")
#add loco and accessions manually

pdf("Ions_Nmedia_tissue__remLOD_accession_avgs_heatmap_big.pdf", height=20, width=20)
pdf("Ions_Heavy_tissue_remLOD_accession_avgs_heatmap_big.pdf", height=20, width=20)
colfunc <- colorRampPalette(c("blue", "yellow"))
heatmap(ions_sc,col=colfunc(11),scale="row", cexCol=4,margins=c(5,10))
dev.off()
#dev.new()

#dont need to use as files already created?
#summarise means avgs sd max
#summary raw
Summary <- gr %>%
  group_by(Accession) %>%
  summarise(Li.7_mean = mean(Li.7), Li.7_stdev = sd(Li.7), n= n(), Li.7_maximum = max(Li.7),
          B.11_mean = mean(B.11), B.11_stdev = sd(B.11), B.11_maximum = max(B.11),
          Na.23_mean = mean(Na.23), Na.23_stdev = sd(Na.23), Na.23_maximum = max(Na.23),
          Mg.24_mean = mean(Mg.24), Mg.24_stdev = sd(Mg.24), Mg.24_maximum = max(Mg.24),
          Al.27_mean = mean(Al.27), Al.27_stdev = sd(Al.27), Al.27_maximum = max(Al.27),
          Si.29_mean = mean(Si.29), Si.29_stdev = sd(Si.29), Si.29_maximum = max(Si.29),
          P.31_mean = mean(P.31), P.31_stdev = sd(P.31), P.31_maximum = max(P.31),
          S.34_mean = mean(S.34), S.34_stdev = sd(S.34), S.34_maximum = max(S.34),
          K.39_mean = mean(K.39), K.39_stdev = sd(K.39), K.39_maximum = max(K.39),
          Ca.43_mean = mean(Ca.43), Ca.43_stdev = sd(Ca.43), Ca.43_maximum = max(Ca.43),
          Ti.47_mean = mean(Ti.47), Ti.47_stdev = sd(Ti.47), Ti.47_maximum = max(Ti.47),
          Cr.52_mean = mean(Cr.52), Cr.52_stdev = sd(Cr.52), Cr.52_maximum = max(Cr.52),
          Mn.55_mean = mean(Mn.55), Mn.55_stdev = sd(Mn.55), Mn.55_maximum = max(Mn.55),
          Fe.56_mean = mean(Fe.56), Fe.56_stdev = sd(Fe.56), Fe.56_maximum = max(Fe.56),
          Co.59_mean = mean(Co.59), Co.59_stdev = sd(Co.59), Co.59_maximum = max(Co.59),
          Ni.60_mean = mean(Ni.60), Ni.60_stdev = sd(Ni.60), Ni.60_maximum = max(Ni.60),
          Cu.63_mean = mean(Cu.63), Cu.63_stdev = sd(Cu.63), Cu.63_maximum = max(Cu.63),
          Zn.66_mean = mean(Zn.66), Zn.66_stdev = sd(Zn.66), Zn.66_maximum = max(Zn.66),
          As.75_mean = mean(As.75), As.75_stdev = sd(As.75), As.75_maximum = max(As.75),
          Rb.85_mean = mean(Rb.85), Rb.85_stdev = sd(Rb.85), Rb.85_maximum = max(Rb.85),
          Sr.88_mean = mean(Sr.88), Sr.88_stdev = sd(Sr.88), Sr.88_maximum = max(Sr.88),
          Mo.98_mean = mean(Mo.98), Mo.98_stdev = sd(Mo.98), Mo.98_maximum = max(Mo.98),
          Cd.111_mean = mean(Cd.111), Cd.111_stdev = sd(Cd.111), Cd.111_maximum = max(Cd.111),
          Sn.120_mean = mean(Sn.120), Sn.120_stdev = sd(Sn.120), Sn.120_maximum = max(Sn.120),
          Ba.135_mean = mean(Ba.135), Ba.135_stdev = sd(Ba.135), Ba.135_maximum = max(Ba.135),
          Pb.208_mean = mean(Pb.208), Pb.208_stdev = sd(Pb.208), Pb.208_maximum = max(Pb.208)
          )
Summary

#summary of blanked values
Summary_b <- gr %>%
  group_by(Accession, Species, Loco) %>%
  summarise(Li.7_mean = mean(Li.7_b), Li.7_b_stdev = sd(Li.7_b), n= n(), Li.7_b_maximum = max(Li.7_b),
            B.11_mean = mean(B.11_b), B.11_b_stdev = sd(B.11_b), B.11_b_maximum = max(B.11_b),
            Na.23_mean = mean(Na.23_b), Na.23_b_stdev = sd(Na.23_b), Na.23_b_maximum = max(Na.23_b),
            Mg.24_mean = mean(Mg.24_b), Mg.24_stdev = sd(Mg.24_b), Mg.24_b_maximum = max(Mg.24_b),
            Al.27_mean = mean(Al.27_b), Al.27_stdev = sd(Al.27_b), Al.27_b_maximum = max(Al.27_b),
            Si.29_mean = mean(Si.29_b), Si.29_stdev = sd(Si.29_b), Si.29_b_maximum = max(Si.29_b),
            P.31_mean = mean(P.31_b), P.31_stdev = sd(P.31_b), P.31_b_maximum = max(P.31_b),
            S.34_mean = mean(S.34_b), S.34_stdev = sd(S.34_b), S.34_b_maximum = max(S.34_b),
            K.39_mean = mean(K.39_b), K.39_stdev = sd(K.39_b), K.39_b_maximum = max(K.39_b),
            Ca.43_mean = mean(Ca.43_b), Ca.43_stdev = sd(Ca.43_b), Ca.43_b_maximum = max(Ca.43_b),
            Ti.47_mean = mean(Ti.47_b), Ti.47_stdev = sd(Ti.47_b), Ti.47_b_maximum = max(Ti.47_b),
            Cr.52_mean = mean(Cr.52_b), Cr.52_stdev = sd(Cr.52_b), Cr.52_b_maximum = max(Cr.52_b),
            Mn.55_mean = mean(Mn.55_b), Mn.55_stdev = sd(Mn.55_b), Mn.55_b_maximum = max(Mn.55_b),
            Fe.56_mean = mean(Fe.56_b), Fe.56_stdev = sd(Fe.56_b), Fe.56_b_maximum = max(Fe.56_b),
            Co.59_mean = mean(Co.59_b), Co.59_stdev = sd(Co.59_b), Co.59_b_maximum = max(Co.59_b),
            Ni.60_mean = mean(Ni.60_b), Ni.60_stdev = sd(Ni.60_b), Ni.60_b_maximum = max(Ni.60_b),
            Cu.63_mean = mean(Cu.63_b), Cu.63_stdev = sd(Cu.63_b), Cu.63_b_maximum = max(Cu.63_b),
            Zn.66_mean = mean(Zn.66_b), Zn.66_stdev = sd(Zn.66_b), Zn.66_b_maximum = max(Zn.66_b),
            As.75_mean = mean(As.75_b), As.75_stdev = sd(As.75_b), As.75_b_maximum = max(As.75_b),
            Rb.85_mean = mean(Rb.85_b), Rb.85_stdev = sd(Rb.85_b), Rb.85_b_maximum = max(Rb.85_b),
            Sr.88_mean = mean(Sr.88_b), Sr.88_stdev = sd(Sr.88_b), Sr.88_b_maximum = max(Sr.88_b),
            Mo.98_mean = mean(Mo.98_b), Mo.98_stdev = sd(Mo.98_b), Mo.98_b_maximum = max(Mo.98_b),
            Cd.111_mean = mean(Cd.111_b), Cd.111_stdev = sd(Cd.111_b), Cd.111_b_maximum = max(Cd.111_b),
            Sn.120_mean = mean(Sn.120_b), Sn.120_stdev = sd(Sn.120_b), Sn.120_b_maximum = max(Sn.120_b),
            Ba.135_mean = mean(Ba.135_b, na.rm=TRUE), Ba.135_stdev = sd(Ba.135_b, na.rm=TRUE), Ba.135_b_maximum = max(Ba.135_b, na.rm=TRUE),
            Pb.208_mean = mean(Pb.208_b), Pb.208_stdev = sd(Pb.208_b), Pb.208_b_maximum = max(Pb.208_b)
  )
Summary_b

write.csv(Summary_b, "DW_ionomics_summary.csv")
#version with na treatment for ba
write.csv(Summary_b, "DW_ionomics_summary-ba_omitna.csv")

#summary b just get means
names(Summary_b)
avg_cols <- stringr::str_detect(names(Summary_b), pattern = "mean") %>% which()
#if contain SD read indicies columns
SD_cols <- stringr::str_detect(names(Summary_b), pattern = "stdev") %>% which()
#if contain SD read indicies columns
max_cols <- stringr::str_detect(names(Summary_b), pattern = "maximum") %>% which()
#if contain SD read indicies columns

Summary_b_s <- Summary_b[, avg_cols] #just means
sumacc <- cbind(Summary_b$Accession, Summary_b$Loco, Summary_b_s)
write.csv(Summary_b_s, "DW_ionomics_summary_justmeans-ba_omitna.csv")
