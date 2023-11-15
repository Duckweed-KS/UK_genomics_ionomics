library(vcfR)
library(ggplot2)
library(ggrepel)

setwd("C:\\Users\\kelli\\OneDrive\\Documents\\Duckweed\\adgenet\\2023\\all_red")

#original
#vcf <- read.vcfR("Cochlearia_4dg_106.purged_pruned.ann.vcf") #Filtered and LD-pruned VCF file

#ks vcf with old 4fds
#variants = 562390
#vcf <- read.vcfR("filtered.F4.4fds.consistent.merged.vcf.gz")

#ks vcf uklight with old 4fds + filtering prune
#variants = 6727
#vcf <- read.vcfR("4fold_allred_ld_pruned_maf0.05_100_50_0.1_use.vcf")

#EDIT
#diff params 1752 variants
#vcf <- read.vcfR("4fold_allred_ld_pruned_maf0.05_500_50_0.1_use.vcf")
#20603 variants
#vcf <- read.vcfR("4fold_allred_ld_pruned_maf0.025_50_50_0.1_use.vcf")
#11088 variants
vcf <- read.vcfR("4fold_allred_ld_pruned_maf0.025_100_50_0.1_use.vcf")
#2916 need to run
#vcf <- read.vcfR("4fold_allred_ld_pruned_maf0.025_500_50_0.1_use.vcf")
#2451 need to run
#vcf <- read.vcfR("4fold_allred_ld_pruned_mis0.8_maf0.05_100_50_0.1_use.vcf")
#561
#vcf <- read.vcfR("4fold_allred_ld_pruned_mis0.8_maf0.05_500_50_0.1_use.vcf")

#Transform VCF to numeric genotypes
df <- extract.gt(vcf)
df[df == "0|0"] <- 0
df[df == "0|1"] <- 1
df[df == "1|0"] <- 1
df[df == "1|1"] <- 2
df[df == "0/0"] <- 0
df[df == "0/1"] <- 1
df[df == "1/0"] <- 1
df[df == "1/1"] <- 2
df[df == "0/0/0/0"] <- 0
df[df == "0/0/0/1"] <- 1
df[df == "0/0/1/1"] <- 2
df[df == "0/1/1/1"] <- 3
df[df == "1/1/1/1"] <- 4
df[df == "0/0/0/0/0/0"] <- 0
df[df == "0/0/0/0/0/1"] <- 1
df[df == "0/0/0/0/1/1"] <- 2
df[df == "0/0/0/1/1/1"] <- 3
df[df == "0/0/1/1/1/1"] <- 4
df[df == "0/1/1/1/1/1"] <- 5
df[df == "1/1/1/1/1/1"] <- 6
df <- data.frame(apply(df,2,function(x)as.numeric(as.character(x))))

#to do before any mis settings
#remove columns by species type
#remove L. minuta
colnames(df)
Lminu <- df[c(1,2,3,7,8,32,35,41,44,47:49,52,53,56,63,67,70,75,77,79,80,83,86,90,93,96,101,104,121,125,130,139,142)]
Lmino <- df[c(5,6,9:24,26:28,30,31,33,36:40,42,43,45,46,50,51,54,55,57:62,64,65,66,68,69,71,72,73,76,78,81,82,84,85,87,88,92,95,97,98,99,100,102,103,105,107,108,109,110,113,115,117,122,123,124,129,131,132,133,136,137,143)]

colnames(Lmino)
colnames(Lminu)

#Remove samples with > 50% missing data
#mis <- apply(df,2,function(x)sum(is.na(x))/length(x))
#df <- df[,mis <= 0.5]

#Remove samples with > 80% missing data
mis <- apply(df,2,function(x)sum(is.na(x))/length(x))
df <- df[,mis <= 0.8]
#562390 obs of 143 var, down to 135 var #crashes
#6727 of 143 var, down to 135 var
#1752 of 143 var, down to 134 var
#20603 of 143 var, down to 134 var
#11088 of 143 var, down to 135 var
#2916 of 143 var, down to 135 var
#with a mis 0.8 and 0.8 again
#2451 of 143 var, down to 135 var
#561 of 143 var, down to 136 var

#look what individuals have been removed
colnames(df)

#Remove samples with > 80% missing data
mis <- apply(Lmino,2,function(x)sum(is.na(x))/length(x))
Lmino <- Lmino[,mis <= 0.8]

#Remove samples with > 80% missing data
mis <- apply(Lminu,2,function(x)sum(is.na(x))/length(x))
Lminu <- Lminu[,mis <= 0.8]

#look what individuals have been removed
#75B
colnames(Lmino)

#31 left out of 34
colnames(Lminu)

df <- Lmino
df <- Lminu

#Calculate allele frequencies
ploidy <- apply(df,2,max,na.rm=T)
p <- apply(df,1,function(x)sum(x,na.rm=T)/sum(ploidy[!is.na(x)]))

#Removing individuals can change allele frequencies, so we make sure that maf > 0.05
#df <- df[p >= 0.05 & p <= 0.95,]
#p <- p[p >= 0.05 & p <= 0.95]

#df colnames rename
colnames(df)
#change names
#include KS69 if 136 var
colnames(df) <- c("AL01"	,
                  "AL02"	,
                  "AL03"	,
                  #"S.intermedia8410"	,
                  "KS02"	,
                  "KS03"	,
                  "KS06A"	,
                  "KS06B"	,
                  "KS09"	,
                  "KS100"	,
                  "KS101"	,
                  "KS104"	,
                  "KS107"	,
                  "KS108"	,
                  "KS109"	,
                  "KS110"	,
                  "KS111"	,
                  "KS112"	,
                  "KS114"	,
                  "KS115"	,
                  "KS116"	,
                  "KS117"	,
                  "KS118"	,
                  "KS119"	,
                  "KS12"	,
                  "KS13"	,
                  "KS14"	,
                  "KS15"	,
                  "KS16"	,
                  "KS17"	,
                  "KS18"	,
                  "KS20"	,
                  "KS21"	,
                  "KS22"	,
                  "KS25"	,
                  "KS27"	,
                  "KS28"	,
                  "KS29"	,
                  "KS33"	,
                  "KS34"	,
                  "KS38"	,
                  "KS39"	,
                  "KS40A"	,
                  "KS42"	,
                  "KS43"	,
                  "KS44A"	,
                  "KS44B"	,
                  "KS45"	,
                  "KS46B"	,
                  "KS47"	,
                  "KS48A"	,
                  "KS48B"	,
                  "KS49"	,
                  "KS50"	,
                  "KS51A"	,
                  "KS51B"	,
                  "KS52"	,
                  "KS53"	,
                  "KS54"	,
                  "KS55"	,
                  "KS56"	,
                  "KS57"	,
                  "KS58B"	,
                  "KS59"	,
                  "KS60"	,
                  "KS61A"	,
                  "KS61B"	,
                  "KS62"	,
                  "KS63"	,
                  "KS64B"	,
                  "KS65A"	,
                  "KS65B"	,
                  "KS66A"	,
                  "KS66B"	,
                  "KS66C"	,
                  "KS67A"	,
                  "KS67B"	,
                  "KS68A"	,
                  "KS68B"	,
                  #"KS69"	, #hash when 135 var
                  "KS70"	,
                  "KS72A"	,
                  "KS72B"	,
                  "KS73"	,
                  "KS74A"	,
                  "KS74B"	,
                  "KS75A"	,
                  #"KS75B"	,
                  "KS76A"	,
                  "KS76B"	,
                  "KS77A"	,
                  "KS77B"	,
                  "KS77C"	,
                  "KS78A"	,
                  "KS80A"	,
                  "KS80B"	,
                  "KS81"	,
                  "KS82A"	,
                  "KS82B"	,
                  "KS83"	,
                  "KS85"	,
                  "KS88"	,
                  "KS91"	,
                  "KS92B"	,
                  "KS95"	,
                  "KS97A"	,
                  "KS97B"	,
                  "KS98"	,
                  "KS99"	,
                  "KSALLOT2"	,
                  "KSAPP1"	,
                  "KSAPP2"	,
                  "KSKEY"	,
                  "KSMOOR1"	,
                  "KSMOOR2"	,
                  "KSNOR1"	,
                  "KSNUFF2"	,
                  "KSSEL1"	,
                  "KSYEAD1"	,
                  "L.japonica9250"	,
                  "L.minuta9260"	,
                  "L.minor7123"	,
                  "L.minor7295"	,
                  "L.minor8389"	,
                  "L.minuta6600"	,
                  "L.punctata0049"	,
                  "L.trisulca7192"	,
                  "L.yungensis9208"	,
                  "LY01A"	,
                  "LY01B"	,
                  "LY02"	,
                  "LY03"	,
                  "MP01"	,
                  "S.intermedia9394"	,
                  "L. gibba131"	,
                  "L.minor7016"	,
                  "L.minor5500"	,
                  #"S.polyrhiza9504"	,
                  #"L. minuta 9581"	,
                  #"L. minuta 9484"	,
                  #"L.minuta7612"	,
                  #"L.minuta6717"	,
                  "KS04")

colnames(df)

#Estimate a covariance matrix
n <- ncol(df)
cov <- matrix(nrow=n,ncol=n)
for(i in 1:n){
  for(j in 1:i){
    x <- mean(c(ploidy[i],ploidy[j]))
    cov[i,j] <- mean((df[,i]-x*p)*(df[,j]-x*p)/(x*p*(1-p)),na.rm=T)
    cov[j,i] <- cov[i,j]
  }	
}

#Do PCA on the matrix and plot

#missing out cov for accessions with NAs in?

#work ploidy colours in sometime: c("#1e90ff", "#ffa500", "#7cfc00"))

pc <- prcomp(cov,scale=T)
xlab <- paste0("PC1 (",round(summary(pc)$importance[2]*100),"%)")
ylab <- paste0("PC2 (",round(summary(pc)$importance[5]*100),"%)")
pcs <- data.frame(PC1=pc$x[,1],PC2=pc$x[,2],id=colnames(df),ploidy=ploidy)
colors <- pc <- prcomp(cov,scale=T)
xlab <- paste0("PC1 (",round(summary(pc)$importance[2]*100),"%)")
ylab <- paste0("PC2 (",round(summary(pc)$importance[5]*100),"%)")
pcs <- data.frame(PC1=pc$x[,1],PC2=pc$x[,2],id=colnames(df),ploidy=ploidy)
colors <- c("#1e90ff", "#ffa500", "#7cfc00")

#not changing names on graph
#cant not do 0.8 as wont take for pca
#for uklight 0.8 missing stays the same doesnt matter which input prune used?
pcs$id #135 acccessions left

#add for 69 if using 136 var
pcs$Species <- c("L.minuta"	,
                 "L.minuta"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "S.polyrhiza"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.turionifera"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.turionifera"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minuta"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.gibba"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minuta"	,
                 #  "L.minuta", #hash when 135 var
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "S.polyrhiza"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "S.polyrhiza"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.trisulca"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.trisulca"	,
                 "S.polyrhiza"	,
                 "L.minor"	,
                 "S.polyrhiza"	,
                 "L.minor"	,
                 "L.trisulca"	,
                 "L.minor"	,
                 "L.gibba"	,
                 "L.trisulca"	,
                 "L.japonica"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.punctata"	,
                 "L.trisulca"	,
                 "L.yungensis"	,
                 "L.minor"	,
                 "L.minuta"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor"	,
                 "S.intermedia"	,
                 "L.gibba"	,
                 "L.minor"	,
                 "L.minor"	,
                 "L.minor")

pcs$Loco <- c(	"MID"	,
               "MID"	,
               "MID"	,
               #	"WW"	,
               "YOR"	,
               "BFD"	,
               "BFD"	,
               "BFD"	,
               "BFD"	,
               "ELG"	,
               "ELG"	,
               "ELG"	,
               "ELG"	,
               "ABE"	,
               "ABE"	,
               "ABE"	,
               "ABE"	,
               "ABE"	,
               "ABE"	,
               "ABE"	,
               "ABE"	,
               "ABE"	,
               "ABE"	,
               "ABE"	,
               "BFD"	,
               "BFD"	,
               "BFD"	,
               "BFD"	,
               "HUL"	,
               "HUL"	,
               "HUL"	,
               "HUL"	,
               "HUL"	,
               "HUL"	,
               "YOR"	,
               "YOR"	,
               "YOR"	,
               "YOR"	,
               "LAN"	,
               "LAN"	,
               "MID"	,
               "MID"	,
               "MID"	,
               "HAS"	,
               "HAS"	,
               "HAS"	,
               "HAS"	,
               "HAS"	,
               "HAS"	,
               "HAS"	,
               "HAS"	,
               "HAS"	,
               "HAS"	,
               "HAS"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "BRI"	,
               "BRI"	,
               "BRI"	,
               "BRI"	,
               "BRI"	,
               "BRI"	,
               "BRI"	,
               "BRI"	,
               "BRI"	,
               "BRI"	,
               "BRI"	,
               #"BRI"	, #hash if 135 ind	
               "BRI"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               #	"NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "NEW"	,
               "GLA"	,
               "GLA"	,
               "GLA"	,
               "GLA"	,
               "GLA"	,
               "ELG"	,
               "ELG"	,
               "ELG"	,
               "ELG"	,
               "ELG"	,
               "BFD"	,
               "BFD"	,
               "BFD"	,
               "HUL"	,
               "BFD"	,
               "BFD"	,
               "HUL"	,
               "BFD"	,
               "YOR"	,
               "BFD"	,
               "WW"	,
               "WW"	,
               "WW"	,
               "WW"	,
               "WW"	,
               "WW"	,
               "WW"	,
               "WW"	,
               "WW"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "COR"	,
               "NCA"	,
               "WW"	,
               "WW"	,
               "WW"	,
               "WW"	,
               #	"WW"	,
               #	"WW"	,
               #	"WW"	,
               #	"WW"	,
               #	"WW"	,
               "YOR"	)

#need to manually change as some accessions missing? 20/34
ggplot(pcs, aes(PC1, PC2, color=as.factor(ploidy)))+
  geom_point(size=4)+
  labs(x=xlab, y=ylab, color="ploidy")+
  geom_text_repel(aes(label=id), size=4, force=20, max.overlaps=Inf, color="black")+
  theme(panel.background = element_blank(),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        axis.line=element_line(color="black",size=0.5),
        axis.text=element_text(size=11,color="black"),
        axis.ticks.length=unit(.15, "cm"),
        axis.ticks=element_line(color="black",size=0.5),
        axis.title=element_text(size=12, color="black"),
        plot.title=element_text(size=14, color="black", hjust = 0.5),
        legend.text=element_text(size=11, color="black"),
        legend.title=element_text(size=12, color="black"),
        legend.key=element_blank(),
        aspect.ratio=0.5)
#dev.new()

as.factor(pcs$Species)
pcs$Species <- factor(pcs$Species, levels = c("L.minor", "L.minuta", "S.polyrhiza", 
                                              "L.trisulca","L.turionifera",
                                              "L.gibba", "L.japonica", "L.yungensis",
                                              "L.punctata", "S.intermedia"))
pcs$Species
mycolors <- c("#7D0900", "#77EE15", "#632770", 
              "#3260B4", "#17706A", 
              "#FF4E32", "#AA3939", "#2E4E13",
              "#FFA256", "#2915B4")
names(mycolors) <- levels(pcs$Species)

#unlabelled colored plot
#species basic version
ggplot(pcs, aes(PC1, PC2, color=as.factor(Species)))+
  geom_point(size=4)+
  scale_color_manual(values = c(mycolors))+
#  stat_ellipse(geom = "polygon",
#               level = 0.7,
#               aes(color = Species), 
#               alpha = 0.25)+
 #stat_ellipse()+
#stat_ellipse(aes(x=PC1, y=PC2,color=Species, fill = Species),
#               linetype = 1,
#               lwd = 1.2, level=0.95, alpha = 0.4, segments= 51)+
  labs(x=xlab, y=ylab, color="Species")+
  #geom_text_repel(aes(label=id), size=4, force=20, max.overlaps=Inf, color="black")+
  theme(panel.background = element_blank(),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        axis.line=element_line(color="black",size=0.5),
        axis.text=element_text(size=11,color="black"),
        axis.ticks.length=unit(.15, "cm"),
        axis.ticks=element_line(color="black",size=0.5),
        axis.title=element_text(size=12, color="black"),
        plot.title=element_text(size=14, color="black", hjust = 0.5),
        legend.text=element_text(face="italic", size=11, color="black"),
        legend.title=element_text(size=12, color="black"),
        legend.key=element_blank(),
        aspect.ratio=0.5)
#dev.new()
#ggsave("PCA_all_newcols_0.8pruned.svg")
ggsave("PCA_all_newcols_0.8pruned.pdf", dpi = 300, width = 15, height = 15 , units = "cm") 


#species basic version
ggplot(pcs, aes(PC1, PC2, color=as.factor(Species)))+
  geom_point(size=4)+
  scale_color_manual(values = c("L.gibba" = "gold",
                                "L.japonica" ="orange",
                                "L.minor" ="red",
                                "L.minuta" = "green",
                                "L.punctata" = "brown",
                                "L.trisulca" = "grey",
                                "L.turionifera" = "blue",
                                "L.yungensis" = "darkgreen",
                                "S.intermedia" = "steelblue",
                                "S.polyrhiza" = "purple"))+
  labs(x=xlab, y=ylab, color="Species")+
  #geom_text_repel(aes(label=id), size=4, force=20, max.overlaps=Inf, color="black")+
  theme(panel.background = element_blank(),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        axis.line=element_line(color="black",size=0.5),
        axis.text=element_text(size=11,color="black"),
        axis.ticks.length=unit(.15, "cm"),
        axis.ticks=element_line(color="black",size=0.5),
        axis.title=element_text(size=12, color="black"),
        plot.title=element_text(size=14, color="black", hjust = 0.5),
        legend.text=element_text(size=11, color="black"),
        legend.title=element_text(size=12, color="black"),
        legend.key=element_blank(),
        aspect.ratio=0.5)
#dev.new()

#for l minor
pcs$Loco <- c("YOR"	,
              "BFD"	,
              "BFD"	,
              "ELG"	,
              "ELG"	,
              "ELG"	,
              "ELG"	,
              "ABE"	,
              "ABE"	,
              "ABE"	,
              "ABE"	,
              "ABE"	,
              "ABE"	,
              "ABE"	,
              "ABE"	,
              "ABE"	,
              "ABE"	,
              "ABE"	,
              "BFD"	,
              "BFD"	,
              "BFD"	,
              "HUL"	,
              "HUL"	,
              "HUL"	,
              "YOR"	,
              "YOR"	,
              "YOR"	,
              "LAN"	,
              "LAN"	,
              "MID"	,
              "MID"	,
              "HAS"	,
              "HAS"	,
              "HAS"	,
              "HAS"	,
              "HAS"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "BRI"	,
              "BRI"	,
              "BRI"	,
              "BRI"	,
              "BRI"	,
              "BRI"	,
              "BRI"	,
              "NEW"	,
              "NEW"	,
              "NEW"	,
              "NEW"	,
              #"NEW"	,
              "NEW"	,
              "NEW"	,
              "NEW"	,
              "NEW"	,
              "GLA",
              "GLA"	,
              "GLA"	,
              "GLA"	,
              "ELG"	,
              "ELG"	,
              "ELG"	,
              "ELG"	,
              "BFD"	,
              "HUL"	,
              "BFD"	,
              "BFD"	,
              "WW"	,
              "WW"	,
              "WW"	,
              "COR"	,
              "COR"	,
              "COR"	,
           "NCA"	,
              "WW"	,
              "WW"	,
              "YOR"	)

#rename l.minor species
colnames(Lmino) <- c("KS02"	,
                     "KS03"	,
                     "KS09"	,
                     "KS100"	,
                     "KS101"	,
                     "KS104"	,
                     "KS107"	,
                     "KS108"	,
                     "KS109"	,
                     "KS110"	,
                     "KS111"	,
                     "KS112"	,
                     "KS114"	,
                     "KS115"	,
                     "KS116"	,
                     "KS117"	,
                     "KS118"	,
                     "KS119"	,
                     "KS13"	,
                     "KS14"	,
                     "KS15"	,
                     "KS17"	,
                     "KS18"	,
                     "KS21"	,
                     "KS27"	,
                     "KS28"	,
                     "KS29"	,
                     "KS33"	,
                     "KS34"	,
                     "KS39"	,
                     "KS40A"	,
                     "KS43"	,
                     "KS44A"	,
                     "KS47"	,
                     "KS48A"	,
                     "KS50"	,
                     "KS51A"	,
                     "KS52"	,
                     "KS53"	,
                     "KS54"	,
                     "KS55"	,
                     "KS56"	,
                     "KS57"	,
                     "KS59"	,
                     "KS60"	,
                     "KS61A"	,
                     "KS62"	,
                     "KS63"	,
                     "KS65A"	,
                     "KS65B"	,
                     "KS66A"	,
                     "KS67A"	,
                     "KS68A"	,
                     "KS70"	,
                     "KS72A"	,
                     "KS73"	,
                     "KS74A"	,
                     "KS75A"	,
                     #"KS75B"	,
                     "KS77B"	,
                     "KS80A"	,
                     "KS81"	,
                     "KS82A",
                     "KS82B"	,
                     "KS83"	,
                     "KS88"	,
                     "KS91"	,
                     "KS95"	,
                     "KS97B"	,
                     "KS98"	,
                     "KS99"	,
                     "KSALLOT2"	,
                     "KSKEY"	,
                     "KSMOOR2"	,
                     "KSNUFF2"	,
                     "L.minor7123"	,
                     "L.minor7295"	,
                     "L.minor8389"	,
                     "LY01A"	,
                     "LY02"	,
                     "LY03"	,
                     "MP01"	,
                     "L.minor7016"	,
                     "L.minor5500"	,
                     "KS04"	
)

#run this for l mino and l minuta once pca form
pcs$Loco <- as.factor(pcs$Loco)
pcs$Loco <- factor(pcs$Loco, levels = c("COR", "BRI", "HAS",
                                        "NEW", "GLA", "ABE",
                                        "ELG", "BFD", "YOR",
                                        "HUL", "LAN", "MID", 
                                        "WW"))

#reorder and color places
#rem NCA/MP01
pcs <- pcs[-(81),]
colnames(pcs) <- c("PC1", "PC2", "id", "ploidy", "Region")
pcs$Region <- factor(pcs$Region, levels = c("ELG", "ABE", "GLA", "LAN","BFD", "YOR", "HUL", "MID", "NEW", "BRI", "HAS", "COR", "WW"))
pcs$Region
mycolors <- c("#632770", "#2915B4", "#3260B4", "#17706A",
              "#2E4E13", "#2D882D", "#77EE15", "#ABF057",
              "#FFA256", "#FF4E32", "#AA3939", "#7D0900", "#000000")
#loco
ggplot(pcs, aes(PC1, PC2, color=as.factor(Region)))+
  geom_point(aes(shape = Region), size=3.5)+
  scale_shape_manual(values = c(16,16,17,16,16,16,17,16,16,16,17,16,16)) +
  scale_color_manual(values = c(mycolors))+
  labs(x=xlab, y=ylab, color="Region")+
  ggtitle("L. minor") +
  #geom_text_repel(aes(label=id), size=4, force=20, max.overlaps=Inf, color="black")+
  theme(panel.background = element_blank(),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        axis.line=element_line(color="black",size=0.5),
        axis.text=element_text(size=11,color="black"),
        axis.ticks.length=unit(.15, "cm"),
        axis.ticks=element_line(color="black",size=0.5),
        axis.title=element_text(size=12, color="black"),
        plot.title=element_text(size=14, face="italic", color="black", hjust = 0),
        legend.text=element_text(size=11, color="black"),
        legend.title=element_text(size=12, color="black"),
        legend.key=element_blank(),
        aspect.ratio=0.5)
#dev.new()
#ggsave("PCA_Lmino_newcols_0.8pruned.svg")
ggsave("PCA_Lmino_newcols_shapes_0.8pruned.pdf", dpi = 300, width = 15, height = 15 , units = "cm") 


#l minuta rename
colnames(Lminu)
colnames(df)
colnames(Lminu) <- c("AL01"	,
                     "AL02"	,
                     "AL03"	,
                     "KS06A"	,
                     "KS06B"	,
                     "KS20"	,
                     "KS25"	,
                     "KS38"	,
                     "KS42"	,
                     "KS44B"	,
                     "KS45"	,
                     "KS46B"	,
                     "KS48B"	,
                     "KS49"	,
                     "KS51B"	,
                     "KS58B"	,
                     "KS61B"	,
                     "KS64B"	,
                     "KS66C"	,
                     "KS67B"	,
                     "KS68B"	,
                    # "KS69"	,#
                     "KS72B"	,
                     "KS74B"	,
                     "KS76B"	,
                     "KS77C"	,
                     "KS80B"	,
                     "KS85"	,
                     "KS92B"	,
                     "L.minuta9260"	,
                     "L.minuta6600"	,
                     "LY01B"	)

pcs$Loco <- c("MID"	,
              "MID"	,
              "MID"	,
              "BFD"	,
              "BFD"	,
              "HUL"	,
              "YOR"	,
              "MID"	,
              "HAS"	,
              "HAS"	,
              "HAS"	,
              "HAS"	,
              "HAS"	,
              "HAS"	,
              "COR"	,
              "COR"	,
              "COR"	,
              "BRI"	,
              "BRI"	,
              "BRI"	,
              "BRI"	, 
              "NEW"	,
              "NEW"	,
              "NEW"	,
              "NEW"	,
              "NEW"	,
              "GLA"	,
              "GLA"	,
              "WW"	,
              "WW"	,
              "COR"	)

#reorder and color places
colnames(pcs) <- c("PC1", "PC2", "id", "ploidy", "Region")
pcs$Region <- factor(pcs$Region, levels = c("ELG", "ABE", "GLA", "LAN","BFD", "YOR", "HUL", "MID", "NEW", "BRI", "HAS", "COR", "WW"))
pcs$Region
mycolors <- c("#3260B4", 
              "#2E4E13", "#2D882D", "#77EE15", "#ABF057",
              "#FFA256", "#FF4E32", "#AA3939", "#7D0900", "#000000")
#loco
ggplot(pcs, aes(PC1, PC2, color=as.factor(Region)))+
  geom_point(aes(shape = Region), size=3.5)+
  scale_shape_manual(values = c(17,16,16,17,16,16,16,17,16,16)) +
  scale_color_manual(values = c(mycolors))+
  labs(x=xlab, y=ylab, color="Region")+
  ggtitle("L. minuta") +
  #geom_text_repel(aes(label=id), size=4, force=20, max.overlaps=Inf, color="black")+
  theme(panel.background = element_blank(),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        axis.line=element_line(color="black",size=0.5),
        axis.text=element_text(size=11,color="black"),
        axis.ticks.length=unit(.15, "cm"),
        axis.ticks=element_line(color="black",size=0.5),
        axis.title=element_text(size=12, color="black"),
        plot.title=element_text(size=14, face="italic", color="black", hjust = 0),
        legend.text=element_text(size=11, color="black"),
        legend.title=element_text(size=12, color="black"),
        legend.key=element_blank(),
        aspect.ratio=0.5)
#dev.new()
#ggsave("PCA_Lmino_newcols_0.8pruned.svg")
#ggsave("PCA_Lminu_newcols_0.8pruned.pdf", dpi = 300, width = 15, height = 15 , units = "cm") 

