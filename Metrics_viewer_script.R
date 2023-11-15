#script to view vcftool metrics for all_red
setwd("C:\\Users\\kelli\\OneDrive - The University of Nottingham\\Nottingham\\Duckweed\\adgenet\\2023\\all_red")

pi.all <- read.table("DW_all_samples_10kb.windowed.pi",header=T)
pi.Ljp <- read.table("lemna_jap_snps.vcf_10kb.windowed.pi",header=T)
pi.Lmo <- read.table("lemna_minor2_snps.vcf_10kb.windowed.pi",header=T)
pi.Lmu <- read.table("lemna_minuta_snps.vcf_10kb.windowed.pi",header=T)
pi.Lmojp <- read.table("lemna_mo.jap2_snps.vcf_10kb.windowed.pi",header=T)

#histogram
hist(pi.all$PI,br=20)
hist(pi.Ljp$PI,br=20)
#box wisp
boxplot(pi.all$PI,ylab="diversity")
boxplot(pi.Ljp$PI,ylab="diversity")
#pi along chromo 12
pi.chr12 <- subset(pi.all, CHROM == "chr12")
#find out how many rows your objects have
nrow(pi.chr12)
nrow(pi.all)

#see thr first and last lines
head(pi.chr12)
head(pi.all)

tail(pi.chr12)
tail(pi.all)

#see a summary of the data frame
summary(pi.chr12)
summary(pi.all)
summary(pi.Ljp)
summary(pi.Lmo)
summary(pi.Lmu)

mean(pi.Ljp$PI) #0.005
mean(pi.Lmo$PI) #0.003
mean(pi.Lmu$PI) #9.109092e-05
mean(pi.Lmojp$PI) #0.004

#plot pi for choromo12
plot(pi.chr12$BIN_START,pi.chr12$PI,xlab="position",ylab="diversity")
#diversity along
plot(pi.all$BIN_START,pi.all$PI,xlab="position",ylab="diversity")

plot(pi.Ljp$BIN_START,pi.Ljp$PI,xlab="position",ylab="diversity")

#taj d
taj.all <- read.table("DW_all_samples_10kb.Tajima.D",header=T)
taj.Ljp <- read.table("lemna_jap_snps_TajD.vcf_10kb.Tajima.D",header=T)
taj.Lmo <- read.table("lemna_minor2_snps.vcf_TajD_10kb.Tajima.D",header=T)
taj.Lmu <- read.table("lemna_minuta_snps_TajD.vcf_10kb.Tajima.D",header=T)
taj.Lmojp <- read.table("lemna_mo.jap2_snps.vcf_TajD_10kb.Tajima.D",header=T)

#histogram
hist(taj.all$TajimaD,br=20)
plot(taj.chr36$BIN_START,taj.chr36$TajimaD,xlab="position",ylab="Tajima's D")
#all
plot(taj.all$BIN_START,taj.all$TajimaD,xlab="position",ylab="Tajima's D")

hist(taj.Ljp$TajimaD,br=20)

mean(taj.Ljp$TajimaD) #done in excel 0.442259696
mean(taj.Lmo$TajimaD) # excel 0.03213046
mean(taj.Lmu$TajimaD) #0.806684927
mean(taj.Lmojp$TajimaD) #0.067847843

#fst
fst.p1p2 <- read.table("pop1_vs_pop2_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p1p2$MEAN_FST,br=20)
#ALL
plot(fst.p1p2$BIN_START,fst.p1p2$MEAN_FST,xlab="position",ylab="FST")
fst.p1p2$MEAN_FST
library(dplyr)
mean(fst.p1p2$MEAN_FST) #0.26 for pop1 pop2

#fst
fst.p1p3 <- read.table("pop1_vs_pop3_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p1p3$MEAN_FST,br=20)
#ALL
plot(fst.p1p3$BIN_START,fst.p1p3$MEAN_FST,xlab="position",ylab="FST")
fst.p1p3$MEAN_FST
library(dplyr)
mean(fst.p1p3$MEAN_FST) #0.58 for pop1 pop3

#fst
fst.p1p4 <- read.table("pop1_vs_pop4_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p1p4$MEAN_FST,br=20)
#ALL
plot(fst.p1p4$BIN_START,fst.p1p4$MEAN_FST,xlab="position",ylab="FST")
fst.p1p4$MEAN_FST
library(dplyr)
mean(fst.p1p4$MEAN_FST) #0.55 for pop1 pop4

#fst
fst.p1p5 <- read.table("pop1_vs_pop5_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p1p5$MEAN_FST,br=20)
#ALL
plot(fst.p1p5$BIN_START,fst.p1p5$MEAN_FST,xlab="position",ylab="FST")
fst.p1p5$MEAN_FST
library(dplyr)
mean(fst.p1p5$MEAN_FST) #0.31 for pop1 pop5

#fst
fst.p2p3 <- read.table("pop2_vs_pop3_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p2p3$MEAN_FST,br=20)
#ALL
plot(fst.p2p3$BIN_START,fst.p2p3$MEAN_FST,xlab="position",ylab="FST")
fst.p2p3$MEAN_FST
library(dplyr)
mean(fst.p2p3$MEAN_FST) #0.31 for pop2 pop3

#fst
fst.p2p4 <- read.table("pop2_vs_pop4_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p2p4$MEAN_FST,br=20)
#ALL
plot(fst.p2p4$BIN_START,fst.p2p4$MEAN_FST,xlab="position",ylab="FST")
fst.p2p4$MEAN_FST
library(dplyr)
mean(fst.p2p4$MEAN_FST) #0.31 for pop2 pop4

fst.p2p5 <- read.table("pop2_vs_pop5_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p2p5$MEAN_FST,br=20)
#ALL
plot(fst.p2p5$BIN_START,fst.p2p5$MEAN_FST,xlab="position",ylab="FST")
fst.p2p5$MEAN_FST
library(dplyr)
mean(fst.p2p5$MEAN_FST) #0.57 for pop2 pop4

fst.p3p4 <- read.table("pop3_vs_pop4_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p3p4$MEAN_FST,br=20)
#ALL
plot(fst.p3p4$BIN_START,fst.p3p4$MEAN_FST,xlab="position",ylab="FST")
fst.p3p4$MEAN_FST
library(dplyr)
mean(fst.p3p4$MEAN_FST) #0.9 for pop3 pop4

fst.p3p5 <- read.table("pop3_vs_pop5_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p3p5$MEAN_FST,br=20)
#ALL
plot(fst.p3p5$BIN_START,fst.p3p5$MEAN_FST,xlab="position",ylab="FST")
fst.p3p5$MEAN_FST
library(dplyr)
mean(fst.p3p5$MEAN_FST) #0.25 for pop3 pop5

fst.p4p5 <- read.table("pop4_vs_pop5_FST_10kb.windowed.weir.fst",header=T)
#hist
hist(fst.p4p5$MEAN_FST,br=20)
#ALL
plot(fst.p4p5$BIN_START,fst.p4p5$MEAN_FST,xlab="position",ylab="FST")
fst.p4p5$MEAN_FST
library(dplyr)
mean(fst.p4p5$MEAN_FST) #0.25 for pop3 pop5
