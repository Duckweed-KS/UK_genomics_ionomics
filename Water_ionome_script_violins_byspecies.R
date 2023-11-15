setwd("C:\\Users\\kelli\\OneDrive - The University of Nottingham\\Nottingham\\Year 2\\Duckweed ionomics analysis")

gr <- read.csv("Water_summary_justmeans_banaomit_loqrem_alldata_rname_speciesadd.csv") #added sp loco country

names(gr)

#gr$Species <- factor(gr$Species,levels = c("L. minor", "L. japonica", "L. minuta", "L. trisulca", "L. gibba",
#                                     "L. turionifera", "Lmu/Lmo", "Ltr/Lmo", "Lmo/Ltr/Lmu", "S. polyrhiza"))


gr$Species <- factor(gr$Species,levels = c("L. minor", "L. minuta","S. polyrhiza", "L. trisulca","L. turionifera",
                                           "L. gibba","L. japonica", "Lmu/Lmo", "Ltr/Lmo", "Lmo/Ltr/Lmu"))

mycolors <- c("#7D0900", "#77EE15", "#632770", 
              "#3260B4", "#17706A", 
              "#FF4E32", "#AA3939", "#2E4E13",
              "#FFA256", "#2915B4")
names(mycolors) <- levels(gr$Species)
#violin plots
library(ggplot2)
B <- ggplot(gr, aes(x=Species, y=B, color=Species)) + 
  geom_violin(trim=FALSE)
B + stat_summary(fun.y=mean, geom="point", size=2, color="red")
B + geom_boxplot(width=0.1)
B + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
B + geom_jitter(shape=16, position=position_jitter(0.2))

Na <- ggplot(gr, aes(x=Species, y=Na, color=Species)) + 
  geom_violin(trim=FALSE)
Na + geom_jitter(shape=16, position=position_jitter(0.2))

Li <- ggplot(gr, aes(x=Species, y=Li, color=Species)) + 
  geom_violin(trim=FALSE)
Li + geom_jitter(shape=16, position=position_jitter(0.2))

Mg <- ggplot(gr, aes(x=Species, y=Mg, color=Species)) + 
  geom_violin(trim=FALSE)
Mg + geom_jitter(shape=16, position=position_jitter(0.2))

Al <- ggplot(gr, aes(x=Species, y=Al, color=Species)) + 
  geom_violin(trim=FALSE)
Al + geom_jitter(shape=16, position=position_jitter(0.2))

Si <- ggplot(gr, aes(x=Species, y=Si, color=Species)) + 
  geom_violin(trim=FALSE)
Si + geom_jitter(shape=16, position=position_jitter(0.2))

P <- ggplot(gr, aes(x=Species, y=P, color=Species)) + 
  geom_violin(trim=FALSE)
P + geom_jitter(shape=16, position=position_jitter(0.2))

S <- ggplot(gr, aes(x=Species, y=S, color=Species)) + 
  geom_violin(trim=FALSE)
S + geom_jitter(shape=16, position=position_jitter(0.2))

K <- ggplot(gr, aes(x=Species, y=K, color=Species)) + 
  geom_violin(trim=FALSE)
K + geom_jitter(shape=16, position=position_jitter(0.2))

Ca <- ggplot(gr, aes(x=Species, y=Ca, color=Species)) + 
  geom_violin(trim=FALSE)
Ca + geom_jitter(shape=16, position=position_jitter(0.2))

Mn <- ggplot(gr, aes(x=Species, y=Mn, color=Species)) + 
  geom_violin(trim=FALSE)
Mn + geom_jitter(shape=16, position=position_jitter(0.2))

Fe <- ggplot(gr, aes(x=Species, y=Fe, color=Species)) + 
  geom_violin(trim=FALSE)
Fe + geom_jitter(shape=16, position=position_jitter(0.2))

Co <- ggplot(gr, aes(x=Species, y=Co, color=Species)) + 
  geom_violin(trim=FALSE)
Co + geom_jitter(shape=16, position=position_jitter(0.2))

Ni <- ggplot(gr, aes(x=Species, y=Ni, color=Species)) + 
  geom_violin(trim=FALSE)
Ni + geom_jitter(shape=16, position=position_jitter(0.2))

Cu <- ggplot(gr, aes(x=Species, y=Cu, color=Species)) + 
  geom_violin(trim=FALSE)
Cu + geom_jitter(shape=16, position=position_jitter(0.2))

Zn <- ggplot(gr, aes(x=Species, y=Zn, color=Species)) + 
  geom_violin(trim=FALSE)
Zn + geom_jitter(shape=16, position=position_jitter(0.2))

As <- ggplot(gr, aes(x=Species, y=As, color=Species)) + 
  geom_violin(trim=FALSE)
As + geom_jitter(shape=16, position=position_jitter(0.2))

Rb <- ggplot(gr, aes(x=Species, y=Rb, color=Species)) + 
  geom_violin(trim=FALSE)
Rb + geom_jitter(shape=16, position=position_jitter(0.2))

Sr <- ggplot(gr, aes(x=Species, y=Sr, color=Species)) + 
  geom_violin(trim=FALSE)
Sr + geom_jitter(shape=16, position=position_jitter(0.2))

Mo <- ggplot(gr, aes(x=Species, y=Mo, color=Species)) + 
  geom_violin(trim=FALSE)
Mo + geom_jitter(shape=16, position=position_jitter(0.2))

Cd <- ggplot(gr, aes(x=Species, y=Cd, color=Species)) + 
  geom_violin(trim=FALSE)
Cd + geom_jitter(shape=16, position=position_jitter(0.2))

Pb <- ggplot(gr, aes(x=Species, y=Pb, color=Species)) + 
  geom_violin(trim=FALSE)
Pb + geom_jitter(shape=16, position=position_jitter(0.2))

#stats
pairwise.wilcox.test(gr$B, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Na, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Mg, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Al, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Li, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Si, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$P, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$S, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$K, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Ca, gr$Species,
                     p.adjust.method = "BH") # 0.0071 L. minor from L jp and L minu
pairwise.wilcox.test(gr$Mn, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Fe, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Co, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Ni, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Cu, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Zn, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$As, gr$Species,
                     p.adjust.method = "BH") # L jp and l mino 0.025
pairwise.wilcox.test(gr$Rb, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Sr, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Mo, gr$Species,
                     p.adjust.method = "BH") # L jp and l mino 0.019
pairwise.wilcox.test(gr$Cd, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Pb, gr$Species,
                     p.adjust.method = "BH")

#ca, as and mo diff:# ca 0.0071 L. minor from L jp and L minu
# as L jp and l mino 0.025 # mo L jp and l mino 0.019
#most ns high variability within species, 
Ca <- ggplot(gr, aes(x=Species, y=Ca, color=Species)) + 
  geom_violin(trim=FALSE)
Ca + geom_jitter(shape=16, position=position_jitter(0.2))

# l jp and l minu more than l mino?

As <- ggplot(gr, aes(x=Species, y=As, color=Species)) + 
  geom_violin(trim=FALSE)
As + geom_jitter(shape=16, position=position_jitter(0.2))

# l jp more than l mino?

Mo <- ggplot(gr, aes(x=Species, y=Mo, color=Species)) + 
  geom_violin(trim=FALSE)
Mo + geom_jitter(shape=16, position=position_jitter(0.2))
# l jp more than l mino?

#try levi plots
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(gridExtra)

#change order of subgroups

#violin jitter box
Ca <- ggplot(gr, aes(x=Species, y=Ca, fill=Species)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(width=0.75, color="grey") +
  #scale_x_continuous(limits=c("four", "three", "two", "one")) +
  scale_fill_manual(values=mycolors) +
  geom_boxplot(width=0.2, color="black") +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1),
               geom="pointrange", color="black",
               shape = 18, size = 0.75,
               position = position_dodge(width = 0.9))+
  theme_classic(base_size = 14) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  # ggtitle("") +
  xlab("Species") +
  ylab("Ca in water sites") +
  ylim(0,80000)  +
  #ggtitle("Ca content in water sites")+
  theme(axis.text.x = element_text(angle = 0))+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")    
Ca

#ggsave(file="Lemna_mgcontent.pdf", width=6, height=4.5, dpi=300)

#violin jitter box
As <- ggplot(gr, aes(x=Species, y=As, fill=Species)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(width=0.75, color="grey") +
  #scale_x_continuous(limits=c("four", "three", "two", "one")) +
  scale_fill_manual(values=mycolors) +
  geom_boxplot(width=0.2, color="black") +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1),
               geom="pointrange", color="black",
               shape = 18, size = 0.75,
               position = position_dodge(width = 0.9))+
  theme_classic(base_size = 14) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5, size=10)) +
  # ggtitle("") +
  xlab("Species") +
  ylab("As in water sites") +
  ylim(0,10)  +
  #ggtitle("As content in water sites")+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")    
As

#violin jitter box
Mo <- ggplot(gr, aes(x=Species, y=Mo, fill=Species)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(width=0.75, color="grey") +
  #scale_x_continuous(limits=c("four", "three", "two", "one")) +
  scale_fill_manual(values=mycolors) +
  geom_boxplot(width=0.2, color="black") +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1),
               geom="pointrange", color="black",
               shape = 18, size = 0.75,
               position = position_dodge(width = 0.9))+
  theme_classic(base_size = 14) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5, size=10)) +
# ggtitle("") +
  xlab("Species") +
  ylab("Mo in water sites") +
  ylim(0,10)  +
  #ggtitle("Mo content in water sites")+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")    
Mo

#violin jitter box
B <- ggplot(gr, aes(x=Species, y=B, fill=Species)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(width=0.75, color="grey") +
  #scale_x_continuous(limits=c("four", "three", "two", "one")) +
  scale_fill_manual(values=mycolors) +
  geom_boxplot(width=0.2, color="black") +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1),
               geom="pointrange", color="black",
               shape = 18, size = 0.75,
               position = position_dodge(width = 0.9))+
  theme_classic(base_size = 14) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5, size=10)) +
  # ggtitle("") +
  xlab("Species") +
  ylab("B in water sites") +
  ylim(0,500)  +
 # ggtitle("B content in water sites")+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")    
B
# grid and save
#gridAB  <- grid.arrange(Ca, B, Mo, As)
gridAB  <- grid.arrange(B, Mo, As, ncol=2)
ggsave(filename="Water_byspecies_sig_labels_noca.pdf", width=10, height=7, plot=gridAB)

#kruskal wallis
kruskal.test(Ca ~ Species, data = gr) #sig
kruskal.test(As ~ Species, data = gr) #sig
kruskal.test(Mo ~ Species, data = gr) #sig

#test others
kruskal.test(Li ~ Species, data = gr) #ns
kruskal.test(B ~ Species, data = gr) #sig
kruskal.test(Na ~ Species, data = gr) #ns
kruskal.test(Mg ~ Species, data = gr) #sig 
kruskal.test(Al ~ Species, data = gr) #sig
kruskal.test(Si ~ Species, data = gr) #ns
kruskal.test(P ~ Species, data = gr) #sig
kruskal.test(S ~ Species, data = gr) #ns
kruskal.test(K ~ Species, data = gr) #ns
kruskal.test(Mn ~ Species, data = gr) #ns
kruskal.test(Fe ~ Species, data = gr) #ns
kruskal.test(Co ~ Species, data = gr) #ns
kruskal.test(Ni ~ Species, data = gr) #ns
kruskal.test(Cu ~ Species, data = gr) #sig 
kruskal.test(Zn ~ Species, data = gr) #ns
kruskal.test(Rb ~ Species, data = gr) #ns
kruskal.test(Sr ~ Species, data = gr) #sig
kruskal.test(Cd ~ Species, data = gr) #ns
kruskal.test(Pb ~ Species, data = gr) #ns

### Dunn test
#install.packages("FSA")
library(FSA)
dunnTest(Ca ~ Species,
              data=gr,
              method="bh") # Ljp - Lmino, Lmino-Lminu
dunnTest(As ~ Species,
         data=gr,
         method="bh") # Ljp - Lmino
dunnTest(Mo ~ Species,
         data=gr,
         method="bh") # Ljp - Lmino

#new ones from KW test to try
#B, Mg, Al, P, Cu, Sr
#just B sig, to include plot above
dunnTest(B ~ Species,
         data=gr,
         method="bh") # Ljp - Lmino
dunnTest(Mg ~ Species,
         data=gr,
         method="bh") # ns
dunnTest(Al ~ Species,
         data=gr,
         method="bh") # ns
dunnTest(P ~ Species,
         data=gr,
         method="bh") # ns
dunnTest(Cu ~ Species,
         data=gr,
         method="bh") # ns
dunnTest(Sr ~ Species,
         data=gr,
         method="bh") # ns

#heat map for species differences 

#summarise so avg per sp
#loop this function so do for each column?
library(dplyr)
sum <- gr %>% group_by(Species) %>% 
  summarise_all(.funs = c(mean="mean"))

sum$Species <- factor(gr$Species,levels = c("L. minor", "L. minuta","S. polyrhiza", "L. trisulca","L. turionifera",
                                           "L. gibba","L. japonica", "Lmu/Lmo", "Ltr/Lmo", "Lmo/Ltr/Lmu"))
#change col names
names(sum)
colnames(sum) <- c("Species", "Accession", "Site", "Loco",
                   "Li", "B", "Na", "Mg","Al", "Si", "P", "S", 
                   "K", "Ca", "Mn", "Fe","Co", "Ni", "Cu", "Zn", 
                   "As", "Rb", "Sr", "Mo","Cd", "Pb")
                   
#plot in new heatmap
library(RColorBrewer)
#scale numeric data
ions_all <- sum[5:26]
ions_sc <- scale(ions_all,center=T,scale=T)
ions_sc <- as.matrix(ions_sc)

log10(ions$Na)

#assign row names from df
#rownames(ions_sc) <- gr$Accession
rownames(ions_sc) <- sum$Species
heatmap(ions_sc)
heatmap(ions_sc, Colv = NA, Rowv = NA, scale="column")
#change color scheme yellow blue
colfunc <- colorRampPalette(c("blue", "yellow"))
heatmap(ions_sc,col=colfunc(11),scale="row")

#tiff  normally shows full dataset but not here
#tto make pretty, rename, add sclae bar?
par(mar=c(7,4,4,2)+0.1)
#tiff('Heatmap_species_flavours_1.tiff', units="in", width=14, height=12, res=300, compression = 'lzw')
#tiff('Heatmap_allflav_avgs_species_newsp.tiff', units="in", width=12, height=12, res=300, compression = 'lzw')
tiff('Water_ionome_avgs_species.tiff', units="in", width=15, height=12, res=300, compression = 'lzw')
par(mfrow = c(3, 3),  mar=c(5,4.5,4,2))
colfunc <- colorRampPalette(c("blue", "yellow"))
heatmap(gr,col=colfunc(15),scale="column", cexCol=1.2,margins=c(12,8))
#heatmap(ions_sc,col=colfunc(15),scale="column",cexCol=0.9,margins=c(12,8))
dev.off()
