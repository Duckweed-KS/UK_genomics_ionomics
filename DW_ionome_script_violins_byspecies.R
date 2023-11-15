setwd("C:\\Users\\kelli\\OneDrive - The University of Nottingham\\Nottingham\\Year 2\\Duckweed ionomics analysis")

gr <- read.csv("DW_ionomics_summary_justmeans-ba_omitna_addspecies.csv") #added sp loco country

names(gr)

#gr$Species <- factor(gr$Species,levels = c("L. minor", "L. japonica", "L. minuta", "L. trisulca", "L. gibba",
#                                     "L. turionifera", "Lmu/Lmo", "Ltr/Lmo", "Lmo/Ltr/Lmu", "S. polyrhiza"))

length(unique(gr$Species)) #8
levels(factor(gr$Species))

gr$Species <- factor(gr$Species,levels = c("L. minor", "L. minuta","S. polyrhiza", "L. trisulca","L. turionifera",
                                           "L. gibba","L. japonica", "Lmu/Lmo"))

mycolors <- c("#7D0900", "#77EE15", "#632770", 
              "#3260B4", "#17706A", 
              "#FF4E32", "#AA3939", "#2E4E13")
names(mycolors) <- levels(gr$Species)

#remove when not more than 1 rep - Lgibba and Ltrisulca here
gr <- read.csv("DW_ionomics_summary_justmeans-ba_omitna_addspecies_remlowdups.csv") #added sp loco country

length(unique(gr$Species)) #6
levels(factor(gr$Species))


gr$Species <- factor(gr$Species,levels = c("L. minor", "L. minuta","S. polyrhiza", "L. turionifera",
                                           "L. japonica", "Lmu/Lmo"))

mycolors <- c("#7D0900", "#77EE15", "#632770", 
              "#17706A", 
               "#AA3939", "#2E4E13")
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
                     p.adjust.method = "BH") # L minu from L mino, S poly from L minu, L turi from L minu, Ljp L minu, L jp L mino, Lmu/mo L mino and S poly
#when 6 sp: L minu from L,mo, Spol, L tu, Ljp, L turio diff Lmo, L jp diff to Lmo, L tu, Lmu/Lmo diff to Lmo, S pol, L turi, L jp
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
                     p.adjust.method = "BH") # Ljp from Lmo,Ljp from Lmu/mo
#6 sp: Ljp from Lmo, Lmu/Lmo from Ljp
pairwise.wilcox.test(gr$Ca, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Mn, gr$Species,
                     p.adjust.method = "BH") # Lmu/Lmo from Lmo
#6 sp: S pol diff Lmo and Lmu, L jp diff S pol, Lmu/Lmo diff L mo, S pol, L jp
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
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Rb, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Sr, gr$Species,
                     p.adjust.method = "BH")
#6 sp: Lmu/Lmo diff Lmo, S pol and L jp
pairwise.wilcox.test(gr$Mo, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Cd, gr$Species,
                     p.adjust.method = "BH")
pairwise.wilcox.test(gr$Pb, gr$Species,
                     p.adjust.method = "BH")

#mg, k, mn diff between species
Mg <- ggplot(gr, aes(x=Species, y=Mg, color=Species)) + 
  geom_violin(trim=FALSE)
Mg + geom_jitter(shape=16, position=position_jitter(0.2))

K <- ggplot(gr, aes(x=Species, y=K, color=Species)) + 
  geom_violin(trim=FALSE)
K + geom_jitter(shape=16, position=position_jitter(0.2))

Mn <- ggplot(gr, aes(x=Species, y=Mn, color=Species)) + 
  geom_violin(trim=FALSE)
Mn + geom_jitter(shape=16, position=position_jitter(0.2))

#try levi plots
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(gridExtra)

#change order of subgroups

#violin jitter box
Mg <- ggplot(gr, aes(x=Species, y=Mg, fill=Species)) + # fill=name allow to automatically dedicate a color for each group
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
  ylab("Mg content of duckweed") +
  ylim(0,10000)  +
  #ggtitle("Ca content in water sites")+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")    
Mg

#ggsave(file="Lemna_mgcontent.pdf", width=6, height=4.5, dpi=300)

#violin jitter box
K <- ggplot(gr, aes(x=Species, y=K, fill=Species)) + # fill=name allow to automatically dedicate a color for each group
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
  ylab("K content of duckweed") +
  ylim(0,120000)  +
  #ggtitle("As content in water sites")+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")    
K

#violin jitter box
Mn <- ggplot(gr, aes(x=Species, y=Mn, fill=Species)) + # fill=name allow to automatically dedicate a color for each group
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
  ylab("Mn content of duckweeds") +
  ylim(0,5000)  +
  #ggtitle("Mo content in water sites")+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")    
Mn

# grid and save
#gridAB  <- grid.arrange(Ca, B, Mo, As)
gridAB  <- grid.arrange(K, Mg, Mn, ncol=2)
ggsave(filename="DW_byspecies_sig_labels_6sp.pdf", width=10, height=7, plot=gridAB)

#kruskal wallis
kruskal.test(Mg ~ Species, data = gr) #sig
kruskal.test(K ~ Species, data = gr) #sig
kruskal.test(Mn ~ Species, data = gr) #sig

#test others
kruskal.test(Li ~ Species, data = gr) #ns
kruskal.test(B ~ Species, data = gr) #sig
kruskal.test(Na ~ Species, data = gr) #sig 
kruskal.test(Mg ~ Species, data = gr) #sig 
kruskal.test(Al ~ Species, data = gr) #ns
kruskal.test(Si ~ Species, data = gr) #sig
kruskal.test(P ~ Species, data = gr) #sig
kruskal.test(S ~ Species, data = gr) #sig
kruskal.test(K ~ Species, data = gr) #sig
kruskal.test(Mn ~ Species, data = gr) #sig
kruskal.test(Fe ~ Species, data = gr) #sig
kruskal.test(Co ~ Species, data = gr) #ns
kruskal.test(Ni ~ Species, data = gr) #ns
kruskal.test(Cu ~ Species, data = gr) #ns
kruskal.test(Zn ~ Species, data = gr) #ns
kruskal.test(Rb ~ Species, data = gr) #ns
kruskal.test(Sr ~ Species, data = gr) #ns
kruskal.test(Cd ~ Species, data = gr) #ns
kruskal.test(Pb ~ Species, data = gr) #ns

### Dunn test
#install.packages("FSA")
library(FSA)
dunnTest(Mg ~ Species,
         data=gr,
         method="bh") # all sig?
dunnTest(K ~ Species,
         data=gr,
         method="bh") # Ljp - Lmino
dunnTest(Mn ~ Species,
         data=gr,
         method="bh") # Lmu/Lmo from Ljp and also Lmo and L turi and S poly

#new ones from KW test to try
#B, Na, Si, P, S, Fe
#none others to include above
dunnTest(B ~ Species,
         data=gr,
         method="bh") # ns
dunnTest(Na ~ Species,
         data=gr,
         method="bh") # ns
dunnTest(Si ~ Species,
         data=gr,
         method="bh") # ns
dunnTest(P ~ Species,
         data=gr,
         method="bh") # ns
dunnTest(S ~ Species,
         data=gr,
         method="bh") # ns
dunnTest(Fe ~ Species,
         data=gr,
         method="bh") # ns
