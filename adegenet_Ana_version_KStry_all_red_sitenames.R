#===============================================================================
#                               Welcome!
# This adgenet script was provided by Levi Yant and further altered.
# From an initial VCF file (from GATK) it does a PCA, K-means clustering
# and also calculates a matrix of genetic distances and does an AMOVA.
#
# It was written for plants so it can deal with different ploidies (useful!)
#
# If a comment doesn't have name, it was made by Levi, otherwise it has the
# name of the person that has done it between hashtags
#
# Script by: Filip Kolar 2017, further edits by Sian Bray 2018 & Levi Yant 2022
# altered by: Ana C. da Silva (Matthew Gaskins helped correct for Nas)
# Date: May-July 2022
#===============================================================================

#Ana# set working directory if different than what you are using
#setwd("/Users/leviyant/Dropbox/Teaching/Semester2/project/DATA/test")

#Ana# install packages if you don't have them!
#install.packages("adegenet", dep=TRUE)
#install.packages("StAMPP")

#Ana# this setting should print warnings as they occur
options(warn=1)

#Ana# call the libraries needed:
library(adegenet)
library(adegraphics)
library(vcfR)
library(StAMPP)
library(MASS)
library(ggplot2)
library(adegraphics) #not strictly necessary for all of this (homebrew R installs will interfere)
#library(geosphere) #Ana# added to allow geographic distance in Km
library(dplyr)#Ana# added here to make your life easier
library(tidyverse)
#library(pegas) #Ana# Loaded automatically with StAMPP
#library(ape) #Ana# Loaded automatically with StAMPP
#library(ade4) #Ana# Loaded automatically with adegenet!

######################=========MODIFIED FUNCTIONS=========######################

setwd("C:\\Users\\kelli\\OneDrive\\Documents\\Duckweed\\adgenet\\2023\\all_red")

# a function for conversion from vcfR object to genlight in tetraploids
##Levi##: note not all of this is necessary for LIFE4136 project, but some is helpful
vcfR2genlight.tetra <- function (x, n.cores = 1) 
{
  bi <- is.biallelic(x)
  if (sum(!bi) > 0) {
    msg <- paste("Found", sum(!bi), "loci with more than two alleles.")
    msg <- c(msg, "\n", paste("Objects of class genlight only support loci with two alleles."))
    msg <- c(msg, "\n", paste(sum(!bi), "loci will be omitted from the genlight object."))
    warning(msg)
    x <- x[bi, ]
  }
  x <- addID(x)
  CHROM <- x@fix[, "CHROM"]
  POS <- x@fix[, "POS"]
  ID <- x@fix[, "ID"]
  x <- extract.gt(x)
  x[x == "0|0"] <- 0     #Ana# for diploids it's only lines with hashtag below
  x[x == "0|1"] <- 1     #diploid#
  x[x == "1|0"] <- 1     #diploid#
  x[x == "1|1"] <- 2     #diploid#
  x[x == "0/0"] <- 0     #diploid#
  x[x == "0/1"] <- 1     #diploid#
  x[x == "1/0"] <- 1     #diploid#
  x[x == "1/1"] <- 2     #diploid#
  x[x == "1/1/1/1"] <- 4
  x[x == "0/1/1/1"] <- 3
  x[x == "0/0/1/1"] <- 2
  x[x == "0/0/0/1"] <- 1
  x[x == "0/0/0/0"] <- 0
  x[x == "0/0/0/0/0/0"] <- 0
  x[x == "0/0/0/0/0/1"] <- 1
  x[x == "0/0/0/0/1/1"] <- 2
  x[x == "0/0/0/1/1/1"] <- 3
  x[x == "0/0/1/1/1/1"] <- 4
  x[x == "0/1/1/1/1/1"] <- 5
  x[x == "1/1/1/1/1/1"] <- 6
  if (requireNamespace("adegenet")) {
    x <- new("genlight", t(x), n.cores = n.cores)
  }
  else {
    warning("adegenet not installed")
  }
  adegenet::chromosome(x) <- CHROM
  adegenet::position(x) <- POS
  adegenet::locNames(x) <- ID
  return(x)
}
# a patch for MUCH MUCH faster PCA calculation on genlight objects
# see https://github.com/thibautjombart/adegenet/pull/150
glPcaFast <- function(x,
                      center=TRUE,
                      scale=FALSE,
                      nf=NULL,
                      loadings=TRUE,
                      alleleAsUnit=FALSE,
                      returnDotProd=FALSE){
  
  if(!inherits(x, "genlight")) stop("x is not a genlight object")
  # keep the original mean / var code, as it's used further down
  # and has some NA checks...
  if(center) {
    vecMeans <- glMean(x, alleleAsUnit=alleleAsUnit)
    if(any(is.na(vecMeans))) stop("NAs detected in the vector of means")
  }
  if(scale){
    vecVar <- glVar(x, alleleAsUnit=alleleAsUnit)
    if(any(is.na(vecVar))) stop("NAs detected in the vector of variances")
  }
  # convert to full data, try to keep the NA handling as similar to the original as possible
  # - dividing by ploidy keeps the NAs
  mx <- t(sapply(x$gen, as.integer)) / ploidy(x)
  # handle NAs
  NAidx <- which(is.na(mx), arr.ind = T)
  if (center) {
    mx[NAidx] <- vecMeans[NAidx[,2]]
  } else {
    mx[NAidx] <- 0
  }
  # center and scale
  mx <- scale(mx,
              center = if (center) vecMeans else F,
              scale = if (scale) vecVar else F)
  # all dot products at once using underlying BLAS to support thousands of samples,
  # this could be replaced by 'Truncated SVD', but it would require more changes
  # in the code around
  allProd <- tcrossprod(mx) / nInd(x) # assume uniform weights
  
  ## PERFORM THE ANALYSIS ## ---------------------------------------------------
  # eigen analysis
  eigRes <- eigen(allProd, symmetric=TRUE, only.values=FALSE)
  rank <- sum(eigRes$values > 1e-12)
  eigRes$values <- eigRes$values[1:rank]
  eigRes$vectors <- eigRes$vectors[, 1:rank, drop=FALSE]
  # scan nb of axes retained
  if(is.null(nf)){
    barplot(eigRes$values, main="Eigenvalues", col=heat.colors(rank))
    cat("Select the number of axes: ")
    nf <- as.integer(readLines(n = 1))
  }
  # rescale PCs
  res <- list()
  res$eig <- eigRes$values
  nf <- min(nf, sum(res$eig>1e-10))
  ##res$matprod <- allProd # for debugging
  ## use: li = XQU = V\Lambda^(1/2)
  eigRes$vectors <- eigRes$vectors * sqrt(nInd(x)) # D-normalize vectors
  res$scores <- sweep(eigRes$vectors[, 1:nf, drop=FALSE],2, sqrt(eigRes$values[1:nf]), FUN="*")
  
  ## GET LOADINGS ## -----------------------------------------------------------
  # need to decompose X^TDV into a sum of n matrices of dim p*r
  # but only two such matrices are represented at a time
  if(loadings){
    if(scale) {
      vecSd <- sqrt(vecVar)
    }
    res$loadings <- matrix(0, nrow=nLoc(x), ncol=nf) # create empty matrix
    ## use: c1 = X^TDV
    ## and X^TV = A_1 + ... + A_n
    ## with A_k = X_[k-]^T v[k-]
    myPloidy <- ploidy(x)
    for(k in 1:nInd(x)){
      temp <- as.integer(x@gen[[k]]) / myPloidy[k]
      if(center) {
        temp[is.na(temp)] <- vecMeans[is.na(temp)]
        temp <- temp - vecMeans
      } else {
        temp[is.na(temp)] <- 0
      }
      if(scale){
        temp <- temp/vecSd
      }
      res$loadings <- res$loadings + matrix(temp) %*% eigRes$vectors[k, 1:nf, drop=FALSE]
    }
    res$loadings <- res$loadings / nInd(x) # don't forget the /n of X_tDV
    res$loadings <- sweep(res$loadings, 2, sqrt(eigRes$values[1:nf]), FUN="/")
  }
  ## FORMAT OUTPUT ## ----------------------------------------------------------
  colnames(res$scores) <- paste("PC", 1:nf, sep="")
  if(!is.null(indNames(x))){
    rownames(res$scores) <- indNames(x)
  } else {
    rownames(res$scores) <- 1:nInd(x)
  }
  if(!is.null(res$loadings)){
    colnames(res$loadings) <- paste("Axis", 1:nf, sep="")
    if(!is.null(locNames(x)) & !is.null(alleles(x))){
      rownames(res$loadings) <- paste(locNames(x),alleles(x), sep=".")
    } else {
      rownames(res$loadings) <- 1:nLoc(x)
    }
  }
  if(returnDotProd){
    res$dotProd <- allProd
    rownames(res$dotProd) <- colnames(res$dotProd) <- indNames(x)
  }
  res$call <- match.call()
  class(res) <- "glPca"
  return(res)
}
######################====================================######################

# IMPORT SNP data from VCF

#prune not harsh
#20603 variants
#vcf <- read.vcfR("4fold_allred_ld_pruned_maf0.025_50_50_0.1_use.vcf")

#11088 variants
vcf <- read.vcfR("4fold_allred_ld_pruned_maf0.025_100_50_0.1_use.vcf")

#prune harsh
#vcf <- read.vcfR("4fold_allred_ld_pruned_mis0.8_maf0.05_500_50_0.1_use.vcf")

# convert to genlight 	
##Levi## this uses the modified function vcfR2genlight.tetra (see Modified functions section)
aa.genlight <- vcfR2genlight.tetra(vcf)
#Ana# here the bit "tetra" just means that this could be used with tetraploids

locNames(aa.genlight) <- paste(vcf@fix[,1],vcf@fix[,2],sep="_")  # add real SNP.names
## ----
# Ana explains:
# We can use the feature below to group the populations by their characteristics!
# However to do that, I had to rename the populations (on the vcf) so that the
# first two characters now mean inland (IN) or coastal (CO), and then there are 
# three characters that identify location, and one character that identifies sample nr
# So if you choose 2, you get Inland vs Coastal, if 5 grouping by location,
#and 6 for all individuals (no grouping) - which is very useful!
## ----
pop(aa.genlight) <-substr(indNames(aa.genlight),1,5) # add pop names: here pop names are first chars of ind name

#check    =====VERY IMPORTANT===
aa.genlight$pop
indNames(aa.genlight)
ploidy(aa.genlight)

#Ana# use this to save file and change names using vcftools so that order keeps the same!
#write.table(indNames(aa.genlight), file="population_names.txt", sep="\t", quote=F, col.names=F)

######################====================================######################
#   PCA     --------------------------------------------------------------------
#----
#Matt# added this to get rid of the missing values:
toRemove <- is.na(glMean(aa.genlight, alleleAsUnit = FALSE)) # TRUE where NA
which(toRemove) # position of entirely non-typed loci
aa.genlight_correct <- aa.genlight[, !toRemove]
#Ana# NOTE HERE - > which(toRemove) # position of entirely non-typed loci
# gave "named integer(0)" for Emma's script
#----

aa.genlight$ind.names #143

#KS added from MZH script
length(aa.genlight$ind.names) #143

#TO EDIT LABELS MANUALLY
labels  = list(
  "AL01"	=	c	(	"MID-Lmu-ALP",	"MID"	,	"Lminuta"	)	,
  "AL02"	=	c	(	"MID-Lmu-ALD",	"MID"	,	"Lminuta"	)	,
  "AL03"	=	c	(	"MID-Lmu-SHR",	"MID"	,	"Lminuta"	)	,
  "ERR3957957"	=	c	(	"Sin8410"	,	"WW"	,	"Sin"	)	,
  "KS02"	=	c	(	"YOR-Lmo-BIS"	,	"YOR"	,	"Lminor"	)	,
  "KS03"	=	c	(	"BFD-Lmo-ALL1",	"BFD"	,	"Ljp"	)	,
  "KS06A"	=	c	(	"BFD-Lmu-NUF1",	"BFD"	,	"Lminuta"	)	,
  "KS06B"	=	c	(	"BFD-Lmu-NUF2",	"BFD"	,	"Lminuta"	)	,
  "KS09"	=	c	(	"BFD-Lmo-YEA"	,	"BFD"	,	"Lminor"	)	,
  "KS100"	=	c	(	"ELG-Lmo-BUR"	,	"ELG"	,	"Lminor"	)	,
  "KS101"	=	c	(	"ELG-Lmo-MAV"	,	"ELG"	,	"Lminor"	)	,
  "KS104"	=	c	(	"ELG-Lmo-LNB"	,	"ELG"	,	"Lminor"	)	,
  "KS107"	=	c	(	"ELG-Lmo-COO"	,	"ELG"	,	"Lminor"	)	,
  "KS108"	=	c	(	"ABE-Lmo-TOL"	,	"ABE"	,	"Lminor"	)	,
  "KS109"	=	c	(	"ABE-Lmo-DEE"	,	"ABE"	,	"Lminor"	)	,
  "KS110"	=	c	(	"ABE-Lmo-ALL"	,	"ABE"	,	"Lminor"	)	,
  "KS111"	=	c	(	"ABE-Lmo-HAZ"	,	"ABE"	,	"Lminor"	)	,
  "KS112"	=	c	(	"ABE-Lmo-COU"	,	"ABE"	,	"Lminor"	)	,
  "KS114"	=	c	(	"ABE-Lmo-DUT"	,	"ABE"	,	"Lminor"	)	,
  "KS115"	=	c	(	"ABE-Lmo-DON"	,	"ABE"	,	"Lminor"	)	,
  "KS116"	=	c	(	"ABE-Lmo-BLK"	,	"ABE"	,	"Lminor"	)	,
  "KS117"	=	c	(	"ABE-Lmo-DEN"	,	"ABE"	,	"Lminor"	)	,
  "KS118"	=	c	(	"ABE-Lmo-KWS",	"ABE"	,	"Lminor"	)	,
  "KS119"	=	c	(	"ABE-Lmo-POT",	"ABE"	,	"Lminor"	)	,
  "KS12"	=	c	(	"BFD-Spo-MOO1"	,	"BFD"	,	"Spo"	)	,
  "KS13"	=	c	(	"BFD-Lmo-ELD"	,	"BFD"	,	"Lminor"	)	,
  "KS14"	=	c	(	"BFD-Lmo-APP"	,	"BFD"	,	"Ljp"	)	,
  "KS15"	=	c	(	"BFD-Lmo-BRE"	,	"BFD"	,	"Ljp"	)	,
  "KS16"	=	c	(	"HUL-Ltu-EAS"	,	"HUL"	,	"Ltu"	)	,
  "KS17"	=	c	(	"HUL-Lmo-PEA"	,	"HUL"	,	"Ljp"	)	,
  "KS18"	=	c	(	"HUL-Lmo-CRE"	,	"HUL"	,	"Ljp"	)	,
  "KS20"	=	c	(	"HUL-Lmu-BEV"	,	"HUL"	,	"Lminuta"	)	,
  "KS21"	=	c	(	"HUL-Lmo-WAL",	"HUL"	,	"Ljp"	)	,
  "KS22"	=	c	(	"HUL-Ltu-NOR",	"HUL"	,	"Ltu"	)	,
  "KS25"	=	c	(	"YOR-Lmu-ESC",	"YOR"	,	"Lminuta"	)	,
  "KS27"	=	c	(	"YOR-Lmo-BUR",	"YOR"	,	"Lminor"	)	,
  "KS28"	=	c	(	"YOR-Lmo-HES"	,	"YOR"	,	"Ljp"	)	,
  "KS29"	=	c	(	"YOR-Lmo-TAD",	"YOR"	,	"Lminor"	)	,
  "KS33"	=	c	(	"LAN-Lmo-SIL"	,	"LAN"	,	"Lminor"	)	,
  "KS34"	=	c	(	"LAN-Lmo-CAR"	,	"LAN"	,	"Lminor"	)	,
  "KS38"	=	c	(	"MID-Lmu-ELV"	,	"MID"	,	"Lminuta"	)	,
  "KS39"	=	c	(	"MID-Lmo-LIM"	,	"MID"	,	"Lminor"	)	,
  "KS40A"	=	c	(	"MID-Lmo-CAL"	,	"MID"	,	"Ljp"	)	,
  "KS42"	=	c	(	"HAS-Lmu-HEL"	,	"HAS"	,	"Lminuta"	)	,
  "KS43"	=	c	(	"HAS-Lmo-GIL"	,	"HAS"	,	"Lminor"	)	,
  "KS44A"	=	c	(	"HAS-Lmo-LAN"	,	"HAS"	,	"Lminor"	)	,
  "KS44B"	=	c	(	"HAS-Lmu-LAN"	,	"HAS"	,	"Lminuta"	)	,
  "KS45"	=	c	(	"HAS-Lmu-WIL"	,	"HAS"	,	"Lminuta"	)	,
  "KS46B"	=	c	(	"HAS-Lmu-WHE"	,	"HAS"	,	"Lminuta"	)	,
  "KS47"	=	c	(	"HAS-Lmo-BRE"	,	"HAS"	,	"Lminor"	)	,
  "KS48A"	=	c	(	"HAS-Lmo-UDI",	"HAS"	,	"Lminor"	)	,
  "KS48B"	=	c	(	"HAS-Lmu-UDI",	"HAS"	,	"Lminuta"	)	,
  "KS49"	=	c	(	"HAS-Lmu-MIL",	"HAS"	,	"Lminuta"	)	,
  "KS50"	=	c	(	"HAS-Lmo-CRO"	,	"HAS"	,	"Lminor"	)	,
  "KS51A"	=	c	(	"COR-Lmo-COR"	,	"COR"	,	"Lminor"	)	,
  "KS51B"	=	c	(	"COR-Lmu-COR",	"COR"	,	"Lminuta"	)	,
  "KS52"	=	c	(	"COR-Lmo-KWO",	"COR"	,	"Lminor"	)	,
  "KS53"	=	c	(	"COR-Lmo-GRA",	"COR"	,	"Lminor"	)	,
  "KS54"	=	c	(	"COR-Lmo-MEN",	"COR"	,	"Lminor"	)	,
  "KS55"	=	c	(	"COR-Lmo-COM",	"COR"	,	"Lminor"	)	,
  "KS56"	=	c	(	"COR-Lmo-TRG",	"COR"	,	"Lminor"	)	,
  "KS57"	=	c	(	"COR-Lmo-TRW",	"COR"	,	"Lminor"	)	,
  "KS58B"	=	c	(	"COR-Lmu-PIN"	,	"COR"	,	"Lminuta"	)	,
  "KS59"	=	c	(	"COR-Lmo-INN",	"COR"	,	"Lminor"	)	,
  "KS60"	=	c	(	"COR-Lmo-AND",	"COR"	,	"Lminor"	)	,
  "KS61A"	=	c	(	"COR-Lmo-HEL",	"COR"	,	"Lminor"	)	,
  "KS61B"	=	c	(	"COR-Lmu-HEL",	"COR"	,	"Lminuta"	)	,
  "KS62"	=	c	(	"COR-Lmo-JAP",	"COR"	,	"Ljp"	)	,
  "KS63"	=	c	(	"BRI-Lmo-PUX"	,	"BRI"	,	"Lminor"	)	, #bad seq?
  "KS64B"	=	c	(	"BRI-Lmu-WEM"	,	"BRI"	,	"Lminuta"	)	,
  "KS65A"	=	c	(	"BRI-Lmo-CLA1"	,	"BRI"	,	"Lminor"	)	,
  "KS65B"	=	c	(	"BRI-Lmo-CLA2",	"BRI"	,	"Lminor"	)	,
  "KS66A"	=	c	(	"BRI-Lmo-LAM",	"BRI"	,	"Lminor"	)	,
  "KS66B"	=	c	(	"BRI-Lgi-LAM"	,	"BRI"	,	"Lgibba"	)	,
  "KS66C"	=	c	(	"BRI-Lmu-LAM"	,	"BRI"	,	"Lminuta"	)	,
  "KS67A"	=	c	(	"BRI-Lmo-NAI",	"BRI"	,	"Lminor"	)	,
  "KS67B"	=	c	(	"BRI-Lmu-NAI",	"BRI"	,	"Lminuta"	)	,
  "KS68A"	=	c	(	"BRI-Lmo-VAL",	"BRI"	,	"Lminor"	)	,
  "KS68B"	=	c	(	"BRI-Lmu-VAL",	"BRI"	,	"Lminuta"	)	,
  "KS69"	=	c	(	"BRI-Lmu-NEW"	,	"BRI"	,	"Lminuta"	)	,
  "KS70"	=	c	(	"BRI-Lmo-MLA"	,	"BRI"	,	"Lminor"	)	,
  "KS72A"	=	c	(	"NEW-Lmo-TRE"	,	"NEW"	,	"Lminor"	)	,
  "KS72B"	=	c	(	"NEW-Lmu-TRE"	,	"NEW"	,	"Lminuta"	)	,
  "KS73"	=	c	(	"NEW-Lmo-LLI",	"NEW"	,	"Lminor"	)	,
  "KS74A"	=	c	(	"NEW-Lmo-NAS",	"NEW"	,	"Lminor"	)	,
  "KS74B"	=	c	(	"NEW-Lmu-NAS",	"NEW"	,	"Lminuta"	)	,
  "KS75A"	=	c	(	"NEW-Lmo-CHA"	,	"NEW"	,	"Lminor"	)	,
  "KS75B"	=	c	(	"NEW-Lmu-CHA",	"NEW"	,	"Lminor"	)	, #bad seq
  "KS76A"	=	c	(	"NEW-Lmo-MAL",	"NEW"	,	"Lminor"	)	,
  "KS76B"	=	c	(	"NEW-Lmu-MAL",	"NEW"	,	"Lminuta"	)	,
  "KS77A"	=	c	(	"NEW-Spo-FOU",	"NEW"	,	"Spolyrhiza"	)	,
  "KS77B"	=	c	(	"NEW-Lmo-FOU"	,	"NEW"	,	"Lminor"	)	,
  "KS77C"	=	c	(	"NEW-Lmu-FOU",	"NEW"	,	"Lminuta"	)	,
  "KS78A"	=	c	(	"NEW-Spo-FIV"	,	"NEW"	,	"Spolyrhiza"	)	,
  "KS80A"	=	c	(	"NEW-Lmo-PER"	,	"NEW"	,	"Lminor"	)	,
  "KS80B"	=	c	(	"NEW-Lmu-PER",	"NEW"	,	"Lminuta"	)	,
  "KS81"	=	c	(	"NEW-Lmo-HAW",	"NEW"	,	"Lminor"	)	,
  "KS82A"	=	c	(	"NEW-Lmo-LLA1",	"NEW"	,	"Lminor"	)	,
  "KS82B"	=	c	(	"NEW-Lmo-LLA2"	,	"NEW"	,	"Lminor"	)	,
  "KS83"	=	c	(	"GLA-Lmo-PER",	"GLA"	,	"Lminor"	)	,
  "KS85"	=	c	(	"GLA-Lmu-KEL",	"GLA"	,	"Lminuta"	)	,
  "KS88"	=	c	(	"GLA-Lmo-MAR",	"GLA"	,	"Lminor"	)	,
  "KS91"	=	c	(	"GLA-Lmo-ROB",	"GLA"	,	"Lminor"	)	,
  "KS92B"	=	c	(	"GLA-Lmu-BOG",	"GLA"	,	"Lminuta"	)	,
  "KS95"	=	c	(	"ELG-Lmo-SBA",	"ELG"	,	"Lminor"	)	,
  "KS97A"	=	c	(	"ELG-Ltr-ALP",	"ELG"	,	"Ltr"	)	,
  "KS97B"	=	c	(	"ELG-Lmo-ALP",	"ELG"	,	"Lminor"	)	,
  "KS98"	=	c	(	"ELG-Lmo-FOR",	"ELG"	,	"Lminor"	)	,
  "KS99"	=	c	(	"ELG-Lmo-SAN",	"ELG"	,	"Lminor"	)	,
  "KSALLOT2"	=	c	(	"BFD-Lmo-ALL2",	"BFD"	,	"Ljp"	)	,
  "KSAPP1"	=	c	(	"BFD-Ltr-APP",	"BFD"	,	"Ltr"	)	,
  "KSAPP2"	=	c	(	"BFD-Spo-APP"	,	"BFD"	,	"Spo"	)	,
  "KSKEY"	=	c	(	"HUL-Lmo-KEY",	"HUL"	,	"Ljp"	)	,
  "KSMOOR1"	=	c	(	"BFD-Spo-MOO2",	"BFD"	,	"Spo"	)	,
  "KSMOOR2"	=	c	(	"BFD-Lmo-MOO",	"BFD"	,	"Ljp"	)	,
  "KSNOR1"	=	c	(	"HUL-Ltr-NOR",	"HUL"	,	"Ltr"	)	,
  "KSNUFF2"	=	c	(	"BFD-Lmo-NUF3",	"BFD"	,	"Lminor"	)	,
  "KSSEL1"	=	c	(	"YOR-Lgi-SEL",	"YOR"	,	"Lgi"	)	,
  "KSYEAD1"	=	c	(	"BFD-Ltr-YEA",	"BFD"	,	"Ltr"	)	,
  "L.jap9250"	=	c	(	"Ljp9250"	,	"WW"	,	"Ljp"	)	,
  "L.min"	=	c	(	"Lmu9260"	,	"WW"	,	"Lminuta"	)	,
  "L.minor7123"	=	c	(	"Lmo7123"	,	"WW"	,	"Lminor"	)	,
  "L.minor7295"	=	c	(	"Lmo7295"	,	"WW"	,	"Lminor"	)	,
  "L.minor8389"	=	c	(	"Lmo8389"	,	"WW"	,	"Lminor"	)	,
  "L.minuta6600"	=	c	(	"Lmu6600"	,	"WW"	,	"Lminuta"	)	,
  "L.punc"	=	c	(	"Lpu0049"	,	"WW"	,	"Lpu"	)	,
  "L.tris"	=	c	(	"Ltr7192"	,	"WW"	,	"Ltr"	)	,
  "L.yung"	=	c	(	"Lyu9208"	,	"WW"	,	"Lyu"	)	,
  "LY01A"	=	c	(	"COR-Lmo-TRE"	,	"COR"	,	"Ljp"	)	,
  "LY01B"	=	c	(	"COR-Lmu-TRE"	,	"COR"	,	"Lminuta"	)	,
  "LY02"	=	c	(	"COR-Lmo-SHE",	"COR"	,	"Ljp"	)	,
  "LY03"	=	c	(	"COR-Lmo-HOU",	"COR"	,	"Ljp"	)	,
  "MP01"	=	c	(	"NCA-Lmo-MPO",	"NCA"	,	"Ljp"	)	,
  "S.int"	=	c	(	"Sin9394"	,	"WW"	,	"Sin"	)	,
  "SRR074103"	=	c	(	"Lgi131"	,	"WW"	,	"Lgi"	)	,
  "SRR10958777"	=	c	(	"Lmo7016"	,	"WW"	,	"Lminor"	)	,
  "SRR10958800"	=	c	(	"Lmo5500"	,	"WW"	,	"Lminor"	)	,
  "SRR11472010"	=	c	(	"Spo9504"	,	"WW"	,	"Spo"	)	,
  "SRR8291593"	=	c	(	"Lmu9581"	,	"WW"	,	"Lminuta"	)	,
  "SRR8291594"	=	c	(	"Lmu9484"	,	"WW"	,	"Lminuta"	)	,
  "SRR8291595"	=	c	(	"Lmu7612"	,	"WW"	,	"Lminuta"	)	,
  "SRR8291596"	=	c	(	"Lmu6717"	,	"WW"	,	"Lminuta"	)	,
  "onea3"	=	c	(	"YOR-Lmo-SEL",	"YOR"	,	"Lminor"))

# extract relevant sample names / populations
samples <- character()
populations <- character()
species <- character()
for (vcf_ind in indNames(aa.genlight)){
  stored = labels[[vcf_ind]]
  samples <- c(samples, stored[1])
  populations <- c(populations, stored[2])
  species <- c(species, stored[3])
}

# update genlight details
indNames(aa.genlight) <- samples
#pop(aa.genlight) <- populations
pop(aa.genlight) <- species

#run PCA
# this uses the modified function glPcaFast to run faster!
#pca.1 <- glPcaFast(aa.genlight_correct, nf=300)
pca.1 <- glPcaFast(aa.genlight, nf=300) # used this as correction not needed here!

#Plotting PCA
scatter(pca.1,posi="bottomleft")  # plot scatter with the individual labels
title("PCA of duckweed species groupings")
loadingplot(pca.1)

# proportion of explained variance by each axes
pca.1$eig[1]/sum(pca.1$eig) # 31% raw, 50% harsh prune proportion of variation explained by 1st axis
pca.1$eig[2]/sum(pca.1$eig) # 14% raw, 14% harsh prune proportion of variation explained by 2nd axis
pca.1$eig[3]/sum(pca.1$eig) # proportion of variation explained by 3rd axis
pca.1$eig[4]/sum(pca.1$eig) # proportion of variation explained by 4th axis
pca.1$eig[5]/sum(pca.1$eig) # proportion of variation explained by 5th axis
pca.1$eig[6]/sum(pca.1$eig) # proportion of variation explained by 6th axis
pca.1$eig[7]/sum(pca.1$eig) # proportion of variation explained by 7th axis
#1 and 2 = 43% and 25%

#Ana# doing something extra here to get different graphs - ignore for now!
#----
# pca.1$scores
# PCA_matrix <- pca.1$scores
# PCAmatrix <- as_tibble(PCA_matrix)
# PCAmatrix
# indNames(aa.genlight) # use this to confirm if order is the same in the first excel (raw data)
# 
# add_column(PCAmatrix, before="PC1", newColname="Population")
# PCAmatrix
# ggplot(PCAmatrix, aes(PC1, PC2), colour=population)+
#   geom_point(shape=2)
# 
# PCAmatrix["Population"] <- indNames(aa.genlight)
# PCAmatrix %>% relocate(Population, .before = PC1)
# 
# ggplot(vcf_final, aes(POS, AFD)) +
#   geom_point(shape=16) +
#   labs(y= "Absolute AFD", x = "Position in genome")
#----

## ----
# Ana explains:
# There is a way to check which axis you need to consider using random samples
# (statistically) but it is not appropriate here, because we only have 3 variables
# (genotype: 00 / 01-10 / 11) when doing PCA genomics you only focus on PCA1 and PCA2
## ----

# just to see pops coloured in a palette
col <- funky(10) #Ana# adjust the number here to the total of locations you have
col <- c("gold", "lightblue", "red", "green",
         "grey", "brown", "blue", "pink",
         "orange", "purple")
#col <- c("lightblue", "chocolate4") # use this for COASTAL vs INLAND

#Ana# you can adjust "xax" and "yax" for the PCAs you want to compare (here use just 1 and 2)
s.class(pca.1$scores, pop(aa.genlight), xax=1, yax=2, col=transp(col,1))


#Ana# Define your graphs as objects:
g1 <- s.class(pca.1$scores, pop(aa.genlight), xax=1, yax=2, col=transp(col,1),
              plabels.box.draw =F, plabels.cex = 0, paxes.draw=T, plegend.drawKey=T,
              ylab="PC2 = 14%", xlab="PC1 = 31%", ppoints.cex=0.5, ppoints.col="black")
g1 #Ana# I like to see the graphs asap - but comment out before doing the pdf
g2 <- s.label (pca.1$scores, xax=1, yax=2, ppoints.col="black", ppoints.pch=1,
               plabels = list(box = list(draw = FALSE), optim =F),
               ylab="PCA1", xlab="PCA2", paxes.draw=T, pgrid.draw=F, plabels.cex=0.7, plot = FALSE)
g2 #Ana# I like to see the graphs asap - but comment out before doing the pdf
# save nice figs
pdf ("PCA_all_red_weakprune_named.pdf", width=14, height=7)
ADEgS(c(g1, g2), layout = c(1, 2))
dev.off()
#dev.new()

#KS look at SNPs in dataset
aa.genlight$loc.names
aa.genlight$position
aa.genlight$n.loc #20603

#KS try ape
library(ape)
tre <- nj(dist(as.matrix(aa.genlight)))
#missing data not allowed

#do mis or exclude by name
#do one by one
aa.genlight <- aa.genlight[indNames(aa.genlight) != ("Sin8410")]
aa.genlight <- aa.genlight[indNames(aa.genlight) != ("Spo9504")]
aa.genlight <- aa.genlight[indNames(aa.genlight) != ("Lmu9581")]
aa.genlight <- aa.genlight[indNames(aa.genlight) != ("Lmu9484")]
aa.genlight <- aa.genlight[indNames(aa.genlight) != ("Lmu7612")]
aa.genlight <- aa.genlight[indNames(aa.genlight) != ("Lmu6717")]
#aa.genlight <- aa.genlight[indNames(aa.genlight) != ("KS69")]
#aa.genlight <- aa.genlight[indNames(aa.genlight) != ("KS75B")]

aa.genlight$ind.names #137 left

tre <- njs(dist(as.matrix(aa.genlight)))

#===============================================================================
#Ana# next part doesn't matter as ploidy is fixed here, so I commented everything!
#===============================================================================
# #ploidy - differentiated plots
# pdf ("PCA_all_ploidycol_SNPs_ax12_1K_less.pdf", width=14, height=7)
# g3 <- s.class(pca.1$scores, as.factor(as.vector(ploidy(aa.genlight))), xax=1, yax=2, col=transp(c("#FF0000", "#0000FF", "#00FF00")), 
#               ellipseSize=0, starSize=0, ppoints.cex=4, paxes.draw=T, pgrid.draw =F, plab.cex = 0 , plot = FALSE)
# g4 <- s.label (pca.1$scores, xax=1, yax=2, ppoints.col = "red", plabels = list(box = list(draw = FALSE), 
#                                                                                optim = TRUE), paxes.draw=T, pgrid.draw =F, plabels.cex=1, plot = FALSE)
# ADEgS(c(g3, g4), layout = c(1, 2))
# dev.off()
#===============================================================================

#Ana# I've used the tutorial "Analysing genome-wide SNP data using adegenet 2.1.5"
# to do more detailed graphs, as described in pages 42/43/44/45
#===============================================================================
#first we start by doing a NJ tree
#NJtree <- nj(dist(as.matrix(aa.genlight_correct)))
NJtree <- njs(dist(as.matrix(aa.genlight)))

#KS: FOR BOTH PRUNED AND UNPRUNED GETTING ERROR FOR NJTREE
#works when manually remove poor data mis 0.8 NAs

#Ana: I've pasted here the output
## Phylogenetic tree with 29 tips and 27 internal nodes.
## Tip labels:
##   COLAN1, COSFG5, INBRU1, INBRU2, INBRU3, INBRU4, ...
## 
## Unrooted; includes branch lengths.

#Now to plot the tree we use:
plot(NJtree, typ="unrooted", cex=0.7) # As our tree is unrooted I've changed type from "fan" to "unrooted"
title(expression("Neighbour-joining tree of UK duckweeds"))
#save the tree using:
write.tree((NJtree),file="NJ.tree_populations_4ds_all_red_10000var.tre")
# If you want to root the tree you can use dataset with B.nigra and use type=fan

# The correspondence between both analyses (pca and NJ) can be better assessed
# using colors based on PCs. We use this by using the colorplot function:
myCol <- colorplot(pca.1$scores, pca.1$scores, transp=TRUE, cex=3)
abline(h=0,v=0, col="grey")
add.scatter.eig(pca.1$eig[1:28],2,1,2, posi="topright")

# Now the final pretty figure
plot(NJtree, typ="unrooted", show.tip=TRUE, cex=0.5)
tiplabels(pch=20, col=myCol, cex=3)
title(expression("Neighbour-joining tree of duckweeds"))

tiff('NJtree_all_red_10000var_sitenames.tiff', units="in", width=14, height=12, res=300, compression = 'lzw')
# Now the final pretty figure
plot(NJtree, typ="unrooted", show.tip=TRUE, cex=0.5)
tiplabels(pch=20, col=myCol, cex=3)
title(expression("Neighbour-joining tree of Duckweeds"))
dev.off()
#Ana# Don't forget to save the images you need!

#===============================================================================
#  K-means clustering     ------------------------------------------------------

## find clusters = K-means clustering        !!!MANUAL INPUT ALLERT!!!
grp <- find.clusters(aa.genlight, max.n.clust=20, glPca = pca.1, perc.pca = 100, n.iter=1e6, n.start=1000) #Manual input here
##Ana# - here we are retaining 2 PCs (due to having 3 genotypes) and 3 clusters
## show this with 3 and 6 and check what you get --
# save the grouping
write.table(grp$grp, file="grouping_allred_4ds_5clusters.txt", sep="\t", quote=F, col.names=F)

#===============================================================================
#  distance-based analyses     -------------------------------------------------

# Calculate Nei's distances between individuals/pops
#Ana# Note here raw data is used, not the corrected for NAs
# ---

aa.D.ind <- stamppNeisD(aa.genlight, pop = FALSE) # Nei's 1972 distance between indivs
# export matrix - for SplitsTree
stamppPhylip(aa.D.ind, file="aa.indiv_all_red10000var_Neis_distance_sitenames_4ds.phy.dst")

aa.D.pop <- stamppNeisD(aa.genlight, pop = TRUE)   # Nei's 1972 distance between pops
# export matrix - for SplitsTree
stamppPhylip(aa.D.pop, file="aa.pops_all_red10000var_Neis_distance_sitenames_4ds.phy.dst") 

#Ana# the stamppNeisD  is having problem identifying populations here.
# if you use in line 240 pop(aa.genlight) <-substr(indNames(aa.genlight),1,6)
# matrices aa.D.ind and aa.D.pop are the same! what you are calculating is INDIVIDUALS
# since "6" corresponds to full name including the sample nr (1-5)

### create the dist objects                 #Ana# this alters the matrices above
colnames(aa.D.ind) <- rownames(aa.D.ind)
aa.D.ind.dist <-as.dist(aa.D.ind, diag=T)
attr(aa.D.ind.dist, "Labels") <-rownames(aa.D.ind)   # name the rows of a matrix  

colnames(aa.D.pop) <- rownames(aa.D.pop) 
aa.D.pop.dist <-as.dist(aa.D.pop, diag=T)
attr(aa.D.pop.dist, "Labels") <-rownames(aa.D.pop)   # name the rows of a matrix  

#Ana# Table with distances between individuals and populations is now ready!
aa.D.ind # individuals
aa.D.pop # populations
Dgen_ind <- aa.D.ind.dist
Dgen_pop <- aa.D.pop.dist
#check for NAs. cant see any

#KS CUT NAS AND RENAME SAMPLES SO FIT FOR DST SPLITSTREE

# plot and save NJ tree
plot(nj(aa.D.ind), typ="unrooted", cex=0.7)
title(expression("Neighbour-joining tree of distance-based analysis of UK duckweeds "))
write.tree(nj(aa.D.ind),file="NJ.All_red_10000var_distance_tree_outgroups.tre")
# ----
### Isolation By Distance (IBD)
#-----------------------
# pop_coords_with_names.txt (tab seperated values - tsv) file is a 
# three-column file with sample IDs, and coordinates belonging to each
# individual (lat - latitude; long - longitude)
#Ana# you have to manually create this file as it was not provided!

#Ana# - here you have to match the order of these coordinates with the sample
# same order that you have in the aa.D.ind.dist files, otherwise it doesn't work!
coords <- read.csv ("pop_coords_with_names.txt", sep ="\t")
coords
# subset to get rid of sample ID
xy.coords.only <- subset(coords, select=c("Lat","Long"))
xy.coords.only
# now transform table into distance matrix
Dgeo <- dist(xy.coords.only)
Dgeo

# or be fancy and transform the coordinates into km!
b <- distm(xy.coords.only[2:1], xy.coords.only[2:1], fun = distVincentyEllipsoid)
b1 <- b/1000 # to convert from m to km
# and now you have a distance matrix in km
Dgeo_km <- as.dist(b1)
Dgeo_km

#test IBD
IBD <- mantel.randtest(Dgen_ind, Dgeo)
IBD
plot(IBD)

#Ana# do a simple scatter plot
plot(Dgeo, Dgen_ind, pch=20, cex=0.5) #Ana# substituted "pop" by "ind"
abline(lm(Dgen_ind~Dgeo)) #Ana# substituted "pop" by "ind"

#Ana# this next bit doesn't work as Google now requires an API key
#========---------------------------------------------------------------========
##optionally, check plotting the points on a map
#install.packages("ggmap")
#library(ggmap)
#map <- get_map(location = c(lon = 14, lat = 50), zoom = 5)
#mapPoints <- ggmap(map) + geom_point(data = coords, aes(x = lon, y = lat, colour="blue")) + geom_text(data = coords, aes(x = lon, y = lat,label = pop, colour = "red"), size = 4, vjust = 0, hjust = -0.5)
#mapPoints
#========---------------------------------------------------------------========

#plot and check for denser areas in the plot indicating sub-groups

#Ana# for individuals
#----
dens <- kde2d(Dgeo, Dgen_ind, n=500)
#Ana# I removed lims=c(-1, 47, 0, 0.18) from above and substituted "pop" by "ind"
myPal <- colorRampPalette(c("white","blue","gold","orange", "red"))
plot(Dgeo, Dgen_ind, pch=21, cex=0.4, ylab="Genetic distance between individuals (Nei's 1972)", xlab="Geographic Distance")
image(dens, col=transp(myPal(500),0.7), add=TRUE)
#abline(lm(Dgen_ind~Dgeo))
title("Correlation of Genetic and Geographic distances by individuals")
#----
#Ana# for individuals but using geographic distance in Km
#----
dens <- kde2d(Dgeo_km, Dgen_ind, n=500)
#Ana# I removed lims=c(-1, 47, 0, 0.18) from above and substituted "pop" by "ind"
myPal <- colorRampPalette(c("white","blue","gold","orange", "red"))
plot(Dgeo_km, Dgen_ind, pch=21, cex=0.4, ylab="Genetic distance between individuals (Nei's 1972)", xlab="Geographic Distance (km)")
image(dens, col=transp(myPal(500),0.7), add=TRUE)
#abline(lm(Dgen_ind~Dgeo_km))
title("Correlation of Genetic and Geographic distances by individuals")
#----

#Ana# for populations you have to make sure that your coordinates file only has populations!
# otherwise you get: "Error in mantel.randtest(Dgen, Dgeo) : Non convenient dimension"

# Remove duplicated rows in order to have location for only 1 individual of the population
xy.population <- coords %>% distinct(Lat, .keep_all=TRUE) %>% subset(select=c("Lat","Long"))
xy.population
# now transform table into distance matrix
Dgeo_pop <- dist(xy.population)
Dgeo_pop

# or be fancy and transform the coordinates into km!
c <- distm(xy.population[2:1], xy.population[2:1], fun = distVincentyEllipsoid)
c1 <- c/1000 # to convert from meters to km
# and now you have a distance matrix in km
Dgeo_pop_km <- as.dist(c1)
Dgeo_pop_km

#test IBD
IBD1 <- mantel.randtest(Dgen_pop, Dgeo_pop)
IBD1
plot(IBD1)

#Ana# do a simple scatter plot
plot(Dgeo_pop, Dgen_pop, pch=20, cex=0.5)
abline(lm(Dgen_pop~Dgeo_pop))

#Ana# and now the fancy plot:
#----
#plot and check for denser areas in the plot indicating sub-groups
dens <- kde2d(Dgeo_pop, Dgen_pop, n=500) #Ana# I removed lims=c(-1, 47, 0, 0.18) from here
myPal <- colorRampPalette(c("white","blue","gold","orange", "red"))
plot(Dgeo_pop, Dgen_pop, pch=20, cex=0.4, ylab="Genetic distance between populations (Nei's 1972)", xlab="Geographic Distance")#, ylim=c(0, 0.3))
image(dens, col=transp(myPal(500),0.7), add=TRUE) 
#abline(lm(Dgen_pop~Dgeo_pop)) #Ana# not enough points to do this properly!
title("Correlation of Genetic and Geographic distances by populations")
#----
#Ana# add even fancier version with geographic distance in km!
#----
#plot and check for denser areas in the plot indicating sub-groups
dens <- kde2d(Dgeo_pop_km, Dgen_pop, n=500) #Ana# I removed lims=c(-1, 47, 0, 0.18) from here
myPal <- colorRampPalette(c("white","blue","gold","orange", "red"))
plot(Dgeo_pop_km, Dgen_pop, pch=20, cex=0.4, ylab="Genetic distance between populations (Nei's 1972)", xlab="Geographic Distance (Km)", ylim=c(0, 0.14))
image(dens, col=transp(myPal(500),0.7), add=TRUE) 
#abline(lm(Dgen_pop~Dgeo_pop_km)) #Ana# not enough points to do this properly!
title("Correlation of Genetic and Geographic distances by populations")
#----

### AMOVA
#---------

# Ana# This part didn't come with any explanation so we're not going to use it
pops<-as.factor(substr(rownames(aa.D.ind),1,3))
(res <- pegas::amova(aa.D.ind.dist ~ pops))  # default nperm=1000
