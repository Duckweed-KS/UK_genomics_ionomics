#!/bin/bash
#SBATCH --job-name=copy_uk9
#SBATCH --partition=defq
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=64g
#SBATCH --time=24:00:00
#SBATCH --output=/gpfs01/home/stxks22/copy_uk10.out
#SBATCH --error=/gpfs01/home/stxks22/copy_uk10.err

echo STARTED `date`

vcftools --remove nonL.minuta.txt --vcf lemna_snps.vcf.recode.vcf --recode --out lemna_minuta_snps.vcf
vcftools --remove nonL.minor.txt --vcf lemna_snps.vcf.recode.vcf --recode --out lemna_minor2_snps.vcf
vcftools --remove nonL.jp.txt --vcf lemna_snps.vcf.recode.vcf --recode --out lemna_jap_snps.vcf
vcftools --remove nonL.mojp.txt --vcf lemna_snps.vcf.recode.vcf --recode --out lemna_mo.jap2_snps.vcf

echo FINISHED `date`
