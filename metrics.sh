#!/bin/bash
#SBATCH --job-name=metrics
#SBATCH --partition=defq
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=64g
#SBATCH --time=24:00:00
#SBATCH --output=/gpfs01/home/stxks22/fst.out
#SBATCH --error=/gpfs01/home/stxks22/fst.err

echo STARTED `date`

vcftools --vcf lemna_minuta_snps.vcf.recode.vcf --window-pi 10000 --out lemna_minuta_snps.vcf_10kb
vcftools --vcf lemna_minuta_snps.vcf.recode.vcf --TajimaD 10000 --outlemna_minuta_snps.vcf_TajD_10kb
vcftools --vcf lemna_minor2_snps.vcf.recode.vcf --window-pi 10000 --out lemna_minor2_snps.vcf_10kb
vcftools --vcf lemna_minor2_snps.vcf.recode.vcf --TajimaD 10000 --out lemna_minor2_snps.vcf_TajD_10kb
vcftools --vcf lemna_jap_snps.vcf.recode.vcf --window-pi 10000 --out lemna_jap_snps.vcf.recode.vcf_10kb
vcftools --vcf lemna_jap_snps.vcf.recode.vcf --TajimaD 10000 --out lemna_jap_snps.vcf.recode.vcf_TajD_10kb
vcftools --vcf lemna_mo.jap2_snps.vcf.recode.vcf --window-pi 10000 --out lemna_mo.jap2_snps.vcf_10kb
vcftools --vcf lemna_mo.jap2_snps.vcf.recode.vcf --TajimaD 10000 --out lemna_mo.jap2_snps.vcf_TajD_10kb

#population 1 Lemna japonica, poulation 2 Lemna minor,  population 3 Lemna minuta, population 4 Spirodela polyrhiza, population 5 - Lemna hybrids

vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population1 --weir-fst-pop population2 --fst-window-size 10000 --out pop1_vs_pop2_FST_10kb
vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population1 --weir-fst-pop population3 --fst-window-size 10000 --out pop1_vs_pop3_FST_10kb
vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population1 --weir-fst-pop population4 --fst-window-size 10000 --out pop1_vs_pop4_FST_10kb
vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population1 --weir-fst-pop population5 --fst-window-size 10000 --out pop1_vs_pop5_FST_10kb
vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population2 --weir-fst-pop population3 --fst-window-size 10000 --out pop2_vs_pop3_FST_10kb
vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population2 --weir-fst-pop population4 --fst-window-size 10000 --out pop2_vs_pop4_FST_10kb
vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population2 --weir-fst-pop population5 --fst-window-size 10000 --out pop2_vs_pop5_FST_10kb
vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population3 --weir-fst-pop population4 --fst-window-size 10000 --out pop3_vs_pop4_FST_10kb
vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population3 --weir-fst-pop population5 --fst-window-size 10000 --out pop3_vs_pop5_FST_10kb
vcftools --vcf filtered.F4.4fds.consistent.merged.vcf.gz --weir-fst-pop population4 --weir-fst-pop population5 --fst-window-size 10000 --out pop4_vs_pop5_FST_10kb

echo FINISHED `date`

