#!/bin/bash
#SBATCH --job-name=plink
#SBATCH --partition=defq
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=64g
#SBATCH --time=48:00:00
#SBATCH --output=/gpfs01/home/stxks22/%x.out
#SBATCH --error=/gpfs01/home/stxks22/%x.err

echo STARTED `date`

plink --vcf uklightaddadd_filtered_nomiss_MAF.F4.vcf.gz  --make-bed -out test --allow-extra-chr

echo FINISHED `date`
