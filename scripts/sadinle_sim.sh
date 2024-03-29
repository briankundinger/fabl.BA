#!/bin/bash
#SBATCH --array=1-300
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=sadinle_sim
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=16G
#
# modules
#

Rscript code/sadinle_sim.R
