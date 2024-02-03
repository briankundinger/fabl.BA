# Code Supplement for Bayesian Analysis

This repo includes code to accompany our manuscript, “Efficient and Scalable Bipartite Matching through Fast Beta Linkage (fabl)”. The required functions are stored in the *R* folder, and they can be loaded by selecting **Build -> Load All** from the top menu of RStudio.

In particular;

- **gibbs_base** is used to run the gibbs sampler as described in Section 3
- **hash** is used to conduct the hashing and computation of $\tilde{\Gamma}$ as described in Section 4.1. 
- **gibbs_efficient** is used to run the gibbs sampler as described in Section 4.2
- **combine_hash** is used to synthesize summary statistics from multiple batches of comparison vectors, as described in Section 4.3
- **sei** implements Storage Efficient Indexing, as described in Section 4.4. 

The accuracy simulation from Section 5.2 can be found in *code/sadinle_sim.R*. The speed simulations from Section 5.1 cab be found in *code/speed_big.R* and *code/speed_big2.R*. After running this code, plots can be generated through files found in the *local* folder.

Data for the National Long Term Care Study (NLTCS) and El Salvador human rights data are restricted, and therefore not included here. 
