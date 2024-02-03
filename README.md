# Code Supplement for Bayesian Analysis

I have prepared code for running the Fast Beta Linkage (fabl) method to accompany our submission to Bayesian Analysis. The required functions are stored in the *R* folder, and they can be loaded by selecting **Build -> Load All** from the top menu of RStudio.

In particular,

- **gibbs_base** is used to run the gibbs sampler as described in Section 3
- **hash** is used to conduct the hashing and computation of $\tilde{\Gamma}$ as described in Section 4.1. 
- **gibbs_efficient** is used to run the gibbs sampler as described in Section 4.2
- **combine_hash** is used to synthesize summary statistics from multiple batches of comparison vectors, as described in Section 4.3
- **sei** implements Storage Efficient Indexing, as described in Section 4.4. 

Data for the National Long Term Care Study (NLTCS) and El Salvador human rights data are restricted, and therefore not included here. 
