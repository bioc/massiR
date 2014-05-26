bioc_massiR
===========

Repository for the bioconductor package massiR

The problem
------------
Given that the sex of many species is an easily observable and usually unambiguous classification, it is surprising the number of microarray data sets in public repositories that lack the associated sample sex information. Sex-biased gene expression in normal and pathological tissues is a well recognized for both sex chromosome and autosomal genes.Therefore, the absence of sample sex information restricts the reuse of gene expression data sets where the researcher intends to factor the effect of sex in reanalysis or reinterpretation, or when intending to include such data sets in larger gene expression meta-analyses. 

This is why we developed massiR, an R package for predicting the sex of samples in microarray data sets. This package allows researchers to expand their analyses to retrospectively incorporate sex as a variable, generate or confirm sex information associated with publicly available data sets, to accurately predict the sex of samples missing sex information, or as a simple sanity check for your own microarray gene expression data.

All of the scripts and documentation for the massiR Bioconductor package are available in this repo.

Please cite the below publication if you use massiR in your work:

Buckberry S, Bent S, Bianco-Miotto T and Roberts C (2014). “massiR: a method for predicting the sex of samples in gene expression microarray datasets.” Bioinformatics.

http://dx.doi.org/10.1093/bioinformatics/btu161
http://bioinformatics.oxfordjournals.org/content/early/2014/03/22/bioinformatics.btu161.

