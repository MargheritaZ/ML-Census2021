# ML-Census2021 
In this repository I will share machine learning algorithms for the 'signs of life' project of the 2021 icelandic Census. 

It contains the R-scripts for random forest classification of individuals, optimised for several performance measures under a given tolerance level for specificity.

In 'Random_forest_cutoff_tuning.r' the only tuning parameter is the cutoff, while in 'Random_forest_cutoff_strata' we stratify the sample according to prensence in the country, and optimize 'sampsize' and 'cutoff' simultaneously.
