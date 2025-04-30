This project contains three folders for analyses:

Input: include data used in running analyses scripts (e.g., megastudy data, covariate data)

Output: include output at various stages of analyses (i.e, graphs, regression output tables)

Scripts: various scripts to run full set of analyses. To run all relevant analyses included, run "covariates_main.R"


To run the full analyses pipeline, run: .\Scripts\covariates_main.R
This file is also where main parameters of analyses can be set (e.g. covariates and datasets to analysis)

NOTE: PCA component and resulting regression analyses are coded according to findings from previous step which required visual inspections (e.g. the number of principle components within covariates analyses)

While most data analyses run in R (with scripts written in version 4.1.2), Jupyter Notebook (Python3) is used to create LaTex-friendly tables from output Excel sheets and R dataframes. A corresponding "LaTex Table Formatting.ipynb" file can be found in ".\Scripts\".

_____________________
We have included a .pdf file with only the Supplements section of the paper (in .zip folder)
