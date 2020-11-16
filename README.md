# IEO_forecasts


This repositories makes available the code to produce all figures and tables in the paper:

An, Collodel and Loungani (2021). "When (where and why) forecasters get it wrong?". Unpublished manuscript.

Please cite us if you refer to our paper.

## Author

Umberto Collodel (umberto.collodel@gmail.com)

## Language

R

## Raw Data:

Download all files in the folder

https://www.dropbox.com/sh/33sosx2ltq6vif4/AADbEPy8EGOPjfPsJh9fVWdRa?dl=0

`weo_rgdp`: GDP growth forecasts contained in Spring and Fall WEO publication \\
`weo_january_ypdate`: GDP growth forecasts contained in January WEO publication \\
`weo_july_update`: GDP growth forecast contained in July publication \\


## Organization

1. Preparation datasets

2. Short-term analysis (current-year and year-ahead): bias and efficiency

3. Comparison with official and private sector of short-term forecasts 

4. Medium-term analysis: bias and efficiency

5. Programs and forecast errors


The main file of each section cleans the global environment, installs and loads the packages required 
and sources all the scripts in the section. The function file contains custom functions to generate tables and produce graphs for the section. Individual files run the function and export the output.
