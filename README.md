# When_where_and_why

This repositories makes available the code to produce all figures and tables in the paper:
An, Collodel and Loungani (2021). "When (where and why) forecasters get it wrong?". Unpublished manuscript.
Please cite us if you refer to our paper.

We assemble a large panel of real GDP forecasts from multiple sources and with these new data study the performance of different forecasters over time and the determinants of forecast errors. We find that a) forecasters are extremely precise during normal periods, but never predict the onset and extent of a recession b) forecasts from different sources are extremely correlated and c) that political economy considerations play an important role in understanding forecast errors.

![Forecast errors during normal times and recessions: a dismal perfomance](../When_where_why_material/output/figures/comparison/inability_recessions.png)


## Author

Umberto Collodel (umberto.collodel@gmail.com) <br/>
Zidong An (zidong.an@ruc.edu.cn) <br/>
Prakash Loungani (ploungani@imf.org)

## Language

R

## Raw Data:

Download all files in the folder

https://www.dropbox.com/sh/33sosx2ltq6vif4/AADbEPy8EGOPjfPsJh9fVWdRa?dl=0


The folder contains:

* IMF World Economic Outlook Data

`weo_rgdp`: GDP growth forecasts contained in Spring and Fall WEO publication, one sheet for each year/vintage 
`weo_january_ypdate`: GDP growth forecasts contained in January WEO publication, one sheet for each year
`weo_july_update`: GDP growth forecast contained in July publication, one sheet for each year

* World Bank GEP Data

`GEP_forecast_vintages_06232020`: GDP growth forecasts contained in Summer and Winter GEP publication, available both in long and wide format

* Consensus Data

`gdp_2008_2019_firstweek`:  GDP growth forecasts contained in April and September version of Consensus survey, one sheet for each year/vintage - mainly advanced economies available in the first week <br/>
`gdp_2008_2019_secondweek`:  GDP growth forecasts contained in April and September version of Consensus survey, one sheet for each year/vintage - mainly emerking markets and low income economies in the second week

* MONA Data

`mona_2002-2020_macro`: forecasts for main macroeconomic variables for inception of program and subsequent reviews


**Note**: the script retrieves European Commission forecasts from AMECO directly scraping the [website](https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/ameco-archive_en") and downloanding them, this is why they are not available as raw data.



## Organization

0. Main Source

1. Preparation datasets

2. Short-term analysis (current-year and year-ahead): bias and efficiency

3. Comparison with official and private sector of short-term forecasts 

4. Medium-term analysis: bias and efficiency

5. Programs and forecast errors

The main sourcing file runs the entire project.
The main file of each section cleans the global environment, installs and loads the packages required 
and sources all the scripts in the section. The function file contains custom functions to generate tables and produce graphs for the section. Individual files run the function and export the output.


## License

The data and codes are under the MIT license. This means that you can use everything as you please for research or commercial purposes, as long as you refer back to us.

## Contributing

If you find irregularities or bugs, please open an issue here.