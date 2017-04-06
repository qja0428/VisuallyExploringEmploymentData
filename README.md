# VisuallyExploringEmploymentData

## Introduction

This project will introduce you to the US employment data provided by the Bureau of Labor Statistics (BLS) of the United States government. The BLS is the federal agency responsible for measuring labor market activity and working conditions and prices in the US economy. Its mission is the collection, analysis, and dissemination of essential economic information to support public and private decision-making. In this project, we will use the aggregate annual data on employment and pay, stratified by geography and industry. This data can be downloaded as a compressed comma-separated value (csv) file at [2015 QCEW data](https://data.bls.gov/cew/data/files/2015/csv/2015_annual_singlefile.zip), which contains the single file 2015 (the last available full year of data).annual.singlefile.csv . This file has 38 columns and about 3.5 million rows.

In this project, I will do the following:
* Preparing for analysis
* Importing employment data into R
* Exploring the employment data
* Obtaining and merging additional data
* Adding geographical information
* Extracting state- and county-level wage and employment information
* Visualizing geographical distributions of pay
* Exploring where the jobs are, by industry
* Animating maps for a geospatial time series
* Benchmarking performance for some common tasks

Files list:
* employment.R: r script file
* VisuallyExploringEmploymentData.pdf: pdf file from R markdown
* VisuallyExploringEmploymentData.Rmd: R markdown file
* agglevel_titles.csv: QCEW Aggregation Level Codes
* area_titles.csv: QCEW Area Codes and Titles (For NAICS coded data)
* industry_titles.csv: QCEW Industry Codes and Titles (For NAICS Coded Data)
* ownership_titles.csv: QCEW Ownership Codes (For NAICS coded data)
* size_titles.csv: QCEW Establishment Size Classes (For NAICS-Based Data)

