# IJC445-Data-Visualisation

This repository contains the code, cleaned data, and graphical outputs for the IJC445 Coursework

## What is the objective of the report?

This report analyses spatial and temporal varaition in NO2 concentration across air quality monitoring sites in South Yorkshire, through production of a composite visual, consisting of 4 plots:

- A geospatial bubble map comparing average NO2 concentrations 
- A boxplot comparing the distributions of NO2 
- A seasonal line pot showing average monthl NO2 concentrations 
- A heatmap showing diurnal trends in NO2 concentrations 

## How this repository is structured

- `code/` contains the script needed to create the visualisations.
 
- `data/` contains the cleaned air quality data ready to be inputted into the script.

- `plots/` contains all of the figures produced in the report.

## How do you run the code?

1) Download the project
2) Open the project folder in RStudio
3) Confirm working directory includes `code/`,`data/` and `plots/` files. If necessary, set working directory:
```r
 setwd("~/Documents/Data Visualisation")
```
4) Run the script
```r
 source("code/Data_Visualisation.R")
```
