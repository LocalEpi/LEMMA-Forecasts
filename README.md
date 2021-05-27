# LEMMA-Forecasts
This reposistory containts forecasts from the LEMMA model. The main LEMMA sites are

https://localepi.github.io/LEMMA/  
https://github.com/LocalEpi/LEMMA

These forecasts can be reproduced by installing the LEMMA package and running Code/RunCAcounties.R


As of March 20, these forecasts have been updated to run using the `master` branch of LEMMA including vaccines and variants. 

Note: Except for San Francisco, our forecasts are not adjusted for mutual aid transfers between counties. 

## LEMMA-Forecasts Package

The directory `LEMMA.forecasts` now contains an R package where all functionality used for creating forecasts and projecting scenarios is collected. 
The package documentation is available at its [https://localepi.github.io/LEMMA-Forecasts/](https://localepi.github.io/LEMMA-Forecasts/).

## Contributors
LEMMA is a collaborative effort between experts in Medicine, Public Health, and Data Science, including but not limited to

- Joshua Schwab - UC Berkeley
- Laura B. Balzer - UMass Amherst
- Elvin Geng - Washington University
- James Peng - UC San Francisco
- Maya L. Petersen - UC Berkeley
- Sean L. Wu - UC Berkeley

We have moved our model fitting from R to Stan. Our Stan implementation is based on the "Santa Cruz County COVID-19 Model" (https://github.com/jpmattern/seir-covid19) by Jann Paul Mattern (UC Santa Cruz) and Mikala Caton (Santa Cruz County Health Services Agency). We are very grateful to Paul and Mikala for generously sharing their code and helping us.
