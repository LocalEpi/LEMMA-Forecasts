# LEMMA-Forecasts
This reposistory containts forecasts from the LEMMA model. The main LEMMA sites are

https://localepi.github.io/LEMMA/  
https://github.com/LocalEpi/LEMMA

These forecasts can be reproduced by installing the LEMMA package and running Code/RunCAcounties.R

The .xlsx files have quantile forecasts of hospitalization, ICU and deaths due to COVID-19.  
The .pdf files have plots with these same forecasts, along with posterior distributions of the model parameters.


## Contributors
LEMMA is a collaborative effort between experts in Medicine, Public Health, and Data Science, including but not limited to

- Joshua Schwab - UC Berkeley
- Laura B. Balzer - UMass Amherst
- Elvin Geng - Washington University
- James Peng - UC San Francisco
- Maya L. Petersen - UC Berkeley

We have moved our model fitting from R to Stan. Our Stan implementation is based on the "Santa Cruz County COVID-19 Model" (https://github.com/jpmattern/seir-covid19) by Jann Paul Mattern (UC Santa Cruz) and Mikala Caton (Santa Cruz County Health Services Agency). We are very grateful to Paul and Mikala for generously sharing their code and helping us.
