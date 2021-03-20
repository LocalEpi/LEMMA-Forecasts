# LEMMA-Forecasts
This reposistory containts forecasts from the LEMMA model. The main LEMMA sites are

https://localepi.github.io/LEMMA/  
https://github.com/LocalEpi/LEMMA

These forecasts can be reproduced by installing the LEMMA package and running Code/RunCAcounties.R


As of March 20, these forecasts have been updated to run using the `master` branch of LEMMA including vaccines and variants. Additional documentation will be posted by March 24.


Note: Except for San Francisco, our forecasts are not adjusted for mutual aid transfers between counties. 


## Contributors
LEMMA is a collaborative effort between experts in Medicine, Public Health, and Data Science, including but not limited to

- Joshua Schwab - UC Berkeley
- Laura B. Balzer - UMass Amherst
- Elvin Geng - Washington University
- James Peng - UC San Francisco
- Maya L. Petersen - UC Berkeley

We have moved our model fitting from R to Stan. Our Stan implementation is based on the "Santa Cruz County COVID-19 Model" (https://github.com/jpmattern/seir-covid19) by Jann Paul Mattern (UC Santa Cruz) and Mikala Caton (Santa Cruz County Health Services Agency). We are very grateful to Paul and Mikala for generously sharing their code and helping us.
