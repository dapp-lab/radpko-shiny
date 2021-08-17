This is repository contains the code used to generate the
[RADPKO interface](https://dapp-lab.org/radpko/) on the 
Data-driven Analysis of Peace Project [website](https://dapp-lab.org/).
It contains three R scripts:

- `prep.R` processes the latest version of the RADPKO data
(`radpko_bases_cc.csv`) in preparation for the Shiny app
- `dependencies.R` loads required dependencies that packrat fails to identify
- `app.R` serves the RADPKO interface
