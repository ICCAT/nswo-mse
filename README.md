# MSE for North Atlantic Swordfish

This repository contains the code and documentation for developing the management
strategy evaluation (MSE) for the North Atlantic Swordfish fishery.

## SWOMSE Package

The `SWOMSE` R package has been developed to conduct a management strategy 
evaluation (MSE) for the Atlantic swordfish fishery.

The package includes a collection of swordfish operating models (OMs), 
a swordfish data object, and management procedures (MPs) that can be evaluated
with closed-loop simulation testing, and applied to the data object for management advice.

### Installation 

The `SWOMSE` package can be installed from this GitHub repository using the `devtools` 
package:
```
# install.packages("devtools")
devtools::install_github("ICCAT/nswo-mse", auth_token='your_personal_access_token')
```

Note that as this repository is private, you will need to provide a personal access
token to download and install from GitHub. See [here](https://help.github.com/en/github/authenticating-to-github/creating-a-personal-access-token-for-the-command-line)
for information on creating a personal access token.

Alternatively, you can clone the repository on your machine and build and install 
the package locally with RStudio.

### User Manual
The `SWOMSE` user manual can be accessed with:
```
SWO_userguide()
```
The `SWOMSE` package depends on the [`DLMtool`](https://github.com/DLMtool/DLMtool)
and [`MSEtool`](https://github.com/tcarruth/MSEtool) R packages. All functions and 
features from these packages are available in `SWOMSE`. Help documentation for 
these packages and can accessed with:
```
DLMtool::userguide()
MSEtool::userguide()
```

### Shiny App
An interactive application for presenting the MSE results is currently being 
developed using the R Shiny framework. Access the app with:
```
Shiny()
```

Note that the app is under development and all results are preliminary and for
testing purposes only. 

## MSE Specifications
The MSE Specifications document is available as a Google Doc  [here](https://docs.google.com/document/d/1L_1P5L3lcikzNhOSCOdUBSDfsa_NwN0tpIWvzxQ5xWw/edit?usp=sharing).

It is designed as a living document that will continue to be updated to reflect 
the current assumptions and structure of the MSE process. 

Please use the Comment feature for questions, comments, or feedback. These
will be incorporated into the document as the model is updated.

## Project Progress
Progress on the 2019 project and 2020 proposed work is available in a Google Sheet  [here](https://docs.google.com/spreadsheets/d/1itPZ8xgDWNYepMJdZAh4ERSrdzmlSCLDBvV7QmXHyWw/edit?usp=sharing).

A list of proposed management procedures and performance metrics will be developed
in this Google Sheet as they are proposed by the SWO Group. 

All users have Edit privileges. Details and/or proposed MPs/performance 
metrics as they are discussed.

## Bug Reports
Please report bugs and other issues relating to the code using the 
[Issues](https://github.com/ICCAT/nswo-mse/issues) feature.

For other comments and questions on the SWO MSE process, please use the Comment feature
in the MSE Specifications document described above. 



