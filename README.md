# MSE for North Atlantic Swordfish

This repository contains the code and documentation for developing the management
strategy evaluation (MSE) for the North Atlantic Swordfish fishery.

## SWOMSE Package

The `SWOMSE` R package has been developed to conduct a management strategy 
evaluation (MSE) for the Atlantic swordfish fishery.

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

### Shiny App
An interactive application for presenting the MSE results is currently being 
developed using the R Shiny framework. Access the app with:
```
Shiny()
```

Note that the app is under development and all results are preliminary and for
testing purposes only. 

## MSE Specifications
NOTE: The previous Google Docs version of the MSE Specifications document was corrupted.

The MSE Specifications document is available [here](https://iccat.github.io/nswo-mse/TS/Trial_Specs.html).

It is designed as a living document that will continue to be updated to reflect 
the current assumptions and structure of the MSE process. 

## Bug Reports
Please report bugs and other issues relating to the code using the 
[Issues](https://github.com/ICCAT/nswo-mse/issues) feature.

For other comments and questions on the SWO MSE process, please use the Comment feature
in the MSE Specifications document described above. 



