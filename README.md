# MSE for North Atlantic Swordfish

This repository contains the code and documentation for developing the management
strategy evaluation (MSE) for the North Atlantic Swordfish fishery.

More information on the North Atlantic Swordfish MSE is available on the project's [Homepage](https://iccat.github.io/nswo-mse/). 

## SWOMSE Package

The `SWOMSE` R package has been developed to conduct a management strategy 
evaluation (MSE) for the Atlantic swordfish fishery.

### Installation 

The `SWOMSE` package can be installed from this GitHub repository using the `remotes` 
package:
```
# install.packages("remotes")
remotes::install_github("ICCAT/nswo-mse")
```

Alternatively, you can clone the repository on your machine and build and install 
the package locally with RStudio.

## Home Page

The North Atlantic Swordfish MSE homepage is available [here](https://iccat.github.io/nswo-mse/). The homepage include links to the [Trial MSE Specifications document](https://iccat.github.io/nswo-mse/TS/Trial_Specs.html), [CMP Development Guide](https://iccat.github.io/nswo-mse/cMPdevelopment/CMP-Development-Guide.html), and [SWOMSE User Manual](https://iccat.github.io/nswo-mse/UserManual/User_Manual.html), as well as a summary of the Operating Models and links to relevant SCRS papers.

### User Manual
The `SWOMSE` user manual can also be accessed with:
```
library(SWOMSE)
SWO_userguide()
```

## Bug Reports
Please report bugs and other issues relating to the code using the 
[Issues](https://github.com/ICCAT/nswo-mse/issues) feature.




