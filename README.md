# LTabundR
*R package for design-based line-transect density estimation*  


The `R` package `LTabundR` offers tools that facilitate and standardize design-based line-transect abundance estimation of cetaceans, based on typical workflow used following NOAA Fisheries ship surveys in the central and eastern Pacific (e.g., [Barlow 2006](https://www.google.com/url?q=https://onlinelibrary.wiley.com/doi/10.1111/j.1748-7692.2006.00032.x&sa=D&source=docs&ust=1676392605333708&usg=AOvVaw07ERYd9m2kJg0LttBFVyil), [Barlow and Forney 2007](https://spo.nmfs.noaa.gov/content/abundance-and-population-density-cetaceans-california-current-ecosystem), [Bradford et al. 2017](https://spo.nmfs.noaa.gov/content/fishery-bulletin/abundance-estimates-cetaceans-line-transect-survey-within-us-hawaiian), [Bradford et al. 2021](https://repository.library.noaa.gov/view/noaa/29004)).

This `R` package is a consolidation of code developed over many years by many NOAA Fisheries scientists, primarily Jay Barlow, Tim Gerrodette, Jeff Laake, Karin Forney, Amanda Bradford, and Jeff Moore. This package was developed by Eric Keen and Amanda Bradford with support from the [NOAA Fisheries National Protected Species Toolbox Initiative](https://www.fisheries.noaa.gov/national/population-assessments/national-protected-species-toolbox-initiative).  

### `BETA`-testing only
This package is currently in `beta` testing and is not yet ready for widespread use.  


### Installation

```{r}
# Install 'devtools' package, if needed
if (!require('devtools')) install.packages('devtools')

# Install LTabundR remotely from GitHub
library(devtools)
devtools::install_github('PIFSC-Protected-Species-Division/LTabundR')

# Load into session
library(LTabundR)
```

Note that this package contains large built-in datasets and may take several minutes to install.   


### Vignette

Visit [**this vignette**](https://emk-noaa.github.io/LTAvignette/) for a complete guide to using `LTabundR`. 
