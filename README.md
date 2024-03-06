# LTabundR

*R package for design-based line-transect abundance estimation*

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

Visit [**this vignette**](https://pifsc-protected-species-division.github.io/LTabundR-vignette/index.html) for a complete guide to using `LTabundR`.

### Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

### License

This content was created by U.S. Government employees as part of their official duties. This content is not subject to copyright in the United States (17 U.S.C. §105) and is in the public domain within the United States of America. Additionally, copyright is waived worldwide through the CC0 1.0 Universal public domain dedication.
