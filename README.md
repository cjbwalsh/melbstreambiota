# melbstreambiota: an R package for interrogating habitat suitability models for stream biota and macroinvertebrate indices for the Melbourne region

This package provides outputs of habitat suitability models (aka species distribution models) of 59 families of stream macroinvertebrates, 11 species of native fish, and female and total platypus in the Melbourne (SE Australia) region. Predict fish species, platypus, and macroinvertebrate family occurrence and macroinvertebrate indices  (LUMaR, SIGNAL2, number of sensitive families) for any reach in the region.  Calculate the indices based on imported data and compare to predicted values.  Diagnostic tools for interpreting results.  

To install the package:

`install.package("devtools")` (if not already installed)

`devtools::install_github("cjbwalsh/melbstreambiota", build_manual = TRUE, build_vignettes = TRUE)`
    
Building the vignette can take 5-10 min. To install the package without the vignette:

`devtools::install_github("cjbwalsh/melbstreambiota")`

The package draws large data files from an associated Open Science Framework repository (https://osf.io/mcxrq), and stores them in the user's cache.  The file of antecedent flow conditions (sri48moW.rda) is updated annually (in the first two months of the year). After installing the package this file can be updated using the function `update_sri()`.

Associated resources for using the package:  

- Manual, which details the usage of all package functions, and contents of all package data;
https://osf.io/79kdp 

- Vignette, which illustrates the functions of the package with worked examples.
https://osf.io/v2tda  

- Pre-print manuscript describing the LUMaR index (held back until improved models are developed)
https://doi.org/10.31219/osf.io/392kv

Old versions available at:  

- https://tools.thewerg.unimelb.edu.au/documents/misc/melbstreambiota_0.1.0.tar.gz

- https://tools.thewerg.unimelb.edu.au/documents/misc/melbstreambiota_0.1.1.tar.gz

Changes from 0.1.1 to version 0.1.2

- Fish and platypus models have been revised.  Only 11 native fish species are retained from the original 23. An additional female platypus model has been added to match published work (see Vignette)

- Partial fish barrier predictors have been revised

- Recast to permit installation from github. To achieve this the data files needed to make predictions (model objects and antecedent flow predictors) are downloaded as necessary (for each function) from an associated Open Science Framework repository ([https://osf.io/mcxrq/](https://osf.io/mcxrq/)) and cached in a user-specific data directory.

- Spatial data objects in the package have been converted to simple features objects, and all mapping functions converted to plotting with the sf package (from sp).

- The subcatchment layer (subcs) for the stream network is not used in the package, but is useful for identifying subc ids for sampling sites (and other stream network analysis). It is now stored (with a copy of the streams and coast layers) in an associated Open Science Framework repository ([https://osf.io/mcxrq/](https://osf.io/mcxrq/)).

- sri48moW data updated, and the function `update_sri()` added to permit updating of these data without the need to update the package.

- The vignette has been edited to reflect the above changes.
