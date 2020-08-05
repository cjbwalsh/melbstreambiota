# melbstreambiota: an R package for interrogating habitat suitability models for stream biota and macroinvertebrate indices for the Melbourne region

Outputs of habitat suitability models (aka species distribution models) of 59 families 
of stream macroinvertebrates, 23 species of fish, and female and total platypus in
the Melbourne (SE Australia) region. Predict fish species, platypus, and macroinvertebrate 
family occurrence and macroinvertebrate indices  (LUMaR, SIGNAL2, number of sensitive 
families) for any reach in the region.  Calculate the indices based on imported data 
and compare to predicted values.  Diagnostic tools for interpreting results.  
    
The R package is non-standard, and contains several large data files:  

- most importantly, 84 Boosted Regression Tree Model objects which are used by the package to make predictions; but also 

- a map of the stream network; and  

- a table of environmental data for each of 8231 subcatchments that the network is split into, including a large table of monthly antecedent flow estimates for every subcatchment in the region.  

These large data files are not kept on this github repository, so if you are building the package from source, 
you will need to be loaded separately into your local github folders from the mirror on the
University of Melbourne shared drive: wergStaff/ChrisW/rstudio_projects/melbstreambiota.  
If you have access to this folder, you should copy the following 3 files to the appropriate directory:  

- R/sysdata.R (the model data)  

- data/sri48moW.rda (the antecedent flow data. Warning. do not copy any of the earlier versions of this file)  

- data/mwStreamsMap.rda

The compiled package is publicly available at 
https://tools.thewerg.unimelb.edu.au/LUMaR/  

Download instructions from that site follow:  

install.packages('https://tools.thewerg.unimelb.edu.au/documents/misc/melbstreambiota_0.1.1.tar.gz', repos = NULL, type = 'source')
Please note that the package is 665 Mb, and can take ~15 min to install once it is downloaded on a powerful computer. 
(Once it is installed, it is quick to load and use).
Associated resources for using the package:
Manual, which details the usage of all package functions, and contents of all package data;
https://tools.thewerg.unimelb.edu.au/documents/melbstreambiota.pdf
Vignette, which illustrates the functions of the package with worked examples.
https://tools.thewerg.unimelb.edu.au/documents/melbstreambiota_vignette.pdf


Old versions available at:
https://tools.thewerg.unimelb.edu.au/documents/misc/melbstreambiota_0.1.0.tar.gz
