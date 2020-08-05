# melbstreambiota: an R package for interrogating habitat distribution models of macroinvertebrates, fish and platypus in streams of the Melbourne Region

The R package is non-standard, and contains several large data files: 
most importantly, 70 Boosted Regression Tree Model objects which are used by the package to make predictions;
but also a map of the stream network; and 
a table of environmental data for each of 8231 subcatchments that the network is split into;
including a large table of monthly antecedent flow estimates for every subcatchment in the region.
These large data are not kept on this github repository, so will need to be loaded separately into your local github folders from the
university of Melbourne shared drive: wergStaff/ChrisW/rstudio_projects/melbstreambiota.  If you have access to this folder, the following commands 
will copy the requisite files to your git folder 

The compiled package is publicly available at 
https://tools.thewerg.unimelb.edu.au/LUMaR/

Download instructions from that site follow
install.packages('https://tools.thewerg.unimelb.edu.au/documents/misc/melbstreambiota_0.1.1.tar.gz', repos = NULL, type = 'source')
Please note that the package is 663 Mb, and can take ~15 min to install once it is downloaded on a powerful computer. 
(Once it is installed, it is quick to load and use).
Associated resources for using the package:
Manual, which details the usage of all package functions, and contents of all package data;
https://tools.thewerg.unimelb.edu.au/documents/melbstreambiota.pdf
Vignette, which illustrates the functions of the package with worked examples.
https://tools.thewerg.unimelb.edu.au/documents/melbstreambiota_vignette.pdf
