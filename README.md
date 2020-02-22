# Amadeus-Datawork

This package corresponds to the paper "Measuring productivity dispersion: a parametric approach using the Lévy alpha-stable distribution" by Jangho Yang, Torsten Heinrich, Julian Winkler, François Lafond, Pantelis Koutroumpis, and Doyne Farmer (2019; available at https://arxiv.org/pdf/1910.05219). It contains the code used for the analyses performed in the paper.

# Dependencies

Requires R version ```3.6.2``` or newer and packages ```pacman, dplyr, StableEstim, lmomco, devtools, colorspace, RColorBrewer, msir, plotly, scales, grDevices, boot, xtable, tidyr, zoo, ggplot2, cowplot, foreach, tidyverse, Rcpp, RcppArmadillo, BH, gridextra, reshape2, feather``` as well as ```finity``` from ```https://github.com/x0range/finity```

One visualization script ( ```17.Goodness_Comparisons.py``` ) in addition requires Python (version ```3.7``` or later) with packages ```matplotlib```, ```pandas```, ```feather-format```.

# Installation

No installation of Amadeus-Datawork is required. Just download with 

```git clone https://github.com/Orbis-Amadeus-Oxford/Amadeus-Datawork.git```

The dependencies can be installed from the R shell with

```install.package("pacman")
library(pacman)
pacman:p_load(dplyr, StableEstim, lmomco, devtools, colorspace, RColorBrewer, msir, plotly, scales, 
grDevices, boot, xtable, tidyr, zoo, ggplot2, cowplot, foreach, tidyverse, Rcpp, RcppArmadillo, 
BH, gridextra, reshape2)
```

And (to install ```finity``` from ```https://github.com/x0range/finity```): Download the tar.gz and call

```Rcmd INSTALL finity_0.1.tar.gz```

or build the tar.gz yourself and install that.


# Usage

Make sure all required packages are installed and the necessary data is present. Then run the ```R``` files with names starting with a number ( ```01..R, 02.Amadeus_masterfile.R, ..., 16.Compile_Levy_and_Subbotin_Goodness_Test_Results_into_DF.R`.R``` ) in order. They will produce data, results and plots. For details, see the "Files" section. Finally, run ```17.Goodness_Comparisons.py``` with Python; it will produce one last figure.


# Files

* ```01..R``` Script for preprocessing of EU KLEMS deflators.

* ```02.Amadeus_masterfile.R``` Script to clean the raw data from by-country Amadeus raw csv files. Variables in these should be labeled as indicated in Appendix A.1 of the paper.

* ```03.Data_Clean.R``` Script to clean and combine the country-level Rda file generated from the masterfile. It also creats several objects for indexing variables that will be used in other scripts.

* ```04.Descriptive_Analysis.R``` This script generates tables and plots for descriptive statistics, including the number of observations, the proportion of different size types, and overall patterns of the tail behavior of the distribution.

* ```05.Levy_PDF.R``` This script generates an example PDF of Levy 

* ```06.Fitting_Levy_Boot.R``` Function to do Levy alpha stable parameter error estimates using bootstraps.

* ```07.Plot_Levy_Fit.R``` This script generates the plots of distributions with Levy fitted lines and plots of estimated parameters by years, sizes, and sectors.

* ```08.PWT_Delta_Compare.R``` This script is to generate the time series plot of the estimated location parameters of LP change to compare them to the aggregate LP from PWT

* ```09.Compare_Levy_Subo.R``` Script to perform cross-validation with Levy-alpha stable fits and AEP/Subbotin fits in comparison

* ```10.Dispersion_Metrics_Time.R``` This script is to plot the time series of dispersion metrics in Section 4 

* ```11.MLE_QT.R``` This script is to compare the performance of ML and QT for levy estimation (See Appendix)

* ```12.QT_GMM_Fitting.R``` Script to fit subsamples with McCulloch's method and GMM in comparison.

* ```13.plotting_QT_GMM_fits.R``` Script to visualize the McCulloch's and GMM fitting performance comparison generated in ```12.QT_GMM_Fitting.R```

* ```14.Data_finite_moment_tests.R``` Script to perform moment finiteness tests using Trapani's method and the ```finity``` package.

* ```15.Scaling_of_sd.R``` Script for visualization of sd development (divergence) with increasing sample size.

* ```16.Compile_Levy_and_Subbotin_Goodness_Test_Results_into_DF.R``` Script for compiling results generated by ```09.Compare_Levy_Subo.R``` into a data frame.

* ```17.Goodness_Comparisons.py``` Script for visualization goodness tests as packaged by ```16.Compile_Levy_and_Subbotin_Goodness_Test_Results_into_DF.R```. This script is the only one that needs to be run with Python.

* Internal files of the package in the subdirectories ```fittinglevy```, ```fittingAEP```, ```crossvalidation```: These files are auxiliary files imported by some of the scripts above for internal use; they are not meant to be executed directly.


# Data

This package expects Amadeus / Orbis Europe (Bureau van Dijk) firm level data as well EU KLEMS industry level deflator data. The Amadeus / Orbis data should be present in csv files each containing all observations for one country. Variables in these should be labeled as indicated in Appendix A.1 of the paper.
