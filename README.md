# Creating a Streamflow Reconstruction App

This repository contains code designed to create a Shiny app to easily view streamflow reconstructions. These reconstructions can be several hundred years old, so it is useful to be able to interact with the time series (zooming or summarizing as needed).  

The resulting app is available at (http://www.paleoflow.org/).

## Getting Started

These instructions will allow you to process data and generate the Shiny app. All code is written in R. See Prerequisites and Running sections below for detailed instructions.

### Prerequisites

In order to run this code, you must install:
* [R for Statistical Computing](https://www.r-project.org/)

All necesary R packages will be installed automatically in the first file.

## Running the Code

### Running all scripts at once

Code is numbered in order of operations.  If you would like to simply recreate (https://jstagge.shinyapps.io/paleo_flow), you may run the following from any command line after installing R. For more detailed information about each file, see below:

```
Rscript 01_processing.R
Rscript 02_paleo_app.R
```

<!---
### Running scripts step-by-step
The following file prepares the file system, installing any necesary packages and creating folders for model output.

```
Rscript 00_prepare_file_system.R
```
The next script downloads and processes USGS streamflow for the relevant sites. In this case, the relevant stream gauges are 10109001 and 10011500, located on the Logan and Bear rivers of Utah, respectively.
```
Rscript 01_process_streamflows.R
```
The following scripts each fit a model described in [Stagge et al. (2017)](http://) and then reconstruct flow. For all models, except the MF model, these steps are separated into "_fitting" and "_reconstruct" files.
```
Rscript 02_mf_model.R
Rscript 03_ap_model_fit.R
Rscript 04_ap_model_reconstruct.R
Rscript 05_apr_model_fit.R
Rscript 06_apr_model_reconstruct.R
```
The following files run a PCA analysis on regional tree-ring chronologies and then use these, along with global circulation indices as predictors. File naming follows the same sceme:
```
Rscript 07_pca_tree_ring.R
Rscript 08_apr_model_predictors_fit.R
Rscript 09_apr_model_predictors_reconstruct.R
```
Finally, a series of goodness of fit tests are run, in addition to several plots used to validate the model results:
```
Rscript 10_plot_gof_results.R
```


## Reference and How to Cite

For any description of this methodology, please use the following citation:

* Stagge, J.H., Rosenberg, D.E., DeRose, R.J., and Rittenour, T.M. (2017) "Monthly paleostream-flow reconstruction from annual tree-ring chronologies." Journal of Hydrology.

For any use of this code, please cite the above paper and the following:

* I will apply to zenodo to get a DOI for this.

-->

## Authors

* **James H. Stagge** - *Owner* - [jstagge](https://github.com/jstagge)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thank you to Justin DeRose for providing several tree-ring chronologies.
* Additional thanks to the International Tree-Ring Data Bank for providing further chronologies and global climate reconstructions. 

[![DOI](https://zenodo.org/badge/84977163.svg)](https://zenodo.org/badge/latestdoi/84977163)
[![Analytics](https://ga-beacon.appspot.com/UA-93682740-1/paleo_flow_shiny/readme)](https://github.com/igrigorik/ga-beacon)