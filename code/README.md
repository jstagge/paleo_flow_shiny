# Creating a Streamflow Reconstruction App

[![DOI](https://zenodo.org/badge/84977163.svg)](https://zenodo.org/badge/latestdoi/84977163)

This repository contains code associated with the Paleoflow website, available at [http://www.paleoflow.org/](http://www.paleoflow.org/)

## Getting Started

These instructions will allow you to process data and generate the Shiny app. All code is written in R. See Prerequisites and Running sections below for detailed instructions.

### Prerequisites

In order to run this code, you must install:
* [R for Statistical Computing](https://www.r-project.org/)

## Running the Code
You must set the working directory to the Code folder after pulling the code.

### Preparing the file system

All necessary R packages will be installed automatically by the file [00-prepare_file_system.R](https://github.com/jstagge/paleo_flow_shiny/blob/master/code/00-prepare_file_system.R)

```
Rscript 00-prepare_file_system.R
```

### Preparing the flow and site database

To prepare flows and site information, run the next three code files. Files [01-process_monthly.R](https://github.com/jstagge/paleo_flow_shiny/blob/master/code/01-process_monthly.R) and [02-process_annual.R](https://github.com/jstagge/paleo_flow_shiny/blob/master/code/02-process_annual.R) process the monthly and annual flow data, respectively. File [03_merge_preprocess.R](https://github.com/jstagge/paleo_flow_shiny/blob/master/code/03_merge_preprocess.R) merges the output from these files into a single file, along with processing site information.

```
Rscript 01-process_monthly.R
Rscript 02-process_annual.R
Rscript 03_merge_preprocess.R
```

### Testing the website locally

To run the website locally, run the file [04_paleo_app.R](https://github.com/jstagge/paleo_flow_shiny/blob/master/code/04_paleo_app.R)

```
Rscript 04_paleo_app.R
```

## Organization of the website code repository
All website code is located in the [code/paleo_flow/](https://github.com/jstagge/paleo_flow_shiny/blob/master/code/paleo_flow/) directory.

Within this folder, the global.R file opens the required packages and reads in all data. The ui.R file sets up the User Interface. The server.R runs the server-side code, by directing the server to external/app.R, which contains all server-side code.


## Reference and How to Cite

For description of this website and package, please use the following citation:

* Stagge, J.H., Rosenberg, D.E., and DeRose, R.J. (2019) "PaleoFlow: An Online Visualization Tool for Reconstructed Streamflows." SoftwareX. [In review](https://www.journals.elsevier.com/softwarex).

For use of this code, please cite the above paper and the following:

* Stagge, J.H. (2019). jstagge/paleo_flow_shiny: Version 3.1. Zenodo. doi: 10.5281/zenodo.3361845 [http://doi.org/10.5281/zenodo.3361845](http://doi.org/10.5281/zenodo.3361845)

## Authors

* **James H. Stagge** - *Owner* - [jstagge](https://github.com/jstagge)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* This work was funded by Utah Mineral Lease funds.
* The authors thank developers of the TreeFlow website and the NOAA World Data Center for Paleoclimatology for hosting climate reconstruction data. 
* We would also like to thank the authors of numerous R packages cited in the article.



[![Analytics](https://ga-beacon.appspot.com/UA-93682740-1/paleo_flow_shiny/readme)](https://github.com/igrigorik/ga-beacon)