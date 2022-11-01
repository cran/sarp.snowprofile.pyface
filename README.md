# sarp.snowprofile.pyface

An R package that aims at creating an interface between software utilities in snowpack and avalanche research that are being developed in R and python.

Related R packages:  
  
  - sarp.snowprofile
  - sarp.snowprofile.alignment
  
Python utilities embedded here:  

  - Random forest model to compute probability of layer instability for dry snow (Mayer, 2022)
  
Utilities developed by other research teams, but translated into native R:

  - Critical crack length parametrization from Richter (2019)  

## Installation

Since this package employs python code next to R code, its installation can be slightly more challenging than standard R packages. In the following you will get a brief overview of your installation options.

This package depends on the R package `reticulate`, which was designed to handle the
interface between python and R. In most instances, `reticulate` will successfully guide you through an interactive installation session to setup the interface correctly. It is capable of locating and linking your existing python installation, or if there is no python installed on your machine, it will prompt you/ guide you through
a process to install miniconda, which in turn manages your python installation. 

If you have python installed already (either with miniconda or pip), in each R session you can specify preferred python environments to be activated (see `?reticulate::use_python`).
If you don't specify an environment, reticulate will search for one that satisfies the python dependencies of this R package. If it doesn't find such an 
environment, it will automatically create a new one called 'r-reticulate' and will install the required dependencies (Note that this automated step will often fail, see below for troubleshooting). All R and python dependencies are listed in
the DESCRIPTION file. More in-depth information about `reticulate`, its installation, and the interface between python and R in general can be found at https://rstudio.github.io/reticulate/.

### Installation -- troubleshooting

In case the automated installation procedure fails on your system, you can use the following sequence of R commands to setup the package manually. This sequence of commands was used to successfully install the package on an Amazon server for operational use.

```
## install R package reticulate:
install.packages('reticulate')

## manually install miniconda (for python) with required dependencies from within R:
reticulate::install_miniconda()
reticulate::py_install('joblib')
reticulate::py_install('numpy')
reticulate::py_install('pandas')
reticulate::py_install('scikit-learn==0.22.1')
## see DESCRIPTION file for any additional dependencies
## and `?reticulate::py_install` for additional arguments to the installation, e.g. pip = TRUE

## install sarp.snowprofile.pyface from bitbucket repository
install_bitbucket('sfu-arp/sarp.snowprofile.pyface')
```

For additional tips and examples on how to install this package on a HPC cluster that manages python with `pip`, such as Compute Canada's cluster 'Cedar', see `sarp.snowprofile.pyface/inst/hpc_setup/README.md`.


## Getting started with the R package

After the successful installation of `sarp.snowprofile.pyface`, you can ignore the fact that some of its code runs in python instead of R. You can therefore relax and use the corresponding R functions from this package. However, if you do want to include some python functions into your own scripts, check out the two files `test.R` and `test.py` in `inst/python` to familiarize yourself with intermingling the two languages.





