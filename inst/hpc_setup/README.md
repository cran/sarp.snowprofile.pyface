# Package installation on a HPC cluster with python managed by pip

While the python installation and management through `reticulate` is per default managed via Miniconda,
high performance computing facilities (such as Compute Canada cluster 'cedar') ask users not to
install Miniconda, but use `pip` and standard virtual python environments instead.
In this case, users need to manually create a virtual environment that satisfies the python
dependencies of this R package (i.e., listed in the DESCRIPTION file).

A suitable environment `py-snowpack` can be created on cedar as follows:

```
module load python/3.7
module load scipy-stack/2021a  # contains numpy, pandas, etc
virtualenv --no-download py-snowpack  # creates the virtual environment 'py-snowpack' in $HOME directory
source py-snowpack/bin/activate  # activates environment
pip install --no-index --upgrade pip
pip install joblib --no-index
pip install scikit-learn==0.22.1 --no-index  # specific version!
deactivate
```

To use this specific virtual environment in your R session and by this R package,
the requirements to activating the environment need to be called in a job (bash) script,

```
module load python/3.7
module load scipy-stack/2021a 
```

and the following line should be included in your R script to direct `reticulate` to your environment 

```
library(sarp.snowprofile.pyface)
reticulate::use_virtualenv("~/py-snowpack")  # use the path to the virtual environment installed above
```
