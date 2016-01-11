# itrak
## Tools for processing eye-tracking data

`itrak` provides functions to facilitate the processing of data from visual attention experiments. Currently the functions can be divided into two categories: attention bias and pupillometry.

### Attention bias functions:
The attention bias functions calculate attention bias scores using data from tasks such as the dot probe. 

### Pupillometry functions:
The pupillometry functions denoise and standardize data from the continuous measurement of pupil diameter. This includes functions for correcting signal artifacts, normalizing to a baseline reference period, and extracting low-frequency pupil-dilation signals from high-frequency noise.

## Installation instructions:
To install `itrak` in R, you first need to install the `devtools` package if you haven’t already:
```
	install.packages("devtools")
```

Once `devtools` is installed, use the following command to install `itrak` on your system:
```
	devtools::install_github("jashu/itrak")
```

After that, load it as you would any package:
```	
	library(itrak)
```
