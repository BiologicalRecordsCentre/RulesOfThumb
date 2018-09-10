# RulesOfThumb

An assessment of the data requirements in order to run an occupancy model and produce useful results.  Also included is a function to estimate whether or not a new dataset or a regional/habitat sub-division of the data will produce a useful model.

A good starting point is the rmarkdown results: ```TSDA_Analysis.pdf```.  This explains the project and how it all runs.

## Viewing all data and querying by region or habitat

If you want to play with the habitat and regional results, and query by whichever region or habitat you wish, check out the file results_calculator.R.  For this to run, you'll need to download the function script ./Results/plotting_function.R and all the data files in ```./Results/metrics```, ```./Habitat``` and ```./Regional```.

## Using your own dataset

This script can also be used to run the metrics on any dataset you wish.  You need to set up your data and calculate metrics as explained in the comments of the script ```results_calculator.R```.  For this to run, you need to download this script and the function script ```./Results/plotting_function.R```.

## Acknowledgements

This work would not have been possible without the help of, from CEH: Tom August, Michael Pocock, Nick Isaac, Gary Powney and Charlie Outhwaite.  Thanks also to Jenni Border from BTO for her work on combining habitat and regional data for the UK.

Mark Logie, 10 Sept 2018
