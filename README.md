> The mathematical theorems which justify most frequentist statistical procedures apply only to random samples.
>
> -- *"[Why Is Random Sampling Important?](https://www.ma.utexas.edu/users/mks/statmistakes/RandomSampleImportance.html)" - Martha K. Smith, 2012*


## Novel R script for the Analysis of Non-randomised Data (NoRAND)

------------------------------------------------------------------------

**NoRAND** is a simple program designed to perform basic interpretive statistics on non-randomised metabolomics data. 

The main NoRAND functions:

- Processing of non-randomised metabolomics data
- Graphical output of compound abundance between samples
- Comparative statistical models to estimate the contribution of Type I error in the sample
- Parametric and non-parametric interpretive statistical output


### Installation of NoRAND


### Dependencies
For interpretive statistics of metabolites, NoRAND employs the qvalue package from BioConductor. 

### Runnning NoRAND
It is highly recommended that you run NoRAND (NoRAND_main.R) through RStudio (LINK), as it provides an intuitive way of working with R scripts. This also makes it easy to save plots produced by the script, as one can simply right-click on the plot in the viewer window of RStudio and export an image file. However, NoRAND will still work without issue from the command line in Windows, Ubuntu 16.04, or MacOSX. Simply navigate to the "main" folder of NoRAN (where the readme D and type "Rscript NoRAND_main.R".


## Using NoRAND
-------------

NoRAND includes a set of example data (in the folder example data/) 


comparison of raw data files and R's processsing... also, graphs! example output!


### NoRAND's file format expectations
NoRAND expects data in th

#### Metabolite data
NoRAND is designed to work with relative abundance values obtained from metabolomics experiments using mass spectrometry.

#> Name	S02	S03	S04	S05	S06
#> Aspartic acid	114.763241	101.7201702	108.9033102	87.91465664	102.4046656
#> Phenylalanine	48.47677892	54.55288819	55.24501607	46.59827622	51.18515474
#> Leucine	92.74353472	111.1656868	95.42636172	81.76610921	90.2420409
#> Alanine	246.0528951	237.6442629	258.883971	180.6158395	284.2731637



### Injection order data
As NoRAND is based upon the analysis of samples by order, you must also provide it with the order in which the samples were run on the equipment. Each sample should be named as in the metabolite data file. Controls should be labelled as C## (where ## = 01, 02, 03...) and cases as S## (## = 01, 02, 03...). Tests and blanks or calibration samples should be labelled T## or B##.
This should be ordered in three columns, the first having the injection order (beginning at 1) and the second containing the name of the sample. Please also indicate whether a sample is case or control in a third column (called, exactly, "Case control"). Blanks and tests should have this column remain empty (not 0) so that NoRAND will ignore them appropriately. Like so:

#> injection order	Name case.control
#> 1	T1                             
#> 2	T2                 
#> 3	T3
#> 4	C1 1
#> 5	C2 1
#> 6	C3 1
#> 7	C4 1
#> 8	C5 1
#> 9	C6 1
#> 10	C7    1
#> 11	C8    1
#> 12	C9    1
#> 13	C10   1
#> 14	B1
#> 15	B2
#> 16	B3

There must be exactly the same number of C/S samples in the injection order file as in the metabolite data, or NoRAND will not produce the correct output.

### Choosing your own data
NoRAND employs R's inbuilt user interface for importing your own data into the script. Simply choose "N" at the "Use example data?" prompt when NoRAND is first run, and an explorer menu will pop up and allow you to navigate to your files of choice.


### Adjusting program parameters

#### Step model
The step model estimates the effect of a "break" in measurements - this is to correct for the rare circumstance of a "batch" effect with only two batches. In the example data, this break occured at sample C36. If you are using NoRAND to analyse your own data, and you suspect a similar event has occured during your data collection procedures, you may alter a parameter to reflect the individual circumstance of your data. In Modules/statfunctions.R, line 10, the vector "IsBreak" is a numeric that contains a listed sample number, as found in the vector Lorder. This value should be changed to the subset of Lorder (i.e. Lorder[36]) that corresponds to the last sample measured before the break in measurements.

#### Plotting
To ensure comparability between plots when assessing model quality, NoRAND uses a fixed Y axis scale for log(metabolite relative abundance). By default, when plotting on log scale NoRAND will scale from -2 to +3 orders of magnitude. If you are working with data that needs a larger y axis - such as a raw, non-normalised dataset for prospective analysis - you can change the values in the vector "y.lim" on line 7 of Modules/plotfunctions.R from c(-2,3) to whatever you like, with the format " y.lim <- c([lowerlimit],[upperlimit]) ". The logarithmic plots will still be displayed on the same scale, now with your specified scaling.

#### Interpretive statistics
NoRAND's q-value procedures use a lambda cutoff of 0.01 (following Benjamini-Hochburg). If you wish to use a more stringent, or relaxed, false discovery rate cutoff, you can alter the "lambda" setting for vectors "qvals.mod[1-3]" in lines 78-80 of Modules/statfunctions.R.

#### R^2 output
By default, NoRAND will produce an exported table, in .tdt (tab-delimited text file) format, of all R^2 values for each compound and both models. This may be found as exports/NoRAND_R2.tdt.

If the user does not desire this function, it may be disabled by commenting out the line (NO.) 'GetAllR2()' in NoRAND_main.R. Alternatively, if the user wishes to edit the output of the function or change the name of the file produced (for example) the function GetAllR2() is straightforward to edit, and located in Modules/statfunctions.R. Just alter the vector "newfile" 


Contact me
----------

You are welcome to:

-   submit suggestions on how to improve this software, and report bugs: <https://github.com/kdea020/metabolomics-project/issues>
-   fork this project, edit it, or send a pull request: <https://github.com/kdea020/metabolomics-project>
-   let me know if this has been useful for you!
