> The mathematical theorems which justify most frequentist statistical procedures apply only to random samples.
>
> -- *"[Why Is Random Sampling Important?](https://www.ma.utexas.edu/users/mks/statmistakes/RandomSampleImportance.html)" - Martha K. Smith, 2012*


## Novel R script for the Analysis of Non-randomised Data (NoRAND)

------------------------------------------------------------------------

**NoRAND** is a simple R script designed to perform basic interpretive statistics on non-randomised metabolomics data. 

The main NoRAND functions:

- Processing of non-randomised metabolomics data
- Graphical output of compound abundance between samples
- Comparative statistical models to estimate the contribution of Type I errors in the data
- Parametric and non-parametric interpretive statistical output


### Installation of NoRAND
Installation of NoRAND is simple: download the files and folders from this Github repository altogether in a .zip archive, and then extract them (keeping the directories intact) to the folder of your choice. Please ensure that you have R version 3.3.0 or later installed on your operating system of choice.


### Dependencies
*NoRAND requires R version 3.3.0 or later to be installed, and its use under earlier versions is not supported.*

Additionally, there are two external R packages upon which NoRAND is dependent: 

#### Qvalue
- For interpretive statistics of metabolites, NoRAND employs the qvalue package from BioConductor. This package is not available through CRAN.
- Full installation instructions and more information are available at [the official BioConductor page for the qvalue package.](https://bioconductor.org/packages/release/bioc/html/qvalue.html)

- A quick install may be performed by opening R and entering:

```
source("https://bioconductor.org/biocLite.R")
biocLite("qvalue")
```
- qvalue is (C) 2016 BioConductor and used here under the provisions of the LGPL.

#### Testthat
- Testthat is used by NoRAND for automated testing of functions. You may download it from [CRAN](https://cran.r-project.org/web/packages/testthat/index.html).
- Alternatively, testthat can be acquired by running R from the command line (or RStudio) and entering `install.packages(testthat)`.
- Testthat is (C) Hadley Wickham, 2016, and used here under the provisions of the MIT license. 


### Runnning NoRAND
Simply: Open R through the command line, or another method, and use the command `source(*NoRAND_main.R*)`

It is highly recommended that you run NoRAND (source *NoRAND_main.R*) through [RStudio](www.rstudio.com), as it provides a convenient and accessible way of opening and working with R scripts. RStudio also makes it easy to save plots produced by the script, as one can simply right-click on the plot in the viewer window of RStudio and export an image file.

However, NoRAND will still work without issue from the command line in Windows, Ubuntu 16.04, or MacOSX. Simply navigate to the "main" folder of NoRAND (whereever NoRAND_main.R is located) and type `Rscript NoRAND_main.R`.


## Using NoRAND
-------------

NoRAND includes a set of example data (in the folder /example data/) to illustrate the ideal use of the program. NoRAND will give you a choice about using this data when you run it.


### NoRAND's file format expectations
NoRAND expects data in .CSV format. It requires two files - a dataset of metabolite measurements, and a table containing the run order of samples - the precise format of which will be outlined below.

#### Metabolite data
NoRAND is designed to work with relative abundance values obtained from metabolomics experiments using mass spectrometry. It accepts this data in .CSV (comma-separated values) as, for example, can be exported from Microsoft Excel. In a .CSV file, each new line is a new row; values in columns are separated by comma. Observations (per compound) should be in rows, with sample designation (i.e. C01, C02...) in columns. Like so:

Name | S02 | S03 | S04 | S05 | S06
--- | --- | --- | --- | --- | ---
Aspartic acid | 114.763241 | 101.720170108.9033102 | 87.91465664 | 102.4046656 | 107.3506941
Phenylalanine | 48.47677892 | 54.55288819 | 55.24501607 | 46.59827622 | 51.18515474

##### In raw .CSV:

```

Name,S02,S03,S04,S05,S06
Aspartic acid,114.763241,101.7201702,108.9033102,87.91465664,102.4046656,107.3506941
Phenylalanine,48.47677892,54.55288819,55.24501607,46.59827622,51.18515474

```

### Injection order data
As NoRAND is based upon the analysis of samples by order, you must also provide it with the order in which the samples were run on the equipment. Each sample should be named as in the metabolite data file. Controls should be labelled as `C##` (where ## = 01, 02, 03...) and cases as `S##` (## = 01, 02, 03...). Tests and blanks or calibration samples should be labelled as anything other than C##/S##.

*The file should be ordered in three columns*, the first having the injection order (beginning at 1) and the second containing the name of the sample. Please also indicate whether a sample is case or control in a third column (called, exactly, "Case control"). Blanks and tests should have this column remain empty (not 0) so that NoRAND will ignore them appropriately. Like so:

injection order | Name | Case control
--- | --- | --- 
1 | T1 |
2 | T2 | 
3 | T3 | 
4 | C1 | 1
5 | C2 | 1
6 | C3 | 1
7 | C4 | 1
8 | C5 | 1

##### In raw . CSV:

```

injection order,Name,Case control,
1,T1,
2,T2,
3,T3,
4,C1,1
5,C2,1
6,C3,1
7,C4,1
8,C5,1

```

*There must be exactly the same number of C/S samples in the injection order file as in the metabolite data, or NoRAND will not produce the correct output.*


### Choosing your own data
NoRAND employs R's inbuilt user interface for importing your own data into the script. Simply choose "N" at the "Use example data?" prompt when NoRAND is first run, and an explorer menu will pop up and allow you to navigate to your files of choice in .CSV format.

#### Compound selection
NoRAND will ask you to enter a compound name. This prompt takes a text string and will search through all compound names to match the string you enter. It supports partial matching. You can also press [ENTER] at the prompt to get a list of all compounds found. Then, simply enter the number that corresponds to the compound you wish to plot.


#### Plotting
Once a compound to analyse has been chosen, NoRAND will produce three plots of the compound (relative abundance vs. sample order) on a log scale. Here are examples of the three plots from phenylalanine in the example data:

##### Basic plot with regression line:
![A plot](https://github.com/kdea020/metabolomics-project/blob/master/example%20data/phen_linear.png)

##### Corrected for run order by linear regression:
![A plot](https://github.com/kdea020/metabolomics-project/blob/master/example%20data/phen_2.png)

#### Corrected for variance introduced by a 'break':
![A plot](https://github.com/kdea020/metabolomics-project/blob/master/example%20data/phen_3.png)


These example plots are also available as .pngs in the /example data/ folder of this Github repository.


#### Statistical output
After drawing the plots, NoRAND will dump statistical output to the console. This includes p-values and q-values for each model, as well as the direction of change for metabolite concentration across samples for each model (as corrections can alter the magnitude or even the sign of this change).

NoRAND will also produce a simple plaintext summary in /exports/ called `[Compound name (i.e. phenylalanine)]_statistics.txt`.


### Adjusting program parameters

#### Step model
The step model estimates the effect of a "break" in measurements - this is to correct for the rare circumstance of a "batch" effect with only two batches. In the example data, this break occured at sample C36. If you are using NoRAND to analyse your own data, and you suspect a similar event has occured during your data collection procedures, you may alter a parameter to reflect the individual circumstance of your data.

In Modules/statfunctions.R, line 10, the vector `IsBreak` is a numeric that contains a listed sample number, as found in the vector `Lorder`. This value should be changed to the subset of `Lorder` (i.e. `Lorder[36]`) that corresponds to the last sample measured before the break in measurements.

#### Plotting
To ensure comparability between plots when assessing model quality, NoRAND uses a fixed Y axis scale for log(metabolite relative abundance). By default, when plotting on log scale NoRAND will scale from -2 to +3 orders of magnitude. If you are working with data that needs a larger y axis - such as a raw, non-normalised dataset for prospective analysis - this can be altered.

Change the values in the vector `y.lim` on line 7 of Modules/plotfunctions.R from `y.lim <- c(-2,3)` to whatever integers you like, with the format `y.lim <- c([lowerlimit],[upperlimit])`. The logarithmic plots will still be displayed on the same scale, now with your specified scale.

#### Interpretive statistics
NoRAND's q-value procedures use a lambda cutoff of 0.01 (following Benjamini-Hochburg). If you wish to use a more stringent, or relaxed, false discovery rate cutoff, you can alter the "lambda" setting for vectors `qvals.mod1-3` in lines 78-80 of Modules/statfunctions.R.

#### R^2 output
By default, NoRAND will produce an exported table, in .tdt (tab-delimited text file) format, of all R^2 values for each compound and both models. This may be found in *exports/NoRAND_R2.tdt.*

If the user does not desire this function, it may be disabled by commenting out the `GetAllR2()` near the bottom of NoRAND_main.R. Alternatively, if the user wishes to edit the output of the function or change the name of the file produced (for example) the function `GetAllR2()` is straightforward to edit, and located in Modules/statfunctions.R. Just alter the vector `newfile` to the file path and name you wish to export to instead.


Contact me
----------

You are welcome to:

-   submit suggestions on how to improve this software, and report bugs: <https://github.com/kdea020/metabolomics-project/issues>
-   fork this project, edit it, or send a pull request: <https://github.com/kdea020/metabolomics-project>
-   let me know if this has been useful for you!
