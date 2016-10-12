<hr/>
> The mathematical theorems which justify most frequentist statistical procedures apply only to random samples.
>
> -- *"[Why Is Random Sampling Important?](https://www.ma.utexas.edu/users/mks/statmistakes/RandomSampleImportance.html)" - Martha K. Smith, 2012*

<<<<<<< HEAD
_this document is the first impression of the project_
_please try to keep it simple yet interesting_
_a good readme file will contribute to the mark for documentation_
_please remove these instructions (in italics) in the final version_
_feel free to adjust the instructions and/or their order as you see fit_

## Project Description

_insert a description of what the project is all about_
_make only sensible assumptions about the domain knowledge of the user_
_link to resources within your repo or other resources on the web if need be_

Metabolomics Project Trello board:
https://trello.com/b/YHwLrEsh/metabolomics-project-scrum-board

## Data Description

_indicate the size of the data_
_provide a throrough metadata_

## Running the program

_insert instructions for repeating your work_
_make only sensible assumptions about resources user needs_
_for instance, a user might have R/Python but might not have all the packages_

=======
<hr/>
NOvel R script for  NORAND
=======

------------------------------------------------------------------------

**NORAND** is a simple program designed to perform basic interpretive statistics on non-randomised metabolomics data. 

The main NORAND functions:

- something
- Another thing.
- At least one more thing.


### Installation

You can install:




Using NORAND
-------------

NORAND includes a set of example data (in "/example data/") 

Below are quick examples of how janitor tools are commonly used. A full description of each function can be found in janitor's [catalog of functions](https://github.com/sfirke/janitor/blob/master/vignettes/introduction.md).

### Choosing your own data

Take this roster of teachers at a fictional American high school, stored in the Microsoft Excel file [dirty\_data.xlsx](https://github.com/sfirke/janitor/blob/master/dirty_data.xlsx): ![All kinds of dirty.](dirty_data.PNG)

Dirtiness includes:

-   Dreadful column names
-   Rows and columns containing Excel formatting but no data
-   Dates stored as numbers
-   Values spread inconsistently over the "Certification" columns

Here's that data after being read in to R:

``` r
library(pacman) # for loading packages
p_load(readxl, janitor, dplyr)

roster_raw <- read_excel("dirty_data.xlsx") # available at http://github.com/sfirke/janitor
glimpse(roster_raw)
#> Observations: 17
#> Variables: 12
#> $ First Name        <chr> "Jason", "Jason", "Alicia", "Ada", "Desus", "Chien-Shiung", "Chien-Shiung", NA,...
#> $ Last Name         <chr> "Bourne", "Bourne", "Keys", "Lovelace", "Nice", "Wu", "Wu", NA, "Joyce", "Lamar...
#> $ Employee Status   <chr> "Teacher", "Teacher", "Teacher", "Teacher", "Administration", "Teacher", "Teach...
#> $ Subject           <chr> "PE", "Drafting", "Music", NA, "Dean", "Physics", "Chemistry", NA, "English", "...
#> $ Hire Date         <dbl> 39690, 39690, 37118, 27515, 41431, 11037, 11037, NA, 32994, 27919, 42221, 34700...
#> $ % Allocated       <dbl> 0.75, 0.25, 1.00, 1.00, 1.00, 0.50, 0.50, NA, 0.50, 0.50, NA, NA, 0.80, NA, NA,...
#> $ Full time?        <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", NA, "No", "No", "No", "No", "N...
#> $ do not edit! ---> <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $ Certification     <chr> "Physical ed", "Physical ed", "Instr. music", "PENDING", "PENDING", "Science 6-...
#> $ Certification     <chr> "Theater", "Theater", "Vocal music", "Computers", NA, "Physics", "Physics", NA,...
#> $ Certification     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $                   <dttm> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
```

Excel formatting led to an untitled empty column and 5 empty rows at the bottom of the table (only 12 records have any actual data). Bad column names are preserved.

Clean it with janitor functions:

``` r
roster <- roster_raw %>%
  clean_names() %>%
  remove_empty_rows() %>%
  remove_empty_cols() %>%
  convert_to_NA(c("TBD", "PENDING")) %>%
  mutate(hire_date = excel_numeric_to_date(hire_date),
         main_cert = use_first_valid_of(certification, certification_2)) %>%
  select(-certification, -certification_2) # drop unwanted columns

roster
#> # A tibble: 12 × 8
#>      first_name last_name employee_status    subject  hire_date percent_allocated full_time      main_cert
#>           <chr>     <chr>           <chr>      <chr>     <date>             <dbl>     <chr>          <chr>
#> 1         Jason    Bourne         Teacher         PE 2008-08-30              0.75       Yes    Physical ed
#> 2         Jason    Bourne         Teacher   Drafting 2008-08-30              0.25       Yes    Physical ed
#> 3        Alicia      Keys         Teacher      Music 2001-08-15              1.00       Yes   Instr. music
#> 4           Ada  Lovelace         Teacher       <NA> 1975-05-01              1.00       Yes      Computers
#> 5         Desus      Nice  Administration       Dean 2013-06-06              1.00       Yes           <NA>
#> 6  Chien-Shiung        Wu         Teacher    Physics 1930-03-20              0.50       Yes   Science 6-12
#> 7  Chien-Shiung        Wu         Teacher  Chemistry 1930-03-20              0.50       Yes   Science 6-12
#> 8         James     Joyce         Teacher    English 1990-05-01              0.50        No   English 6-12
#> 9          Hedy    Lamarr         Teacher    Science 1976-06-08              0.50        No           <NA>
#> 10       Carlos    Boozer           Coach Basketball 2015-08-05                NA        No    Physical ed
#> 11        Young    Boozer           Coach       <NA> 1995-01-01                NA        No Political sci.
#> 12      Micheal    Larsen         Teacher    English 2009-09-15              0.80        No    Vocal music
```

The core janitor cleaning function is `clean_names()` - call it whenever you load data into R.

### Examining dirty data

#### Finding duplicates

Use `get_dupes()` to identify and examine duplicate records during data cleaning. Let's see if any teachers are listed more than once:

``` r
roster %>% get_dupes(first_name, last_name)
#> # A tibble: 4 × 9
#>     first_name last_name dupe_count employee_status   subject  hire_date percent_allocated full_time
#>          <chr>     <chr>      <int>           <chr>     <chr>     <date>             <dbl>     <chr>
#> 1 Chien-Shiung        Wu          2         Teacher   Physics 1930-03-20              0.50       Yes
#> 2 Chien-Shiung        Wu          2         Teacher Chemistry 1930-03-20              0.50       Yes
#> 3        Jason    Bourne          2         Teacher        PE 2008-08-30              0.75       Yes
#> 4        Jason    Bourne          2         Teacher  Drafting 2008-08-30              0.25       Yes
#> # ... with 1 more variables: main_cert <chr>
```
Yes, some teachers appear twice. We ought to address this before counting employees.

#### Producing R^2 output



Together, these tabulation functions reduce R's deficit against Excel and SPSS when it comes to quick, informative counts.

Contact me
----------

You are welcome to:

-   submit suggestions on how to improve this software, and report bugs: <https://github.com/kdea020/metabolomics-project/issues>
-   fork this project, edit it, or send a pull request: <https://github.com/kdea020/metabolomics-project>
-   let me know if this has been useful for you!
>>>>>>> upstream/master
