# `edlf8360`

An R package for EDLF 8360 at UVA's School of Education and Human Development (Fall 2018). 

## installation
You know the drill:
```
devtools::install_github("McCartneyAC/edlf8360")
```

## Data
Over time, the package will grow to contain all datasets contained within the textbooks: Hox, Moerbeek and van de Schoot (2018) and Rabe-Hesketh and Skrondal (2012). For now, I am getting a consistent error with `haven::read_sav` so will need to use SPSS in the library to convert files to `.csv` then to `.rda`. Additionally, this means that the popularity data `pop.dat` from Hox et al. (2018) contains no header rows as that is how the data are given on [their github repo](https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/tree/master/chapter%202/popularity/MPLUS). 

## Note from Quinn

I am using read.dta from the foreign package to read in Stata data files:

read.dta("hsb.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = FALSE, 
               convert.underscore = FALSE, warn.missing.labels = TRUE)

### Datasets:
* `hsb` (High School and Beyond) 
* `pop` (Student Popularity)
* more to come 

## Functions

So far, I have found two useful functions: 

### Center

Centers a variable on a given center. Should be useable within a tidy data pipe, e.g. 
```
hsb %>%
  center(ses, mean(ses))
  
  
# or 

hsb %>%
  center(mathach, 5)
```  


### Margins Plots

Takes a pre-specified linear model and plots a margins plot using ggplot2. This is done without styling or labels, so these must be added post-hoc. 

``` 
model5<-lm(mathach ~ minority + female + ses, data = hsb)
plot_margins(model5)
```

### Calc_ICC
coming as soon as we cover the topic
