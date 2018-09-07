# `edlf8360`

An R package for EDLF 8360 at UVA's School of Education and Human Development (Fall 2018). 

## installation
You know the drill:
```
devtools::install_github("McCartneyAC/edlf8360")
```

## Data
Over time, the package will grow to contain all datasets contained within the textbooks: Hox, Moerbeek and van de Schoot (2018) and Rabe-Hesketh and Skrondal (2012). For now, I am getting a consistent error with `haven::read_sav` so will need to use SPSS in the library to convert files to `.csv` then to `.rda`. Additionally, this means that the popularity data `pop.dat` from Hox et al. (2018) contains no header rows as that is how the data are given on [their github repo](https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/tree/master/chapter%202/popularity/MPLUS). 


### Datasets:
* `hsb` (High School and Beyond) 
* `pop` (Student Popularity)
* more to come 

#### useage: 

```
hsb<-data(hsb)
```

## Functions

So far, I have found three useful functions: 

### Center

Centers a variable on a given center. Should be useable within a tidy data pipe, e.g. 
```
hsb %>%
  center(ses, mean(ses))
  
  
# or 

hsb %>%
  center(mathach, 5)
```  
### Regress
Named as an analogue to stata's `regress` command, this function has two features. First, it allows regression to be the final step of a data pipeline, and secondly it specifies a linear model and summarizes it in a single step. It also means that the `data=` argument of a `lm()` is no longer necessary. It does this by calling `summarize(lm())` while re-ordering the arguments of `lm()` for `data=` to come first. For example: 

```
hsb %>% 
   filter(schoolid %in% c(1224, 1288, 1296)) %>% 
   center(ses, mean(ses)) %>% 
   regress(mathach~female + minority)
```
```
Call:
lm(formula = ..1, data = df)

Residuals:
     Min       1Q   Median       3Q      Max 
-13.7125  -5.0128  -0.0476   5.2404  14.8554 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   12.453      1.069  11.653  < 2e-16 ***
female        -1.573      1.230  -1.279 0.203471    
minority      -4.137      1.219  -3.394 0.000941 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6.634 on 117 degrees of freedom
Multiple R-squared:  0.1044,	Adjusted R-squared:  0.08905 
F-statistic: 6.817 on 2 and 117 DF,  p-value: 0.001584

```

### Margins Plots

Takes a pre-specified linear model and plots a margins plot using ggplot2. This is done without styling or labels, so these must be added post-hoc. 

``` 
model5<-lm(mathach ~ minority + female + ses, data = hsb)
plot_margins(model5)
```

### Calc_ICC
coming as soon as we cover the topic
