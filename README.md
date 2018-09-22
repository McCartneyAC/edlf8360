# `edlf8360`

An R package for EDLF 8360 at UVA's School of Education and Human Development (Fall 2018). 

## Installation
You know the drill:
```
devtools::install_github("McCartneyAC/edlf8360")
```

## Data
Over time, the package will grow to contain all datasets contained within the textbooks: Hox, Moerbeek and van de Schoot (2018) and Rabe-Hesketh and Skrondal (2012). For now, I am getting a consistent error with `haven::read_sav` so will need to use SPSS in the library to convert files to `.csv` then to `.rda`. Additionally, this means that the popularity data `pop.dat` from Hox et al. (2018) contains no header rows as that is how the data are given on [their github repo](https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/tree/master/chapter%202/popularity/MPLUS). 


### Datasets:
* `hsb` (High School and Beyond) 
* `pop` (Student Popularity)
* `hours` (from Ziliak (1997) and consists of panel data on 532 men with annual waves from 1979 to 1988.)
* Datasets from Field, Miles, and Field: 
  * `surgery`
  * `honeymoon`
  * `miller2007`
  * `hill2007`
* `jsp` from homework 1
* more to come 

#### useage: 

```
data(hsb)

data(hours)
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

The first example above does a grand-mean center; however, often in HLM we need to group mean center. This is easily accomplished:
```
hsb %>% 
  group_by(schoolid) %>% 
  center(mathach, mean(mathach)) 
```

### Standardize

Similar to center, standardize performs a variable adjustment that can be contained within a pipe. In this case, `standardize` performs a z-transformation on a given variable, as such:

```
hsb %>% 
  select(ses, mathach) %>% 
  standardize(mathach) %>% 
  head()
```
```
# A tibble: 6 x 2
      ses mathach
    <dbl>   <dbl>
1 -1.53    -0.999
2 -0.588    1.01 
3 -0.528    1.11 
4 -0.668   -0.577
5 -0.158    0.749
6  0.0220  -1.19 
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
NOTE: The structure of this means that no specified model will be saved into the global environment, so, e.g., the `plot_margins` function can't be used after `regress`. 

This function also works with clustered standard errors (though it requires more packages to be downloaded to do so). 

```
regress(hsb, formula = mathach ~ ses , cluster = "schoolid")
```
```
R^2= 0.13014 

            Estimate Std. Error  t value      Pr(>|t|)
(Intercept) 12.74740  0.1694473 75.22927  0.000000e+00
ses          3.18387  0.1334850 23.85190 9.677759e-126
```

### Margins Plots

Takes a pre-specified linear model and plots a margins plot using ggplot2. This is done without styling or labels, so these must be added post-hoc. 

``` 
model5<-lm(mathach ~ minority + female + ses, data = hsb)
plot_margins(model5)
```
![](https://github.com/McCartneyAC/edlf8360/blob/master/data/margin_model5.png)

### Calc_ICC
~coming as soon as we cover the topic~

`psych` provides `psych::ICC` which has several different implementations of ICC calculations; however, these are specifically derived for inter-rater reliability estimates and it's not clear if they'll be useful for mixed effects models because 1) which specification do we need? 2) the matrix operation means we'll need some procedure for using `select`, I guess, with the variables being different (not the case for inter-rater) and 3) it should be as simple as doing `icc(model5)` `> 0.25` or something. I'm going to keep thinking about this. 



#### SOLVED. 

```
model2<-lmer(lnwg ~ 1 + (1 |id), data = hours)
icc(model2)
[1] 0.8250423
```

### Effective Sample Size:

`ess()` will calculate an effective sample size when given a null model, a number of participants, and a number of clusters. 

```
ess(model1, participants = 1000, clusters = 50)
```
