Basic Example
================
Ruben Cabrera
2023-08-01

## Basic Example

Basic Example

<https://www.youtube.com/watch?v=oI1_SV1Rpfc>

``` r
library(nlme)
library(lmerTest)
library(ggplot2)
library(dplyr) # For filter
library(lattice)
# install.packages("sjPlot")
library(sjPlot) # To use plot_model
```

## Data

Weight ~ Weeks + (1\|Subjects)

``` r
df <- data.frame(
  Weight=c(102,96,83,79,97,93,79,77,95,87,78,75,93,85,74,72),
  Subjects=c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4),
  Diet=c('A','A','B','B','A','A','B','B','A','A','B','B','A','A','B','B'),
  Weeks=c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3)
)
```

## Plot data

``` r
ggplot(df, aes(x = Weeks, y = Weight, group = factor(Subjects), color = factor(Subjects))) + geom_line()+ geom_point() + xlab('Weeks')
```

![](021-LinearMixedEffects_files/figure-gfm/plotdata-1.png)<!-- -->

## Model 1

``` r
M1 <- lmer(Weight ~ Weeks+(1|Subjects),REML = F, data = df)
summary(M1)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: Weight ~ Weeks + (1 | Subjects)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     78.2     81.3    -35.1     70.2       12 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.59974 -0.76145 -0.02532  0.71385  1.20036 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Subjects (Intercept) 72.965   8.542   
    ##  Residual              1.186   1.089   
    ## Number of obs: 16, groups:  Subjects, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  89.7750     4.2952  4.0584   20.90 2.76e-05 ***
    ## Weeks        -2.9750     0.2436 12.0000  -12.21 3.97e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr)
    ## Weeks -0.085

``` r
random.effects(M1)  # ranefe(M1)  random.effects(M1)
```

    ## $Subjects
    ##   (Intercept)
    ## 1   11.391193
    ## 2    4.917510
    ## 3   -6.784918
    ## 4   -9.523784
    ## 
    ## with conditional variances for "Subjects"

# Plots Model M1

``` r
# Residuals vs Fitted
plot(M1, which = 1)
```

![](021-LinearMixedEffects_files/figure-gfm/M1_plots-1.png)<!-- -->

``` r
# Dotplot of random effects
dotplot(ranef(M1, condVar = TRUE))
```

    ## $Subjects

![](021-LinearMixedEffects_files/figure-gfm/M1_plots-2.png)<!-- -->

``` r
# Install and load the sjPlot package

# Plot fixed and random effects
sjPlot::plot_model(M1, type = "re")
```

![](021-LinearMixedEffects_files/figure-gfm/M1_plots-3.png)<!-- -->

``` r
# Plot general fit

ggplot(df, aes(x = Weeks, y = Weight, color = factor(Subjects))) +
  geom_point() +
  geom_line(aes(group = Subjects), linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "black")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](021-LinearMixedEffects_files/figure-gfm/M1_plots-4.png)<!-- -->

``` r
# Extract fitted values
df$fitted <- fitted(M1)

# Plot fitted

ggplot(df, aes(x = Weeks, y = Weight, color = Subjects)) +
  geom_point() +
  geom_line(aes(y = fitted, group = Subjects)) +
  labs(title = "Fitted Values for Each Subject", x = "Weeks", y = "Weight")
```

![](021-LinearMixedEffects_files/figure-gfm/M1_plots-5.png)<!-- --> \#
Model 2

``` r
M2 <- lmer(Weight ~ Weeks+(1+Weeks|Subjects),REML = F, data = df)
summary(M2)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: Weight ~ Weeks + (1 + Weeks | Subjects)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     80.1     84.8    -34.1     68.1       10 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.66979 -0.49133  0.01576  0.59856  1.34245 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  Subjects (Intercept) 81.8810  9.0488        
    ##           Weeks        0.1494  0.3865   -0.88
    ##  Residual              0.9375  0.9682        
    ## Number of obs: 16, groups:  Subjects, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  89.7750     4.5425  4.0000   19.76 3.87e-05 ***
    ## Weeks        -2.9750     0.2902  4.0000  -10.25  0.00051 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr)
    ## Weeks -0.634

``` r
random.effects(M2)  # ranefe(M2)
```

    ## $Subjects
    ##   (Intercept)      Weeks
    ## 1   11.910145 -0.3533220
    ## 2    5.429013 -0.3186238
    ## 3   -7.159983  0.2485599
    ## 4  -10.179176  0.4233859
    ## 
    ## with conditional variances for "Subjects"

# Plots Model M2

``` r
# Residuals vs Fitted
plot(M2, which = 1)
```

![](021-LinearMixedEffects_files/figure-gfm/M2_plots-1.png)<!-- -->

``` r
# Extract fitted values
df$fitted <- fitted(M2)

# Plot fitted

ggplot(df, aes(x = Weeks, y = Weight, color = Subjects)) +
  geom_point() +
  geom_line(aes(y = fitted, group = Subjects)) +
  labs(title = "Fitted Values for Each Subject", x = "Weeks", y = "Weight")
```

![](021-LinearMixedEffects_files/figure-gfm/M2_plots-2.png)<!-- -->

# Model 3

``` r
M3 <- lmer(Weight ~ Weeks+(0+Weeks|Subjects),REML = F, data = df)
summary(M3)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: Weight ~ Weeks + (0 + Weeks | Subjects)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    118.4    121.4    -55.2    110.4       12 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.70521 -0.55968  0.00879  0.61121  1.93468 
    ## 
    ## Random effects:
    ##  Groups   Name  Variance Std.Dev.
    ##  Subjects Weeks  9.778   3.127   
    ##  Residual       39.928   6.319   
    ## Number of obs: 16, groups:  Subjects, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   89.775      2.643 12.000  33.962  2.7e-13 ***
    ## Weeks         -2.975      2.107  7.500  -1.412    0.198    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr)
    ## Weeks -0.538

``` r
random.effects(M3)  # ranefe(M3)
```

    ## $Subjects
    ##       Weeks
    ## 1  3.815645
    ## 2  1.382480
    ## 3 -2.211968
    ## 4 -2.986157
    ## 
    ## with conditional variances for "Subjects"

# Plots Model M3

``` r
# Residuals vs Fitted
plot(M3, which = 1)
```

![](021-LinearMixedEffects_files/figure-gfm/M3_plots-1.png)<!-- -->

``` r
# Extract fitted values
df$fitted <- fitted(M3)

# Plot fitted

ggplot(df, aes(x = Weeks, y = Weight, color = Subjects)) +
  geom_point() +
  geom_line(aes(y = fitted, group = Subjects)) +
  labs(title = "Fitted Values for Each Subject", x = "Weeks", y = "Weight")
```

![](021-LinearMixedEffects_files/figure-gfm/M3_plots-2.png)<!-- -->

# Model 4

``` r
M4 <- lmer(Weight ~ Weeks+Diet+(1|Subjects),REML = F, data = df)
summary(M4)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: Weight ~ Weeks + Diet + (1 | Subjects)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     70.3     74.2    -30.2     60.3       11 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.76023 -0.85112  0.06013  0.81347  1.03987 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Subjects (Intercept) 5.930    2.435   
    ##  Residual             1.186    1.089   
    ## Number of obs: 16, groups:  Subjects, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  97.9625     1.8019   4.3477  54.367 2.51e-07 ***
    ## Weeks        -2.9750     0.2436  12.0000 -12.215 3.97e-08 ***
    ## DietB       -16.3750     2.4953   4.0000  -6.562  0.00279 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) Weeks 
    ## Weeks -0.203       
    ## DietB -0.692  0.000

``` r
random.effects(M4)  # ranefe(M3)
```

    ## $Subjects
    ##   (Intercept)
    ## 1    3.095180
    ## 2   -3.095180
    ## 3    1.309499
    ## 4   -1.309499
    ## 
    ## with conditional variances for "Subjects"

``` r
coef(M4)
```

    ## $Subjects
    ##   (Intercept)  Weeks   DietB
    ## 1   101.05768 -2.975 -16.375
    ## 2    94.86732 -2.975 -16.375
    ## 3    99.27200 -2.975 -16.375
    ## 4    96.65300 -2.975 -16.375
    ## 
    ## attr(,"class")
    ## [1] "coef.mer"

``` r
fixef(M4)
```

    ## (Intercept)       Weeks       DietB 
    ##     97.9625     -2.9750    -16.3750

# Plots Model M4

``` r
# Residuals vs Fitted
plot(M4, which = 1)
```

![](021-LinearMixedEffects_files/figure-gfm/M4_plots-1.png)<!-- -->

``` r
# Extract fitted values
df$fitted <- fitted(M4)

# Plot fitted by Subjects
p <- ggplot()
p <- p + geom_point(data=df, aes(x = Weeks, y = Weight, color = Subjects))
p
```

![](021-LinearMixedEffects_files/figure-gfm/M4_plots-2.png)<!-- -->

``` r
plot_fit_by_subjects <- geom_line(data = df, aes(x = Weeks, y = fitted, group = Subjects)) 
q <- p + plot_fit_by_subjects
q + labs(title = "Fitted Values for Each Subject", x = "Weeks", y = "Weight")
```

![](021-LinearMixedEffects_files/figure-gfm/M4_plots-3.png)<!-- -->

``` r
# Plot fitted by Diet
df$fit_by_Diet <- ifelse(df$Diet=='A',fixef(M4)["(Intercept)"] + df$Weeks*fixef(M4)["Weeks"],
                         fixef(M4)["(Intercept)"] + df$Weeks*fixef(M4)["Weeks"]+fixef(M4)["DietB"])


plot_fit_by_diets <- geom_line(data = df, aes(x = Weeks, y = fit_by_Diet, group = Diet)) 
r <- p + plot_fit_by_diets
r + labs(title = "Fitted Values for Each Diet", x = "Weeks", y = "Weight")
```

![](021-LinearMixedEffects_files/figure-gfm/M4_plots-4.png)<!-- -->

``` r
plot_model(M4,type ="pred", terms = c("Weeks","Diet"))
```

![](021-LinearMixedEffects_files/figure-gfm/M4_plots-5.png)<!-- -->
