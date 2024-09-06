Basic Example
================
Ruben Cabrera
2024-07-29

# Basic Example

Adding Covariances between random effects
<https://nlmixr2.org/articles/addingCovariances.html>

``` r
library(nlmixr2)
library(ggplot2)
library(ggforce) # facet_wrap_paginate
library(dplyr) # For filter 
```

## Data

Phenobarbitol data:

This is from a PK study in neonatal infants. They received multiple
doses of phenobarbital for seizure prevention. A data frame with 744
rows and 8 columns

- ID: Infant ID
- TIME: Time of (hr)
- AMT: Dose in (ug/kg)
- WT: Weight (kg)
- APGR: A 5-minute Apgar score to measure infant health
- DV: The concentration of phenobarbitol in the serum (ug/mL)
- MDV: If the dependent variable (DV) is missing; 0 for observations, 1
  for doses
- EVID: Event ID

``` r
# ?pheno_sd
head(pheno_sd)
```

    ##   ID TIME  AMT  WT APGR   DV MDV EVID
    ## 1  1  0.0 25.0 1.4    7  0.0   1    1
    ## 2  1  2.0  0.0 1.4    7 17.3   0    0
    ## 3  1 12.5  3.5 1.4    7  0.0   1    1
    ## 4  1 24.5  3.5 1.4    7  0.0   1    1
    ## 5  1 37.0  3.5 1.4    7  0.0   1    1
    ## 6  1 48.0  3.5 1.4    7  0.0   1    1

``` r
summary(pheno_sd)
```

    ##        ID             TIME             AMT               WT      
    ##  Min.   : 1.00   Min.   :  0.00   Min.   : 0.000   Min.   :0.60  
    ##  1st Qu.:13.00   1st Qu.: 19.23   1st Qu.: 2.300   1st Qu.:1.10  
    ##  Median :25.00   Median : 59.30   Median : 3.000   Median :1.30  
    ##  Mean   :28.24   Mean   : 63.70   Mean   : 5.281   Mean   :1.49  
    ##  3rd Qu.:44.00   3rd Qu.: 96.42   3rd Qu.: 4.400   3rd Qu.:1.70  
    ##  Max.   :59.00   Max.   :389.80   Max.   :70.000   Max.   :3.60  
    ##       APGR              DV              MDV              EVID       
    ##  Min.   : 1.000   Min.   : 0.000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.: 5.000   1st Qu.: 0.000   1st Qu.:1.0000   1st Qu.:1.0000  
    ##  Median : 7.000   Median : 0.000   Median :1.0000   Median :1.0000  
    ##  Mean   : 6.394   Mean   : 5.328   Mean   :0.7917   Mean   :0.7917  
    ##  3rd Qu.: 8.000   3rd Qu.: 0.000   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :10.000   Max.   :67.900   Max.   :1.0000   Max.   :1.0000

``` r
str(pheno_sd)
```

    ## 'data.frame':    744 obs. of  8 variables:
    ##  $ ID  : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ TIME: num  0 2 12.5 24.5 37 48 60.5 72.5 85.3 96.5 ...
    ##  $ AMT : num  25 0 3.5 3.5 3.5 3.5 3.5 3.5 3.5 3.5 ...
    ##  $ WT  : num  1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 ...
    ##  $ APGR: int  7 7 7 7 7 7 7 7 7 7 ...
    ##  $ DV  : num  0 17.3 0 0 0 0 0 0 0 0 ...
    ##  $ MDV : int  1 0 1 1 1 1 1 1 1 1 ...
    ##  $ EVID: int  1 0 1 1 1 1 1 1 1 1 ...

``` r
# Plot data AMT Dose in (ug/kg)
ggplot(filter(pheno_sd,EVID==1), aes(x=TIME, y = AMT, group=factor(ID), color=factor(ID))) + geom_line() + geom_point() + ylim(c(0, 70)) + xlab('Time')
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
## Plot data DV concentration of phenobarbitol

ggplot(filter(pheno_sd,EVID==0), aes(x=TIME, y = DV, group=factor(ID), color=factor(ID))) + geom_line() + geom_point() + ylim(c(0, 70)) + xlab('Time')
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
# Could be interesting plot AMT vs DV (using the last data recorded in both axis)
```

``` r
# The superposition of doses as vertical black lines on concentration profiles

pAMT <- geom_segment(data=filter(pheno_sd,EVID==1), aes(x=TIME, xend=TIME, y=0, yend=AMT), size=1)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
pDV <- geom_point(data=filter(pheno_sd,EVID==0), aes(x=TIME, y=DV,  color=factor(ID)), size=3)

ggplot() + pAMT
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot() + pDV
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ggplot() + pAMT + pDV + facet_wrap_paginate(ID~., ncol=3, nrow=3, page=1) + theme(legend.position="none") + ylab("Dose and Concentration (mg/L)")
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

# Model

## Compartmental Models

Pharmacokinetics is the study of the time course of a drug and its
metabolites following introduction into the body. In this section we
describe a class of models that has been extensively used in such
studies to model individual drug concentrations, $y(t)$, as a function
of the time $t$.

Let $A_i(t)$ represent the amount of drug in compartment $i$, $i=0,1$ at
time $t$, with compartment 0 representing the site from which absorption
occurs. The differential equations describing the drug flow between the
compartments may be assumed to be of the form

$$ \frac{d A_0}{d t} = -k_a A_0
$$

$$ \frac{d A_1}{d t} = k_a A_0 - k_e A_1
$$

where $k_a$ is the absorption rate constant associated with flow from
compartment 0 to compartment 1, and $k_e$ is the elimination rate
constant.

## One Compartment

With only one compartment $A_1$, the differential equation for the
amount $A_1$ is

$$ \frac{d A_1}{dt } = - k_e A_1 $$

The concentration is

$$C_p = \frac{A_1(t)}{V(t)}$$ and considering $V$ as volume, the
clearance \$ CL \$ is defined from

$$k_e = \frac{CL}{V}$$

It is usual to parametrize the model in terms of clearance $CL$ and
volume $V$ instead of $k_e$.

The actual fitting parametrization is carried out in term of so called
typical values for clearance and volume $tcl$ and $tv$ :

$$
\begin{align}
  CL =& e^{ tcl + \eta_{CL} }  \\
  V  =& e^{ tv  + \eta_{V} } 
\end{align}
$$ where $\eta_{CL}$ and $\eta_{V}$ are random variables obeying normal
distributions with mean zero and covariance matrix $\Omega$. The
components of $\Omega$ are then additional fit parameters. The matrix
$\Omega$ and the variables $\eta$ are responsible to explain random
inter-individual variability.

$$(\eta_{CL} , \eta_{V}) \approx   N(0, \Omega)  $$ If $\Omega$ is a
diagonal matrix, the inter-individual variabilities are independent.
Otherwise, they have some correlation.

The residual error model provides the final random variability on the
predicted concentration. The residual error model in this example is

$$C_{obs} = C_p + \varepsilon$$

where $C_{obs}$ is the observed concentration, $C_p$ is the
concentration in plasma and $\varepsilon$ is a normal random variable
(specified by $add.err=\sigma^2$).

$$ \varepsilon \approx N(0, \sigma^2) $$

``` r
pheno <- function() {
  ini({
    tcl <- log(0.008) # typical value of clearance
    tv <-  log(0.6)   # typical value of volume
    eta.cl + eta.v ~ c(1, 0.01, 1) ## var(eta.cl), cov(eta.cl, eta.v), var(eta.v)
                      # interindividual variability on clearance and volume
    add.err <- 0.1    # residual variability
  })
  model({
    cl <- exp(tcl + eta.cl) # individual value of clearance
    v <- exp(tv + eta.v)    # individual value of volume
    ke <- cl / v            # elimination rate constant
    d/dt(A1) = - ke * A1    # model differential equation
    cp = A1 / v             # concentration in plasma
    cp ~ add(add.err)       # define error model
  })
}
```

The fit is performed by the function nlmixr/nlmixr2 specifying the
model, data and estimate

``` r
fit <- nlmixr(pheno, pheno_sd, "saem", control=list(print=0), table=list(cwres=TRUE, npde=TRUE))
```

    ## ℹ parameter labels from comments will be replaced by 'label()'

    ## → loading into symengine environment...

    ## → pruning branches (`if`/`else`) of saem model...

    ## ✔ done

    ## → finding duplicate expressions in saem model...

    ## [====|====|====|====|====|====|====|====|====|====] 0:00:00

    ## ✔ done

    ## rxode2 2.0.13 using 6 threads (see ?getRxThreads)
    ##   no cache: create with `rxCreateCache()`

    ## Calculating covariance matrix

    ## → loading into symengine environment...

    ## → pruning branches (`if`/`else`) of saem model...

    ## ✔ done

    ## → finding duplicate expressions in saem predOnly model 0...

    ## → finding duplicate expressions in saem predOnly model 1...

    ## → finding duplicate expressions in saem predOnly model 2...

    ## ✔ done

    ## → Calculating residuals/tables

    ## → loading into symengine environment...

    ## → pruning branches (`if`/`else`) of full model...

    ## ✔ done

    ## → calculate jacobian

    ## → calculate sensitivities

    ## → calculate ∂(f)/∂(η)

    ## → calculate ∂(R²)/∂(η)

    ## → finding duplicate expressions in inner model...

    ## → optimizing duplicate expressions in inner model...

    ## → finding duplicate expressions in EBE model...

    ## → optimizing duplicate expressions in EBE model...

    ## → compiling inner model...

    ## ✔ done

    ## → finding duplicate expressions in FD model...

    ## → optimizing duplicate expressions in FD model...

    ## → compiling EBE model...

    ## ✔ done

    ## → compiling events FD model...

    ## ✔ done
    ## ✔ done

    ## → compress origData in nlmixr2 object, save 33192

    ## → compress phiM in nlmixr2 object, save 233144

    ## → compress parHist in nlmixr2 object, save 7880

    ## → compress saem0 in nlmixr2 object, save 1640

``` r
print(fit)
```

    ## ── nlmixr² SAEM OBJF by FOCEi approximation ──
    ## 
    ##           OBJF      AIC      BIC Log-likelihood Condition#(Cov) Condition#(Cor)
    ## FOCEi 688.6792 985.5502 1003.811      -486.7751        7.570439        6.604927
    ## 
    ## ── Time (sec $time): ──
    ## 
    ##         setup optimize covariance saem table compress
    ## elapsed 0.002        0          0 7.73  2.47      0.1
    ## 
    ## ── Population Parameters ($parFixed or $parFixedDf): ──
    ## 
    ##                          Parameter  Est.     SE %RSE    Back-transformed(95%CI)
    ## tcl     typical value of clearance -4.99 0.0743 1.49 0.00677 (0.00586, 0.00784)
    ## tv         typical value of volume 0.346 0.0538 15.5          1.41 (1.27, 1.57)
    ## add.err       residual variability  2.83                                   2.83
    ##         BSV(CV%) Shrink(SD)%
    ## tcl         52.4      1.50% 
    ## tv          41.0      1.09% 
    ## add.err                     
    ##  
    ##   Covariance Type ($covMethod): linFim
    ##   Correlations in between subject variability (BSV) matrix:
    ##     cor:eta.v,eta.cl 
    ##           0.989  
    ##  
    ## 
    ##   Full BSV covariance ($omega) or correlation ($omegaR; diagonals=SDs) 
    ##   Distribution stats (mean/skewness/kurtosis/p-value) available in $shrink 
    ##   Censoring ($censInformation): No censoring
    ## 
    ## ── Fit Data (object is a modified tibble): ──
    ## # A tibble: 155 × 26
    ##   ID     TIME    DV EPRED  ERES   NPDE     NPD   PDE    PD  PRED    RES    WRES
    ##   <fct> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl>
    ## 1 1        2   17.3  18.8 -1.53 -0.422  0.0167 0.337 0.507  17.5 -0.222 -0.0297
    ## 2 1      112.  31    29.9  1.15  0.394  0.394  0.653 0.653  27.9  3.11   0.254 
    ## 3 2        2    9.7  10.9 -1.18 -0.994 -0.193  0.16  0.423  10.5 -0.813 -0.162 
    ## # ℹ 152 more rows
    ## # ℹ 14 more variables: IPRED <dbl>, IRES <dbl>, IWRES <dbl>, CPRED <dbl>,
    ## #   CRES <dbl>, CWRES <dbl>, eta.cl <dbl>, eta.v <dbl>, A1 <dbl>, cl <dbl>,
    ## #   v <dbl>, ke <dbl>, tad <dbl>, dosenum <dbl>

``` r
str(fit)
```

    ## nlmxr2FD [155 × 26] (S3: nlmixr2FitData/nlmixr2FitCore/nlmixr2.saem/tbl_df/tbl/data.frame)
    ##  $ ID     : Factor w/ 59 levels "1","2","3","4",..: 1 1 2 2 2 3 3 3 4 4 ...
    ##  $ TIME   : num [1:155] 2 112.5 2 63.5 135.5 ...
    ##  $ DV     : num [1:155] 17.3 31 9.7 24.6 33 18 23.8 24.3 20.8 23.9 ...
    ##  $ EPRED  : num [1:155] 18.8 29.9 10.9 20 27.6 ...
    ##  $ ERES   : num [1:155] -1.53 1.15 -1.18 4.61 5.35 ...
    ##  $ NPDE   : num [1:155] -0.422 0.394 -0.994 -0.358 0.553 ...
    ##  $ NPD    : num [1:155] 0.0167 0.3943 -0.1934 0.664 0.5534 ...
    ##  $ PDE    : num [1:155] 0.337 0.653 0.16 0.36 0.71 ...
    ##  $ PD     : num [1:155] 0.507 0.653 0.423 0.747 0.71 ...
    ##  $ PRED   : num [1:155] 17.5 27.9 10.5 19.2 26.9 ...
    ##  $ RES    : num [1:155] -0.222 3.11 -0.813 5.395 6.113 ...
    ##  $ WRES   : num [1:155] -0.0297 0.2545 -0.1618 0.6382 0.5135 ...
    ##  $ IPRED  : num [1:155] 18.5 29.6 12.5 23 32.4 ...
    ##  $ IRES   : num [1:155] -1.184 1.374 -2.771 1.619 0.615 ...
    ##  $ IWRES  : num [1:155] -0.418 0.485 -0.978 0.571 0.217 ...
    ##  $ CPRED  : num [1:155] 17.5 27.8 10.3 18.9 26.4 ...
    ##  $ CRES   : num [1:155] -0.196 3.161 -0.641 5.739 6.631 ...
    ##  $ CWRES  : num [1:155] -0.025 0.245 -0.113 0.578 0.468 ...
    ##  $ eta.cl : num [1:155] -0.0743 -0.0743 -0.2123 -0.2123 -0.2123 ...
    ##  $ eta.v  : num [1:155] -0.0533 -0.0533 -0.1704 -0.1704 -0.1704 ...
    ##  $ A1     : num [1:155] 24.8 39.7 14.9 27.4 38.6 ...
    ##  $ cl     : num [1:155] 0.00629 0.00629 0.00548 0.00548 0.00548 ...
    ##  $ v      : num [1:155] 1.34 1.34 1.19 1.19 1.19 ...
    ##  $ ke     : num [1:155] 0.00469 0.00469 0.0046 0.0046 0.0046 ...
    ##  $ tad    : num [1:155] 2 4 2 11.5 11.5 ...
    ##  $ dosenum: num [1:155] 1 10 1 6 12 1 7 12 1 5 ...
    ##  $ cor       :  correlation matrix of theta, calculated from covariance of theta
    ##  $ finalUi   :  The final ui used to run the model
    ##  $ foceiControl:  Get the focei control required for creating the nlmixr object
    ##  $ iniUi     :  The initial ui used to run the model
    ##  $ ipredModel:  rxode2 estimation model for fit
    ##  $ modelName :  name of the model used for nlmixr2 model fit
    ##  $ omegaR    :  correlation matrix of omega
    ##  $ parHistStacked:  stacked parameter history
    ##  $ phiR      :  correlation matrix of each individual's eta (if present)
    ##  $ phiRSE    :  relative standard error of each individual's eta (if present)
    ##  $ phiSE     :  standard error of each individual's eta (if present)
    ##  $ rxControl :  rxode2 solving options
    ##  $ ui        :  The final ui used to run the model
    ##  $ ui        :  rxode2 user interface
    ##  $ conditionNumberCor:  Condition Number (Correlation)
    ##  $ conditionNumberCov:  Condition Number (Covariance)
    ##  $ cov       :  Covariance of fixed effects
    ##  $ covMethod :  Covariance Method for fixed effects
    ##  $ etaObf    :  ETAs and their individual objective function contribution (if applicable)
    ##  $ objDf     :  Objective Function DF
    ##  $ omega     :  Omega Matrix
    ##  $ origData  :  Original Data
    ##  $ parFixed  :  Formatted Parameter Values for Fixed effects
    ##  $ parFixedDf:  Parameter Values for Fixed Effects (data frame)
    ##  $ parHist   :  Parameter History
    ##  $ scaleInfo :  Scaling Information
    ##  $ shrink    :  Shrinkage data frame
    ##  $ table     :  Table Control Value
    ##  $ fixef     :  Fixed effects
    ##  $ time      :  Timing data frame

# Output description

``` r
# Estimation of DV: PRED and IPRED

mydata <- pheno_sd[pheno_sd$EVID==0,]
plot(mydata$DV,fit$PRED)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-1.png)<!-- -->

``` r
plot(mydata$DV,fit$IPRED)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-2.png)<!-- -->

``` r
plot(mydata$DV,fit$EPRED)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-3.png)<!-- -->

``` r
# What is the difference between PRED and IPRED?
  
# eta.ka:
plot(mydata$ID,fit$eta.ka)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-4.png)<!-- -->

``` r
plot(mydata$ID,fit$eta.cl)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-5.png)<!-- -->

``` r
plot(mydata$ID,fit$eta.v)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-6.png)<!-- -->

``` r
plot(mydata$ID,fit$cp)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-7.png)<!-- -->

``` r
plot(mydata$DV,fit$cp)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-8.png)<!-- -->

``` r
plot(mydata$DV,fit$depot)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-9.png)<!-- -->

``` r
plot(mydata$DV,fit$center)   
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-10.png)<!-- -->

``` r
plot(mydata$DV,fit$ka)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-11.png)<!-- -->

``` r
plot(mydata$DV,fit$cl)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-12.png)<!-- -->

``` r
plot(mydata$DV,fit$v)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-13.png)<!-- -->

``` r
plot(mydata$DV,fit$tad)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-14.png)<!-- -->

``` r
plot(mydata$DV,fit$depot)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-15.png)<!-- -->

``` r
plot(mydata$DV,fit$dosenum)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/output-16.png)<!-- -->

Plots

``` r
library(xpose.nlmixr2)
```

    ## Loading required package: xpose

    ## 
    ## Attaching package: 'xpose'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
xpdb = xpose_data_nlmixr(fit)


library(xpose)
plt <- dv_vs_ipred(xpdb)

library(ggPMX)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## 
    ## Attaching package: 'ggPMX'

    ## The following object is masked from 'package:xpose':
    ## 
    ##     get_data

``` r
ctr = pmx_nlmixr(fit)
pmx_plot_dv_ipred(ctr)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_model-1.png)<!-- -->

Plots

``` r
plot(fit)
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-1.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-2.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-3.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-4.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-5.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-6.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-7.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-8.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-9.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-10.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-11.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-12.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-13.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-14.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-15.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-16.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-17.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-18.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-19.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-20.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-21.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-22.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-23.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-24.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-25.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-26.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-27.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-28.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-29.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-30.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-31.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-32.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-33.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-34.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-35.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-36.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-37.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-38.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-39.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-40.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-41.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-42.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-43.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-44.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-45.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-46.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-47.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-48.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-49.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-50.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-51.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-52.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-53.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-54.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-55.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-56.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-57.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-58.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-59.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-60.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-61.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-62.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-63.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-64.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-65.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-66.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-67.png)<!-- -->

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-68.png)<!-- -->

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-69.png)<!-- -->

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-70.png)<!-- -->

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/plot_fit-71.png)<!-- -->

``` r
plot(augPred(fit))
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
p1 <- vpcPlot(fit, show=list(obs_dv=TRUE));
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
p1 <- p1 + ylab("Concentrations")

## A prediction-corrected VPC
p2 <- vpcPlot(fit, pred_corr = TRUE, show=list(obs_dv=TRUE))
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
p2 <- p2 + ylab("Prediction-Corrected Concentrations")

library(patchwork)
p1 / p2
```

![](102-Example-Adding-Covariances-between-random-effects_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Modified Model

We change the fitting parametrization with typical values for clearance
and volume $tcl$ and $tv$ and also a new variable $WT_{CL}$

$$
\begin{align}
  CL =& e^{ tcl + \eta_{CL} } NWE^{WT_{CL}}  \\
  V  =& e^{ tv  + \eta_{V} } 
\end{align}
$$ Where $NWT$ is the weights normalized bye mean $WT$ so:

``` r
# Calculate median
a <- median(distinct(select(pheno_sd,ID,WT))$WT)
pheno_sd$NWE <- pheno_sd$WT / a
```

``` r
m2 <- function() {
  ini({
    tcl <- log(0.008) # typical value of clearance
    tv <-  log(0.6)   # typical value of volume
    wt.cl <- 0.75
    eta.cl + eta.v ~ c(1, 0.01, 1) ## var(eta.cl), cov(eta.cl, eta.v), var(eta.v)
                      # interindividual variability on clearance and volume
    add.err <- 0.1    # residual variability
  })
  model({
    cl <- exp(tcl + eta.cl)*NWE^wt.cl  
    v <- exp(tv + eta.v)     # individual value of volume
    ke <- cl / v             # elimination rate constant
    d/dt(A1) = - ke * A1     # model differential equation
    cp = A1 / v              # concentration in plasma
    cp ~ add(add.err)        # define error model
  })
}
```

The fit is performed by the function nlmixr/nlmixr2 specifying the
model, data and estimate

``` r
fit2 <- nlmixr(m2, pheno_sd, "saem", control=list(print=0), table=list(cwres=TRUE, npde=TRUE))
```

    ## ℹ parameter labels from comments will be replaced by 'label()'

    ## → loading into symengine environment...

    ## → pruning branches (`if`/`else`) of saem model...

    ## ✔ done

    ## → finding duplicate expressions in saem model...

    ## ✔ done

    ## Calculating covariance matrix

    ## → loading into symengine environment...

    ## → pruning branches (`if`/`else`) of saem model...

    ## ✔ done

    ## → finding duplicate expressions in saem predOnly model 0...

    ## → finding duplicate expressions in saem predOnly model 1...

    ## → finding duplicate expressions in saem predOnly model 2...

    ## → optimizing duplicate expressions in saem predOnly model 2...

    ## ✔ done

    ## → Calculating residuals/tables

    ## → loading into symengine environment...

    ## → pruning branches (`if`/`else`) of full model...

    ## ✔ done

    ## → calculate jacobian

    ## → calculate sensitivities

    ## → calculate ∂(f)/∂(η)

    ## → calculate ∂(R²)/∂(η)

    ## → finding duplicate expressions in inner model...

    ## → optimizing duplicate expressions in inner model...

    ## → finding duplicate expressions in EBE model...

    ## → optimizing duplicate expressions in EBE model...

    ## → compiling inner model...

    ## ✔ done

    ## → finding duplicate expressions in FD model...

    ## → optimizing duplicate expressions in FD model...

    ## → compiling EBE model...

    ## ✔ done

    ## → compiling events FD model...

    ## ✔ done
    ## ✔ done

    ## → compress origData in nlmixr2 object, save 38744

    ## → compress phiM in nlmixr2 object, save 380168

    ## → compress parHist in nlmixr2 object, save 9264

``` r
print(fit2)
```

    ## ── nlmixr² SAEM OBJF by FOCEi approximation ──
    ## 
    ##           OBJF      AIC      BIC Log-likelihood Condition#(Cov) Condition#(Cor)
    ## FOCEi 673.3354 972.2064 993.5103      -479.1032        7.562976        2.050596
    ## 
    ## ── Time (sec $time): ──
    ## 
    ##         setup optimize covariance  saem table compress
    ## elapsed 0.014        0       0.01 38.24  3.49     0.09
    ## 
    ## ── Population Parameters ($parFixed or $parFixedDf): ──
    ## 
    ##                          Parameter  Est.     SE  %RSE   Back-transformed(95%CI)
    ## tcl     typical value of clearance -5.12 0.0455 0.887 0.00595 (0.00544, 0.0065)
    ## tv         typical value of volume 0.346 0.0545  15.8         1.41 (1.27, 1.57)
    ## wt.cl                               1.22  0.115  9.38        1.22 (0.996, 1.44)
    ## add.err       residual variability  2.91                                   2.91
    ##         BSV(CV%) Shrink(SD)%
    ## tcl         14.9      46.8% 
    ## tv          41.4      2.96% 
    ## wt.cl                       
    ## add.err                     
    ##  
    ##   Covariance Type ($covMethod): linFim
    ##   Correlations in between subject variability (BSV) matrix:
    ##     cor:eta.v,eta.cl 
    ##           0.171  
    ##  
    ## 
    ##   Full BSV covariance ($omega) or correlation ($omegaR; diagonals=SDs) 
    ##   Distribution stats (mean/skewness/kurtosis/p-value) available in $shrink 
    ##   Censoring ($censInformation): No censoring
    ## 
    ## ── Fit Data (object is a modified tibble): ──
    ## # A tibble: 155 × 27
    ##   ID     TIME    DV EPRED  ERES   NPDE    NPD    PDE    PD  PRED    RES    WRES
    ##   <fct> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl>
    ## 1 1        2   17.3  19.2 -1.90 -0.830 -0.117 0.203  0.453  17.5 -0.225 -0.0300
    ## 2 1      112.  31    28.7  2.32  0.403  0.403 0.657  0.657  28.3  2.74   0.326 
    ## 3 2        2    9.7  11.3 -1.56 -1.61  -0.176 0.0533 0.43   10.5 -0.806 -0.159 
    ## # ℹ 152 more rows
    ## # ℹ 15 more variables: IPRED <dbl>, IRES <dbl>, IWRES <dbl>, CPRED <dbl>,
    ## #   CRES <dbl>, CWRES <dbl>, eta.cl <dbl>, eta.v <dbl>, A1 <dbl>, cl <dbl>,
    ## #   v <dbl>, ke <dbl>, tad <dbl>, dosenum <dbl>, NWE <dbl>

``` r
str(fit2)
```

    ## nlmxr2FD [155 × 27] (S3: nlmixr2FitData/nlmixr2FitCore/nlmixr2.saem/tbl_df/tbl/data.frame)
    ##  $ ID     : Factor w/ 59 levels "1","2","3","4",..: 1 1 2 2 2 3 3 3 4 4 ...
    ##  $ TIME   : num [1:155] 2 112.5 2 63.5 135.5 ...
    ##  $ DV     : num [1:155] 17.3 31 9.7 24.6 33 18 23.8 24.3 20.8 23.9 ...
    ##  $ EPRED  : num [1:155] 19.2 28.7 11.3 19.4 25.9 ...
    ##  $ ERES   : num [1:155] -1.9 2.32 -1.56 5.15 7.1 ...
    ##  $ NPDE   : num [1:155] -0.82977 0.40338 -1.61336 -0.00836 1.05084 ...
    ##  $ NPD    : num [1:155] -0.117 0.403 -0.176 0.83 1.051 ...
    ##  $ PDE    : num [1:155] 0.2033 0.6567 0.0533 0.4967 0.8533 ...
    ##  $ PD     : num [1:155] 0.453 0.657 0.43 0.797 0.853 ...
    ##  $ PRED   : num [1:155] 17.5 28.3 10.5 19 26.4 ...
    ##  $ RES    : num [1:155] -0.225 2.742 -0.806 5.591 6.579 ...
    ##  $ WRES   : num [1:155] -0.03 0.326 -0.159 0.842 0.889 ...
    ##  $ IPRED  : num [1:155] 18.4 29.4 13.2 23.1 31.5 ...
    ##  $ IRES   : num [1:155] -1.07 1.56 -3.48 1.46 1.54 ...
    ##  $ IWRES  : num [1:155] -0.368 0.536 -1.196 0.501 0.528 ...
    ##  $ CPRED  : num [1:155] 17.5 28.2 10.2 18.6 26.1 ...
    ##  $ CRES   : num [1:155] -0.205 2.765 -0.493 5.965 6.937 ...
    ##  $ CWRES  : num [1:155] -0.0262 0.319 -0.0828 0.785 0.85 ...
    ##  $ eta.cl : num [1:155] -0.0284 -0.0284 -0.1002 -0.1002 -0.1002 ...
    ##  $ eta.v  : num [1:155] -0.0473 -0.0473 -0.2282 -0.2282 -0.2282 ...
    ##  $ A1     : num [1:155] 24.8 39.7 14.8 26 35.4 ...
    ##  $ cl     : num [1:155] 0.00633 0.00633 0.00641 0.00641 0.00641 ...
    ##  $ v      : num [1:155] 1.35 1.35 1.13 1.13 1.13 ...
    ##  $ ke     : num [1:155] 0.00469 0.00469 0.0057 0.0057 0.0057 ...
    ##  $ tad    : num [1:155] 2 4 2 11.5 11.5 ...
    ##  $ dosenum: num [1:155] 1 10 1 6 12 1 7 12 1 5 ...
    ##  $ NWE    : num [1:155] 1.08 1.08 1.15 1.15 1.15 ...
    ##  $ cor       :  correlation matrix of theta, calculated from covariance of theta
    ##  $ finalUi   :  The final ui used to run the model
    ##  $ foceiControl:  Get the focei control required for creating the nlmixr object
    ##  $ iniUi     :  The initial ui used to run the model
    ##  $ ipredModel:  rxode2 estimation model for fit
    ##  $ modelName :  name of the model used for nlmixr2 model fit
    ##  $ omegaR    :  correlation matrix of omega
    ##  $ parHistStacked:  stacked parameter history
    ##  $ phiR      :  correlation matrix of each individual's eta (if present)
    ##  $ phiRSE    :  relative standard error of each individual's eta (if present)
    ##  $ phiSE     :  standard error of each individual's eta (if present)
    ##  $ rxControl :  rxode2 solving options
    ##  $ ui        :  The final ui used to run the model
    ##  $ ui        :  rxode2 user interface
    ##  $ conditionNumberCor:  Condition Number (Correlation)
    ##  $ conditionNumberCov:  Condition Number (Covariance)
    ##  $ cov       :  Covariance of fixed effects
    ##  $ covMethod :  Covariance Method for fixed effects
    ##  $ etaObf    :  ETAs and their individual objective function contribution (if applicable)
    ##  $ objDf     :  Objective Function DF
    ##  $ omega     :  Omega Matrix
    ##  $ origData  :  Original Data
    ##  $ parFixed  :  Formatted Parameter Values for Fixed effects
    ##  $ parFixedDf:  Parameter Values for Fixed Effects (data frame)
    ##  $ parHist   :  Parameter History
    ##  $ scaleInfo :  Scaling Information
    ##  $ shrink    :  Shrinkage data frame
    ##  $ table     :  Table Control Value
    ##  $ fixef     :  Fixed effects
    ##  $ time      :  Timing data frame
