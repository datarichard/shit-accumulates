shit-accumulates
================

Life happens. Sometimes shit accumulates.

Import the working data:

``` r
df <- read_rds("data/quarterly event mhi5.rds")
```

Our working dataframe contains `xwaveid`, `year` of event, `event` code,
number of quarters since the event (`qtr`), MHI-5 score in the same year
the event was reported (`mhi5`), MHI-5 score in the previous (baseline)
year (`basemhi5`), the difference between the MHI-5 score and baseline
(`mhi5.d`), the cumulative tally of each event type (`cumevent`), and
the total number of each event type (`nevent`).

    ## # A tibble: 367,306 × 9
    ##    xwaveid  year event qtr    mhi5 basemhi5 mhi5.d cumevent nevent
    ##    <chr>   <dbl> <chr> <fct> <dbl>    <dbl>  <dbl> <fct>    <fct> 
    ##  1 0100002  2002 leins 4        76       64     12 1        1     
    ##  2 0100003  2002 leinf 4        84       92     -8 1        1     
    ##  3 0100003  2003 ledfr 1        76       84     -8 1        3+    
    ##  4 0100003  2003 ledrl 4        76       84     -8 1        1     
    ##  5 0100003  2005 lesep 4        76       80     -4 1        2     
    ##  6 0100003  2006 lemvd 2        80       76      4 1        3+    
    ##  7 0100003  2006 lefrd 3        80       76      4 1        1     
    ##  8 0100003  2006 ledfr 4        80       76      4 2        3+    
    ##  9 0100003  2006 lesep 4        80       76      4 2        2     
    ## 10 0100003  2007 ledfr 1        68       80    -12 3+       3+    
    ## # ℹ 367,296 more rows

The `mhi5.d` will be our outcome variable of interest (y).

And we want to ask: Is the amount of change in y (`mhi5.d`) larger for
cumulative events? So `cumevent` will be our primary explanatory
variable (x). Note that `cumevent` only counts up to three cumulative
events, and additional events are coded as “3+”.

Let’s select one event with a known substantial effect on mental health
(MHI-5) such as `lefnw` (major financial worsening or loss, e.g.,
bankruptcy)

``` r
lefnw <- df |> 
  filter(event == "lefnw")
```

The simplest answer to our starting question is:

``` r
lm(mhi5.d ~ cumevent, data = lefnw) |> 
  summary()
```

    ## 
    ## Call:
    ## lm(formula = mhi5.d ~ cumevent, data = lefnw)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -96.904 -11.268   0.732  11.092  83.096 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -4.7322     0.2900 -16.318  < 2e-16 ***
    ## cumevent2     1.6366     0.5409   3.026  0.00249 ** 
    ## cumevent3+    1.6407     0.5119   3.205  0.00135 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.95 on 8006 degrees of freedom
    ##   (823 observations deleted due to missingness)
    ## Multiple R-squared:  0.001859,   Adjusted R-squared:  0.00161 
    ## F-statistic: 7.456 on 2 and 8006 DF,  p-value: 0.0005818

The coefficient summary table shows the intercept is negative (-4.7322)
and significantly less than zero (*p* \< 2e-16); and because the
intercept represents the impact of the first event (bankruptcy) this
tells us mental health (MHI-5 score) declines between the baseline year
and year of the event.

The coefficients for cumevent2 and cumevent3+ tell us the additional
impact of further events over and above the first event (i.e., relative
difference to the intercept). Both coefficients are positive and
significant, indicating additional bankruptcies have *less impact than
the first bankruptcy*, as mental health is relatively improved (i.e.,
positive).

------------------------------------------------------------------------

A major concern with this kind of research question is the role of
person-level endogeniety. Are people who respond to 2 or 3+ events
somehow weird or different? (e.g., maybe they are more reactive to
events and so their average change is larger than other peoples average
change). A powerful solution to this concern is to estimate a fixed
effects model. This model includes an intercept for every person
($\alpha_i$), allowing the estimated change in `mhi5.d` score to be
expressed relative to the each person’s intercept.

We have to use `broom::tidy()` to read the coefficient table because
even in this small example there are over 4000 parameters to display
(4,462 person-level intercepts!).

``` r
lm(mhi5.d ~ cumevent + xwaveid, data = lefnw) |> 
  broom::tidy()
```

    ## # A tibble: 4,465 × 5
    ##    term            estimate std.error statistic   p.value
    ##    <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)    -4.00e+ 0    20.0   -2.00e- 1 0.841    
    ##  2 cumevent2       2.94e+ 0     0.717  4.10e+ 0 0.0000414
    ##  3 cumevent3+      2.57e+ 0     0.815  3.15e+ 0 0.00165  
    ##  4 xwaveid0100015  8.00e+ 0    28.3    2.83e- 1 0.777    
    ##  5 xwaveid0100043  1.60e+ 1    28.3    5.66e- 1 0.572    
    ##  6 xwaveid0100045 -1.20e+ 1    28.3   -4.24e- 1 0.671    
    ##  7 xwaveid0100069 -1.11e-11    28.3   -3.92e-13 1.00     
    ##  8 xwaveid0100070  2.00e+ 1    28.3    7.07e- 1 0.479    
    ##  9 xwaveid0100084  4.00e+ 0    28.3    1.41e- 1 0.888    
    ## 10 xwaveid0100097  4.98e+ 0    22.4    2.23e- 1 0.824    
    ## # ℹ 4,455 more rows

The results of the fixed effects model tells us that once the stable
individual differences are accounted for, there is no significant effect
of the first event on mental health as the variation around the
estimated effect is extremely large (std.error = 20.0). This model loses
a lot of power due to the large number of parameters.

However it does allow us to estimate the cumulative effect of additional
events (`cumevent2`, `cumevent3+`) relative to the first event with
sufficient precision (power). This confirms the cumulative effect is
relatively positive and largely consistent with our initial model.

The fixed effects model is a cumbersome model which introduces
complexity (parameters) for the sake of avoiding endogeniety - but there
may be better ways to deal with the endogeniety…

------------------------------------------------------------------------

An alternative way to deal with the assumed endogeneity is to estimate a
random intercepts model. This model doesn’t estimate an intercept for
every person, but instead estimates the variation among the intercepts,
which is only a single extra parameter instead of 4,462 extra
parameters!

``` r
library(lme4)
library(lmerTest)

lmer(mhi5.d ~ cumevent + (1|xwaveid), data = lefnw) |> 
  summary()
```

    ## boundary (singular) fit: see help('isSingular')

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: mhi5.d ~ cumevent + (1 | xwaveid)
    ##    Data: lefnw
    ## 
    ## REML criterion at convergence: 69845.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1143 -0.5947  0.0386  0.5854  4.3856 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  xwaveid  (Intercept)   0       0.00   
    ##  Residual             359      18.95   
    ## Number of obs: 8009, groups:  xwaveid, 4463
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)   -4.7322     0.2900 8006.0000 -16.318  < 2e-16 ***
    ## cumevent2      1.6366     0.5409 8006.0000   3.026  0.00249 ** 
    ## cumevent3+     1.6407     0.5119 8006.0000   3.205  0.00135 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr) cmvnt2
    ## cumevent2  -0.536       
    ## cumevent3+ -0.567  0.304
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

Unfortunately in this example, the model fails to successfully estimate
the variation (“**warning: boundary (singular) fit: see
help(‘isSingular’)**”). We can see this because the Variance parameter
for the xwaveids in the Random effects summary table is 0. Due to this
problem, this model gives us the same results as our simplest model
estimated above.

Another alternative is to identify the endogeneity (or at least it’s
most likely major source). In this case we have the total number of
events people experience. It seems likely that people that experience 3
or more bankruptcies are weird or different from people who only
experience one or two. We can examine the impact of this individual
difference by including it as an explanatory variable in our model
(`nevent`):

``` r
lm(mhi5.d ~ cumevent + nevent, data = lefnw) |> 
  summary()
```

    ## 
    ## Call:
    ## lm(formula = mhi5.d ~ cumevent + nevent, data = lefnw)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -96.394  -9.652   1.279  11.092  83.606 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -4.1152     0.3620 -11.369  < 2e-16 ***
    ## cumevent2     2.7421     0.6651   4.123 3.78e-05 ***
    ## cumevent3+    2.1875     0.7287   3.002  0.00269 ** 
    ## nevent2      -2.2328     0.6834  -3.267  0.00109 ** 
    ## nevent3+     -1.1638     0.6959  -1.672  0.09452 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.94 on 8004 degrees of freedom
    ##   (823 observations deleted due to missingness)
    ## Multiple R-squared:  0.00319,    Adjusted R-squared:  0.002691 
    ## F-statistic: 6.403 on 4 and 8004 DF,  p-value: 3.856e-05

Adding `nevent` to the model explained twice as much variance
(r.squared) and restored the coefficients for cumevents to similar
values as our fixed effects model above. And it did all this without
adding \> 4000 parameters!

One could argue that other sources of endogeneity may still be a
problem, but we can perform a model comparison between this model and
the fixed effects model to determine whether the extra 4000 parameters
are worth it (spoiler: they aren’t! At least not for this event). And we
could add other parameters to address potential sources of endogeneity
such as the baselevel of mental health for each person, or add
parameters to explain other sources of variation such as the
peristimulus interval. But these concerns seem to be relatively
tangential to our research question we began with.

------------------------------------------------------------------------

One more thing…

It is now easy to run the model above for every event and collect the
coeficients in a big table. The results show the cumulative effect after
the first event is positive - most people habituate somewhat to the
negative impact:

``` r
df |> 
  filter(event %in% 
    c("lefnw", "ledsc", "lefrd", "leins", "leinf", "levio", "lepcm")) |> 
  group_by(event) |> 
  nest() |> 
  mutate(fit = map(data, ~lm(
    formula = mhi5.d ~ cumevent + nevent, 
    data = .x))) |> 
  transmute(
    coefs = map(fit, ~broom::tidy(.x))
  ) |> 
  unnest(coefs) |> 
  filter(term %in% c("cumevent2", "cumevent3+")) |> 
  print(n = 14)
```

    ## # A tibble: 14 × 6
    ## # Groups:   event [7]
    ##    event term       estimate std.error statistic  p.value
    ##    <chr> <chr>         <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 leins cumevent2    2.28       0.317     7.20  6.40e-13
    ##  2 leins cumevent3+   2.68       0.323     8.31  9.97e-17
    ##  3 leinf cumevent2    0.121      0.222     0.545 5.86e- 1
    ##  4 leinf cumevent3+   0.332      0.213     1.56  1.19e- 1
    ##  5 lefrd cumevent2    0.0853     0.556     0.154 8.78e- 1
    ##  6 lefrd cumevent3+   0.182      0.715     0.255 7.99e- 1
    ##  7 lefnw cumevent2    2.74       0.665     4.12  3.78e- 5
    ##  8 lefnw cumevent3+   2.19       0.729     3.00  2.69e- 3
    ##  9 lepcm cumevent2    0.795      0.467     1.70  8.85e- 2
    ## 10 lepcm cumevent3+   0.373      0.591     0.632 5.28e- 1
    ## 11 ledsc cumevent2    5.78       1.22      4.74  2.27e- 6
    ## 12 ledsc cumevent3+   7.75       1.92      4.04  5.54e- 5
    ## 13 levio cumevent2    1.25       1.03      1.22  2.24e- 1
    ## 14 levio cumevent3+   1.89       1.19      1.59  1.11e- 1

------------------------------------------------------------------------

#### Controlling for other events

``` r
df7 <- df |> 
  filter(event %in% 
    c("lefnw", "ledsc", "lefrd", "leins", "leinf", "levio", "lepcm")) |> 
  mutate(event = factor(event))

lm(
  formula = mhi5.d ~ cumevent * event + nevent,
  data = df7) |> 
  summary()

# this works but need to get the marginal effects of cumulative events
```

``` r
library(lme4)

lmer(
  formula = mhi5.d ~ cumevent * event + (1|xwaveid),
  data = df7) |> 
  summary()

# this also works but need to get the marginal effects of cumulative events
```
