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
    ##  3 0100003  2003 ledfr 1        76       84     -8 1        3     
    ##  4 0100003  2003 ledrl 4        76       84     -8 1        1     
    ##  5 0100003  2005 lesep 4        76       80     -4 1        2     
    ##  6 0100003  2006 lemvd 2        80       76      4 1        3     
    ##  7 0100003  2006 lefrd 3        80       76      4 1        1     
    ##  8 0100003  2006 ledfr 4        80       76      4 2        3     
    ##  9 0100003  2006 lesep 4        80       76      4 2        2     
    ## 10 0100003  2007 ledfr 1        68       80    -12 3        3     
    ## # ℹ 367,296 more rows

The `mhi5.d` will be our outcome variable (y).

And we want to ask is the amount of change (`mhi5.d`) larger for
cumulative events? So `cumevent` will be our primary explanatory
variable (x).
