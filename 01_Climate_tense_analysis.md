Climate tense analysis
================
Naia Morueta-Holme
November 28 2016

### Prepare data

Clean workspace and load libraries

Read in the raw data

``` r
# NOTE: DOWNLOAD COPY FROM GOOGLE DRIVE AND CHANGE FILE NAME HERE ACCORDINGLY
rawData = read.csv('Data/Data_CC_tense_01Oct2015_v2.csv', as.is=T) 
```

Clean the data

``` r
#Note: several of these clean-ups should be irrelevant once we have the full dataset
# Filter papers that have actually been checked
filledData = rawData[which(rawData$OK_for_analysis != ''),]
table(filledData$OK_for_analysis)
```

    #> 
    #>           maybe          Maybe?              no weather effects 
    #>               4               1             217               1 
    #>             yes 
    #>             197

``` r
# Select papers that are OK for analysis, and the relevant columns
df = filledData[which(filledData$OK_for_analysis == 'yes'),
                    c('ID_old','Publication_year','Journal','paper_focus',
                      'tense','confidence_in_tense','climate_date_range',
                      'climate_date_min','climate_date_max',
                      'climate_data_source','citation',
                      'geographic_extent_of_study','Data_collector_name')]

# Filter papers where climate_data_range is filled correctly
table(df$climate_date_range)
```

    #> 
    #>                         FALSE           TRUE TRUE and FALSE 
    #>              1             94             92              3

``` r
df = subset(df, climate_date_range == T | df$climate_date_range == F)

# Filter papers with full geographic extent information (1 missing)
df = subset(df, geographic_extent_of_study != '')

# Filter papers where focus of study is filled correctly
table(df$paper_focus)
```

    #> 
    #>        ***not temporal climate change effects           not temporal 
    #>                      2                     44                     96 
    #>        weather effects 
    #>                     42

``` r
df = subset(df, paper_focus != "***not temporal")
```

Table with impact factor for each journal (from www.citefactor.org)

``` r
IF_lookup = data.frame(Journal = sort(unique(df$Journal)),IF2014=c(4.454, 5.469, 5.000, 13.042, 4.248, 5.694, 42.351, 9.809))
```

<!-- #-----------------# -->
<!-- # THE FUN BEGINS! # -->
<!-- #-----------------# -->
Heads up!
---------

Need to check a few tense assignments...

    #> 
    #> FALSE  TRUE 
    #>    24   158

![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

Sampling per collector is not even ![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

And journal representation is also uneven ![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-7-1.png)

How about representation across time? ![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-8-1.png)

But looks like there are more papers to be found! ![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-9-1.png)

Preliminary plots
-----------------

![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-12-1.png)![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-12-2.png)![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-12-3.png)![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-12-4.png)

Plots lumping 'unclear' and 'inconsistent' ![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-13-1.png)![](01_Climate_tense_analysis_files/figure-markdown_github/unnamed-chunk-13-2.png)
