
# About

The goal of the current branch `feat` is to add a few new features:

- Export the `rscorecard::dict` file.

Install the `feat` version with

```r
devtools::install_github('jjchern/rscorecard', ref = "feat")
```

Below is the original readme from [`btskinner`](https://github.com/btskinner).

# rscorecard

[![Build Status](https://travis-ci.org/btskinner/rscorecard.svg?branch=master)](https://travis-ci.org/btskinner/rscorecard)
[![GitHub
release](https://img.shields.io/github/release/btskinner/rscorecard.svg)](https://github.com/btskinner/rscorecard)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rscorecard)](http://cran.r-project.org/package=rscorecard)

This package is an R wrapper for the [U.S. Department of Education College Scorecard](https://collegescorecard.ed.gov) API. It allows users to select and filter Scorecard variables with piped commands a la [`dplyr`](http://github.com/hadley/dplyr).

### Install

Install the latest released version from CRAN with

```r
install.packages('rscorecard')
```

Install the latest development version from Github with

```r
devtools::install_github('btskinner/rscorecard')
```

This package relies on the Scorecard data dictionary, so I will attempt to update it in a timely fashion whenever new Scorecard data are released. Because it sometimes takes a few days to get a package on CRAN, you may want to download the developmental version in the days immediately following a data update.

### Dependencies

This package relies on the following packages, available in CRAN:

* dplyr
* jsonlite
* lazyeval
* magrittr

## Example call

```r
library(rscorecard)

df <- sc_init() %>% 
    sc_filter(region == 2, ccbasic == c(21,22,23), locale == 41:43) %>% 
    sc_select(unitid, instnm, stabbr) %>% 
    sc_year(2013) %>% 
    sc_get()
df
```

For more example calls, see the [extended vignette](http://btskinner.me/rscorecard). 

## Data dictionary

To look up information about data elements, use the `sc_dict()` function. 

```r
## search variable descriptions for those containing 'tuition'
sc_dict('tuition')

## search for variable names for those starting with 'st'
sc_dict('^st', search_col = 'varname')

## print entire dictionary (not recommended)
sc_dict('.', limit = Inf)
```

## API key

Get your Data.gov API key at [https://api.data.gov/signup/](https://api.data.gov/signup/).
Save your key in your R environment at the start of your R session using `sc_key()`:

```r
## use your real key in place of the Xs
sc_key('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
```

## Further references

* [College Scorecard Website](https://collegescorecard.ed.gov)
* [Data documentation](https://collegescorecard.ed.gov/assets/FullDataDocumentation.pdf) 
* [Data dictionary [XLS]](https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary.xlsx)
* Reports
	* [Policy paper](https://collegescorecard.ed.gov/assets/BetterInformationForBetterCollegeChoiceAndInstitutionalPerformance.pdf)
	* [Technical paper](https://collegescorecard.ed.gov/assets/UsingFederalDataToMeasureAndImprovePerformance.pdf)
