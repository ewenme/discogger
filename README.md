
# discogger <img src="man/figures/logo.png" width="160px" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/ewenme/discogger.svg?branch=master)](https://travis-ci.org/ewenme/discogger)

## Overview

Tools for working with the [Discogs](https://www.discogs.com) API in R.

Discogs Developers Site: <https://www.discogs.com/developers>

## Install

Development version

``` r
devtools::install_github('ewenme/discogger')
```

## Authenticate

1.  [Create a Discogs API v2
    application](https://www.discogs.com/settings/developers)
2.  Generate a personal access token from your [API application
    page](https://www.discogs.com/settings/developers)
3.  Set your credentials in the System Environment variable
    `DISCOGS_API_TOKEN` by calling the `discogs_api_token()` function
    and entering your Discogs application personal access token when
    prompted.

## Use

``` r
library(discogger)
library(tidyverse)
```

### Who released the most Dance Mania records?

``` r
# get dance mania records listed on discogs
dm_recs <- discogs_label_releases(label_id = 314)

dm_recs$content %>%
  group_by(artist) %>%
  summarise(n_releases = n_distinct(catno)) %>%
  arrange(desc(n_releases)) %>%
  top_n(10)
```

    ## # A tibble: 10 x 2
    ##    artist          n_releases
    ##    <chr>                <int>
    ##  1 Various                 21
    ##  2 DJ Milton               15
    ##  3 D.J. Funk*              13
    ##  4 DJ Deeon                13
    ##  5 Paul Johnson            11
    ##  6 Robert Armani           11
    ##  7 Jammin Gerald           10
    ##  8 Parris Mitchell          9
    ##  9 D.J. Slugo*              7
    ## 10 DJ Funk                  7

## Contribute

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
