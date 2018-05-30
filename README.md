discogger
================

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Tools for working with the [Discogs](https://www.discogs.com) API in R.

Discogs Developers Site: <https://www.discogs.com/developers>

Install
-------

Development version

``` r
devtools::install_github('ewenme/discogger')
```

Authenticate
------------

1.  [Create a Discogs API v2 application](https://www.discogs.com/settings/developers)
2.  Generate a personal access token from your [API application page](https://www.discogs.com/settings/developers)
3.  Set your credentials in the System Environment variable `DISCOGS_API_TOKEN` by calling the `discogs_api_token()` function and entering your Discogs application personal access token when prompted.

Use
---

Contribute
----------

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
