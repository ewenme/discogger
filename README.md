discogger
================

Tools for working with the [Discogs](https://www.discogs.com) API in R.

-   Discogs Developers Site: <https://www.discogs.com/developers>

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
