# Shiny-GEM

## https://www.donaldmellenbruch.com/post/introducing-shiny-gem/

### https://dmellenbruch.shinyapps.io/Shiny_GEM/

[Shiny GEM](https://dmellenbruch.shinyapps.io/Shiny_GEM/) is a web app built using [Rstudio's Shiny framework](https://shiny.rstudio.com/). 'GEM' stands for 'general exploratory methods', as this app aims to simplify a variety of basic EDA tasks.

Shiny GEM makes use of the following R packages:

```
shiny
shinydashboard
shinycssloaders
shinyCustom
DT
rlang
data.table
readxl
readr
magrittr
scales
ggplot2
ggthemes
ggrepel
grid
gridExtra
GGally
lubridate
anytime
```

To run locally, install each of the above packages and run:
```r
shiny::runGitHub('Shiny-GEM', 'dm3ll3n')`
```

Note that `shinyCustom` is not on CRAN; install it with:
```
devtools::install_github("homerhanumat/shinyCustom")
```