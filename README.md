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
rlang
data.table
readxl
readr
magrittr
ggplot2
ggthemes
ggrepel
grid
gridExtra
GGally
ggthemes
lubridate
anytime
```

To run locally, install each of the above packages and run:
```r
shiny::runGitHub('Shiny-GEM', 'dm3ll3n')`
```