
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WaxTools

## Overview

`waxtools` is a package that brings together a set of useful and
practical functions to facilitate work in the field, mainly, of ecology.
It offers functions to customize `ggplot2` graphs, check and correct
`sf` object geometry, create beautiful commented titles to separate
scripts, and perform other specific tasks such as cleaning up data sets,
calculating cumulative specific richness, etc. With its variety of
functions, `waxtools` can be used effectively and conveniently in
various contexts and projects.

## Installation

``` r
devtools::install_github("waxtiz/waxtools")
```

``` r
library(waxtools)
```

## Usage

### waxtheme

``` r
library(ggplot2)
library(waxtools)


ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
  geom_point() +
  labs(title = "First, a chart title",
       subtitle = "Below, a subtitle",
       caption = "And finally, a caption text.") +
  theme_wax(theme = "wax") +
  scale_color_wax_d()
```

![](C:/Users/demo1/Desktop/FDC25/projets/personal_tests/waxtools/README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

wax_theme uses by default the R fonts. However, it is possible to enter
the “wax” value in the theme parameter of the wax_theme function to use
other fonts. In this case, the fonts can be loaded like this:

``` r
sysfonts::font_add_google(name = "Fira Sans", family = "fira-sans")
sysfonts::font_add_google(name = "Roboto Slab", family = "roboto-slab")

showtext::showtext_auto()
```
