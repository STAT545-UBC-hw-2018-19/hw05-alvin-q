Homework 05
================

Homework 05: Factor and figure management
=========================================

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts --------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(gapminder)
library(knitr)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

Part 1: Factor management
-------------------------

### Elaboration for the gapminder data set

Drop Oceania. Filter the Gapminder data to remove observations associated with the continent of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors.

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

We can see 132 levels in country and 5 levels in continent. There are 1704 rows in total.

``` r
gap_filter <- gapminder%>%
  filter(continent != "Oceania")

str(gap_filter)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
levels(gap_filter$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

We can still see 142 levels in country and 5 levels in continent. So empty unused levels like "oceania" persist. However, there are 1680 rows in total.

``` r
gap_filter %>%
  droplevels() %>%
  str()
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
gap_filter$continent %>%
  droplevels() %>%
  levels()
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

After `droplevels`, we can see that the number of rows is still 1680. However, the unused factors are removed. There are only 140 levels in country and 4 levels in continent. Indeed "Oceania", which is unused, is removed.

Reorder the levels of country or continent. Use the forcats package to change the order of the factor levels, based on a principled summary of one of the quantitative variables. Consider experimenting with a summary statistic beyond the most basic choice of the median.

I'll look at Europe in 1952.

``` r
gap_europe1952 <- gapminder %>%
  filter(year == 1952, continent == "Europe")

#Head the filtered file
gap_europe1952 %>%
  head(15) %>%
  kable()
```

| country                | continent |  year|  lifeExp|       pop|  gdpPercap|
|:-----------------------|:----------|-----:|--------:|---------:|----------:|
| Albania                | Europe    |  1952|    55.23|   1282697|  1601.0561|
| Austria                | Europe    |  1952|    66.80|   6927772|  6137.0765|
| Belgium                | Europe    |  1952|    68.00|   8730405|  8343.1051|
| Bosnia and Herzegovina | Europe    |  1952|    53.82|   2791000|   973.5332|
| Bulgaria               | Europe    |  1952|    59.60|   7274900|  2444.2866|
| Croatia                | Europe    |  1952|    61.21|   3882229|  3119.2365|
| Czech Republic         | Europe    |  1952|    66.87|   9125183|  6876.1403|
| Denmark                | Europe    |  1952|    70.78|   4334000|  9692.3852|
| Finland                | Europe    |  1952|    66.55|   4090500|  6424.5191|
| France                 | Europe    |  1952|    67.41|  42459667|  7029.8093|
| Germany                | Europe    |  1952|    67.50|  69145952|  7144.1144|
| Greece                 | Europe    |  1952|    65.86|   7733250|  3530.6901|
| Hungary                | Europe    |  1952|    64.03|   9504000|  5263.6738|
| Iceland                | Europe    |  1952|    72.49|    147962|  7267.6884|
| Ireland                | Europe    |  1952|    66.91|   2952156|  5210.2803|

Note the alphabetical order.

``` r
gap_europe1952 %>%
  ggplot(aes(country, gdpPercap, fill = country)) +
  geom_bar(stat = "identity", show.legend = FALSE) + #bar plot
  coord_flip() + #flip X and y
  theme_bw() #bw theme
```

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-6-1.png)

We can see that the countries are ordered by alphabetical order still.

Let's try `arrange`.

``` r
gap_europe1952 %>%
  arrange(gdpPercap) %>%
  head(15) %>%
  kable()
```

| country                | continent |  year|  lifeExp|       pop|  gdpPercap|
|:-----------------------|:----------|-----:|--------:|---------:|----------:|
| Bosnia and Herzegovina | Europe    |  1952|   53.820|   2791000|   973.5332|
| Albania                | Europe    |  1952|   55.230|   1282697|  1601.0561|
| Turkey                 | Europe    |  1952|   43.585|  22235677|  1969.1010|
| Bulgaria               | Europe    |  1952|   59.600|   7274900|  2444.2866|
| Montenegro             | Europe    |  1952|   59.164|    413834|  2647.5856|
| Portugal               | Europe    |  1952|   59.820|   8526050|  3068.3199|
| Croatia                | Europe    |  1952|   61.210|   3882229|  3119.2365|
| Romania                | Europe    |  1952|   61.050|  16630000|  3144.6132|
| Greece                 | Europe    |  1952|   65.860|   7733250|  3530.6901|
| Serbia                 | Europe    |  1952|   57.996|   6860147|  3581.4594|
| Spain                  | Europe    |  1952|   64.940|  28549870|  3834.0347|
| Poland                 | Europe    |  1952|   61.310|  25730551|  4029.3297|
| Slovenia               | Europe    |  1952|   65.570|   1489518|  4215.0417|
| Italy                  | Europe    |  1952|   65.940|  47666000|  4931.4042|
| Slovak Republic        | Europe    |  1952|   64.360|   3558137|  5074.6591|

We can see that the table is arranged by gdpPerCap now.

``` r
gap_europe1952 %>%
  arrange(gdpPercap) %>%
  ggplot(aes(country, gdpPercap, fill = country)) +
  geom_bar(stat = "identity", show.legend = FALSE) + #bar plot
  coord_flip() + #flip X and y
  theme_bw() #bw theme
```

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-8-1.png)

But arrange has no effect on plotting order. This is still alphabetical on ggplot.

Let's try `fct_reorder`.

``` r
gap_europe1952 %>%
  mutate(country = fct_reorder(country, gdpPercap)) %>%
  head(15) %>%
  kable()
```

| country                | continent |  year|  lifeExp|       pop|  gdpPercap|
|:-----------------------|:----------|-----:|--------:|---------:|----------:|
| Albania                | Europe    |  1952|    55.23|   1282697|  1601.0561|
| Austria                | Europe    |  1952|    66.80|   6927772|  6137.0765|
| Belgium                | Europe    |  1952|    68.00|   8730405|  8343.1051|
| Bosnia and Herzegovina | Europe    |  1952|    53.82|   2791000|   973.5332|
| Bulgaria               | Europe    |  1952|    59.60|   7274900|  2444.2866|
| Croatia                | Europe    |  1952|    61.21|   3882229|  3119.2365|
| Czech Republic         | Europe    |  1952|    66.87|   9125183|  6876.1403|
| Denmark                | Europe    |  1952|    70.78|   4334000|  9692.3852|
| Finland                | Europe    |  1952|    66.55|   4090500|  6424.5191|
| France                 | Europe    |  1952|    67.41|  42459667|  7029.8093|
| Germany                | Europe    |  1952|    67.50|  69145952|  7144.1144|
| Greece                 | Europe    |  1952|    65.86|   7733250|  3530.6901|
| Hungary                | Europe    |  1952|    64.03|   9504000|  5263.6738|
| Iceland                | Europe    |  1952|    72.49|    147962|  7267.6884|
| Ireland                | Europe    |  1952|    66.91|   2952156|  5210.2803|

We can see that the table is not reordered with `fct_reorder`. It is still alphabetical order.

``` r
gap_europe1952 %>%
  mutate(country = fct_reorder(country, gdpPercap)) %>%
  ggplot(aes(country, gdpPercap, fill = country)) +
  geom_bar(stat = "identity", show.legend = FALSE) + #bar plot
  coord_flip() + #flip X and y
  theme_bw() #bw theme
```

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-10-1.png)

However, the plot is reordered by gdpPerCap.

So we need both `arrange` for the table and `fct_reorder` for the plot.

``` r
gap_europe1952 %>%
  arrange(gdpPercap) %>%
  mutate(country = fct_reorder(country, gdpPercap)) %>%
  head(15) %>%
  kable()
```

| country                | continent |  year|  lifeExp|       pop|  gdpPercap|
|:-----------------------|:----------|-----:|--------:|---------:|----------:|
| Bosnia and Herzegovina | Europe    |  1952|   53.820|   2791000|   973.5332|
| Albania                | Europe    |  1952|   55.230|   1282697|  1601.0561|
| Turkey                 | Europe    |  1952|   43.585|  22235677|  1969.1010|
| Bulgaria               | Europe    |  1952|   59.600|   7274900|  2444.2866|
| Montenegro             | Europe    |  1952|   59.164|    413834|  2647.5856|
| Portugal               | Europe    |  1952|   59.820|   8526050|  3068.3199|
| Croatia                | Europe    |  1952|   61.210|   3882229|  3119.2365|
| Romania                | Europe    |  1952|   61.050|  16630000|  3144.6132|
| Greece                 | Europe    |  1952|   65.860|   7733250|  3530.6901|
| Serbia                 | Europe    |  1952|   57.996|   6860147|  3581.4594|
| Spain                  | Europe    |  1952|   64.940|  28549870|  3834.0347|
| Poland                 | Europe    |  1952|   61.310|  25730551|  4029.3297|
| Slovenia               | Europe    |  1952|   65.570|   1489518|  4215.0417|
| Italy                  | Europe    |  1952|   65.940|  47666000|  4931.4042|
| Slovak Republic        | Europe    |  1952|   64.360|   3558137|  5074.6591|

Table is ordered by gdpPerCap...

``` r
gap_europe1952 %>%
  arrange(gdpPercap) %>%
  mutate(country = fct_reorder(country, gdpPercap)) %>%
  ggplot(aes(country, gdpPercap, fill = country)) +
  geom_bar(stat = "identity", show.legend = FALSE) + #bar plot
  coord_flip() + #flip X and y
  theme_bw() #bw theme
```

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-12-1.png)

...And the plot is reordered.

Part 2: File I/O
----------------

Experiment with one or more of write\_csv()/read\_csv() (and/or TSV friends), saveRDS()/readRDS(), dput()/dget(). Create something new, probably by filtering or grouped-summarization of Singer or Gapminder. I highly recommend you fiddle with the factor levels, i.e. make them non-alphabetical (see previous section). Explore whether this survives the round trip of writing to file then reading back in.

Explore the data first.

``` r
#Filter the data
gap_americas1997 <- gapminder %>%
  filter(year == 1997, continent == "Americas")

# Head data
gap_americas1997 %>%
  head(15) %>%
  kable()
```

| country            | continent |  year|  lifeExp|        pop|  gdpPercap|
|:-------------------|:----------|-----:|--------:|----------:|----------:|
| Argentina          | Americas  |  1997|   73.275|   36203463|  10967.282|
| Bolivia            | Americas  |  1997|   62.050|    7693188|   3326.143|
| Brazil             | Americas  |  1997|   69.388|  168546719|   7957.981|
| Canada             | Americas  |  1997|   78.610|   30305843|  28954.926|
| Chile              | Americas  |  1997|   75.816|   14599929|  10118.053|
| Colombia           | Americas  |  1997|   70.313|   37657830|   6117.362|
| Costa Rica         | Americas  |  1997|   77.260|    3518107|   6677.045|
| Cuba               | Americas  |  1997|   76.151|   10983007|   5431.990|
| Dominican Republic | Americas  |  1997|   69.957|    7992357|   3614.101|
| Ecuador            | Americas  |  1997|   72.312|   11911819|   7429.456|
| El Salvador        | Americas  |  1997|   69.535|    5783439|   5154.825|
| Guatemala          | Americas  |  1997|   66.322|    9803875|   4684.314|
| Haiti              | Americas  |  1997|   56.671|    6913545|   1341.727|
| Honduras           | Americas  |  1997|   67.659|    5867957|   3160.455|
| Jamaica            | Americas  |  1997|   72.262|    2531311|   7121.925|

Look at the `str`.

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
str(gap_americas1997)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    25 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 5 12 15 21 24 26 30 33 37 38 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ year     : int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
    ##  $ lifeExp  : num  73.3 62 69.4 78.6 75.8 ...
    ##  $ pop      : int  36203463 7693188 168546719 30305843 14599929 37657830 3518107 10983007 7992357 11911819 ...
    ##  $ gdpPercap: num  10967 3326 7958 28955 10118 ...

We can see that gapminder has 1704 row and the filtered data only has 25 rows. However, the levels are the same. This is because gap\_americas1997 has unused levels.

Let's look at I/O.

``` r
# write the df
gap_americas1997 %>%
  write_csv("output/gap_americas1997_v1")

#Read
read_csv("output/gap_americas1997_v1") %>%
  str()
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    25 obs. of  6 variables:
    ##  $ country  : chr  "Argentina" "Bolivia" "Brazil" "Canada" ...
    ##  $ continent: chr  "Americas" "Americas" "Americas" "Americas" ...
    ##  $ year     : int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
    ##  $ lifeExp  : num  73.3 62 69.4 78.6 75.8 ...
    ##  $ pop      : int  36203463 7693188 168546719 30305843 14599929 37657830 3518107 10983007 7992357 11911819 ...
    ##  $ gdpPercap: num  10967 3326 7958 28955 10118 ...
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 6
    ##   .. ..$ country  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ continent: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ year     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ lifeExp  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ pop      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ gdpPercap: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

We can see that after writing and reading the data, the levels are not maintained. This info is not stored in the written file.

Let's try reordering now.

``` r
gap_americas1997 %>%
  mutate(country = fct_reorder(country, lifeExp)) %>% # reorder factor by lifeExp
  str()
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    25 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Haiti","Bolivia",..: 16 2 7 25 21 12 24 22 11 15 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ year     : int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
    ##  $ lifeExp  : num  73.3 62 69.4 78.6 75.8 ...
    ##  $ pop      : int  36203463 7693188 168546719 30305843 14599929 37657830 3518107 10983007 7992357 11911819 ...
    ##  $ gdpPercap: num  10967 3326 7958 28955 10118 ...

We can see that the factors for lifeExp are reordered.

``` r
gap_americas1997 %>%
  mutate(country = fct_reorder(country, lifeExp)) %>% # reorder factor by lifeExp
  ggplot(aes(country, lifeExp)) +
  geom_point() + #Scatter plot
  coord_flip() + #flip X and y
  theme_bw() #bw theme
```

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-17-1.png)

So the plot is now ordered by lifeExp.

Let's do another round of I/O

``` r
# write the df
gap_americas1997 %>%
  mutate(country = fct_reorder(country, lifeExp)) %>%
  write_csv("output/gap_americas1997_v2")

#read
read_csv("output/gap_americas1997_v2") %>%
  str()
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    25 obs. of  6 variables:
    ##  $ country  : chr  "Argentina" "Bolivia" "Brazil" "Canada" ...
    ##  $ continent: chr  "Americas" "Americas" "Americas" "Americas" ...
    ##  $ year     : int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
    ##  $ lifeExp  : num  73.3 62 69.4 78.6 75.8 ...
    ##  $ pop      : int  36203463 7693188 168546719 30305843 14599929 37657830 3518107 10983007 7992357 11911819 ...
    ##  $ gdpPercap: num  10967 3326 7958 28955 10118 ...
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 6
    ##   .. ..$ country  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ continent: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ year     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ lifeExp  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ pop      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ gdpPercap: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

As we can see, after a round of I/O, nothing is maintained except that values themselves. We do not have any ordering of factors.

``` r
read_csv("output/gap_americas1997_v2") %>%
  ggplot(aes(country, lifeExp)) +
  geom_point() + #Scatter plot
  coord_flip() + #flip X and y
  theme_bw() #bw theme
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-19-1.png)

As expected, when we plot, this is back to alphabetical order.

What if we want to write/read and maintain some order? We would use `arrange`.

``` r
# Arrange
gap_americas1997 %>%
  arrange(lifeExp) %>%
  head(15) %>%
  kable()
```

| country             | continent |  year|  lifeExp|        pop|  gdpPercap|
|:--------------------|:----------|-----:|--------:|----------:|----------:|
| Haiti               | Americas  |  1997|   56.671|    6913545|   1341.727|
| Bolivia             | Americas  |  1997|   62.050|    7693188|   3326.143|
| Guatemala           | Americas  |  1997|   66.322|    9803875|   4684.314|
| Honduras            | Americas  |  1997|   67.659|    5867957|   3160.455|
| Peru                | Americas  |  1997|   68.386|   24748122|   5838.348|
| Nicaragua           | Americas  |  1997|   68.426|    4609572|   2253.023|
| Brazil              | Americas  |  1997|   69.388|  168546719|   7957.981|
| Paraguay            | Americas  |  1997|   69.400|    5154123|   4247.400|
| Trinidad and Tobago | Americas  |  1997|   69.465|    1138101|   8792.573|
| El Salvador         | Americas  |  1997|   69.535|    5783439|   5154.825|
| Dominican Republic  | Americas  |  1997|   69.957|    7992357|   3614.101|
| Colombia            | Americas  |  1997|   70.313|   37657830|   6117.362|
| Venezuela           | Americas  |  1997|   72.146|   22374398|  10165.495|
| Jamaica             | Americas  |  1997|   72.262|    2531311|   7121.925|
| Ecuador             | Americas  |  1997|   72.312|   11911819|   7429.456|

We can see that the df is ordered by lifeExp here. Let's now output and input that.

``` r
# Write the df
gap_americas1997 %>%
  arrange(lifeExp) %>%
  write_csv("output/gap_americas1997_v3")

# Read it back in
read_csv("output/gap_americas1997_v3") %>% 
  head(15) %>%
  kable()
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

| country             | continent |  year|  lifeExp|        pop|  gdpPercap|
|:--------------------|:----------|-----:|--------:|----------:|----------:|
| Haiti               | Americas  |  1997|   56.671|    6913545|   1341.727|
| Bolivia             | Americas  |  1997|   62.050|    7693188|   3326.143|
| Guatemala           | Americas  |  1997|   66.322|    9803875|   4684.314|
| Honduras            | Americas  |  1997|   67.659|    5867957|   3160.455|
| Peru                | Americas  |  1997|   68.386|   24748122|   5838.348|
| Nicaragua           | Americas  |  1997|   68.426|    4609572|   2253.023|
| Brazil              | Americas  |  1997|   69.388|  168546719|   7957.981|
| Paraguay            | Americas  |  1997|   69.400|    5154123|   4247.400|
| Trinidad and Tobago | Americas  |  1997|   69.465|    1138101|   8792.573|
| El Salvador         | Americas  |  1997|   69.535|    5783439|   5154.825|
| Dominican Republic  | Americas  |  1997|   69.957|    7992357|   3614.101|
| Colombia            | Americas  |  1997|   70.313|   37657830|   6117.362|
| Venezuela           | Americas  |  1997|   72.146|   22374398|  10165.495|
| Jamaica             | Americas  |  1997|   72.262|    2531311|   7121.925|
| Ecuador             | Americas  |  1997|   72.312|   11911819|   7429.456|

We can see that the arranged order by lifeExp is maintained.

``` r
read_csv("output/gap_americas1997_v3") %>% 
  ggplot(aes(country, lifeExp)) +
  geom_point() + #Scatter plot
  coord_flip() + #flip X and y
  theme_bw() #bw theme
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-22-1.png)

But when we plot, we are back to alphabetical order.

``` r
read_csv("output/gap_americas1997_v3") %>% 
  ggplot(aes(reorder(country, lifeExp), lifeExp)) +
  geom_point() + #Scatter plot
  coord_flip() + #flip X and y
  theme_bw() #bw theme
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-23-1.png)

We can use `reorder` since we do not have ordered levels here.

Part 3: Visualization design
----------------------------

Remake at least one figure or create a new one, in light of something you learned in the recent class meetings about visualization design and color. Maybe juxtapose your first attempt and what you obtained after some time spent working on it. Reflect on the differences. If using Gapminder, you can use the country or continent color scheme that ships with Gapminder. Consult the dimensions listed in All the Graph Things.

I'll be replotting some of my plots from previous assignments.

OLD:

``` r
gapminder %>%
  filter(continent == "Asia") %>%
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = year)) +
  geom_point() +
  scale_y_log10()
```

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-24-1.png)

NEW:

``` r
gapminder %>%
  filter(continent == "Asia") %>%
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = year)) +
  geom_point() +
  scale_y_continuous(
    label=dollar_format(), # add $ format
    trans  = "log10") + #y axis on log scale
  scale_color_distiller(
    palette = "Oranges") + #Orange palette
  theme_bw() + # change theme
  ylab("GDP per capita") + # change title on Y axis
  xlab("Life Expectancy") #change title on X axis
```

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-25-1.png)

So here, I've re-plotted a scatterplot from HW02. I've changed the theme, the color scale, the titles on the axes, added a dollar format, and transformed the Y axis to a log10 scale.

Here is another plot.

OLD:

``` r
ggplot(gapminder, aes(x = continent, y = pop)) +
  scale_y_log10() +
  geom_violin(aes(fill = continent), show.legend = FALSE) +
  geom_jitter(alpha = 0.2)
```

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-26-1.png)

NEW:

``` r
ggplot(gapminder, aes(x = continent, y = pop)) +
  geom_violin(show.legend = FALSE, aes(fill = continent)) +
  geom_jitter(alpha = 0.2) +
  scale_y_continuous(
    label=comma_format(), # add comma format
    trans = "log10", #y axis on log scale
    breaks = 10^(1:10)) + # log 10 y scale
  scale_fill_brewer(
    palette = "Dark2") + #change fill 
  theme_bw() + #change theme to bw
  theme(
    axis.text = element_text(size = 14), #change size of axis labels
    axis.title = element_text(size = 16)) + #change size of axis text
  ylab("Population") + # Add Y label
  xlab("Continent") # Add X label
```

![](hw05-factor-and-figure_files/figure-markdown_github/unnamed-chunk-27-1.png)

Here I've replotted the same violin and jitter. However, I added a comma format on the Y axis, changed the breaks, transformed to log10, changed the fill colour to "Dark2", changed the theme, increased the font size on the axis labels and titles, and changed the titles on the axes.

Then, make a new graph by converting this visual (or another, if you’d like) to a plotly graph. What are some things that plotly makes possible, that are not possible with a regular ggplot2 graph?

I'll replot the first plot as a plotly.

``` r
plot <- gapminder %>%
  filter(continent == "Asia") %>%
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = year)) +
  geom_point() +
  scale_y_continuous(
    label=dollar_format(), # add $ format
    trans  = "log10") + #y axis on log scale
  scale_color_distiller(
    palette = "Oranges") + #Orange palette
  theme_bw() + # change theme
  ylab("GDP per capita") + # change title on Y axis
  xlab("Life Expectancy") #change title on X axis

ggplotly(plot) #Convert to plotly
```

With this `plotly`, I'm able to interact with the plot. For example, I can drag a box around a region and zoom in. Addtionally, I'm able to hover over points and see the exact values.

``` r
gapminder %>%
  filter(year == 2007) %>%
  plot_ly(x = ~gdpPercap, 
        y = ~lifeExp, 
        z = ~pop,
        color = ~continent, #colour by continent
        type = "scatter3d",
        mode = "markers") %>%
  layout(scene = list(xaxis = list(type = "log"), #log x
                      zaxis = list(type = "log"))) #log z
```

Here is another `plotly`. I have 3 axes (X, Y and Z). I've added colour by continent. Also, I've log transformed 2 axes. I can also do orbital rotations here, which is useful.

Part 4: Writing figures to file
-------------------------------

``` r
# Make plot
plot <- gapminder %>%
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line(lwd = 0.5, show.legend = FALSE) + 
  facet_wrap(~ continent) +
  theme_bw()

ggsave("output/small_plot.png", plot, height = 4, width = 6) #save plot as png (4 x 6)

ggsave("output/big_plot.png", plot, height = 8, width = 12) #save plot as png (8 x 12)

ggsave("output/vector_plot.pdf", plot, height = 4, width = 6) #save plot pdf
```

Here I saved my plot as a raster (png) and vector (pdf). I've also done 2 different sizes:

### 4 x 6 png (small)

![4 x 6 png (small)](output/small_plot.png)

### 8 x 12 png (small)

![8 x 12 png (small)](output/big_plot.png)

### PDF (vector) plot

![PDF (vector) plot](output/vector_plot.pdf)

But I want to do more!
----------------------
