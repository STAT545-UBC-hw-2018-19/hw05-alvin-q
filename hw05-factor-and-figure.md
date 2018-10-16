Homework 05
================

Homework 05: Factor and figure management
=========================================

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ---------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(gapminder)
library(knitr)
```

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
