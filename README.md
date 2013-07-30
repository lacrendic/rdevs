## shoRtcut

shoRtcut is an R package to make the life with R even more easier.

### Installation

You can install `shoRcut` from `github` using the `devtools` package.


```
## [1] TRUE
```





```r
library(devtools)
install_github("shoRtcut", "jbkunst")
library(shoRtcut)
```





### Some functions

#### Frequency table

The function `freqtable` return a `data.frame` with the information of the counts of the levels of variable.


```r
data(credit)
freqtable(credit$marital_status)
```

```
##   variable  freq cumfreq relfreq cumrelfreq
## 1        C 17097   17097 0.34405     0.3440
## 2        D  2142   19239 0.04310     0.3871
## 3        O  2776   22015 0.05586     0.4430
## 4        S 25249   47264 0.50809     0.9511
## 5        V  2430   49694 0.04890     1.0000
## 6    Total 49694      NA 1.00000         NA
```

```r
freqtable(credit$marital_status, add.total = FALSE)
```

```
##   variable  freq cumfreq relfreq cumrelfreq
## 1        C 17097   17097 0.34405     0.3440
## 2        D  2142   19239 0.04310     0.3871
## 3        O  2776   22015 0.05586     0.4430
## 4        S 25249   47264 0.50809     0.9511
## 5        V  2430   49694 0.04890     1.0000
```

```r
freqtable(credit$residence_type, sort.by.count = TRUE, pretty = TRUE)
```

```
##   variable   freq cumfreq relfreq cumrelfreq
## 4        P 37,013  49,694   74.5%      74.5%
## 1        A  6,326   6,326   12.7%      87.2%
## 2        C  4,323  10,649    8.7%      95.9%
## 3        O  2,032  12,681    4.1%     100.0%
## 5    Total 49,694      NA  100.0%        NA%
```


This function also make a cross table if `variable2` argument is given.


```r
freqtable(credit$marital_status, credit$residence_type)
```

```
##    variable    A    C    O     P Total.row
## 1         C 1934  255  556 14352     17097
## 2         D  321   79   99  1643      2142
## 3         O  435  101  143  2097      2776
## 4         S 3375 3872 1160 16842     25249
## 5         V  261   16   74  2079      2430
## 6 Total.col 6326 4323 2032 37013        NA
```

```r
freqtable(credit$marital_status, credit$residence_type, add.total = FALSE, pretty = TRUE)
```

```
##   variable      A      C      O      P
## 1        C  1,934    255    556 14,352
## 2        D    321     79     99  1,643
## 3        O    435    101    143  2,097
## 4        S  3,375  3,872  1,160 16,842
## 5        V    261     16     74  2,079
```



#### Exporting tables

The function `writetable` requirev the name (with path if you want) of the file with the extension and the `data.frame` to export. 


```r
data(cars)

t1 <- cbind(head(cars, 3), var1 = "excel")
t2 <- cbind(tail(cars, 3), var2 = "text")

writetable(t1, "excel_file.xlsx")
writetable(t2, "txt_file.txt")
```


Also support `json` format.


```r
data(mtcars)
writetable(mtcars, "file.json")
```

```
## Error: could not find function "adply"
```


#### Reading tables

The function `readtable` require the names of files to read. You can read simoultaneus files. This function not necessary require all tables with the same number columns because use the `rbind.fill` function in the `plyr` package.



```r
table <- readtable(c("excel_file.xlsx", "txt_file.txt"))
```


```r
table
```

```
##   speed dist  var1 var2
## 1     4    2 excel <NA>
## 2     4   10 excel <NA>
## 3     7    4 excel <NA>
## 4    24   93  <NA> text
## 5    24  120  <NA> text
## 6    25   85  <NA> text
```




#### Other functions
There are other functions and others in development:
* str_capitalize
* str_clean
* str_first_upper
* str_is_email
* str_pattern
* truncate
