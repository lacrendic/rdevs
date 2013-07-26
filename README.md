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
data(iris)
freqtable(iris$Species)
```

```
##     variable freq cumfreq relfreq cumrelfreq
## 1     setosa   50      50  0.3333     0.3333
## 2 versicolor   50     100  0.3333     0.6667
## 3  virginica   50     150  0.3333     1.0000
## 4      Total  150      NA  1.0000         NA
```

```r
freqtable(iris$Species, add.total = FALSE)
```

```
##     variable freq cumfreq relfreq cumrelfreq
## 1     setosa   50      50  0.3333     0.3333
## 2 versicolor   50     100  0.3333     0.6667
## 3  virginica   50     150  0.3333     1.0000
```



#### Exporting tables

The function `writetable` require only the name of the file with the extension. 



```r
data(cars)

t1 <- cbind(head(cars, 3), var1 = "excel")
t2 <- cbind(tail(cars, 3), var2 = "text")

writetable(t1, "excel_file.xlsx")
writetable(t2, "txt_file.txt")
```


#### Reading tables

The function `readtable` require the names of files (always with extension) to read. You can read simoultaneus files.  This function not necessary require all tables with the same number columns.



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

