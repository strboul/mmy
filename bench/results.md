Generate an example data.frame:

    n <- 1e5
    df <- data.frame(
        Date = sort(sample(seq(as.Date("2019-02-27"), as.Date("2019-03-01"), by = "day"), n, replace = TRUE)),
        Category = sample(LETTERS[seq(4L)], n, replace = TRUE),
        Item = replicate(n, paste(sample(c(letters, LETTERS), 4L), collapse = "")),
        Stock = sample(1:100, n, replace = TRUE),
        Sold = sample(c(TRUE, FALSE), n, replace = TRUE)
      )

Display the structure of this object:

    str(df)

    ## 'data.frame':    100000 obs. of  5 variables:
    ##  $ Date    : Date, format: "2019-02-27" "2019-02-27" ...
    ##  $ Category: Factor w/ 4 levels "A","B","C","D": 2 3 3 2 3 3 3 1 2 3 ...
    ##  $ Item    : Factor w/ 99255 levels "aABf","AabN",..: 94327 24712 12090 13888 79774 92754 32058 20083 14968 92722 ...
    ##  $ Stock   : int  78 66 36 94 60 28 54 16 66 73 ...
    ##  $ Sold    : logi  TRUE FALSE TRUE TRUE TRUE FALSE ...

ht
--

Seems not so *fast* (at the moment). However, having an heavily
optimized call isn’t the primary goal of this call.

    system.time(mmy::ht(df))

    ##       Date Category Item Stock Sold 
    ## 1: 17954.000000 B yshN 78     
    ## 2: 17954.000000 C Glqu 66     
    ## 3: 17954.000000 C DedT 36     
    ## 4: 17954.000000 B DpVh 94     
    ## 5: 17954.000000 C uxGj 60     
    ##       -------- -------- -------- -------- --------

    ##    user  system elapsed 
    ##   0.018   0.002   0.020

------------------------------------------------------------------------

------------------------------------------------------------------------

File last created on *2019-03-03*
