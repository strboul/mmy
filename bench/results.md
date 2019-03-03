An example dataset:

    n <- 1e5
    df <- data.frame(
        Date = sort(sample(seq(as.Date("2019-02-27"), as.Date("2019-03-01"), by = "day"), n, replace = TRUE)),
        Category = sample(LETTERS[seq(4L)], n, replace = TRUE),
        Item = replicate(n, paste(sample(c(letters, LETTERS), 4L), collapse = "")),
        Stock = sample(1:100, n, replace = TRUE),
        Sold = sample(c(TRUE, FALSE), n, replace = TRUE)
      )

ht
--

Not so *fast* (at the moment):

    system.time(mmy::ht(df))

    ##       Date Category Item Stock Sold 
    ## 1: 17954.000000 A lRCw 59     
    ## 2: 17954.000000 C opJb 48     
    ## 3: 17954.000000 C hyoW 7      
    ## 4: 17954.000000 B xGZl 80     
    ## 5: 17954.000000 D dwmq 64     
    ##       -------- -------- -------- -------- --------

    ##    user  system elapsed 
    ##   0.020   0.001   0.021

------------------------------------------------------------------------

Last generated on *2019-03-03*
