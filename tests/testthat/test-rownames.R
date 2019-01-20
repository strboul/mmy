
context("testing rownames")

df <- head(mtcars)

test_that("un row names", {

  expect_equal(un_row_names(df),
               structure(
                 list(
                   mpg = c(21, 21, 22.8, 21.4, 18.7, 18.1),
                   cyl = c(6,
                           6, 4, 6, 8, 6),
                   disp = c(160, 160, 108, 258, 360, 225),
                   hp = c(110,
                          110, 93, 110, 175, 105),
                   drat = c(3.9, 3.9, 3.85, 3.08, 3.15,
                            2.76),
                   wt = c(2.62, 2.875, 2.32, 3.215, 3.44, 3.46),
                   qsec = c(16.46,
                            17.02, 18.61, 19.44, 17.02, 20.22),
                   vs = c(0, 0, 1, 1, 0, 1),
                   am = c(1, 1, 1, 0, 0, 0),
                   gear = c(4, 4, 4, 3, 3, 3),
                   carb = c(4,
                            4, 1, 1, 2, 1)
                 ),
                 row.names = c(NA, -6L),
                 class = "data.frame"
               ))

})

test_that("standardize row names", {

  expect_equal(std_row_names(df),
               structure(
                 list(
                   rowname = c(
                     "Mazda RX4",
                     "Mazda RX4 Wag",
                     "Datsun 710",
                     "Hornet 4 Drive",
                     "Hornet Sportabout",
                     "Valiant"
                   ),
                   mpg = c(21,
                           21, 22.8, 21.4, 18.7, 18.1),
                   cyl = c(6, 6, 4, 6, 8, 6),
                   disp = c(160,
                            160, 108, 258, 360, 225),
                   hp = c(110, 110, 93, 110, 175, 105),
                   drat = c(3.9, 3.9, 3.85, 3.08, 3.15, 2.76),
                   wt = c(2.62,
                          2.875, 2.32, 3.215, 3.44, 3.46),
                   qsec = c(16.46, 17.02, 18.61,
                            19.44, 17.02, 20.22),
                   vs = c(0, 0, 1, 1, 0, 1),
                   am = c(1,
                          1, 1, 0, 0, 0),
                   gear = c(4, 4, 4, 3, 3, 3),
                   carb = c(4, 4,
                            1, 1, 2, 1)
                 ),
                 row.names = c(NA, -6L),
                 class = "data.frame"
               ))
  
})

