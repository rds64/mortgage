testthat::context("amortizeing a data.frame of lonas")

test_df <- data.frame(
    id = "A",
    principal = 100000,
    term = 360,
    rate = .04
)

testthat::test_that("amortizing one row data frame with standard names", {    
   amortize(test_df, principal = principal, term = term, rate = rate) 
})

testthat::test_that("amortizing one row data frame with standard names and default arguments", {    
   amortize(test_df)
})

testthat::test_that("amortizing one loan id from data", {    
   amortize(test_df, id = id)
   amortize(test_df, id = "id")
})

test_df <- data.frame(
    id = c("A", "B", "C"),
    principal = c(100000, 200000, 400000),
    term = c(360, 360, 180),
    rate = c(.04, .045, .425)
)

testthat::test_that("amortizing row data with multiple rows frame with standard names", {    
   amortize(test_df, principal = principal, term = term, rate = rate) 
   amortize(test_df, term = term, rate = rate) 
   amortize(test_df, principal = 'principal', term = 'term', rate = 'rate') 
})

names(test_df) <- c('p', 't', 'r')

testthat::test_that("amortizing row data with multiple rows frame with standard names", {    
   amortize(test_df, principal = p, term = t, rate = r) 
})

testthat::test_that("unnesting amortized tibble output", {    
    testthat::expect_equal(    
        (amortize(test_df, principal = p, term = t, rate = r) %>% 
            tidyr::unnest() %>% 
            nrow),
        sum(test_df$t) + length(test_df$t)
    )
})

testthat::test_that("amortizing with default method", {    
        amortize(principal = test_df$p, term = test_df$t, rate = test_df$r)
})

testthat::test_that("amortizing with default method and loan id", {    
    amortize(principal = test_df$p, term = test_df$t, rate = test_df$r, id = test_df$id)
})

