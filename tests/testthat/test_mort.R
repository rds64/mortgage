testthat::context("Testing the Mort function")

testthat::test_that("Testing Warning from mort function", {
    expect_warning(mort(1,1,1,1), "No Missing Arguments Detected!")
    expect_warning(mort(1,1), "mort requires at least 3 full arguments")
})

testthat::test_that("", {
    expect_equal(mort(balance = 100000, rate = (.04 / 12), term = 360), 477.4153)
    expect_equal(mort(payment = 477.4153, rate = (.04 / 12), term = 360), 100000)
    expect_equal(mort(balance = 100000, rate = (.04 / 12), payment = 477.4153), 360)
    expect_equal(mort(balance = 100000, payment = 477.4153, term = 360) * 12, .04)
})

