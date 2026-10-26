test_that("pa_panel standardises columns and keys", {
  p <- as_panel(fx_peer())
  expect_s3_class(p, "pa_panel")
  expect_named(p, c("person", "team", "period", "affect", "weight"))
  expect_type(p$period, "integer")
  expect_equal(data.table::key(p), c("person", "team", "period"))
  expect_equal(attr(p, "pa_names")$weight, "words")
})

test_that("pa_panel defaults weights to 1", {
  p <- pa_panel(fx_peer(), "person", "team", "period", "affect")
  expect_true(all(p$weight == 1))
})

test_that("pa_panel rejects bad input", {
  d <- fx_peer()
  expect_error(pa_panel(d, "person", "team", "period", "nope"), "not found")
  expect_error(pa_panel(rbind(d, d[1, ]), "person", "team", "period",
                        "affect"), "more than one row")
  d2 <- d
  d2$period[1] <- 1.5
  expect_error(pa_panel(d2, "person", "team", "period", "affect"),
               "whole numbers")
  d3 <- d
  d3$words[1] <- 0
  expect_error(pa_panel(d3, "person", "team", "period", "affect", "words"),
               "positive")
  d4 <- d
  d4$team[2] <- NA
  expect_error(pa_panel(d4, "person", "team", "period", "affect"),
               "missing")
})

test_that("pa_panel drops missing scores and warns outside [-1, 1]", {
  d <- fx_peer()
  d$affect[1] <- NA
  expect_message(p <- pa_panel(d, "person", "team", "period", "affect"),
                 "1 row")
  expect_equal(nrow(p), nrow(d) - 1L)
  d$affect[1] <- 2
  expect_warning(pa_panel(d, "person", "team", "period", "affect"),
                 "outside")
})
