flagged <- function(ob) {
  ob[outburst == TRUE, paste(team, period, outburst_type)]
}

test_that("default rule flags only negative-to-positive transitions", {
  ob <- pa_outbursts(as_panel(fx_outbursts(), weight = NULL))
  expect_s3_class(ob, "pa_outbursts")
  expect_equal(flagged(ob), "X 2 to_positive")
})

test_that("direction selects transitions", {
  p <- as_panel(fx_outbursts(), weight = NULL)
  expect_equal(flagged(pa_outbursts(p, direction = "to_negative")),
               "Z 2 to_negative")
  expect_setequal(flagged(pa_outbursts(p, direction = "both")),
                  c("X 2 to_positive", "Z 2 to_negative"))
})

test_that("transitions that start in the neutral band are not outbursts", {
  p <- as_panel(fx_outbursts(), weight = NULL)
  expect_false("Y 2 to_positive" %in% flagged(pa_outbursts(p)))
  expect_true("Y 2 to_positive" %in%
                flagged(pa_outbursts(p, neutral = NULL)))
})

test_that("users can change the threshold, jump and band", {
  p <- as_panel(fx_outbursts(), weight = NULL)
  expect_length(flagged(pa_outbursts(p, threshold = 0.8)), 0L)
  expect_length(flagged(pa_outbursts(p, jump = 1.5)), 0L)
  expect_length(flagged(pa_outbursts(p, neutral = c(-0.7, 0.7))), 0L)
  expect_equal(attr(pa_outbursts(p, jump = 1), "rule")$jump, 1)
})

test_that("single-member teams and gaps never produce outbursts", {
  ob <- pa_outbursts(as_panel(fx_outbursts(), weight = NULL),
                     direction = "both")
  expect_false(any(ob[team %in% c("W", "V"), outburst]))
  expect_true(is.na(ob[team == "V" & period == 3L, team_mean_lag]))
})

test_that("pa_outbursts validates its arguments", {
  p <- as_panel(fx_outbursts(), weight = NULL)
  expect_error(pa_outbursts(p, min_size = 1), "min_size")
  expect_error(pa_outbursts(p, neutral = c(0.5, -0.5)), "neutral")
  expect_error(pa_outbursts(p, jump = -1), "jump")
})
