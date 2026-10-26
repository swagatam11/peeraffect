row_of <- function(x, who, tm, t) {
  x[person == who & team == tm & period == t]
}

test_that("contemporaneous exclusive means match hand calculations", {
  pm <- pa_peer_means(as_panel(fx_peer()))
  r <- row_of(pm, "p1", "A", 1L)
  expect_equal(r$peer_mean, 0.4)
  expect_equal(r$own_other, -0.2)
  expect_equal(r$peer_other, 0.6)

  r <- row_of(pm, "p1", "B", 1L)
  expect_equal(r$peer_mean, 0.6)
  expect_equal(r$own_other, 0.2)
  expect_equal(r$peer_other, 0.4)

  r <- row_of(pm, "p2", "A", 1L)
  expect_equal(r$peer_mean, 0.2)
  expect_true(is.na(r$own_other))
  expect_true(is.na(r$peer_other))

  r <- row_of(pm, "p1", "A", 2L)
  expect_equal(r$peer_mean, (-0.1 * 1 + 0.3 * 2) / 3)
  expect_equal(r$own_other, (0.1 + 0.0) / 2)
  expect_equal(r$peer_other, 1.0)   # B has nobody else; C has p5
  expect_equal(r$n_team, 3L)

  r <- row_of(pm, "p1", "B", 2L)
  expect_true(is.na(r$peer_mean))   # alone in B
  expect_equal(r$own_other, (0.5 + 0.0) / 2)
  expect_equal(r$peer_other, (-0.1 + 0.6 + 1.0) / 4)
})

test_that("lags match hand calculations", {
  pm <- pa_peer_means(as_panel(fx_peer()))
  r <- row_of(pm, "p1", "A", 2L)
  expect_equal(r$own_lag, 0.2)
  expect_equal(r$peer_mean_lag, 0.4)
  expect_equal(r$own_other_lag, -0.2)
  expect_equal(r$peer_other_lag, 0.6)

  r <- row_of(pm, "p1", "B", 2L)
  expect_equal(r$own_lag, -0.2)
  expect_equal(r$peer_mean_lag, 0.6)
  expect_equal(r$own_other_lag, 0.2)
  expect_equal(r$peer_other_lag, 0.4)

  # p4 was absent at t - 1: the lagged team mean is A's full mean.
  r <- row_of(pm, "p4", "A", 2L)
  expect_true(is.na(r$own_lag))
  expect_equal(r$peer_mean_lag, (0.2 * 1 + 0.4 * 3) / 4)
  expect_true(is.na(r$own_other_lag))
  expect_true(is.na(r$peer_other_lag))

  # C did not exist at t - 1; p1's other teams then were A and B.
  r <- row_of(pm, "p1", "C", 2L)
  expect_true(is.na(r$peer_mean_lag))
  expect_equal(r$own_other_lag, (0.2 * 1 - 0.2 * 2) / 3)
  expect_equal(r$peer_other_lag, (0.4 * 3 + 0.6 * 2) / 5)
})

test_that("lags never reach across a gap", {
  pm <- pa_peer_means(as_panel(fx_peer()))
  r <- row_of(pm, "p1", "A", 4L)
  expect_true(all(is.na(unlist(
    r[, c("own_lag", "peer_mean_lag", "own_other_lag", "peer_other_lag")]
  ))))
  expect_equal(r$peer_mean, 0.2)
})

test_that("pa_peer_means does not modify its input", {
  p <- as_panel(fx_peer())
  before <- data.table::copy(p)
  pm <- pa_peer_means(p)
  expect_identical(p, before)
  expect_s3_class(pm, "pa_peer_means")
  expect_equal(nrow(pm), nrow(p))
})
