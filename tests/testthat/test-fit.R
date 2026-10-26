sim_triads <- function(n = 20000, beta = 0.3, seed = 11) {
  set.seed(seed)
  u <- stats::rnorm(n)
  tr <- data.table::data.table(
    peer_team = sample(1:1000, n, TRUE),
    period = sample(1:12, n, TRUE),
    instrument = stats::rnorm(n),
    link_weight = stats::rpois(n, 20)
  )
  tr[, link_affect := 0.5 * instrument + u]
  tr[, outcome := beta * link_affect + u + stats::rnorm(n)]
  data.table::setattr(tr, "aggregated", TRUE)
  data.table::setattr(tr, "class", c("pa_triads", "data.table", "data.frame"))
  tr
}

test_that("pa_fit_iv recovers the peer effect where OLS is biased", {
  tr <- sim_triads()
  fit <- pa_fit_iv(tr)
  expect_s3_class(fit, "fixest")
  est <- unname(stats::coef(fit)["fit_link_affect"])
  se <- unname(fixest::se(fit)["fit_link_affect"])
  expect_lt(abs(est - 0.3), 3 * se)
  ols <- fixest::feols(outcome ~ link_affect | peer_team + period,
                       data = as.data.frame(tr))
  expect_gt(unname(stats::coef(ols)["link_affect"]), 0.6)
})

test_that("pa_fit_iv refuses outburst-team FE on aggregated data", {
  expect_error(pa_fit_iv(sim_triads(n = 500), fe = "outburst_team"),
               "triad-level")
  expect_error(pa_fit_iv(sim_triads(n = 500), fe = "nope"), "Unknown")
})

test_that("pa_fit_fe uses the requested specification", {
  set.seed(3)
  d <- expand.grid(person = 1:150, period = 1:8, slot = 1:2)
  d$team <- sample(1:6, nrow(d), replace = TRUE)
  d <- d[!duplicated(d[, c("person", "team", "period")]), ]
  d$affect <- stats::runif(nrow(d), -1, 1)
  pm <- pa_peer_means(pa_panel(d, "person", "team", "period", "affect"))

  core <- pa_fit_fe(pm)
  expect_s3_class(core, "fixest")
  expect_named(stats::coef(core), c("peer_mean", "peer_mean_lag"))

  full <- pa_fit_fe(pm, spec = "full")
  expect_setequal(names(stats::coef(full)),
                  c("own_lag", "own_other", "own_other_lag", "peer_mean",
                    "peer_mean_lag", "peer_other", "peer_other_lag"))

  expect_error(pa_fit_fe(pm, regressors = "bogus"), "chosen from")
  expect_error(pa_fit_fe(as_panel(fx_peer())), "pa_peer_means")
})
