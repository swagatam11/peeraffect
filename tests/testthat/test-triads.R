test_that("triad-level rows match hand calculations", {
  p <- as_panel(fx_triads())
  tr <- pa_triads(p, pa_outbursts(p), aggregate = FALSE)
  expect_s3_class(tr, "pa_triads")
  expect_false(attr(tr, "aggregated"))
  expect_equal(nrow(tr), 2L)
  expect_false("K" %in% tr$peer_team)   # shares two people with J

  ri <- tr[peer_team == "I"]
  expect_equal(ri$person, "x")
  expect_equal(ri$instrument, 0.7)       # a, excluding x
  expect_equal(ri$link_affect, 0.2)
  expect_equal(ri$link_weight, 2)
  expect_equal(ri$outcome, (0.4 - 0.2) / 2)

  rl <- tr[peer_team == "L"]
  expect_equal(rl$person, "a")
  expect_equal(rl$instrument, 0.9)       # x, excluding a
  expect_equal(rl$outcome, 0.1)
})

test_that("aggregation combines links into one peer team", {
  p <- as_panel(fx_triads_two_links())
  tr <- pa_triads(p, pa_outbursts(p))
  expect_true(attr(tr, "aggregated"))
  ri <- tr[peer_team == "I"]
  expect_equal(ri$n_links, 2L)
  expect_equal(ri$n_outburst_teams, 2L)
  expect_equal(ri$instrument, (0.7 * 1 + 0.6 * 3) / 4)
  expect_equal(ri$link_affect, (0.2 * 2 + 0.0 * 1) / 3)
  expect_equal(ri$link_weight, 3)
  expect_equal(ri$outcome, (0.4 - 0.2) / 2)   # excludes x and y
})

test_that("peer teams with their own outburst are excluded by default", {
  d <- data.frame(
    person = c("x", "a", "x", "b", "x", "a", "x", "b"),
    team = c("J", "J", "I", "I", "J", "J", "I", "I"),
    period = c(1, 1, 1, 1, 2, 2, 2, 2),
    affect = c(-0.8, -0.8, -0.8, -0.8, 0.9, 0.9, 0.9, 0.9)
  )
  p <- pa_panel(d, "person", "team", "period", "affect")
  ob <- pa_outbursts(p)
  expect_equal(nrow(pa_triads(p, ob)), 0L)
  expect_equal(nrow(pa_triads(p, ob, exclude_peer_outbursts = FALSE)), 2L)
})

test_that("no outbursts gives an empty table with a warning", {
  p <- as_panel(fx_triads())
  ob <- pa_outbursts(p, threshold = 0.99)
  expect_warning(tr <- pa_triads(p, ob), "No outbursts")
  expect_equal(nrow(tr), 0L)
  expect_s3_class(tr, "pa_triads")
})
