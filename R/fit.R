.pa_full_regressors <- c("own_lag", "own_other", "own_other_lag",
                         "peer_mean", "peer_mean_lag",
                         "peer_other", "peer_other_lag")

#' Fixed-effects linear-in-means model
#'
#' Regresses each person's affect on exclusive peer means with fixed effects,
#' via [fixest::feols()]. This estimates how strongly affect co-moves within
#' teams. It does not by itself identify a causal peer effect: correlated
#' shocks, reflection and homophilous team formation all bias it.
#'
#' @param data A `pa_peer_means` object from [pa_peer_means()].
#' @param spec `"core"` uses `peer_mean` and `peer_mean_lag`. `"full"` uses
#'   all seven regressors of Sinha (2022), including the own lag. Ignored if
#'   `regressors` is given.
#' @param regressors Optional character vector of regressor names, chosen from
#'   the columns added by [pa_peer_means()].
#' @param fe Fixed effects, from `"person_team"`, `"person"`, `"team"` and
#'   `"period"`. The default, person-by-team and period, follows Sinha (2022).
#' @param cluster Variables to cluster standard errors by, from `"person"`,
#'   `"team"` and `"period"`, or `NULL` for the `fixest` default.
#' @param ... Passed to [fixest::feols()].
#'
#' @return A `fixest` object.
#'
#' @details Rows with a missing regressor are dropped, so each added
#'   regressor narrows the sample: `own_other` needs people active in two or
#'   more teams, and every lag needs activity in the previous period.
#'
#'   With person-by-team fixed effects, the `own_lag` regressor is subject
#'   to Nickell (1981) bias when persons are observed in few periods per
#'   team. That is why `spec = "core"` leaves it out.
#'
#' @references
#' Nickell, S. (1981). Biases in dynamic models with fixed effects.
#' *Econometrica*, 49(6), 1417-1426.
#'
#' @examples
#' set.seed(1)
#' d <- expand.grid(person = 1:40, period = 1:6)
#' d$team <- sample(1:8, nrow(d), replace = TRUE)
#' d$affect <- stats::runif(nrow(d), -1, 1)
#' p <- pa_peer_means(pa_panel(d, "person", "team", "period", "affect"))
#' pa_fit_fe(p)
#' @export
pa_fit_fe <- function(data, spec = c("core", "full"), regressors = NULL,
                      fe = c("person_team", "period"), cluster = "team",
                      ...) {
  .pa_assert_class(data, "pa_peer_means", "run pa_peer_means() first.")
  spec <- match.arg(spec)
  if (is.null(regressors)) {
    regressors <- switch(spec,
                         core = c("peer_mean", "peer_mean_lag"),
                         full = .pa_full_regressors)
  }
  bad <- setdiff(regressors, .pa_full_regressors)
  if (length(regressors) == 0L || length(bad) > 0L) {
    stop("`regressors` must be chosen from: ",
         paste(.pa_full_regressors, collapse = ", "), ".", call. = FALSE)
  }

  fe_terms <- .pa_fe_terms(fe, c(person_team = "person^team",
                                 person = "person", team = "team",
                                 period = "period"))
  rhs <- paste(regressors, collapse = " + ")
  fml <- paste("affect ~", rhs)
  if (length(fe_terms) > 0L) {
    fml <- paste(fml, "|", paste(fe_terms, collapse = " + "))
  }
  cl <- .pa_cluster(cluster, c("person", "team", "period"))

  fixest::feols(stats::as.formula(fml), data = .pa_strip(data),
                cluster = cl, ...)
}

#' Peer-of-peer instrumental-variable model
#'
#' Two-stage least squares on intransitive triads, via [fixest::feols()]. The
#' first stage regresses the linking person's affect in the peer team on the
#' outburst team's mean, which excludes the linking person. The second stage
#' regresses the peer team's mean, which also excludes the linking person, on
#' the instrumented affect of the linking person.
#'
#' @param triads A `pa_triads` object from [pa_triads()].
#' @param fe Fixed effects, from `"peer_team"`, `"period"` and
#'   `"outburst_team"`. The same fixed effects enter both stages. Outburst-team
#'   fixed effects need triad-level data, from `pa_triads(aggregate = FALSE)`.
#' @param cluster Variables to cluster standard errors by, from
#'   `"peer_team"`, `"period"` and `"outburst_team"`, or `NULL`.
#' @param control If `TRUE` (the default), control for `link_weight`, the
#'   linking person's weight in the peer team (word count in Sinha, 2022),
#'   in both stages.
#' @param ... Passed to [fixest::feols()].
#'
#' @return A `fixest` object. The coefficient on `fit_link_affect` is the
#'   peer-effect estimate. First-stage statistics are available through
#'   [fixest::fitstat()], for example `fitstat(fit, "ivf1")`.
#'
#' @details Identification rests on two assumptions. First, the outburst
#'   reaches the peer team only through the linking person, which the triad
#'   construction is designed to ensure. Second, no shock that the fixed
#'   effects fail to absorb hits both teams in the same period. The second
#'   assumption cannot be tested from these data.
#'
#' @examples
#' # A hand-built triad table with a known peer effect of 0.3 and a
#' # confounder (u) that biases OLS. In practice, use pa_triads().
#' set.seed(2)
#' n <- 2000
#' u <- stats::rnorm(n)
#' tr <- data.table::data.table(
#'   peer_team = sample(1:200, n, TRUE),
#'   period = sample(1:10, n, TRUE),
#'   instrument = stats::rnorm(n),
#'   link_weight = stats::rpois(n, 20)
#' )
#' tr[, link_affect := 0.5 * instrument + u]
#' tr[, outcome := 0.3 * link_affect + u + stats::rnorm(n)]
#' data.table::setattr(tr, "class", c("pa_triads", "data.table", "data.frame"))
#' pa_fit_iv(tr)
#' @export
pa_fit_iv <- function(triads, fe = c("peer_team", "period"),
                      cluster = "peer_team", control = TRUE, ...) {
  .pa_assert_class(triads, "pa_triads", "run pa_triads() first.")
  if ("outburst_team" %in% fe && isTRUE(attr(triads, "aggregated"))) {
    stop("Outburst-team fixed effects need triad-level data; ",
         "use pa_triads(..., aggregate = FALSE).", call. = FALSE)
  }
  if (nrow(triads) == 0L) {
    stop("`triads` has no rows to estimate on.", call. = FALSE)
  }
  fe_terms <- .pa_fe_terms(fe, c(peer_team = "peer_team", period = "period",
                                 outburst_team = "outburst_team"))
  rhs <- if (isTRUE(control)) "link_weight" else "1"
  fml <- paste("outcome ~", rhs)
  if (length(fe_terms) > 0L) {
    fml <- paste(fml, "|", paste(fe_terms, collapse = " + "))
  }
  fml <- paste(fml, "| link_affect ~ instrument")
  cl <- .pa_cluster(cluster, c("peer_team", "period", "outburst_team"))

  fixest::feols(stats::as.formula(fml), data = .pa_strip(triads),
                cluster = cl, ...)
}
