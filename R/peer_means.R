#' Exclusive peer means and lags
#'
#' Adds the regressors of the panel linear-in-means model to a `pa_panel`.
#' Every peer mean is weighted by `weight` and excludes the focal person, as
#' is standard in the peer-effects literature (Bramoulle, Djebbari and Fortin,
#' 2009).
#'
#' @param panel A `pa_panel` from [pa_panel()].
#'
#' @return The panel as a `pa_peer_means` object, with these columns added
#'   for each row (person \eqn{i}, focal team \eqn{k}, period \eqn{t}):
#'
#'   | Column | Definition | Sinha (2022) |
#'   |---|---|---|
#'   | `n_team` | members of \eqn{k} at \eqn{t}, including \eqn{i} | |
#'   | `own_lag` | \eqn{i}'s score in \eqn{k} at \eqn{t-1} | \eqn{\alpha_1} |
#'   | `own_other` | \eqn{i}'s mean score in teams other than \eqn{k} at \eqn{t} | \eqn{\alpha_2} |
#'   | `own_other_lag` | \eqn{i}'s mean score in teams other than \eqn{k} at \eqn{t-1} | \eqn{\alpha_3} |
#'   | `peer_mean` | mean of \eqn{k}'s other members at \eqn{t} | \eqn{\alpha_4} |
#'   | `peer_mean_lag` | mean of \eqn{k}'s members other than \eqn{i} at \eqn{t-1} | \eqn{\alpha_5} |
#'   | `peer_other` | mean of other people in \eqn{i}'s other teams at \eqn{t} | \eqn{\alpha_6} |
#'   | `peer_other_lag` | mean of other people in \eqn{i}'s teams other than \eqn{k} at \eqn{t-1} | \eqn{\alpha_7} |
#'
#' @details A quantity is `NA` whenever nobody contributes to it: a person who
#'   is alone in a team has no `peer_mean`, and a person active in only one
#'   team has no `own_other`. Lags look back exactly one period, so they are
#'   `NA` after a gap.
#'
#'   `peer_mean_lag` is defined even when \eqn{i} was absent from \eqn{k} at
#'   \eqn{t-1}; it is then the full mean of \eqn{k} at \eqn{t-1}.
#'
#' @references
#' Bramoulle, Y., Djebbari, H. and Fortin, B. (2009). Identification of peer
#' effects through social networks. *Journal of Econometrics*, 150(1), 41-55.
#'
#' Sinha, S. (2022). 'Are You Listening?': Sentiment Diffusion in GitHub.
#' *Academy of Management Proceedings*.
#'
#' @examples
#' d <- data.frame(
#'   person = c("a", "b", "a", "c", "a", "b"),
#'   team   = c("x", "x", "y", "y", "x", "x"),
#'   period = c(1, 1, 1, 1, 2, 2),
#'   affect = c(0.2, 0.4, -0.2, 0.6, 0.5, -0.1)
#' )
#' p <- pa_panel(d, "person", "team", "period", "affect")
#' pa_peer_means(p)
#' @export
pa_peer_means <- function(panel) {
  .pa_assert_class(panel, "pa_panel", "create it with pa_panel().")
  x <- data.table::copy(panel)
  x[, wy := weight * affect]

  # Team-period totals, and the part contributed by everyone except i.
  x[, `:=`(t_sw = sum(weight), t_swy = sum(wy), t_n = .N),
    by = c("team", "period")]
  x[, `:=`(c_sw = t_sw - weight, c_swy = t_swy - wy, c_n = t_n - 1L)]

  # Person-period totals: i's own scores across teams (p_*), and the
  # others in all of i's teams (q_*).
  x[, `:=`(p_sw = sum(weight), p_swy = sum(wy), p_n = .N,
           q_sw = sum(c_sw), q_swy = sum(c_swy), q_n = sum(c_n)),
    by = c("person", "period")]

  # Contemporaneous regressors.
  x[, peer_mean := data.table::fifelse(c_n > 0L, c_swy / c_sw, NA_real_)]
  x[, own_other := data.table::fifelse(
    p_n > 1L, (p_swy - wy) / (p_sw - weight), NA_real_)]
  x[, peer_other := data.table::fifelse(
    q_n - c_n > 0L, (q_swy - c_swy) / (q_sw - c_sw), NA_real_)]

  # Values at t - 1, shifted forward so they join on (., period).
  rows_l <- x[, list(person, team, period = period + 1L,
                     l_affect = affect, l_w = weight, l_wy = wy,
                     l_c_sw = c_sw, l_c_swy = c_swy, l_c_n = c_n)]
  team_l <- unique(x[, list(team, period = period + 1L,
                            lt_sw = t_sw, lt_swy = t_swy)])
  pers_l <- unique(x[, list(person, period = period + 1L,
                            lp_sw = p_sw, lp_swy = p_swy, lp_n = p_n,
                            lq_sw = q_sw, lq_swy = q_swy, lq_n = q_n)])

  x <- merge(x, rows_l, by = c("person", "team", "period"),
             all.x = TRUE, sort = FALSE)
  x <- merge(x, team_l, by = c("team", "period"), all.x = TRUE, sort = FALSE)
  x <- merge(x, pers_l, by = c("person", "period"), all.x = TRUE, sort = FALSE)

  x[, l_present := !is.na(l_affect)]
  x[, own_lag := l_affect]
  x[, peer_mean_lag := data.table::fcase(
    l_present & l_c_n > 0L, l_c_swy / l_c_sw,
    l_present, NA_real_,
    !is.na(lt_sw), lt_swy / lt_sw,
    default = NA_real_
  )]
  # Remove team k's contribution at t - 1 (only present if i was in k then).
  x[, own_other_lag := data.table::fifelse(
    !is.na(lp_n) & (lp_n - l_present) > 0L,
    (lp_swy - data.table::fcoalesce(l_wy, 0)) /
      (lp_sw - data.table::fcoalesce(l_w, 0)),
    NA_real_
  )]
  x[, peer_other_lag := data.table::fifelse(
    !is.na(lq_n) & (lq_n - data.table::fcoalesce(l_c_n, 0L)) > 0L,
    (lq_swy - data.table::fcoalesce(l_c_swy, 0)) /
      (lq_sw - data.table::fcoalesce(l_c_sw, 0)),
    NA_real_
  )]
  x[, n_team := t_n]

  keep <- c("person", "team", "period", "affect", "weight", "n_team",
            "own_lag", "own_other", "own_other_lag",
            "peer_mean", "peer_mean_lag", "peer_other", "peer_other_lag")
  x <- x[, keep, with = FALSE]
  data.table::setkeyv(x, c("person", "team", "period"))
  data.table::setattr(x, "pa_names", attr(panel, "pa_names"))
  data.table::setattr(x, "class",
                      c("pa_peer_means", "pa_panel", "data.table", "data.frame"))
  x
}
