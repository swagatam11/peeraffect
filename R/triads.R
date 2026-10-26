#' Build intransitive triads around outbursts
#'
#' Finds pairs of teams that share exactly one person in a period in which
#' one of the two teams has an outburst, and builds the variables for the
#' peer-of-peer instrumental-variable design.
#'
#' In a triad, the outburst team \eqn{j} and the peer team \eqn{i} share
#' exactly one linking person \eqn{x} in period \eqn{t}. The outburst can
#' reach team \eqn{i} only through \eqn{x}.
#'
#' @param panel A `pa_panel` from [pa_panel()].
#' @param outbursts A `pa_outbursts` object from [pa_outbursts()] computed on
#'   the same panel.
#' @param exclude_peer_outbursts If `TRUE` (the default), drop triads whose
#'   peer team has an outburst of its own in the same period.
#' @param aggregate If `TRUE` (the default), return one row per peer team and
#'   period. If `FALSE`, return one row per triad.
#'
#' @return A `pa_triads` object, a `data.table`.
#'
#'   With `aggregate = FALSE`, one row per triad, with columns:
#'   * `period`, `outburst_team`, `peer_team`, `person` (the linking person)
#'     and `outburst_type`;
#'   * `instrument`: the outburst team's mean excluding \eqn{x}, and
#'     `instrument_weight`, the total weight behind it;
#'   * `link_affect` and `link_weight`: \eqn{x}'s score and weight in the peer
#'     team;
#'   * `outcome`: the peer team's mean excluding \eqn{x}, and
#'     `outcome_weight`, the total weight behind it.
#'
#'   With `aggregate = TRUE`, triads that share a peer team and period are
#'   combined:
#'   * `instrument` is the weighted mean of the triads' instruments, weighted
#'     by `instrument_weight`;
#'   * `link_affect` is the weighted mean of the linking persons' scores in
#'     the peer team;
#'   * `link_weight` is their total weight;
#'   * `outcome` is the peer team's mean excluding every linking person;
#'   * `n_links` and `n_outburst_teams` count the distinct linking persons
#'     and outburst teams.
#'
#' @details Both team means exclude the linking person, so the instrument and
#'   the outcome never contain \eqn{x}'s own score. A peer team needs at least
#'   one member besides the linking person(s); otherwise its outcome is
#'   undefined and the row is dropped.
#'
#'   Pairs of teams that share two or more people are not triads and are
#'   skipped. Paths between the two teams through a third team are not
#'   checked.
#'
#' @examples
#' d <- data.frame(
#'   person = c("x", "a", "x", "b",  "x", "a", "x", "b"),
#'   team   = c("j", "j", "i", "i",  "j", "j", "i", "i"),
#'   period = c(1, 1, 1, 1,  2, 2, 2, 2),
#'   affect = c(-0.8, -0.8, 0, 0,  0.9, 0.7, 0.2, 0.4)
#' )
#' p <- pa_panel(d, "person", "team", "period", "affect")
#' pa_triads(p, pa_outbursts(p))
#' @export
pa_triads <- function(panel, outbursts, exclude_peer_outbursts = TRUE,
                      aggregate = TRUE) {
  .pa_assert_class(panel, "pa_panel", "create it with pa_panel().")
  .pa_assert_class(outbursts, "pa_outbursts",
                   "create it with pa_outbursts().")

  x <- panel[, list(person, team, period, affect, weight)]
  totals <- x[, list(t_sw = sum(weight), t_swy = sum(weight * affect),
                     t_n = .N), by = c("team", "period")]

  ob <- outbursts[outburst == TRUE,
                  list(outburst_team = team, period, outburst_type)]
  if (nrow(ob) == 0L) {
    warning("No outbursts found; returning an empty triad table.",
            call. = FALSE)
    return(.pa_empty_triads(aggregate, attr(outbursts, "rule")))
  }

  # Members of each outburst team in its outburst period.
  mj <- merge(x, ob, by.x = c("team", "period"),
              by.y = c("outburst_team", "period"))
  data.table::setnames(mj, c("team", "affect", "weight"),
                       c("outburst_team", "ob_affect", "ob_weight"))

  # The other teams those members belong to in the same period.
  tr <- merge(mj, x, by = c("person", "period"), allow.cartesian = TRUE)
  tr <- tr[team != outburst_team]
  data.table::setnames(tr, c("team", "affect", "weight"),
                       c("peer_team", "link_affect", "link_weight"))

  # Keep team pairs that share exactly one person.
  tr[, n_shared := .N, by = c("outburst_team", "peer_team", "period")]
  tr <- tr[n_shared == 1L]
  tr[, n_shared := NULL]

  if (exclude_peer_outbursts) {
    tr <- tr[!ob, on = c(peer_team = "outburst_team", period = "period")]
  }

  # Instrument: outburst team's mean excluding the linking person.
  tr <- merge(tr, totals, by.x = c("outburst_team", "period"),
              by.y = c("team", "period"))
  tr[, instrument_weight := t_sw - ob_weight]
  tr[, instrument := (t_swy - ob_weight * ob_affect) / instrument_weight]
  tr[, c("t_sw", "t_swy", "t_n") := NULL]

  # Peer-team totals, for the outcome.
  tr <- merge(tr, totals, by.x = c("peer_team", "period"),
              by.y = c("team", "period"))

  if (!aggregate) {
    tr <- tr[t_n > 1L]
    tr[, outcome_weight := t_sw - link_weight]
    tr[, outcome := (t_swy - link_weight * link_affect) / outcome_weight]
    out <- tr[, list(period, outburst_team, peer_team, person, outburst_type,
                     instrument, instrument_weight, link_affect, link_weight,
                     outcome, outcome_weight)]
    data.table::setorderv(out, c("period", "peer_team", "outburst_team"))
  } else {
    links <- unique(tr[, list(peer_team, period, person, link_affect,
                              link_weight, t_sw, t_swy, t_n)],
                    by = c("peer_team", "period", "person"))
    agg_links <- links[, list(
      l_swy = sum(link_weight * link_affect),
      link_weight = sum(link_weight),
      n_links = .N,
      t_sw = t_sw[1L], t_swy = t_swy[1L], t_n = t_n[1L]
    ), by = c("peer_team", "period")]
    agg_links <- agg_links[t_n > n_links]
    agg_links[, link_affect := l_swy / link_weight]
    agg_links[, outcome_weight := t_sw - link_weight]
    agg_links[, outcome := (t_swy - l_swy) / outcome_weight]

    agg_inst <- tr[, list(
      instrument = sum(instrument_weight * instrument) /
        sum(instrument_weight),
      instrument_weight = sum(instrument_weight),
      n_outburst_teams = data.table::uniqueN(outburst_team)
    ), by = c("peer_team", "period")]

    out <- merge(agg_inst, agg_links, by = c("peer_team", "period"))
    out <- out[, list(period, peer_team, instrument, instrument_weight,
                      link_affect, link_weight, outcome, outcome_weight,
                      n_links, n_outburst_teams)]
    data.table::setorderv(out, c("period", "peer_team"))
  }

  .pa_set_triads(out, aggregate, attr(outbursts, "rule"))
}

.pa_set_triads <- function(out, aggregate, rule) {
  data.table::setattr(out, "aggregated", aggregate)
  data.table::setattr(out, "rule", rule)
  data.table::setattr(out, "class", c("pa_triads", "data.table", "data.frame"))
  out
}

.pa_empty_triads <- function(aggregate, rule) {
  out <- if (aggregate) {
    data.table::data.table(
      period = integer(), peer_team = character(), instrument = numeric(),
      instrument_weight = numeric(), link_affect = numeric(),
      link_weight = numeric(), outcome = numeric(),
      outcome_weight = numeric(), n_links = integer(),
      n_outburst_teams = integer()
    )
  } else {
    data.table::data.table(
      period = integer(), outburst_team = character(),
      peer_team = character(), person = character(),
      outburst_type = character(), instrument = numeric(),
      instrument_weight = numeric(), link_affect = numeric(),
      link_weight = numeric(), outcome = numeric(),
      outcome_weight = numeric()
    )
  }
  .pa_set_triads(out, aggregate, rule)
}
