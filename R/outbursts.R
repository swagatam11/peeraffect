#' Detect team-level affect outbursts
#'
#' Flags team-periods in which a team's mean affect jumps sharply from one
#' valence to the other. Outbursts select the sample for the peer-of-peer
#' instrumental-variable design in [pa_triads()].
#'
#' @param panel A `pa_panel` from [pa_panel()].
#' @param threshold The current team mean must exceed `threshold` in absolute
#'   value, in the direction of the transition.
#' @param jump The absolute change from the previous period's team mean must
#'   exceed `jump`.
#' @param neutral A length-2 vector `c(lower, upper)` giving the neutral band.
#'   The previous period's team mean must lie beyond the band on the opposite
#'   side: below `lower` for a transition to positive, above `upper` for a
#'   transition to negative. Transitions that start inside the band are not
#'   outbursts. Set `neutral = NULL` to drop this condition.
#' @param direction Which transitions count: `"to_positive"` (negative to
#'   positive), `"to_negative"` (positive to negative), or `"both"`.
#' @param min_size Minimum number of team members in the current period.
#'   Must be at least 2, because the instrument is the outburst team's mean
#'   excluding the linking person.
#'
#' @return A `pa_outbursts` object: a `data.table` with one row per
#'   team-period and columns `team`, `period`, `team_mean`, `team_mean_lag`,
#'   `n_members`, `team_weight`, `outburst` (logical) and `outburst_type`.
#'   The rule's settings are stored in the `rule` attribute.
#'
#' @details Team means are weighted by `weight` and include every member. The
#'   previous period must be exactly `period - 1`; a team with a gap before
#'   the current period cannot have an outburst in it.
#'
#'   The defaults follow the definition in Sinha (2022): a current mean
#'   beyond 0.6, a jump larger than 0.7, and a starting point outside the
#'   neutral band from -0.5 to 0.5 used by Pano and Kashef (2020). With the
#'   defaults, the requirement that the previous mean be non-zero, stated in
#'   Sinha (2022), holds automatically.
#'
#' @references
#' Pano, T. and Kashef, R. (2020). A complete VADER-based sentiment analysis
#' of Bitcoin (BTC) tweets during the era of COVID-19. *Big Data and
#' Cognitive Computing*, 4(4), 33.
#'
#' @examples
#' d <- data.frame(
#'   person = rep(c("a", "b"), 2),
#'   team = "x",
#'   period = c(1, 1, 2, 2),
#'   affect = c(-0.6, -0.7, 0.7, 0.8)
#' )
#' p <- pa_panel(d, "person", "team", "period", "affect")
#' pa_outbursts(p)
#' @export
pa_outbursts <- function(panel, threshold = 0.6, jump = 0.7,
                         neutral = c(-0.5, 0.5),
                         direction = c("to_positive", "to_negative", "both"),
                         min_size = 2L) {
  .pa_assert_class(panel, "pa_panel", "create it with pa_panel().")
  direction <- match.arg(direction)
  .pa_scalar(threshold, "threshold", lower = 0)
  .pa_scalar(jump, "jump", lower = 0)
  .pa_scalar(min_size, "min_size", lower = 2)
  if (!is.null(neutral)) {
    if (!is.numeric(neutral) || length(neutral) != 2L || anyNA(neutral) ||
        neutral[1] > neutral[2]) {
      stop("`neutral` must be NULL or c(lower, upper) with lower <= upper.",
           call. = FALSE)
    }
    lower <- neutral[1]
    upper <- neutral[2]
  } else {
    lower <- Inf
    upper <- -Inf
  }

  tm <- panel[, list(team_mean = sum(weight * affect) / sum(weight),
                     team_weight = sum(weight),
                     n_members = .N),
              by = c("team", "period")]
  lagged <- tm[, list(team, period = period + 1L, team_mean_lag = team_mean)]
  tm <- merge(tm, lagged, by = c("team", "period"), all.x = TRUE, sort = TRUE)

  tm[, up := !is.na(team_mean_lag) & team_mean > threshold &
       team_mean_lag < lower & (team_mean - team_mean_lag) > jump]
  tm[, down := !is.na(team_mean_lag) & team_mean < -threshold &
       team_mean_lag > upper & (team_mean_lag - team_mean) > jump]
  big <- tm$n_members >= min_size
  want_up <- direction %in% c("to_positive", "both")
  want_down <- direction %in% c("to_negative", "both")
  tm[, outburst_type := data.table::fcase(
    up & big & want_up, "to_positive",
    down & big & want_down, "to_negative",
    default = NA_character_
  )]
  tm[, outburst := !is.na(outburst_type)]
  tm[, c("up", "down") := NULL]
  data.table::setcolorder(tm, c("team", "period", "team_mean", "team_mean_lag",
                                "n_members", "team_weight", "outburst",
                                "outburst_type"))

  data.table::setattr(tm, "rule", list(
    threshold = threshold, jump = jump, neutral = neutral,
    direction = direction, min_size = min_size
  ))
  data.table::setattr(tm, "class",
                      c("pa_outbursts", "data.table", "data.frame"))
  tm
}
