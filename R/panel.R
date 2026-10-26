#' Validate long-format affect data
#'
#' Turns a data frame with one row per person, team and period into the
#' standard format the rest of the package expects. Each row holds one
#' person's aggregated affect score in one team during one period, for
#' example a developer's mean comment sentiment in a repository during a
#' month.
#'
#' @param data A data frame.
#' @param person,team,period,affect Names of the columns that identify the
#'   person, the team and the period, and that hold the affect score.
#'   `period` must contain whole numbers in which consecutive periods differ
#'   by one, for example `12 * year + month` for monthly data. Lags use this
#'   spacing, so a gap in a person's or a team's record produces a missing
#'   lag, never a lag reaching back across the gap.
#' @param weight Optional name of a column with positive weights, such as
#'   word counts. Weights enter every team and peer mean. If `NULL`, every row
#'   gets weight 1.
#'
#' @return A `pa_panel`: a `data.table` with columns `person`, `team`,
#'   `period`, `affect` and `weight`, keyed by person, team and period. The
#'   original column names are kept in the `pa_names` attribute.
#'
#' @details Rows with a missing affect score are dropped with a message,
#'   because a missing score means the person's affect was not observed.
#'   Missing values are never recoded to zero: on most sentiment scales zero
#'   is a substantive, neutral score.
#'
#'   The default outburst thresholds in [pa_outbursts()] assume scores on
#'   \[-1, 1\], the range of VADER compound scores. `pa_panel()` warns when
#'   scores fall outside that range.
#'
#' @examples
#' d <- data.frame(
#'   dev = c("a", "b", "a", "b"),
#'   repo = c("r1", "r1", "r1", "r1"),
#'   month = c(1, 1, 2, 2),
#'   vader = c(0.2, 0.4, -0.1, 0.3),
#'   words = c(10, 30, 12, 8)
#' )
#' pa_panel(d, person = "dev", team = "repo", period = "month",
#'          affect = "vader", weight = "words")
#' @export
pa_panel <- function(data, person, team, period, affect, weight = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  args <- list(person = person, team = team, period = period, affect = affect)
  for (nm in names(args)) .pa_check_name(args[[nm]], nm)
  if (!is.null(weight)) .pa_check_name(weight, "weight")

  cols <- c(person, team, period, affect, weight)
  absent <- setdiff(cols, names(data))
  if (length(absent) > 0L) {
    stop("Column(s) not found in `data`: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }

  n <- nrow(data)
  dt <- data.table::data.table(
    person = data[[person]],
    team = data[[team]],
    period = data[[period]],
    affect = data[[affect]],
    weight = if (is.null(weight)) rep(1, n) else data[[weight]]
  )

  if (anyNA(dt$person) || anyNA(dt$team) || anyNA(dt$period)) {
    stop("`person`, `team` and `period` must not contain missing values.",
         call. = FALSE)
  }
  if (!is.numeric(dt$period) ||
      any(abs(dt$period - round(dt$period)) > 1e-8)) {
    stop("`period` must hold whole numbers that increase by one per ",
         "period (e.g. 12 * year + month for monthly data).", call. = FALSE)
  }
  dt[, period := as.integer(round(period))]

  if (!is.numeric(dt$affect)) {
    stop("`affect` must be numeric.", call. = FALSE)
  }
  n_na <- sum(is.na(dt$affect))
  if (n_na > 0L) {
    message(n_na, " row(s) with a missing affect score dropped.")
    dt <- dt[!is.na(affect)]
  }
  if (nrow(dt) == 0L) {
    stop("No rows left after dropping missing affect scores.", call. = FALSE)
  }

  if (!is.numeric(dt$weight) || anyNA(dt$weight) ||
      any(!is.finite(dt$weight)) || any(dt$weight <= 0)) {
    stop("`weight` must be positive and finite in every row.", call. = FALSE)
  }

  if (anyDuplicated(dt, by = c("person", "team", "period")) > 0L) {
    stop("`data` has more than one row for some person, team and period. ",
         "Aggregate to one row per combination first.", call. = FALSE)
  }

  if (any(abs(dt$affect) > 1)) {
    warning("Some affect scores lie outside [-1, 1]. The default thresholds ",
            "in pa_outbursts() assume that range.", call. = FALSE)
  }

  data.table::setkeyv(dt, c("person", "team", "period"))
  data.table::setattr(dt, "pa_names", list(
    person = person, team = team, period = period,
    affect = affect, weight = weight
  ))
  data.table::setattr(dt, "class", c("pa_panel", "data.table", "data.frame"))
  dt
}
