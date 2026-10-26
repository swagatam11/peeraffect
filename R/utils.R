.pa_check_name <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a single column name.", arg), call. = FALSE)
  }
  invisible(x)
}

.pa_assert_class <- function(x, cls, hint) {
  if (!inherits(x, cls)) {
    stop(sprintf("Expected a `%s` object; %s", cls, hint), call. = FALSE)
  }
  invisible(x)
}

.pa_scalar <- function(x, arg, lower = -Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < lower) {
    stop(sprintf("`%s` must be a single number of at least %s.", arg, lower),
         call. = FALSE)
  }
  invisible(x)
}

.pa_fe_terms <- function(fe, allowed) {
  if (is.null(fe) || length(fe) == 0L) return(character(0))
  bad <- setdiff(fe, names(allowed))
  if (length(bad) > 0L) {
    stop("Unknown fixed effect(s): ", paste(bad, collapse = ", "),
         ". Choose from: ", paste(names(allowed), collapse = ", "), ".",
         call. = FALSE)
  }
  unname(allowed[fe])
}

.pa_cluster <- function(cluster, allowed) {
  if (is.null(cluster)) return(NULL)
  bad <- setdiff(cluster, allowed)
  if (length(bad) > 0L) {
    stop("Unknown cluster variable(s): ", paste(bad, collapse = ", "),
         ". Choose from: ", paste(allowed, collapse = ", "), ".",
         call. = FALSE)
  }
  stats::as.formula(paste("~", paste(cluster, collapse = " + ")))
}

.pa_strip <- function(x) {
  # A plain data.frame for fixest, so that no subclass method interferes.
  y <- as.data.frame(x)
  attr(y, "pa_names") <- NULL
  y
}
