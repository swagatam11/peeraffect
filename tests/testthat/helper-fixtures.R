# Small hand-checkable panels shared across tests.

fx_peer <- function() {
  data.frame(
    person = c("p1", "p2", "p1", "p3",
               "p1", "p2", "p4", "p1", "p1", "p5",
               "p1", "p2"),
    team   = c("A", "A", "B", "B",
               "A", "A", "A", "B", "C", "C",
               "A", "A"),
    period = c(1, 1, 1, 1,
               2, 2, 2, 2, 2, 2,
               4, 4),
    affect = c(0.2, 0.4, -0.2, 0.6,
               0.5, -0.1, 0.3, 0.1, 0.0, 1.0,
               0.8, 0.2),
    words  = c(1, 3, 2, 2,
               1, 1, 2, 1, 1, 1,
               1, 1)
  )
}

fx_outbursts <- function() {
  data.frame(
    person = c("a", "b", "a", "b",     # X: -0.6 -> 0.7, to_positive
               "c", "d", "c", "d",     # Y: 0.25 -> 0.96, starts neutral
               "e", "f", "e", "f",     # Z: 0.6 -> -0.7, to_negative
               "g", "g",               # W: one member only
               "h", "i", "h", "i"),    # V: gap between periods 1 and 3
    team   = c(rep("X", 4), rep("Y", 4), rep("Z", 4), rep("W", 2),
               rep("V", 4)),
    period = c(1, 1, 2, 2,  1, 1, 2, 2,  1, 1, 2, 2,  1, 2,  1, 1, 3, 3),
    affect = c(-0.6, -0.6, 0.7, 0.7,
               0.25, 0.25, 0.96, 0.96,
               0.6, 0.6, -0.7, -0.7,
               -0.9, 0.9,
               -0.9, -0.9, 0.9, 0.9)
  )
}

# J has an outburst in period 2. I shares only x with J; L shares only a
# with J; K shares both x and a with J, so K is not a triad.
fx_triads <- function() {
  data.frame(
    person = c("x", "a", "x", "b", "c",
               "x", "a", "x", "b", "c", "x", "a", "a", "d"),
    team   = c("J", "J", "I", "I", "I",
               "J", "J", "I", "I", "I", "K", "K", "L", "L"),
    period = c(1, 1, 1, 1, 1,
               2, 2, 2, 2, 2, 2, 2, 2, 2),
    affect = c(-0.8, -0.8, 0, 0, 0,
               0.9, 0.7, 0.2, 0.4, -0.2, 0.1, 0.3, 0.5, 0.1),
    words  = c(1, 1, 1, 1, 1,
               1, 1, 2, 1, 1, 1, 1, 1, 1)
  )
}

# Adds a second outburst team J2 that reaches I through y.
fx_triads_two_links <- function() {
  rbind(
    fx_triads(),
    data.frame(
      person = c("y", "e", "y", "e", "y"),
      team   = c("J2", "J2", "J2", "J2", "I"),
      period = c(1, 1, 2, 2, 2),
      affect = c(-0.9, -0.9, 0.8, 0.6, 0.0),
      words  = c(1, 1, 1, 3, 1)
    )
  )
}

as_panel <- function(d, weight = "words") {
  pa_panel(d, "person", "team", "period", "affect", weight = weight)
}
