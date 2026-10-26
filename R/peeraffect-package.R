#' peeraffect: peer effects in affect on team networks
#'
#' Tools for intensive longitudinal affect data from digital traces, held in
#' long person-by-team-by-period format. The workflow is:
#'
#' 1. [pa_panel()] validates the data.
#' 2. [pa_peer_means()] adds exclusive peer means and lags.
#' 3. [pa_fit_fe()] fits the fixed-effects linear-in-means model.
#' 4. [pa_outbursts()] flags team-level affect outbursts.
#' 5. [pa_triads()] builds intransitive triads around outbursts.
#' 6. [pa_fit_iv()] fits the peer-of-peer instrumental-variable model.
#'
#' The design builds on Sinha (2022), "'Are You Listening?': Sentiment
#' Diffusion in GitHub", *Academy of Management Proceedings*, which is also
#' chapter 8 of the author's ETH Zurich doctoral thesis.
#'
#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom stats as.formula
NULL

utils::globalVariables(c(
  ".N", "person", "team", "period", "affect", "weight", "wy",
  "t_sw", "t_swy", "t_n", "c_sw", "c_swy", "c_n",
  "p_sw", "p_swy", "p_n", "q_sw", "q_swy", "q_n",
  "l_affect", "l_w", "l_wy", "l_c_sw", "l_c_swy", "l_c_n", "l_present",
  "lt_sw", "lt_swy", "lp_sw", "lp_swy", "lp_n", "lq_sw", "lq_swy", "lq_n",
  "n_team", "peer_mean", "peer_mean_lag", "own_lag", "own_other",
  "own_other_lag", "peer_other", "peer_other_lag",
  "team_mean", "team_mean_lag", "team_weight", "n_members", "up", "down",
  "outburst", "outburst_type", "outburst_team", "peer_team",
  "ob_affect", "ob_weight", "link_affect", "link_weight", "n_shared",
  "instrument", "instrument_weight", "outcome", "outcome_weight",
  "l_swy", "n_links", "n_outburst_teams"
))
