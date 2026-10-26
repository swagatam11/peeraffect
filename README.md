# peeraffect

**Status: In development.** The interface may change without notice.

peeraffect estimates peer effects in affect/ sentiment, primarily diffusion
over interpersonal networks. It uses digital traces: does one
person's expressed affect move the affect of the people they work with? It
treats per-person, per-team, per-period affect scores, such as monthly
sentiment of developers' comments, as intensive longitudinal data in long
person × team × period format.

The package implements two designs:

* a fixed-effects linear-in-means model built on exclusive peer means and
lags, which measures how strongly affect co-moves within and across teams;
* a peer-of-peer instrumental-variable design built on **outbursts**, sharp
team-level swings in affect. An outburst in one team can reach a second
team only through the single person the two teams share, which separates
peer effects from correlated shocks and homophily.

## Installation

```r
# install.packages("remotes")
remotes::install_github("swagatam11/peeraffect")
```

## Usage

```r
library(peeraffect)

p  <- pa_panel(df, person = "dev", team = "repo", period = "month",
               affect = "sentiment", weight = "words")

# Correlation: fixed-effects linear-in-means
pm <- pa_peer_means(p)
pa_fit_fe(pm)

# Causal: outburst-based peer-of-peer IV
ob <- pa_outbursts(p)          # thresholds and neutral band are arguments
tr <- pa_triads(p, ob)
fit <- pa_fit_iv(tr)
fixest::fitstat(fit, "ivf1")   # first-stage F
```

## Background

The design builds on Sinha (2022), "'Are You Listening?': Sentiment Diffusion
in GitHub", *Academy of Management Proceedings*, also chapter 8 of the
author's ETH Zurich doctoral thesis. This package is a fresh implementation 
which builds on that work, primarily to enable others to implement "outburst" based IV design in 
interpersonal affective networks

## Research agenda

1. **Identification.** Affective peer effects in endogenous, repeatedly
measured networks.
2. **Measurement.** How error in text-derived affect propagates into
peer-effect estimates, and how validation and agreement checks should be
built into the analysis.

## Planned next

* A simulation of team-network panels with a known peer effect, correlated
team shocks, and measurement error set by within- and between-person
reliability and by kappa for outburst classification.
* A Monte Carlo helper reporting bias, RMSE, coverage and power across
reliability values.
* A multilevel comparison model via lme4.
* A vignette.

## Roadmap

* Agreement tools for machine versus human affect coding.
* Heterogeneous susceptibility to peers' affect (person × situation).
* A Bayesian multilevel linear-in-means model.
* Informative missingness: silence is not random.
* A validation workflow for a human-coded subsample.

## Citation

Run `citation("peeraffect")` in R.

## License

MIT © Swagatam Sinha

