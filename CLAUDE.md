# Repo notes for Claude

## Purpose

Produce a Wolfram Community post — `community/qmc_bayes.nb` — showing
**quasi-Monte Carlo (QMC)** integration (Sobol' and Halton low-discrepancy
sequences) beating **plain Monte Carlo (MC)** on a *real* Bayesian
parameter-estimation problem: a 6-parameter two-compartment pharmacokinetic
(PK) model fit to the classic Theophylline data set.

The headline result is the empirical convergence law
`O(N^-1 (log N)^d)` for QMC vs `O(N^-1/2)` for MC, demonstrated on the
posterior expectations (model evidence and posterior parameter means),
together with the practical pay-off: QMC reaches a target accuracy with
roughly an order of magnitude fewer expensive model evaluations.

## Pipeline (pure Wolfram Language — no Python)

```
wolfram/fetch_theoph.wls   ── live-fetch the Theophylline data -> data/theoph.csv
                              (Rdatasets mirror of R's `datasets::Theoph`)

community/qmc_bayes_helpers.wl   the single canonical package:
   - Halton + Sobol' (Joe-Kuo) generators, RQMC randomizations
   - 2-compartment oral PK analytic model
   - lognormal priors, log-posterior, adaptive Gaussian proposal
   - QMC/MC importance-sampling estimators + convergence-study driver
   - LoadTheoph[] data loader
   (loaded both by the figure scripts and shipped with the notebook)

wolfram/sequences.wls      ── point-set scatter + star-discrepancy decay
wolfram/convergence.wls    ── RMSE vs N for evidence & posterior means (+ slopes)
wolfram/dimension.wls      ── the (log N)^d effect: convergence at d = 2,4,6
wolfram/pk_fit.wls         ── real Theoph fit: data, posterior predictive, marginals
wolfram/synthetic.wls      ── synthetic-truth recovery (known parameters)
wolfram/payoff.wls         ── model-evaluations-to-tolerance bar chart
wolfram/run_all.wls        ── one entry point, writes docs/images/*.png

community/build_notebook.wls   ── assembles qmc_bayes.nb (+ .pdf)
```

## Conventions

* Plain-text `.wls` / `.wl` is the source of truth; the `.nb` and `.pdf`
  in `community/` are committed *outputs* (for diff-review and so the
  Wolfram Community submission is trivial).
* `community/qmc_bayes_helpers.wl` is the ONE canonical package. The
  `wolfram/*.wls` figure scripts `Get` it via a relative path; the
  notebook ships it as an attachment and the Setup cell `Get`s it. No
  duplicated logic.
* Every long helper is *called* from the notebook with all arguments
  spelled out inline (the user requirement): the reader sees the full
  call and its arguments in the main notebook, the body lives in the
  package.
* Figures live in `docs/images/` only — referenced from both the README
  and the notebook.
* All HTTP fetches use `URLRead[HTTPRequest[...]]` and check the status
  code — `URLDownload` silently writes server error pages on 4xx/5xx.
* All figures are pure Wolfram Language; no external/AI-generated images.

## Numerics that matter (don't regress these)

* The PK concentration is ~0 at t=0 up to floating-point roundoff
  (~-1e-15). The log-posterior must accept tiny-negative predictions
  (`Chop` + `NumberQ && Im==0`), NOT reject on `>= 0`, or the optimizer's
  gradient dies on a flat penalty.
* Prior-as-proposal importance sampling collapses (ESS ~ 1) because the
  6-D prior is far more diffuse than an 11-point likelihood. We instead
  use an **adaptive Gaussian proposal** (Laplace mode + a short
  population-Monte-Carlo refinement of mean/covariance in log-parameter
  space). This is what makes ESS large and the QMC integrand smooth.
* RQMC: Sobol' uses a **digital shift** (XOR random bits — preserves the
  net structure); Halton uses a **Cranley-Patterson rotation** (add U
  mod 1). RMSE at each N is averaged over independent randomizations.

## Commit cadence

Commit + push after each meaningful step (scaffold, data fetcher,
package, each figure script, notebook). Keep messages short and factual.
