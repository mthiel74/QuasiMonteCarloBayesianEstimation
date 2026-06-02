(* ::Package:: *)

(*  problem.wl — the canonical Bayesian PK problem used by every figure
    script.  Centralises the data, the lognormal prior hyper-parameters,
    and the Laplace-Gaussian posterior so all figures refer to the same
    object.  The notebook reproduces these exact calls inline (Setup and
    the "real fit" cells of build_notebook.wls).

    A figure script only needs:
        Get @ FileNameJoin[{repoRoot, "wolfram", "problem.wl"}]
*)

problemRepoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{problemRepoRoot, "community", "qmc_bayes_helpers.wl"}];

(* Prior medians and log-SDs for {ka, CL, V1, Q, V2, sigma}. *)
priorMedians = {1.5, 3.0, 35.0, 2.0, 25.0, 1.0};
priorLogSD   = {0.6, 0.5, 0.5, 0.7, 0.7, 0.5};

(* ---- the real fit: Theophylline Subject 1 ---- *)
theophData = LoadTheoph[];
subj1      = SelectSubject[theophData, 1];

pkProblem = BayesProblem[subj1["dose"], subj1["t"], subj1["c"],
   priorMedians, priorLogSD];

(* Laplace-Gaussian posterior (mode + curvature) in log-parameter space.
   Posterior expectations against this Gaussian are the integrals whose
   QMC vs MC convergence the project measures. *)
pkLaplace = LaplacePosterior[pkProblem];

(* Full-posterior importance-sampling proposal, used only to validate the
   Laplace approximation and for the practical pay-off section. *)
pkProposal = FitProposal[pkProblem];
