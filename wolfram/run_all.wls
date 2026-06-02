#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  run_all.wls — render every figure into docs/images/.
    Order matters: convergence.wls writes data/convergence_cache.mx,
    which payoff.wls consumes. *)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
scripts = {"sequences.wls", "convergence.wls", "dimension.wls",
   "pk_fit.wls", "synthetic.wls", "payoff.wls"};

Do[
   Print["=== ", s, " ==="];
   Get @ FileNameJoin[{repoRoot, "wolfram", s}],
   {s, scripts}];

Print["all figures rendered into ", FileNameJoin[{repoRoot, "docs", "images"}]];
