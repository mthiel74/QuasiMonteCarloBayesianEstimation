#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  payoff.wls — the practical pay-off figure.
      docs/images/payoff.png
    For the posterior-predictive concentration (each integrand evaluation
    is one forward PK model solve), how many model evaluations does each
    method need to reach a given target accuracy?  Fit RMSE = C N^p to the
    measured curves and invert: N(tol) = (C/tol)^(1/|p|).  Because MC needs
    O(tol^-2) and QMC O(tol^-1), the speed-up grows as the accuracy demand
    tightens -- shown as evaluation-count curves vs target accuracy.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "community", "qmc_bayes_helpers.wl"}];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];

{Nlist, rmseCL, rmsePr, refCL, refPred} =
   Import[FileNameJoin[{repoRoot, "data", "convergence_cache.mx"}]];

fitPow[e_] := Module[{c = Fit[Transpose[{Log[Nlist], Log[e]}], {1, x}, x]},
   {Exp[c /. x -> 0], Coefficient[c, x]}];
nForTol[e_, tol_] := With[{cp = fitPow[e]}, (cp[[1]]/tol)^(1/(-cp[[2]]))];

methods = {"MonteCarlo", "Sobol", "Halton"};
cols = {RGBColor[0.75, 0.30, 0.15], RGBColor[0.15, 0.35, 0.65],
        RGBColor[0.20, 0.55, 0.35]};

(* target relative accuracy on the ~7.7 mg/L predictive concentration *)
relTols = 10.^Range[-1.0, -3.0, -0.1];                (* 10% down to 0.1% *)
absTols = relTols refPred;
evalCurve[m_] := nForTol[rmsePr[m], #] & /@ absTols;
curves = evalCurve /@ methods;

(* speed-ups at two representative accuracies *)
spd[rel_] := With[{t = rel refPred},
   nForTol[rmsePr["MonteCarlo"], t]/nForTol[rmsePr["Sobol"], t]];
Print["Sobol speed-up @1% / 0.5% / 0.1%: ",
   NumberForm[{spd[0.01], spd[0.005], spd[0.001]}, 3]];

fig = ListLogLogPlot[
   MapThread[Transpose[{100 relTols, #}] &, {curves}],
   Joined -> True, PlotStyle -> cols, Mesh -> None,
   PlotLegends -> Placed[LineLegend[cols,
      {"Monte Carlo  (N \[Proportional] tol\!\(\*SuperscriptBox[\(\), \(-2\)]\))",
       "Sobol'  (N \[Proportional] tol\!\(\*SuperscriptBox[\(\), \(-1\)]\))",
       "Halton"}], {0.72, 0.28}],
   Frame -> True,
   FrameLabel -> {"target accuracy on E[C(6 h)]  (% RMSE)",
      "forward model solves required"},
   ScalingFunctions -> {"Reciprocal", "Log"},
   GridLines -> Automatic, GridLinesStyle -> Directive[GrayLevel[0.92]],
   Epilog -> {
     Directive[GrayLevel[0.4]],
     Text[Style[ToString[NumberForm[spd[0.001], 3]] <> "\[Times] fewer\nat 0.1%",
        11, Bold], Scaled[{0.30, 0.78}]]},
   PlotLabel -> Style[
     "QMC pay-off grows with the accuracy you demand", 13],
   ImageSize -> 640, AspectRatio -> 0.74];

Export[FileNameJoin[{imgDir, "payoff.png"}], fig, ImageResolution -> 144];
Print["wrote payoff.png"];
