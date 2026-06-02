#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  pk_fit.wls — the real Bayesian fit figures.
      docs/images/pk_fit.png        data + posterior-predictive band
      docs/images/posterior.png     parameter marginals (Laplace posterior)
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "problem.wl"}];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];

(* posterior-predictive band via a Sobol' draw from the Laplace posterior *)
draws = PosteriorDraws[LDPoints["Sobol", 4096, 6], pkLaplace];
tGrid = Subdivide[0.0, 25.0, 200];
curves = ConcentrationCurves[draws, tGrid, subj1["dose"]];
band = Transpose[curves];
qLo  = Quantile[#, 0.05] & /@ band;
qHi  = Quantile[#, 0.95] & /@ band;
qMed = Quantile[#, 0.50] & /@ band;

predBand = ListLinePlot[
   {Transpose[{tGrid, qMed}]},
   PlotStyle -> Directive[RGBColor[0.15, 0.35, 0.65], Thickness[0.006]],
   Filling -> None,
   Epilog -> {
     Directive[Opacity[0.18], RGBColor[0.15, 0.35, 0.65]],
     Polygon[Join[Transpose[{tGrid, qLo}], Reverse@Transpose[{tGrid, qHi}]]],
     Directive[RGBColor[0.80, 0.20, 0.10], PointSize[0.014]],
     Point[Transpose[{subj1["t"], subj1["c"]}]]},
   Frame -> True,
   FrameLabel -> {"time after dose  (h)", "theophylline conc.  (mg/L)"},
   PlotLabel -> Style["Theophylline Subject 1: data + 90% posterior-predictive band", 12],
   PlotRange -> {{0, 25}, {0, All}}, ImageSize -> 620, AspectRatio -> 0.62];

Export[FileNameJoin[{imgDir, "pk_fit.png"}], predBand, ImageResolution -> 144];
Print["wrote pk_fit.png"];

(* ---- parameter marginals from the Laplace posterior ---- *)
names = {"ka  (1/h)", "CL  (L/h)", "V1  (L)", "Q  (L/h)", "V2  (L)", "\[Sigma]  (mg/L)"};
margins = Table[
   Histogram[draws[[All, j]], 30,
      ChartStyle -> Directive[RGBColor[0.15, 0.35, 0.65], Opacity[0.6]],
      Frame -> True, FrameTicks -> {Automatic, None},
      PlotLabel -> Style[names[[j]], 11], ImageSize -> 250,
      AspectRatio -> 0.8],
   {j, 6}];
postFig = Grid[Partition[margins, 3], Spacings -> {1, 1}];
Export[FileNameJoin[{imgDir, "posterior.png"}], postFig, ImageResolution -> 144];
Print["wrote posterior.png"];

(* ---- validate Laplace vs full-posterior importance sampling ---- *)
estIS = ImportanceEstimate[LDPoints["Sobol", 2^16, 6], pkProposal, pkProblem];
Print["Laplace E[theta]: ", NumberForm[LaplaceMeanExact[pkLaplace], {4, 3}]];
Print["full-post  E[theta]: ", NumberForm[estIS["postMean"], {4, 3}]];
Print["IS ESS fraction: ", NumberForm[estIS["essFraction"], {4, 3}]];
