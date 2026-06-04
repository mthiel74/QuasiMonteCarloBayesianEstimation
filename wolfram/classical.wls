#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  classical.wls — classical vs Bayesian fit of the same data.
      docs/images/classical.png
    Fit Subject 1 with classical nonlinear least squares (NonlinearModelFit),
    extract the point estimate and its asymptotic confidence band, and show
    it beside the Bayesian posterior-predictive band.  The point is not that
    one is "right" -- it is what each delivers and what it costs.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "problem.wl"}];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];

orange = RGBColor[0.85, 0.37, 0.13];
teal   = RGBColor[0.153, 0.51, 0.64];

tObs = subj1["t"]; cObs = subj1["c"]; dose = subj1["dose"];
data = Transpose[{tObs, cObs}];

(* ---- classical nonlinear least squares ---- *)
model = PKConcentration[t, {ka, CL, V1, Q, V2}, dose];
nlm = NonlinearModelFit[data, model, {{ka, 1.5}, {CL, 2.8}, {V1, 30},
   {Q, 2.0}, {V2, 20}}, t, MaxIterations -> 500];
nlsPars   = nlm["BestFitParameters"];
nlsErrors = Quiet @ nlm["ParameterErrors"];
Print["NLS estimates: ", NumberForm[{ka, CL, V1, Q, V2} /. nlsPars, {4, 3}]];
Print["NLS std errors: ", NumberForm[nlsErrors, {4, 3}]];

tGrid = Subdivide[0.05, 25., 200];
nlsCurve = nlm[#] & /@ tGrid;

(* ---- Bayesian posterior-predictive band ---- *)
draws = PosteriorDraws[LDPoints["Sobol", 4096, 6], pkLaplace];
curves = ConcentrationCurves[draws, tGrid, dose];
bandT = Transpose[curves];
bLo = Quantile[#, 0.05] & /@ bandT;
bHi = Quantile[#, 0.95] & /@ bandT;
bMed = Quantile[#, 0.50] & /@ bandT;

(* Classical: a single best-fit line.  Its parameter standard errors blow
   up (CL, V1, V2 ~ 1e6-1e7) because a 2-compartment model is not
   identifiable from one subject -- so there is no usable uncertainty band. *)
leftPanel = ListLinePlot[Transpose[{tGrid, nlsCurve}],
   PlotStyle -> Directive[orange, Thickness[0.006]],
   Frame -> True, FrameLabel -> {"time (h)", "conc (mg/L)"},
   PlotLabel -> Style["Classical least squares: one curve, no usable error bars",
      11, orange],
   PlotRange -> {{0, 25}, {0, 11}}, ImageSize -> 380, AspectRatio -> 0.85,
   Epilog -> {Directive[orange, PointSize[0.016]], Point[data]}];

rightPanel = ListLinePlot[Transpose[{tGrid, bMed}],
   PlotStyle -> Directive[teal, Thickness[0.006]],
   Frame -> True, FrameLabel -> {"time (h)", "conc (mg/L)"},
   PlotLabel -> Style["Bayesian posterior (QMC): a full uncertainty band", 11, teal],
   PlotRange -> {{0, 25}, {0, 11}}, ImageSize -> 380, AspectRatio -> 0.85,
   Epilog -> {Directive[Opacity[0.18], teal],
      Polygon[Join[Transpose[{tGrid, bLo}], Reverse@Transpose[{tGrid, bHi}]]],
      Directive[teal, PointSize[0.016]], Point[data]}];

fig = GraphicsRow[{leftPanel, rightPanel}, Spacings -> 18, ImageSize -> 780];
Export[FileNameJoin[{imgDir, "classical.png"}], fig, ImageResolution -> 144];
Print["wrote classical.png"];

(* parameter comparison table for the prose *)
bayesMean = LaplaceMeanExact[pkLaplace];
bayesSD = Sqrt @ Table[
   Module[{d = PosteriorDraws[LDPoints["Sobol", 8192, 6], pkLaplace][[All, j]]},
      Variance[d]], {j, 5}];
Print["param   NLS+-SE                Bayes mean+-SD"];
Do[Print[{"ka","CL","V1","Q","V2"}[[j]], "  ",
   NumberForm[({ka,CL,V1,Q,V2}/.nlsPars)[[j]], {4,3}], " +- ",
   NumberForm[nlsErrors[[j]], {4,3}], "    ",
   NumberForm[bayesMean[[j]], {4,3}], " +- ", NumberForm[bayesSD[[j]], {4,3}]],
   {j, 5}];
