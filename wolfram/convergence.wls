#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  convergence.wls — the headline figure.
      docs/images/convergence.png
    RMSE vs N (log-log) for two posterior expectations of the real
    Theophylline fit, computed against the Laplace-Gaussian posterior:
      left:  E[CL]  -- analytic reference exp(mu + Sigma/2)
      right: posterior-predictive concentration at t = 6 h,
             high-N Sobol' reference (no closed form)
    Sobol' / Halton / Monte Carlo, with fitted slopes and the
    O(N^-1/2) and O(N^-1) reference guides.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "problem.wl"}];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];

Nlist = 2^Range[7, 15];
R     = 200;

(* quantity 1: posterior mean clearance CL (= theta_2). Analytic ref. *)
gCL    = #[[All, 2]] &;
refCL  = LaplaceMeanExact[pkLaplace][[2]];

(* quantity 2: posterior-predictive concentration at t* = 6 h.
   Each integrand evaluation is a forward PK model solve. *)
tStar  = 6.0;
gPredVec = Function[draws, Flatten @ ConcentrationCurves[draws, {tStar}, subj1["dose"]]];
refPred = Mean @ gPredVec[PosteriorDraws[LDPoints["Sobol", 2^19, 6], pkLaplace]];

rmseCL  = Association[# -> ExpectationRMSE[#, Nlist, R, pkLaplace, gCL, refCL] & /@
            {"MonteCarlo", "Sobol", "Halton"}];
rmsePr  = Association[# -> ExpectationRMSE[#, Nlist, R, pkLaplace, gPredVec, refPred] & /@
            {"MonteCarlo", "Sobol", "Halton"}];

slope[e_] := Coefficient[Fit[Transpose[{Log[Nlist], Log[e]}], {1, x}, x], x];

cols = <|"MonteCarlo" -> RGBColor[0.75, 0.30, 0.15],
         "Sobol" -> RGBColor[0.15, 0.35, 0.65],
         "Halton" -> RGBColor[0.20, 0.55, 0.35]|>;

mkPlot[rmse_, ylab_, ttl_, refval_] := ListLogLogPlot[
   Table[Transpose[{Nlist, rmse[m]}], {m, {"MonteCarlo", "Sobol", "Halton"}}],
   Joined -> True, Mesh -> All, PlotMarkers -> {"\[FilledCircle]", 8},
   PlotStyle -> Values[cols],
   Prolog -> {
     Directive[GrayLevel[0.55], Dashed],
     Line[{{Log[Nlist[[1]]], Log[rmse["MonteCarlo"][[1]]]},
           {Log[Last[Nlist]], Log[rmse["MonteCarlo"][[1]]]
              - 0.5 (Log[Last[Nlist]] - Log[Nlist[[1]]])}}],
     Directive[GrayLevel[0.55], Dotted],
     Line[{{Log[Nlist[[1]]], Log[rmse["Sobol"][[1]]]},
           {Log[Last[Nlist]], Log[rmse["Sobol"][[1]]]
              - 1.0 (Log[Last[Nlist]] - Log[Nlist[[1]]])}}]},
   PlotLegends -> Placed[LineLegend[Values[cols],
     {"Monte Carlo  (" <> ToString[NumberForm[slope[rmse["MonteCarlo"]], {3, 2}]] <> ")",
      "Sobol'  (" <> ToString[NumberForm[slope[rmse["Sobol"]], {3, 2}]] <> ")",
      "Halton  (" <> ToString[NumberForm[slope[rmse["Halton"]], {3, 2}]] <> ")"}],
     {0.31, 0.2}],
   Frame -> True, FrameLabel -> {"number of points  N", ylab},
   GridLines -> Automatic, GridLinesStyle -> Directive[GrayLevel[0.92]],
   PlotLabel -> Style[ttl, 13], ImageSize -> 560, AspectRatio -> 0.78];

fig = GraphicsRow[{
    mkPlot[rmseCL, "RMSE of  E[CL]  (L/h)",
       "Posterior mean clearance  (analytic ref)", refCL],
    mkPlot[rmsePr, "RMSE of  E[C(6 h)]  (mg/L)",
       "Posterior-predictive conc. at 6 h", refPred]},
   Spacings -> 20, ImageSize -> 1120];

Export[FileNameJoin[{imgDir, "convergence.png"}], fig, ImageResolution -> 144];
Print["wrote convergence.png"];
Print["refCL=", refCL, "  refPred=", refPred];
Print["E[CL]  slopes MC/Sobol/Halton: ",
   NumberForm[slope[rmseCL[#]], {3, 2}] & /@ {"MonteCarlo", "Sobol", "Halton"}];
Print["E[C6]  slopes MC/Sobol/Halton: ",
   NumberForm[slope[rmsePr[#]], {3, 2}] & /@ {"MonteCarlo", "Sobol", "Halton"}];

Export[FileNameJoin[{repoRoot, "data", "convergence_cache.mx"}],
   {Nlist, rmseCL, rmsePr, refCL, refPred}];
