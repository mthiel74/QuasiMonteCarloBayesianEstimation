#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  sequences.wls — the low-discrepancy intro figures.
      docs/images/point_sets.png       2-D scatter: MC vs Halton vs Sobol'
      docs/images/discrepancy.png       star-discrepancy decay vs N
    Pure visual / numerical motivation, independent of the Bayes problem.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "community", "qmc_bayes_helpers.wl"}];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];

(* ---- Figure: 256-point clouds, three methods ---- *)
nPts = 256;
BlockRandom[SeedRandom[1]; mc = LDPoints["MonteCarlo", nPts, 2]];
hal = LDPoints["Halton", nPts, 2];
sob = LDPoints["Sobol", nPts, 2];

panel[pts_, lab_, col_] := ListPlot[pts,
   PlotStyle -> Directive[col, PointSize[0.012]],
   AspectRatio -> 1, Frame -> True, FrameTicks -> None,
   PlotRange -> {{0, 1}, {0, 1}},
   GridLines -> {Range[0, 1, 1/4], Range[0, 1, 1/4]},
   GridLinesStyle -> Directive[GrayLevel[0.85]],
   PlotLabel -> Style[lab, 13, Bold, col], ImageSize -> 270];

pointFig = GraphicsRow[{
    panel[mc, "Monte Carlo (256)", RGBColor[0.75, 0.30, 0.15]],
    panel[hal, "Halton (256)", RGBColor[0.20, 0.45, 0.30]],
    panel[sob, "Sobol' (256)", RGBColor[0.15, 0.35, 0.65]]},
   Spacings -> 12, ImageSize -> 860];
Export[FileNameJoin[{imgDir, "point_sets.png"}], pointFig,
   ImageResolution -> 144];
Print["wrote point_sets.png"];

(* ---- Figure: L2 star-discrepancy decay ---- *)
Nvals = 2^Range[4, 12];
discMC = Table[
   Mean @ Table[
      StarDiscrepancyL2[RandomizedLDPoints["MonteCarlo", n, 2, 100 r + n]],
      {r, 20}],
   {n, Nvals}];
discHal = StarDiscrepancyL2[LDPoints["Halton", #, 2]] & /@ Nvals;
discSob = StarDiscrepancyL2[LDPoints["Sobol", #, 2]] & /@ Nvals;
Print["discrepancy slopes MC/Halton/Sobol: ",
   NumberForm[Coefficient[Fit[Transpose[{Log[Nvals], Log[#]}], {1, x}, x], x], {3, 2}] & /@
      {discMC, discHal, discSob}];

discFig = ListLogLogPlot[
   {Transpose[{Nvals, discMC}], Transpose[{Nvals, discHal}],
    Transpose[{Nvals, discSob}]},
   Joined -> True, Mesh -> All, PlotMarkers -> Automatic,
   PlotStyle -> {RGBColor[0.75, 0.30, 0.15], RGBColor[0.20, 0.45, 0.30],
                 RGBColor[0.15, 0.35, 0.65]},
   PlotLegends -> Placed[{"Monte Carlo  (\!\(\*SuperscriptBox[\(N\), \
\(-1/2\)]\))", "Halton", "Sobol'  (~\!\(\*SuperscriptBox[\(N\), \
\(-1\)]\))"}, {0.78, 0.82}],
   Frame -> True,
   FrameLabel -> {"number of points  N", "\!\(\*SubscriptBox[\(L\), \(2\)]\) star discrepancy  \!\(\*SuperscriptBox[\(D\), \(*\)]\)"},
   GridLines -> Automatic, GridLinesStyle -> Directive[GrayLevel[0.9]],
   ImageSize -> 620, AspectRatio -> 0.7,
   PlotLabel -> Style["Low-discrepancy sequences fill space more evenly", 13]];
Export[FileNameJoin[{imgDir, "discrepancy.png"}], discFig,
   ImageResolution -> 144];
Print["wrote discrepancy.png"];
