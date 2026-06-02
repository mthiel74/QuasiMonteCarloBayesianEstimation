#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  dimension.wls — the (log N)^d story.
      docs/images/dimension.png
    The QMC error bound carries a (log N)^d factor, so the advantage over
    Monte Carlo erodes as the dimension grows.  We integrate a smooth test
    function with a known exact value and measure, at a fixed budget N, how
    many times more accurate Sobol' is than MC -- the "efficiency gain" --
    as the dimension climbs from 2 to 12.

    Test integrand: f(x) = prod_i (1 + c cos(2 pi x_i)),  exact integral 1.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "community", "qmc_bayes_helpers.wl"}];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];

cAmp = 0.6;
fMean[pts_] := Mean[Times @@@ (1 + cAmp Cos[2 Pi pts])];   (* exact = 1 *)

dims = Range[2, 12];
R    = 120;

rmseAt[method_, d_, n_] := Sqrt @ Mean @ Table[
   (fMean[RandomizedLDPoints[method, n, d, 53 r + d]] - 1.)^2, {r, R}];

(* efficiency gain = MC RMSE / Sobol' RMSE, at two budgets *)
gainAt[n_] := Table[rmseAt["MonteCarlo", d, n]/rmseAt["Sobol", d, n], {d, dims}];
gain4k  = gainAt[4096];
gain64k = gainAt[65536];
Print["gain @4k  by d: ", NumberForm[gain4k, {4, 1}]];
Print["gain @64k by d: ", NumberForm[gain64k, {4, 1}]];

fig = ListLogPlot[
   {Transpose[{dims, gain64k}], Transpose[{dims, gain4k}]},
   Joined -> True, Mesh -> All, PlotMarkers -> {"\[FilledCircle]", 9},
   PlotStyle -> {RGBColor[0.15, 0.35, 0.65], RGBColor[0.20, 0.55, 0.35]},
   PlotLegends -> Placed[LineLegend[
      {RGBColor[0.15, 0.35, 0.65], RGBColor[0.20, 0.55, 0.35]},
      {"N = 65 536", "N = 4 096"}], {0.75, 0.82}],
   Frame -> True,
   FrameLabel -> {"dimension  d", "Sobol\[CloseCurlyQuote] efficiency gain  (MC RMSE / Sobol\[CloseCurlyQuote] RMSE)"},
   GridLines -> {Range[2, 12], Automatic},
   GridLinesStyle -> Directive[GrayLevel[0.92]],
   Epilog -> {Directive[GrayLevel[0.5], Dashed],
      Line[{{2, Log[1]}, {12, Log[1]}}],
      Text[Style["parity (no gain)", 10, GrayLevel[0.45]], {10.3, Log[1.25]}]},
   PlotLabel -> Style["The QMC advantage shrinks as dimension grows  \
(\!\(\*SuperscriptBox[\((log N)\), \(d\)]\) penalty)", 12],
   ImageSize -> 660, AspectRatio -> 0.66];

Export[FileNameJoin[{imgDir, "dimension.png"}], fig, ImageResolution -> 144];
Print["wrote dimension.png"];
