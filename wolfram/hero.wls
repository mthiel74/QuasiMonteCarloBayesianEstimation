#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  hero.wls — the cover animation.
      docs/images/hero.gif      (and hero.mp4)
    The post's thesis in motion: Monte Carlo and Sobol' points accumulate
    in the unit square (top), while the running error of a sample integral
    is traced beneath (bottom).  MC stays jittery and decays slowly;
    Sobol' fills evenly and the error plunges.  ~N^-1 vs ~N^-1/2, watched.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "community", "qmc_bayes_helpers.wl"}];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];

orange = RGBColor[0.85, 0.37, 0.13];
teal   = RGBColor[0.153, 0.51, 0.64];

(* smooth 2-D test integrand with known integral *)
fInt[pt_] := Exp[Total[pt]];           (* over [0,1]^2, exact (e-1)^2 *)
exact = (E - 1.)^2;

(* fixed master point sets; frame n uses the first n of each *)
nMax = 4096;
BlockRandom[SeedRandom[7]; mcAll = RandomReal[1, {nMax, 2}]];
sobAll = LDPoints["Sobol", nMax, 2];

frameNs = Round[2^Range[3, 12]];        (* 8 .. 4096, ten frames *)
runErr[pts_] := Abs[Mean[fInt /@ pts] - exact];

(* precompute the error traces over all frame counts *)
mcErr  = runErr[mcAll[[1 ;; #]]] & /@ frameNs;
sobErr = runErr[sobAll[[1 ;; #]]] & /@ frameNs;

cloud[pts_, col_, lab_, err_] := ListPlot[pts,
   PlotStyle -> Directive[col, PointSize[0.011], Opacity[0.85]],
   AspectRatio -> 1, Frame -> True, FrameTicks -> None,
   PlotRange -> {{0, 1}, {0, 1}},
   GridLines -> {Range[0, 1, 0.25], Range[0, 1, 0.25]},
   GridLinesStyle -> GrayLevel[0.88],
   PlotLabel -> Column[{
      Style[lab, 14, Bold, col],
      Style["error = " <> ToString[NumberForm[err, {3, 3}]] <>
            "   (N = " <> ToString[Length[pts]] <> ")", 10, GrayLevel[0.35]]},
      Alignment -> Center, Spacings -> 0.2],
   ImageSize -> 300];

errPanel[k_] := ListLogLogPlot[
   {Transpose[{frameNs[[1 ;; k]], mcErr[[1 ;; k]]}],
    Transpose[{frameNs[[1 ;; k]], sobErr[[1 ;; k]]}]},
   Joined -> True, Mesh -> All, PlotMarkers -> {"\[FilledCircle]", 7},
   PlotStyle -> {orange, teal},
   PlotRange -> {{6, 5000}, {3.*^-5, 0.5}},
   Frame -> True,
   FrameLabel -> {"number of points  N", "integration error"},
   GridLines -> Automatic, GridLinesStyle -> GrayLevel[0.92],
   PlotLegends -> Placed[
      LineLegend[{orange, teal}, {"Monte Carlo", "Sobol\[CloseCurlyQuote]"}],
      {0.8, 0.82}],
   ImageSize -> 620, AspectRatio -> 0.42];

frame[k_] := Rasterize @ Column[{
    Row[{cloud[mcAll[[1 ;; frameNs[[k]]]], orange, "Monte Carlo", mcErr[[k]]],
         cloud[sobAll[[1 ;; frameNs[[k]]]], teal, "Sobol\[CloseCurlyQuote]", sobErr[[k]]]},
        Spacer[14]],
    errPanel[k]},
   Alignment -> Center, Spacings -> 0.8];

frames = frame /@ Range[Length[frameNs]];
(* hold the last frame a beat *)
allFrames = Join[frames, {Last[frames], Last[frames]}];

anim = AnimatedImage[allFrames, FrameRate -> 1.4,
   AnimationRepetitions -> Infinity];

Export[FileNameJoin[{imgDir, "hero.gif"}], anim, "GIF"];
Print["wrote hero.gif  (", Length[allFrames], " frames)"];
Quiet @ Check[
   Export[FileNameJoin[{imgDir, "hero.mp4"}], anim, "MP4"];
   Print["wrote hero.mp4"], Print["mp4 export skipped"]];
Print["final errors  MC=", ScientificForm[Last[mcErr], 2],
   "  Sobol=", ScientificForm[Last[sobErr], 2]];
