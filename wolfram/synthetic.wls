#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  synthetic.wls — correctness on synthetic truth.
      docs/images/synthetic.png
    Generate concentrations from KNOWN parameters, fit, and show the
    QMC posterior means (with 90% credible intervals) recover the truth.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "problem.wl"}];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];

thetaTrue = {1.20, 2.50, 30.0, 1.80, 22.0, 0.45};   (* ka,CL,V1,Q,V2,sigma *)
doseSyn   = 320.0;
tDesign   = {0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 20, 24, 30};

{synProblem, synConc} = SyntheticProblem[thetaTrue, doseSyn, tDesign,
   priorMedians, priorLogSD, 4242];
synLaplace = LaplacePosterior[synProblem];

draws  = PosteriorDraws[LDPoints["Sobol", 8192, 6], synLaplace];
postMu = Mean /@ Transpose[draws];
postLo = Quantile[#, 0.05] & /@ Transpose[draws];
postHi = Quantile[#, 0.95] & /@ Transpose[draws];

names = {"ka", "CL", "V1", "Q", "V2", "\[Sigma]"};

(* recovery panel: normalize each parameter by its true value so all six
   fit on one axis; truth sits at 1.  Parameters run bottom-to-top ka..sigma. *)
relMu = postMu/thetaTrue; relLo = postLo/thetaTrue; relHi = postHi/thetaTrue;
recovery = ListPlot[
   Table[{relMu[[j]], j}, {j, 6}],
   PlotStyle -> Directive[RGBColor[0.15, 0.35, 0.65], PointSize[0.02]],
   Frame -> True,
   FrameTicks -> {{Table[{j, names[[j]]}, {j, 6}], None}, {Automatic, None}},
   PlotRange -> {{0.25, 2.55}, {0.5, 6.5}},
   GridLines -> {{1.0}, None},
   GridLinesStyle -> Directive[RGBColor[0.8, 0.2, 0.1], Thick, Dashed],
   Epilog -> {Directive[RGBColor[0.15, 0.35, 0.65], Thick],
      Table[Line[{{relLo[[j]], j}, {relHi[[j]], j}}], {j, 6}],
      Directive[RGBColor[0.15, 0.35, 0.65], PointSize[0.02]],
      Point[Table[{relMu[[j]], j}, {j, 6}]]},
   FrameLabel -> {"posterior estimate / true value", None},
   PlotLabel -> Style["Synthetic-truth recovery (90% CI; truth = red line)", 11],
   ImageSize -> 620, AspectRatio -> 0.55];

Export[FileNameJoin[{imgDir, "synthetic.png"}], recovery, ImageResolution -> 144];
Print["wrote synthetic.png"];
Print["true:   ", NumberForm[thetaTrue, {4, 3}]];
Print["postMu: ", NumberForm[postMu, {4, 3}]];
