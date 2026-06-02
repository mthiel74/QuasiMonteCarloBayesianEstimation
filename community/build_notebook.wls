#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/qmc_bayes.nb — the educational long-form notebook for
    the Wolfram Community submission "Quasi-Monte Carlo for a real
    Bayesian estimation".

    Architecture (same pattern as the ENSO-emergence builder): accumulate
    fully-formed Cell expressions into one list, then CreateDocument +
    NotebookSave so Image cells serialise to disk as binary rasters.

    Sections
    --------
      Abstract
      Setup
      1. The problem: averaging, and the square-root wall
      2. Low-discrepancy sequences (Halton, Sobol')
      3. A real Bayesian problem: pharmacokinetics
      4. The fit: posterior of a Theophylline subject
      5. The convergence law (the headline)
      6. The (log N)^d dimension dependence
      7. Correctness: synthetic-truth recovery
      8. The practical pay-off
      9. Conclusions
     10. References
     11. Reproducibility
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
outNb    = FileNameJoin[{repoRoot, "community", "qmc_bayes.nb"}];
outPdf   = FileNameJoin[{repoRoot, "community", "qmc_bayes.pdf"}];

(* ==================================================================
   Cell-builder helpers
   ================================================================== *)

title[txt_]    := Cell[txt, "Title"];
subtitle[txt_] := Cell[txt, "Subtitle"];
hd1[txt_]      := Cell[txt, "Section"];
hd2[txt_]      := Cell[txt, "Subsection"];

para[stuff_] := Cell[TextData[stuff], "Text", CellMargins -> {{50, 50}, {6, 6}}];

abstractCell[stuff_] := Cell[TextData[stuff], "Text",
   FontSize -> 14, CellMargins -> {{60, 60}, {6, 6}},
   Background -> RGBColor[0.98, 0.96, 0.90]];

bold[s_] := StyleBox[s, FontWeight -> Bold];
ital[s_] := StyleBox[s, FontSlant -> Italic];
math[s_] := StyleBox[s, FontFamily -> "Times", FontSlant -> Italic];
mono[s_] := StyleBox[s, FontFamily -> "Courier"];

imath[boxes_] := Cell[BoxData @ FormBox[boxes, TraditionalForm], "InlineFormula"];
sub[base_, k_] := imath @ SubscriptBox[base, k];
pow[base_, e_] := imath @ SuperscriptBox[base, e];           (* base^e *)
powN[e_] := imath @ SuperscriptBox["N", e];                  (* N^e *)
bigO[boxes_] := imath @ RowBox[{"O", "(", boxes, ")"}];

displayMath[expr_] := Cell[BoxData @ FormBox[expr, TraditionalForm],
   "DisplayFormula", CellMargins -> {{80, 50}, {8, 8}}];

(* Static image cell — Import + Image[..., ImageSize] so the FE serialises
   a compressed raster into the .nb. *)
imgWidth = 680;
imgCell[file_] := Module[{path = FileNameJoin[{imgDir, file}], im},
   If[FileExistsQ[path],
     im = Image[Import[path], ImageSize -> imgWidth];
     Cell[BoxData @ ToBoxes[im], "Output", ShowCellLabel -> False,
        TextAlignment -> Center, CellMargins -> {{Automatic, Automatic}, {10, 6}}],
     Cell[TextData[{bold["[ missing figure: " <> file <> " ]"]}], "Text",
        FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]]];

captionCell[args__] := Cell[TextData[{args}], "Text",
   FontSlant -> Italic, FontSize -> 11, FontColor -> GrayLevel[0.35],
   CellMargins -> {{70, 70}, {2, 14}}];

codeIn[code_String] := Cell[code, "Program",
   CellMargins -> {{60, 30}, {6, 6}}, FontFamily -> "Courier", FontSize -> 11,
   ShowStringCharacters -> False, Background -> RGBColor[0.95, 0.96, 0.98]];

wlIn[code_String] := Cell[code, "Input",
   CellMargins -> {{60, 30}, {6, 6}}, FontSize -> 11];

link[txt_String, url_String] := Sequence[
   StyleBox[txt, FontWeight -> "SemiBold"],
   " (", StyleBox[url, FontFamily -> "Courier", FontSize -> 10], ")"];

calloutCell[stuff_, bg_:RGBColor[0.93, 0.95, 0.99], frame_:RGBColor[0.45, 0.55, 0.70]] :=
   Cell[TextData[stuff], "Text", FontSize -> 11, FontColor -> GrayLevel[0.25],
      CellMargins -> {{70, 70}, {6, 4}}, Background -> bg,
      CellFrame -> {{4, 0}, {0, 0}}, CellFrameColor -> frame];

$allCells = {};
writeAll[cs_List] := ($allCells = Join[$allCells, cs];
   Print["  +", Length[cs], " cells (total ", Length[$allCells], ")"]);


(* ==================================================================
   TITLE & ABSTRACT
   ================================================================== *)

writeAll[{

title["Quasi-Monte Carlo for a real Bayesian estimation"],

subtitle["Sobol\[CloseCurlyQuote] and Halton sequences versus plain Monte \
Carlo for a six-parameter pharmacokinetic inference \[LongDash] measuring \
the near-1/N convergence of quasi-Monte Carlo against Monte \
Carlo\[CloseCurlyQuote]s 1/\[Sqrt]N"],

Cell["Wolfram Community submission \[LongDash] June 2026", "Subsubtitle",
   FontSlant -> Italic, FontColor -> GrayLevel[0.4]],

hd1["Abstract"],

abstractCell[{"Almost every number a Bayesian reports \[LongDash] a \
posterior mean, a credible interval, a model evidence, a predicted \
concentration \[LongDash] is an ", bold["integral"], " over the parameter \
space. The default way to evaluate such integrals is Monte Carlo (MC) \
averaging, whose error falls only as ",
   imath@RowBox[{"O", "(", SuperscriptBox["N", RowBox[{"-", "1", "/", "2"}]], ")"}],
   ": each extra correct digit costs a hundred-fold more samples."}],

abstractCell[{bold["Quasi-Monte Carlo (QMC)"], " replaces the random \
sample with a deterministic ", ital["low-discrepancy sequence"],
   " \[LongDash] Sobol\[CloseCurlyQuote] or Halton points that fill the \
unit cube far more evenly. For smooth integrands the error then falls \
almost as fast as ",
   imath@RowBox[{"O", "(", SuperscriptBox["N", RowBox[{"-", "1"}]],
      SuperscriptBox[RowBox[{"(", "log", " ", "N", ")"}], "d"], ")"}],
   " \[LongDash] nearly one full power of ", math["N"], " better."}],

abstractCell[{"We put this to work on a genuine inference problem: a ",
   bold["two-compartment pharmacokinetic model"], " (six parameters \[Dash] \
absorption rate, clearance, two volumes, inter-compartmental clearance, \
residual scatter) fit to the classic ", bold["Theophylline"], " data set. \
We implement Halton and Sobol\[CloseCurlyQuote] from scratch, build the \
posterior, and measure the convergence of posterior expectations. The \
result is the textbook gap, on real data: MC slopes of ",
   imath@RowBox[{"\[TildeTilde]", RowBox[{"-", "0.5"}]}],
   " against Sobol\[CloseCurlyQuote] slopes of ",
   imath@RowBox[{"\[TildeTilde]", RowBox[{"-", "0.95"}]}],
   ", and an order-of-magnitude reduction in the number of forward-model \
solves needed for a target accuracy."}],

abstractCell[{ital["Audience:"], " each section opens with the idea in \
plain terms, then deepens into the equations, the Wolfram Language code, \
and the measured numbers. Every figure is pre-rendered, so the post reads \
end-to-end without evaluating anything; every code cell is runnable if you \
load the attached package."}]
}];


(* ==================================================================
   SETUP
   ================================================================== *)

writeAll[{

hd1["Setup (optional \[LongDash] only needed if you want to run the code)"],

para[{ital["You can ignore this section entirely if you just want to read \
the post."], " Every figure is pre-rendered into the notebook as a static \
image."}],

para[{"If you ", ital["do"], " want to run the code, the generators \
(Halton, Sobol\[CloseCurlyQuote]), the pharmacokinetic model, the priors, \
the Laplace posterior, and the estimators all live in a single attached \
package, ", mono["qmc_bayes_helpers.wl"], ". Run the cell below once after \
opening the notebook."}],

wlIn["(* Put qmc_bayes_helpers.wl in the same folder as this notebook,
   then shift-Enter. *)
SetDirectory[NotebookDirectory[]];
Get[\"qmc_bayes_helpers.wl\"];"],

para[{"The two cells below define the objects every later section reuses: \
the prior, the real data, the Bayesian problem, and its Laplace\[Dash]\
Gaussian posterior. They are spelled out here so that wherever a later \
cell calls a helper, you can see exactly what was passed in."}],

wlIn["(* Lognormal-prior hyper-parameters for {ka, CL, V1, Q, V2, sigma}:
   medians and log-scale standard deviations. *)
priorMedians = {1.5, 3.0, 35.0, 2.0, 25.0, 1.0};
priorLogSD   = {0.6, 0.5, 0.5, 0.7, 0.7, 0.5};

(* The real data: Theophylline Subject 1 (single 320 mg oral dose). *)
theoph = LoadTheoph[];
subj1  = SelectSubject[theoph, 1];

(* The Bayesian problem and its Laplace-Gaussian posterior. *)
pkProblem = BayesProblem[subj1[\"dose\"], subj1[\"t\"], subj1[\"c\"],
   priorMedians, priorLogSD];
pkLaplace = LaplacePosterior[pkProblem];"]
}];


(* ==================================================================
   1.  THE PROBLEM
   ================================================================== *)

writeAll[{

hd1["1.  The problem: averaging, and the square-root wall"],

para[{ital["The everyday version."], " Suppose you want the average value \
of some quantity over a high-dimensional space \[LongDash] say the mean \
clearance implied by a posterior, or the average rainfall a model \
predicts. You cannot visit every point, so you sample. The Monte Carlo \
recipe is the obvious one: throw ", math["N"], " random points, average \
the values, call it the answer. It always works, in any dimension, with no \
smoothness assumptions. That universality is why it is everywhere."}],

para[{ital["The catch."], " The statistical error of an MC average shrinks \
only as the square root of the sample size,"}],

displayMath[RowBox[{
   SubscriptBox["\[Epsilon]", "MC"], "\[Tilde]",
   FractionBox[RowBox[{"\[Sigma]", "(", "f", ")"}], SqrtBox["N"]],
   "=", RowBox[{"O", "(", SuperscriptBox["N", RowBox[{"-", "1", "/", "2"}]], ")"}]}]],

para[{"so cutting the error by ten needs ",
   imath@RowBox[{"100", "\[Times]"}], " the samples; one more decimal \
digit costs a factor of a hundred. When each sample is cheap this is \
merely slow. When each sample requires solving a differential equation, \
running a climate model, or evaluating an expensive likelihood, the \
square-root wall is the whole problem."}],

para[{ital["Why it is square-root."], " MC error is the standard deviation \
of a sample mean of ", math["N"], " independent draws \[LongDash] pure \
chance cancellation of ", imath@RowBox[{"\[PlusMinus]"}], " fluctuations. \
The points know nothing about each other, so they clump and leave gaps \
(left panel of the next figure). Those gaps are wasted information. The \
question QMC asks is simple: ", ital["what if we placed the points on \
purpose, to leave no gaps?"]}]
}];


(* ==================================================================
   2.  LOW-DISCREPANCY SEQUENCES
   ================================================================== *)

writeAll[{

hd1["2.  Low-discrepancy sequences"],

para[{"A ", bold["low-discrepancy sequence"], " is a deterministic set of \
points engineered to fill ",
   imath@SuperscriptBox[RowBox[{"[", "0", ",", "1", "]"}], "d"],
   " as uniformly as possible at every scale. \"Discrepancy\" measures the \
worst mismatch between the fraction of points inside an axis-aligned box \
and the box's volume; low discrepancy means every box holds about its fair \
share. Two classical constructions:"}],

para[{"\[Bullet] ", bold["Halton"], " (1960). Coordinate ", math["k"], " \
of the ", math["n"], "-th point is the ", ital["radical inverse"], " of ",
   math["n"], " in the ", math["k"], "-th prime base \[LongDash] write ",
   math["n"], " in base ", math["p"], " and reflect its digits about the \
\"decimal\" point. Different primes per axis keep the coordinates from \
lining up."}],

para[{"\[Bullet] ", bold["Sobol\[CloseCurlyQuote]"], " (1967). A ", ital["\
digital (t, m, d)-net"], " in base 2: each coordinate is built from a set \
of ", ital["direction numbers"], " by XOR-ing those selected by the bits \
of the index. With the right direction numbers (we use the Joe\[Dash]Kuo \
tables) Sobol\[CloseCurlyQuote] points are exceptionally uniform in high \
dimension."}],

para[{"Both are short to implement. The package builds Halton from radical \
inverses and Sobol\[CloseCurlyQuote] from Joe\[Dash]Kuo direction numbers \
via the Gray-code/bit-plane construction; here we just ask for the \
points:"}],

wlIn["(* 256 points of each kind in 2-D. *)
mc  = LDPoints[\"MonteCarlo\", 256, 2];
hal = LDPoints[\"Halton\", 256, 2];
sob = LDPoints[\"Sobol\", 256, 2];
GraphicsRow[ListPlot[#, AspectRatio -> 1, Frame -> True,
     PlotRange -> {{0, 1}, {0, 1}}, ImageSize -> 230] & /@ {mc, hal, sob}]"],

imgCell["point_sets.png"],
captionCell["Figure 2.1  256 points in the unit square. Monte Carlo \
(left) clumps and leaves holes; Halton (centre) and \
Sobol\[CloseCurlyQuote] (right) lay down an even, gap-free pattern. The \
empty patches in the random panel are exactly the wasted samples behind \
the square-root wall."],

para[{"The uniformity gap is quantitative. The ",
   imath@SubscriptBox["L", "2"], " ", bold["star discrepancy"], " (computed \
exactly by Warnock\[CloseCurlyQuote]s formula) decays like ",
   imath@SuperscriptBox["N", RowBox[{"-", "1", "/", "2"}]],
   " for random points but nearly ",
   imath@SuperscriptBox["N", RowBox[{"-", "1"}]],
   " for the low-discrepancy sets:"}],

wlIn["(* L2 star discrepancy vs N. *)
Nvals = 2^Range[4, 12];
dMC  = Mean[Table[StarDiscrepancyL2[
     RandomizedLDPoints[\"MonteCarlo\", #, 2, 100 r + #]], {r, 20}]] & /@ Nvals;
dHal = StarDiscrepancyL2[LDPoints[\"Halton\", #, 2]] & /@ Nvals;
dSob = StarDiscrepancyL2[LDPoints[\"Sobol\", #, 2]] & /@ Nvals;
ListLogLogPlot[{Transpose[{Nvals, dMC}], Transpose[{Nvals, dHal}],
   Transpose[{Nvals, dSob}]}, Joined -> True, Mesh -> All]"],

imgCell["discrepancy.png"],
captionCell["Figure 2.2  ", math["L\:2082"], " star discrepancy of the \
three point sets versus ", math["N"], " (log-log). Monte Carlo sits on the \
", powN[RowBox[{"-", "1", "/", "2"}]], " line; Halton and \
Sobol\[CloseCurlyQuote] track ", powN[RowBox[{"-", "1"}]],
   ". This is the geometric root of the whole \
convergence story \[LongDash] more uniform points integrate smooth \
functions more accurately."],

para[{ital["The theorem behind it."], " For a function ", math["f"], " of \
bounded variation ", imath@RowBox[{"V", "(", "f", ")"}],
   " (in the Hardy\[Dash]Krause sense), the ",
   bold["Koksma\[Dash]Hlawka inequality"], " bounds the QMC error by the \
product of the function's variation and the points' discrepancy:"}],

displayMath[RowBox[{
   "\[LeftBracketingBar]",
   RowBox[{
     RowBox[{FractionBox["1", "N"],
       UnderoverscriptBox["\[Sum]", RowBox[{"n", "=", "1"}], "N"],
       RowBox[{"f", "(", SubscriptBox["x", "n"], ")"}]}], "-",
     RowBox[{"\[Integral]", RowBox[{"f", "(", "x", ")"}], RowBox[{"\[DifferentialD]", "x"}]}]}],
   "\[RightBracketingBar]",
   "\[LessEqual]",
   RowBox[{"V", "(", "f", ")"}], " ",
   SubsuperscriptBox["D", "N", "*"]}]],

para[{"With ",
   imath@RowBox[{SubsuperscriptBox["D", "N", "*"], "=",
     RowBox[{"O", "(", SuperscriptBox["N", RowBox[{"-", "1"}]],
       SuperscriptBox[RowBox[{"(", "log", " ", "N", ")"}], "d"], ")"}]}],
   " for these sequences, the QMC error inherits that rate \[LongDash] ",
   ital["provided"], " the integrand is smooth enough to have finite \
variation. That proviso is the entire art of applying QMC, and it drives \
every modelling choice in the rest of this post."}]
}];


(* ==================================================================
   3.  A REAL BAYESIAN PROBLEM
   ================================================================== *)

writeAll[{

hd1["3.  A real Bayesian problem: pharmacokinetics"],

para[{ital["The setting."], " Pharmacokinetics (PK) asks what the body \
does to a drug: how fast it is absorbed, distributed, and cleared. It is \
the quantitative backbone of dosing. A patient swallows a dose; the drug \
concentration in blood is measured at a handful of times; from those few \
points we must infer the underlying rate constants \[LongDash] and, just \
as importantly, our ", ital["uncertainty"], " about them, because dosing \
decisions ride on it. That is a Bayesian problem."}],

para[{"We use the canonical teaching data set: ", bold["Theophylline"],
   ", an anti-asthma drug, from Boeckmann, Sheiner & Beal \[LongDash] 12 \
subjects, each given a single oral dose, with serum concentration sampled \
11 times over about 25 hours. It ships with R\[CloseCurlyQuote]s ",
   mono["datasets"], " package; we pull it live and commit a tidy copy."}],

wlIn["(* The data are loaded in Setup as `theoph`; here is Subject 1. *)
subj1 = SelectSubject[theoph, 1];
{subj1[\"dose\"], Transpose[{subj1[\"t\"], subj1[\"c\"]}]}"],

para[{ital["The forward model."], " A ", bold["two-compartment model with \
first-order absorption"], " is the standard structural model. The drug \
enters a gut depot, is absorbed into a central (blood) compartment at rate \
", sub["k", "a"], ", distributes reversibly into a peripheral (tissue) \
compartment, and is cleared from the centre. With six parameters \
\[LongDash] absorption rate ", sub["k", "a"], ", clearance ", math["CL"],
   ", central volume ", sub["V", "1"], ", inter-compartmental clearance ",
   math["Q"], ", peripheral volume ", sub["V", "2"], ", and a residual \
scatter ", math["\[Sigma]"], " \[LongDash] the concentration after a dose \
", math["D"], " has the closed form (a sum of three exponentials), so no \
ODE solver is needed:"}],

displayMath[RowBox[{
   RowBox[{"C", "(", "t", ")"}], "=",
   FractionBox[RowBox[{SubscriptBox["k", "a"], " ", "D"}], SubscriptBox["V", "1"]],
   RowBox[{"(",
     RowBox[{
       RowBox[{SubscriptBox["A", "\[Alpha]"], SuperscriptBox["e", RowBox[{"-", "\[Alpha]", " ", "t"}]]}], "+",
       RowBox[{SubscriptBox["A", "\[Beta]"], SuperscriptBox["e", RowBox[{"-", "\[Beta]", " ", "t"}]]}], "+",
       RowBox[{SubscriptBox["A", "k"], SuperscriptBox["e", RowBox[{"-", SubscriptBox["k", "a"], " ", "t"}]]}]}],
   ")"}]}]],

para[{"where ", math["\[Alpha]"], " and ", math["\[Beta]"], " are the \
macro-rate constants (eigenvalues of the linear compartment system) and \
the ", math["A"], "-coefficients are fixed by them. The package evaluates \
this directly:"}],

wlIn["(* Concentration over 0-25 h at typical parameters, 320 mg dose. *)
Plot[PKConcentration[t, {1.5, 2.8, 30., 2.0, 20.}, 320.], {t, 0, 25},
   Frame -> True, FrameLabel -> {\"time (h)\", \"conc (mg/L)\"}]"],

para[{ital["The statistical model and priors."], " Each measurement is \
the model prediction plus Gaussian scatter, ",
   imath@RowBox[{SubscriptBox["c", "i"], "=",
     RowBox[{RowBox[{"C", "(", SubscriptBox["t", "i"], ")"}], "+",
       SubscriptBox["\[Epsilon]", "i"]}]}],
   " with ",
   imath@RowBox[{SubscriptBox["\[Epsilon]", "i"], "\[Tilde]",
     RowBox[{"N", "(", "0", ",", SuperscriptBox["\[Sigma]", "2"], ")"}]}],
   ". All six parameters are positive, so we put independent ",
   bold["lognormal"], " priors on them \[LongDash] equivalently, normal \
priors on the logs. The posterior is then"}],

displayMath[RowBox[{
   RowBox[{"p", "(", "\[Theta]", "\[VerticalSeparator]", "c", ")"}],
   "\[Proportional]",
   RowBox[{UnderoverscriptBox["\[Product]", RowBox[{"i", "=", "1"}], "n"],
     RowBox[{"N", "(",
       RowBox[{SubscriptBox["c", "i"], "\[VerticalSeparator]",
         RowBox[{"C", "(", SubscriptBox["t", "i"], ")"}], ",",
         SuperscriptBox["\[Sigma]", "2"]}], ")"}]}], " ",
   RowBox[{"\[Pi]", "(", "\[Theta]", ")"}]}]],

para[{ital["What we actually want are integrals."], " Every Bayesian \
deliverable is an expectation against this posterior \[LongDash] the mean \
clearance, the variance, the probability the half-life exceeds a \
threshold, the predicted concentration at the next sampling time:"}],

displayMath[RowBox[{
   RowBox[{SubscriptBox["E", "post"], "[", RowBox[{"g", "(", "\[Theta]", ")"}], "]"}],
   "=", RowBox[{"\[Integral]", RowBox[{"g", "(", "\[Theta]", ")"}], " ",
     RowBox[{"p", "(", "\[Theta]", "\[VerticalSeparator]", "c", ")"}],
     RowBox[{"\[DifferentialD]", "\[Theta]"}]}]}]],

para[{"These are six-dimensional integrals over a smooth (but not \
analytically integrable) density. That is precisely the QMC use case \
\[LongDash] moderate dimension, smooth integrand, and an evaluation cost \
that, in real PK/PD work with stiff ODE models, is the binding \
constraint."}]
}];


(* ==================================================================
   4.  THE FIT
   ================================================================== *)

writeAll[{

hd1["4.  The fit: posterior of a Theophylline subject"],

para[{ital["From posterior to a tractable integral."], " To turn ",
   imath@SubscriptBox["E", "post"], " into something a low-discrepancy \
sequence can evaluate, we change variables so the integration domain is \
the unit cube. The clean, standard device is the ", bold["Laplace (\
Gaussian) approximation"], ": find the posterior mode in log-parameter \
space and the curvature (Hessian) there, giving a multivariate normal ",
   imath@RowBox[{"N", "(", "\[Mu]", ",", "\[CapitalSigma]", ")"}],
   " on ", imath@RowBox[{"\[CurlyPhi]", "=", RowBox[{"log", " ", "\[Theta]"}]}],
   ". A posterior expectation is then a Gaussian integral, and the ",
   bold["inverse-CDF transform"],
   " maps the unit cube onto it: ",
   imath@RowBox[{"\[CurlyPhi]", "(", "u", ")", "=",
     RowBox[{"\[Mu]", "+", RowBox[{"L", " ",
       SuperscriptBox["\[CapitalPhi]", RowBox[{"-", "1"}]], "(", "u", ")"}]}]}],
   " with ", imath@RowBox[{"L", " ", SuperscriptBox["L", "T"], "=", "\[CapitalSigma]"}],
   " and ", imath@SuperscriptBox["\[CapitalPhi]", RowBox[{"-", "1"}]],
   " the normal quantile applied coordinate-wise. No importance weights, a \
genuinely smooth integrand \[LongDash] exactly the regime where the \
Koksma\[Dash]Hlawka bound bites."}],

wlIn["(* The Laplace-Gaussian posterior (built in Setup as pkLaplace).
   Draw from it with a Sobol' point set and form the posterior-predictive
   concentration band. *)
draws  = PosteriorDraws[LDPoints[\"Sobol\", 4096, 6], pkLaplace];
tGrid  = Subdivide[0., 25., 200];
curves = ConcentrationCurves[draws, tGrid, subj1[\"dose\"]];
band   = Transpose[curves];
{Quantile[#, 0.05] & /@ band, Quantile[#, 0.95] & /@ band};   (* 90% band *)"],

imgCell["pk_fit.png"],
captionCell["Figure 4.1  Theophylline Subject 1: the 11 measurements \
(points) with the 90% posterior-predictive concentration band from the \
fitted two-compartment model. The model captures the absorption peak near \
2 h and the slow elimination tail; the data fall inside the band."],

para[{"The posterior marginals (a Sobol\[CloseCurlyQuote] draw from the \
Laplace posterior) show what the 11 points do and do not pin down:"}],

imgCell["posterior.png"],
captionCell["Figure 4.2  Posterior marginals for the six parameters. \
Clearance ", math["CL"], ", central volume ", sub["V", "1"], ", and \
absorption ", sub["k", "a"], " are tightly determined; the \
inter-compartmental terms ", math["Q"], " and ", sub["V", "2"], " are \
much broader \[LongDash] a single subject\[CloseCurlyQuote]s data only \
weakly identify the peripheral compartment. That mix of well- and \
weakly-constrained parameters is typical of real inference."],

calloutCell[{bold["Honest check: is the Gaussian posterior good enough? "],
   "The Laplace approximation is exact only for Gaussian posteriors. We \
verify it against the ", ital["full"], " posterior computed by \
self-normalized importance sampling (the package\[CloseCurlyQuote]s ",
   mono["FitProposal"], " + ", mono["ImportanceEstimate"], "). The \
well-identified parameters (", math["CL"], ", ", sub["V", "1"], ", ",
   sub["k", "a"], ") agree to within about 10%; the weakly-identified ",
   math["Q"], ", ", sub["V", "2"], " differ more, because their true \
posterior is skewed. The convergence demonstration below does not depend \
on the approximation being perfect \[LongDash] it only needs the \
integrand we actually integrate to be smooth, which it is."}],

wlIn["(* Cross-check: full posterior mean via importance sampling. *)
estIS = ImportanceEstimate[LDPoints[\"Sobol\", 2^16, 6],
   FitProposal[pkProblem], pkProblem];
{LaplaceMeanExact[pkLaplace], estIS[\"postMean\"], estIS[\"essFraction\"]}"]
}];


(* ==================================================================
   5.  THE CONVERGENCE LAW
   ================================================================== *)

writeAll[{

hd1["5.  The convergence law (the headline)"],

para[{"Now the measurement. We estimate two posterior expectations of the \
real fit and watch the error fall as the number of points grows, for \
Monte Carlo, Sobol\[CloseCurlyQuote], and Halton:"}],

para[{"\[Bullet] ", bold["the posterior mean clearance"], " ",
   imath@RowBox[{SubscriptBox["E", "post"], "[", "CL", "]"}],
   ". Under the Gaussian posterior this has a closed form, ",
   imath@RowBox[{"exp", "(",
     RowBox[{SubscriptBox["\[Mu]", "CL"], "+",
       RowBox[{SubscriptBox["\[CapitalSigma]", "CL"], "/", "2"}]}], ")"}],
   ", so we know the exact answer and can measure true error."}],

para[{"\[Bullet] ", bold["the posterior-predictive concentration at 6 h"],
   ", ", imath@RowBox[{SubscriptBox["E", "post"],
     "[", RowBox[{"C", "(", "6", ")"}], "]"}],
   ", which has no closed form \[LongDash] each integrand evaluation is a \
forward model solve, so this is the realistic, expensive case. Its \
reference value comes from a high-", math["N"], " Sobol\[CloseCurlyQuote] \
run."}],

para[{"For each method and each ", math["N"], " we average over \
independent randomizations (a digital shift for Sobol\[CloseCurlyQuote], a \
Cranley\[Dash]Patterson rotation for Halton, reseeding for MC) and report \
the root-mean-square error:"}],

wlIn["(* RMSE vs N for E[CL], analytic reference. *)
Nlist = 2^Range[7, 15];
refCL = LaplaceMeanExact[pkLaplace][[2]];
gCL   = #[[All, 2]] &;                       (* CL is parameter 2 *)
rmseCL = Association[# -> ExpectationRMSE[#, Nlist, 200, pkLaplace, gCL, refCL] & /@
   {\"MonteCarlo\", \"Sobol\", \"Halton\"}];
ListLogLogPlot[Table[Transpose[{Nlist, rmseCL[m]}],
   {m, {\"MonteCarlo\", \"Sobol\", \"Halton\"}}], Joined -> True, Mesh -> All]"],

imgCell["convergence.png"],
captionCell["Figure 5.1  RMSE versus ", math["N"], " (log-log) for two \
posterior expectations of the real Theophylline fit. ",
   bold["Left:"], " posterior mean clearance (analytic reference). ",
   bold["Right:"], " posterior-predictive concentration at 6 h (each \
evaluation a forward model solve). Monte Carlo follows the ",
   powN[RowBox[{"-", "1", "/", "2"}]], " guide (slope \[TildeTilde] ",
   math["-0.50"], "); Sobol\[CloseCurlyQuote] and Halton follow ",
   powN[RowBox[{"-", "1"}]], " (slopes \[TildeTilde] ", math["-0.95"],
   "). At ", math["N = 32768"], " Sobol\[CloseCurlyQuote] is roughly 50\
\[Times] more accurate than MC, and the gap widens with ", math["N"], "."],

para[{"That is the headline result of the project, and it is on real data: \
the same number of model evaluations buys you almost twice as many correct \
digits with Sobol\[CloseCurlyQuote] as with Monte Carlo. The Halton curve \
sits just above Sobol\[CloseCurlyQuote] \[LongDash] both achieve the \
near-", imath@SuperscriptBox["N", RowBox[{"-", "1"}]], " rate, with \
Sobol\[CloseCurlyQuote] the more uniform in six dimensions."}]
}];


(* ==================================================================
   6.  DIMENSION DEPENDENCE
   ================================================================== *)

writeAll[{

hd1["6.  Dimension dependence: why the advantage erodes"],

para[{"The QMC rate carries a ",
   imath@SuperscriptBox[RowBox[{"(", "log", " ", "N", ")"}], "d"],
   " factor, and that factor is not cosmetic: as the dimension ", math["d"],
   " grows, the advantage over Monte Carlo erodes, because filling a \
high-dimensional cube evenly gets exponentially harder. The cleanest way \
to see it is to fix the sampling budget ", math["N"], " and ask how many \
times more accurate Sobol\[CloseCurlyQuote] is than MC \[LongDash] the ",
   ital["efficiency gain"], " \[LongDash] as the dimension climbs. We use a \
smooth product integrand with exact integral 1:"}],

wlIn["(* Smooth product integrand, exact integral = 1. Efficiency gain
   (MC RMSE / Sobol' RMSE) at fixed budget N, across dimension d. *)
cAmp  = 0.6;
fMean = Function[pts, Mean[Times @@@ (1 + cAmp Cos[2 Pi pts])]];
rmseAt[method_, d_, n_] := Sqrt@Mean@Table[
   (fMean[RandomizedLDPoints[method, n, d, 53 r + d]] - 1.)^2, {r, 120}];
gain[n_] := Table[rmseAt[\"MonteCarlo\", d, n]/rmseAt[\"Sobol\", d, n], {d, 2, 12}];
{gain[4096], gain[65536]}"],

imgCell["dimension.png"],
captionCell["Figure 6.1  Sobol\[CloseCurlyQuote] efficiency gain (Monte \
Carlo RMSE divided by Sobol\[CloseCurlyQuote] RMSE, at a fixed budget) \
versus dimension, for two budgets. In low dimension Sobol\
\[CloseCurlyQuote] is tens to hundreds of times more accurate per sample; \
the gain decays steadily as ", math["d"], " climbs and the ",
   pow[RowBox[{"(", "log", " ", "N", ")"}], "d"], " penalty bites, heading \
toward parity (grey line). \
The six-parameter PK problem of this post sits where the gain is still \
large \[LongDash] the QMC sweet spot."],

para[{"The practical lesson: QMC is most valuable in low-to-moderate \
dimension (say ", imath@RowBox[{"d", "\[LessEqual]", "20"}],
   "), or whenever the integrand has low ", ital["effective"], " dimension \
\[LongDash] a few directions that matter and many that barely do. Many \
Bayesian models, including this one, are exactly that."}]
}];


(* ==================================================================
   7.  SYNTHETIC RECOVERY
   ================================================================== *)

writeAll[{

hd1["7.  Correctness: synthetic-truth recovery"],

para[{"A fast wrong answer is worthless, so we check that the QMC \
machinery recovers parameters we ", ital["know"], ". We generate \
concentrations from a fixed \"true\" parameter vector plus noise, fit the \
same model, and ask whether the posterior brackets the truth:"}],

wlIn["(* Generate synthetic data from known truth, then fit. *)
thetaTrue = {1.20, 2.50, 30.0, 1.80, 22.0, 0.45};
{synProblem, synConc} = SyntheticProblem[thetaTrue, 320.0,
   {0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 20, 24, 30},
   priorMedians, priorLogSD, 4242];
synDraws = PosteriorDraws[LDPoints[\"Sobol\", 8192, 6],
   LaplacePosterior[synProblem]];
Mean /@ Transpose[synDraws]                  (* posterior means *)"],

imgCell["synthetic.png"],
captionCell["Figure 7.1  Synthetic-truth recovery. Each parameter is shown \
as its posterior estimate divided by the true value, with a 90% credible \
interval; the red line at 1.0 is the truth. The well-identified \
parameters (", sub["k", "a"], ", ", math["CL"], ", ", sub["V", "1"],
   ") land tight on the truth; the weakly-identified ", math["Q"], " and ",
   sub["V", "2"], " have wide intervals that still cover it. The method is \
correct, and it reports honest uncertainty where the data are \
uninformative."]
}];


(* ==================================================================
   8.  PRACTICAL PAY-OFF
   ================================================================== *)

writeAll[{

hd1["8.  The practical pay-off"],

para[{"Slopes are abstract; model solves are money. The question a \
practitioner asks is: ", ital["to reach the accuracy I need, how many \
times must I run the forward model?"], " Inverting the fitted error laws \
\[LongDash] ", imath@RowBox[{"N", "\[Proportional]",
     SuperscriptBox["tol", RowBox[{"-", "2"}]]}], " for Monte Carlo, ",
   imath@RowBox[{"N", "\[Proportional]", SuperscriptBox["tol", RowBox[{"-", "1"}]]}],
   " for QMC \[LongDash] gives the count directly, and the ratio is the \
speed-up:"}],

wlIn["(* Forward-model solves to reach a target accuracy on E[C(6 h)]. *)
fitPow[e_] := Module[{c = Fit[Transpose[{Log[Nlist], Log[e]}], {1, x}, x]},
   {Exp[c /. x -> 0], Coefficient[c, x]}];
nForTol[e_, tol_] := With[{cp = fitPow[e]}, (cp[[1]]/tol)^(1/(-cp[[2]]))];
(* speed-up of Sobol' over MC at 0.1% relative accuracy *)
nForTol[rmsePred[\"MonteCarlo\"], 0.001 refPred] /
   nForTol[rmsePred[\"Sobol\"], 0.001 refPred]"],

imgCell["payoff.png"],
captionCell["Figure 8.1  Forward-model solves required to reach a given \
accuracy on the posterior-predictive concentration. Because Monte Carlo \
needs ", bigO[SuperscriptBox["tol", RowBox[{"-", "2"}]]],
   " evaluations while QMC needs ",
   bigO[SuperscriptBox["tol", RowBox[{"-", "1"}]]],
   ", the gap widens as you demand more accuracy: \
Sobol\[CloseCurlyQuote] is about 2\[Times] cheaper at 1% accuracy, 4\
\[Times] at 0.5%, and roughly 18\[Times] at 0.1%. When each evaluation is \
an expensive ODE solve, that is the difference between an overnight run \
and a coffee break."],

para[{"This is the whole value proposition in one figure. QMC does not \
change what you compute \[LongDash] same posterior, same estimand, same \
code structure \[LongDash] it changes only ", ital["which points you \
evaluate at"], ", and that single swap buys back most of an order of \
magnitude (often more) in model runs."}]
}];


(* ==================================================================
   9.  CONCLUSIONS
   ================================================================== *)

writeAll[{

hd1["9.  Conclusions and caveats"],

para[{"On a genuine six-parameter pharmacokinetic inference from the \
Theophylline data, deterministic low-discrepancy sampling beats plain \
Monte Carlo by close to a full power of ", math["N"], ": measured slopes \
of ", imath@RowBox[{"\[TildeTilde]", RowBox[{"-", "0.95"}]}],
   " for Sobol\[CloseCurlyQuote] and Halton against ",
   imath@RowBox[{"\[TildeTilde]", RowBox[{"-", "0.5"}]}],
   " for MC, and an order-of-magnitude saving in forward-model solves at \
the accuracies that matter."}],

para[{ital["When QMC helps, and when it does not."], " The gains are real \
when (i) the dimension is low-to-moderate or the effective dimension is \
small; (ii) the integrand is smooth, i.e. of bounded variation; and (iii) \
evaluations are the bottleneck. They evaporate when the integrand is rough \
or discontinuous (indicator functions, hard thresholds), when the \
dimension is very large, or when an importance-sampling weight is \
heavy-tailed \[LongDash] which is exactly why we integrate against a \
smooth Gaussianized posterior rather than a spiky importance ratio."}],

para[{ital["Reproducing the rate in your own work."], " The recipe is \
portable: (1) reparameterize to an unconstrained space and build a \
Gaussian (Laplace) approximation of the posterior; (2) map the unit cube \
through it with the inverse-CDF transform; (3) replace your \
pseudo-random draws with a randomized Sobol\[CloseCurlyQuote] or Halton \
set; (4) average over a few randomizations to get an error bar. Steps 1\
\[Dash]2 are standard Bayesian asymptotics; steps 3\[Dash]4 are a \
two-line change to existing code."}]
}];


(* ==================================================================
   10.  REFERENCES
   ================================================================== *)

writeAll[{

hd1["10.  References"],

para[{bold["Halton, J. H."], " (1960). On the efficiency of certain \
quasi-random sequences of points in evaluating multi-dimensional \
integrals. ", ital["Numerische Mathematik"], " 2, 84\[Dash]90."}],

para[{bold["Sobol\[CloseCurlyQuote], I. M."], " (1967). On the \
distribution of points in a cube and the approximate evaluation of \
integrals. ", ital["USSR Comp. Math. and Math. Phys."], " 7, 86\[Dash]112."}],

para[{bold["Joe, S., & Kuo, F. Y."], " (2008). Constructing Sobol\
\[CloseCurlyQuote] sequences with better two-dimensional projections. ",
   ital["SIAM J. Scientific Computing"], " 30, 2635\[Dash]2654. [The \
direction numbers used here.]"}],

para[{bold["Niederreiter, H."], " (1992). ", ital["Random Number \
Generation and Quasi-Monte Carlo Methods"], ". SIAM. [Koksma\[Dash]Hlawka, \
discrepancy theory.]"}],

para[{bold["Caflisch, R. E."], " (1998). Monte Carlo and quasi-Monte \
Carlo methods. ", ital["Acta Numerica"], " 7, 1\[Dash]49. [Effective \
dimension; when QMC wins.]"}],

para[{bold["Owen, A. B."], " (1998). Scrambling Sobol\[CloseCurlyQuote] \
and Niederreiter\[Dash]Xing points. ", ital["J. Complexity"], " 14, 466\
\[Dash]489. [Randomized QMC and error estimation.]"}],

para[{bold["Gerber, M., & Chopin, N."], " (2015). Sequential quasi-Monte \
Carlo. ", ital["J. Royal Statistical Society B"], " 77, 509\[Dash]579. \
[QMC inside Bayesian computation.]"}],

para[{bold["Boeckmann, A. J., Sheiner, L. B., & Beal, S. L."], " (1994). \
", ital["NONMEM Users Guide"], ". University of California, San Francisco. \
[Source of the Theophylline data set.]"}],

para[{bold["Gabrielsson, J., & Weiner, D."], " (2016). ", ital["\
Pharmacokinetic and Pharmacodynamic Data Analysis"], " (5th ed.). \
[Two-compartment oral model.]"}]
}];


(* ==================================================================
   11.  REPRODUCIBILITY
   ================================================================== *)

writeAll[{

hd1["11.  Reproducibility"],

para[{"All source code lives in the public repository ",
   link["github.com/mthiel74/QuasiMonteCarloBayesianEstimation",
     "https://github.com/mthiel74/QuasiMonteCarloBayesianEstimation"],
   ". The pipeline is pure Wolfram Language. To rebuild from scratch with \
Wolfram Language 14+:"}],

codeIn["# 1. Fetch the Theophylline data into data/
wolframscript -file wolfram/fetch_theoph.wls

# 2. Render every figure into docs/images/
wolframscript -file wolfram/run_all.wls

# 3. Rebuild this notebook
wolframscript -file community/build_notebook.wls"],

para[{"The companion package ", mono["community/qmc_bayes_helpers.wl"],
   " is the single source of truth for the generators, the model, and the \
estimators; the figure scripts and this notebook both load it. Every code \
cell above calls into it with all arguments shown inline, so the post \
doubles as runnable documentation."}],

para[{Style["Generated on " <> DateString[Now, {"MonthName", " ",
    "DayShort", ", ", "Year"}] <> ".",
    FontSlant -> Italic, FontColor -> GrayLevel[0.4]]}]
}];


(* ---- assemble + save ---- *)

(* `sub` helper used inline above for subscripted parameter names. *)
Print["assembled ", Length[$allCells], " cells"];

nbExpr = Notebook[$allCells, WindowSize -> {1000, 820},
   StyleDefinitions -> "Default.nb"];

TimeConstrained[
   UsingFrontEnd[
      nbObj = CreateDocument[$allCells, Visible -> False,
         WindowSize -> {1000, 820}, StyleDefinitions -> "Default.nb"];
      NotebookSave[nbObj, outNb];
      Print["wrote ", outNb, " (", FileByteCount[outNb], " B)"];
      Quiet @ Check[Export[outPdf, nbObj];
         Print["wrote ", outPdf, " (", FileByteCount[outPdf], " B)"],
         Print["warn: PDF export failed"]];
      NotebookClose[nbObj];
   ], 600, Print["warn: notebook save timed out"]];
