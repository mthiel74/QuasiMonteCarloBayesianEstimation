#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/qmc_bayes.nb — the Wolfram Community post
    "Quasi-Monte Carlo for a real Bayesian estimation".

    Layout follows the teal "Roboto Condensed" house style of the
    UK-rivers post (433126256.nb): left-aligned text, teal section
    headings, indented Input/Output cells with brackets, a hero
    animation up top.  No "how to cite" section.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
outNb    = FileNameJoin[{repoRoot, "community", "qmc_bayes.nb"}];
outPdf   = FileNameJoin[{repoRoot, "community", "qmc_bayes.pdf"}];

tealCol = RGBColor[0.153, 0.51, 0.64];

(* ==================================================================
   Stylesheet — adopted from the reference post.
   ================================================================== *)
styleDefs = Notebook[{
   Cell[StyleData[StyleDefinitions -> "Default.nb"]],
   Cell[StyleData["Text"], CellMargins -> {{0, 12}, {7, 8}}, FontSize -> 15,
      LineSpacing -> {1, 3}],
   Cell[StyleData["Title"], CellMargins -> {{0, 3}, {15, 0}},
      FontFamily -> "Roboto Condensed", FontColor -> tealCol],
   Cell[StyleData["Subtitle"], CellMargins -> {{0, 3}, {18, 12}},
      FontFamily -> "Roboto Condensed", FontSize -> 19,
      FontColor -> GrayLevel[0.3]],
   Cell[StyleData["Section"], CellMargins -> {{0, 3}, {12, 22}},
      FontFamily -> "Roboto Condensed", FontSize -> 28, FontColor -> tealCol],
   Cell[StyleData["Subsection"], CellMargins -> {{0, 3}, {8, 16}},
      FontFamily -> "Roboto Condensed", FontSize -> 20,
      FontColor -> RGBColor[0.2, 0.42, 0.5]],
   Cell[StyleData["Subsubsection"], CellMargins -> {{0, 3}, {6, 12}},
      FontFamily -> "Roboto Condensed", FontSize -> 16],
   Cell[StyleData["Input"], CellMargins -> {{45, 5}, {4, 6}},
      StyleHints -> <|"CodeFont" -> "Source Sans Pro"|>, FontSize -> 13],
   Cell[StyleData["Output"], CellMargins -> {{45, 5}, {8, 6}}, FontSize -> 13],
   Cell[StyleData["Item"], CellMargins -> {{24, 12}, {4, 6}}, FontSize -> 15],
   Cell[StyleData["DisplayFormula"], CellMargins -> {{36, 10}, {10, 10}}],
   Cell[StyleData["Program"], CellMargins -> {{45, 12}, {6, 6}},
      FontFamily -> "Courier", FontSize -> 12]},
   Visible -> False];

(* ==================================================================
   Cell builders
   ================================================================== *)
title[t_]    := Cell[t, "Title"];
subtitle[t_] := Cell[t, "Subtitle"];
section[t_]  := Cell[t, "Section"];
subsec[t_]   := Cell[t, "Subsection"];
para[s_]     := Cell[TextData[s], "Text"];
lead[s_]     := Cell[TextData[s], "Text", FontSize -> 16,
   FontColor -> GrayLevel[0.2]];
item[s_]     := Cell[TextData[s], "Item"];

bold[s_] := StyleBox[s, FontWeight -> Bold];
ital[s_] := StyleBox[s, FontSlant -> Italic];
math[s_] := StyleBox[s, FontFamily -> "Times", FontSlant -> Italic];
mono[s_] := StyleBox[s, FontFamily -> "Source Code Pro", FontColor -> tealCol];

imath[b_]      := Cell[BoxData @ FormBox[b, TraditionalForm], "InlineFormula"];
sub[a_, k_]    := imath @ SubscriptBox[a, k];
pow[a_, e_]    := imath @ SuperscriptBox[a, e];
powN[e_]       := imath @ SuperscriptBox["N", e];
bigO[b_]       := imath @ RowBox[{"O", "(", b, ")"}];
displayMath[e_] := Cell[BoxData @ FormBox[e, TraditionalForm], "DisplayFormula"];

caption[args__] := Cell[TextData[{args}], "Text", FontSlant -> Italic,
   FontSize -> 11.5, FontColor -> GrayLevel[0.4], CellMargins -> {{45, 40}, {2, 14}}];

heroCaption[args__] := Cell[TextData[{args}], "Text", FontSize -> 12,
   FontColor -> GrayLevel[0.4]];

codeIn[c_String] := Cell[c, "Program", ShowStringCharacters -> False,
   Background -> RGBColor[0.96, 0.97, 0.98]];

wlIn[c_String] := Cell[c, "Input"];

link[t_String, u_String] := Sequence[StyleBox[t, FontWeight -> "SemiBold"],
   " (", StyleBox[u, FontFamily -> "Source Code Pro", FontSize -> 10], ")"];

callout[args__] := Cell[TextData[{args}], "Text", FontSize -> 13,
   CellMargins -> {{36, 40}, {8, 8}}, Background -> RGBColor[0.93, 0.96, 0.97],
   CellFrame -> {{3, 0}, {0, 0}}, CellFrameColor -> tealCol, FrameMargins -> 12];

imgCell[file_] := Module[{path = FileNameJoin[{imgDir, file}], im},
   If[FileExistsQ[path],
     im = Image[Import[path], ImageSize -> 660];
     Cell[BoxData @ ToBoxes[im], "Output", ShowCellLabel -> False,
        CellMargins -> {{45, 5}, {8, 8}}],
     Cell[TextData[{bold["[ missing figure: " <> file <> " ]"]}], "Text",
        FontColor -> RGBColor[0.7, 0.1, 0.1]]]];

animCell[file_] := Module[{path = FileNameJoin[{imgDir, file}], all, anim},
   If[! FileExistsQ[path],
     Return @ Cell[TextData[{bold["[ missing animation: " <> file <> " ]"]}],
        "Text", FontColor -> RGBColor[0.7, 0.1, 0.1]]];
   all = Import[path, {"GIF", "ImageList"}];
   all = ColorQuantize[ImageResize[#, {600}], 220] & /@ all;
   anim = AnimatedImage[all, FrameRate -> 1.4, AnimationRunning -> True,
      AnimationRepetitions -> Infinity];
   Cell[BoxData @ ToBoxes[anim], "Output", ShowCellLabel -> False,
      CellMargins -> {{20, 5}, {6, 10}}]];

$allCells = {};
writeAll[cs_List] := ($allCells = Join[$allCells, cs];
   Print["  +", Length[cs], " cells (", Length[$allCells], ")"]);


(* ==================================================================
   HERO + TITLE + LEAD
   ================================================================== *)
writeAll[{

animCell["hero.gif"],

heroCaption[
   "The whole post in one animation. Left: ", bold["Monte Carlo"],
   " points scattered at random clump and leave gaps. Right: ",
   bold["Sobol\[CloseCurlyQuote]"], " low-discrepancy points fill the same \
square evenly. Below: the error of a sample integral as the points \
accumulate \[LongDash] Monte Carlo crawls down the ", math["1/\[Sqrt]N"],
   " line while Sobol\[CloseCurlyQuote] plunges along ", math["1/N"],
   ". Generated near the end of this post with one call to ",
   mono["heroAnimation[]"], "."],

title["Quasi-Monte Carlo for a real Bayesian estimation"],

subtitle["Sobol\[CloseCurlyQuote] and Halton sequences against plain Monte \
Carlo on a six-parameter pharmacokinetic fit \[LongDash] and the textbook \
convergence gap, measured on real data"],

lead[{"Almost every number a Bayesian reports \[LongDash] a posterior mean, \
a credible interval, a predicted drug concentration \[LongDash] is secretly \
an ", bold["integral"], " over the space of unknown parameters. This post \
is about computing those integrals an order of magnitude more cheaply."}],

para[{"The default tool is ", bold["Monte Carlo"], ": average the integrand \
over random samples. It is universal and assumption-free, but its error \
shrinks only as ", powN[RowBox[{"-", "1", "/", "2"}]],
   " \[LongDash] one more correct digit costs a hundred times more samples. \
", bold["Quasi-Monte Carlo (QMC)"], " keeps the same averaging but replaces \
the random sample with a deterministic ", ital["low-discrepancy sequence"],
   " (Sobol\[CloseCurlyQuote], Halton) engineered to fill space evenly. For \
smooth integrands the error then falls almost as fast as ",
   bigO[RowBox[{SuperscriptBox["N", RowBox[{"-", "1"}]],
      SuperscriptBox[RowBox[{"(", "log", " ", "N", ")"}], "d"]}]],
   " \[LongDash] nearly a full power of ", math["N"], " better."}],

para[{"We test that promise on a genuine inference problem: a \
six-parameter pharmacokinetic model fit to the classic ",
   bold["Theophylline"], " data. We build Halton and \
Sobol\[CloseCurlyQuote] from scratch, set up the posterior, contrast it \
with classical curve-fitting, and measure the convergence. The headline, \
on real data: Monte-Carlo slopes near ",
   imath@RowBox[{"-", "0.5"}], " against Sobol\[CloseCurlyQuote] slopes near \
", imath@RowBox[{"-", "0.95"}], ", and roughly a 4\[Times]\[Dash]18\[Times] \
saving in expensive model evaluations."}],

para[{ital["How to read this:"], " every section opens with the plain idea, \
then the equations, the runnable Wolfram Language, and the measured \
numbers. All figures are pre-rendered \[LongDash] the post reads straight \
through \[LongDash] and every code cell runs if you load the attached \
package."}]
}];


(* ==================================================================
   SETUP
   ================================================================== *)
writeAll[{

section["Setup"],

para[{ital["Skip this if you only want to read."], " To run the code, put \
the attached package ", mono["qmc_bayes_helpers.wl"], " next to the \
notebook and evaluate the three cells below once. They define the \
generators, the pharmacokinetic model, the data, the priors, and the \
posterior that every later section reuses."}],

wlIn["SetDirectory[NotebookDirectory[]];
Get[\"qmc_bayes_helpers.wl\"];"],

wlIn["(* Lognormal-prior medians and log-scale SDs for {ka,CL,V1,Q,V2,sigma} *)
priorMedians = {1.5, 3.0, 35.0, 2.0, 25.0, 1.0};
priorLogSD   = {0.6, 0.5, 0.5, 0.7, 0.7, 0.5};

(* Real data: Theophylline Subject 1 (single 320 mg oral dose) *)
theoph = LoadTheoph[];
subj1  = SelectSubject[theoph, 1];"],

wlIn["(* The Bayesian problem and its Laplace-Gaussian posterior *)
pkProblem = BayesProblem[subj1[\"dose\"], subj1[\"t\"], subj1[\"c\"],
   priorMedians, priorLogSD];
pkLaplace = LaplacePosterior[pkProblem];"]
}];


(* ==================================================================
   1. THE SQUARE-ROOT WALL
   ================================================================== *)
writeAll[{

section["1.  The square-root wall"],

para[{"Want the average of something over a space too big to enumerate? \
Sample it. Throw ", math["N"], " random points, average the values. This \
Monte-Carlo estimate works in any dimension with no smoothness \
assumptions \[LongDash] which is why it is everywhere. The price is the \
error,"}],

displayMath[RowBox[{SubscriptBox["\[Epsilon]", "MC"], "\[Tilde]",
   FractionBox[RowBox[{"\[Sigma]", "(", "f", ")"}], SqrtBox["N"]], "=",
   RowBox[{"O", "(", SuperscriptBox["N", RowBox[{"-", "1", "/", "2"}]], ")"}]}]],

para[{"Halving the error means quadrupling the work; one more decimal digit \
costs a hundredfold. When each sample is an expensive model solve, that \
square-root wall is the entire problem. Its cause is visible: random points \
clump and leave gaps, wasting information. What if we placed them on \
purpose?"}],

wlIn["mc  = LDPoints[\"MonteCarlo\", 256, 2];
hal = LDPoints[\"Halton\", 256, 2];
sob = LDPoints[\"Sobol\", 256, 2];
GraphicsRow[ListPlot[#, AspectRatio -> 1, Frame -> True,
   PlotRange -> {{0, 1}, {0, 1}}, ImageSize -> 230] & /@ {mc, hal, sob}]"],

imgCell["point_sets.png"],
caption["Figure 1.  256 points in the unit square. Monte Carlo (left) \
clumps and leaves holes; Halton (centre) and Sobol\[CloseCurlyQuote] \
(right) lay down an even, gap-free pattern."]
}];


(* ==================================================================
   2. LOW-DISCREPANCY SEQUENCES
   ================================================================== *)
writeAll[{

section["2.  Low-discrepancy sequences"],

para[{"A ", bold["low-discrepancy sequence"], " fills the cube as evenly as \
possible at every scale. ", bold["Halton"], " uses the radical inverse of \
the index in a different prime base per coordinate; ",
   bold["Sobol\[CloseCurlyQuote]"], " is a base-2 digital net built from \
\"direction numbers\" (we use the Joe\[Dash]Kuo tables). Both are a few \
lines, both are in the attached package. Their ", math["L\:2082"], " ",
   bold["star discrepancy"], " \[LongDash] the worst box-vs-volume mismatch, \
by Warnock\[CloseCurlyQuote]s exact formula \[LongDash] decays like ",
   powN[RowBox[{"-", "1", "/", "2"}]], " for random points but nearly ",
   powN[RowBox[{"-", "1"}]], " for these sequences:"}],

wlIn["Nvals = 2^Range[4, 12];
dMC  = Mean[Table[StarDiscrepancyL2[
     RandomizedLDPoints[\"MonteCarlo\", #, 2, 100 r + #]], {r, 20}]] & /@ Nvals;
dHal = StarDiscrepancyL2[LDPoints[\"Halton\", #, 2]] & /@ Nvals;
dSob = StarDiscrepancyL2[LDPoints[\"Sobol\", #, 2]] & /@ Nvals;
ListLogLogPlot[{Transpose[{Nvals, dMC}], Transpose[{Nvals, dHal}],
   Transpose[{Nvals, dSob}]}, Joined -> True, Mesh -> All]"],

imgCell["discrepancy.png"],
caption["Figure 2.  ", math["L\:2082"], " star discrepancy versus ",
   math["N"], " (log-log). Monte Carlo rides the ", math["N\^(-1/2)"],
   " line; Halton and Sobol\[CloseCurlyQuote] track ", math["N\^(-1)"], "."],

para[{ital["The theorem behind it."], " For an integrand ", math["f"], " of \
bounded variation ", imath@RowBox[{"V", "(", "f", ")"}], " (Hardy\[Dash]\
Krause), the ", bold["Koksma\[Dash]Hlawka inequality"], " splits the QMC \
error into the function\[CloseCurlyQuote]s variation times the \
points\[CloseCurlyQuote] discrepancy:"}],

displayMath[RowBox[{"\[LeftBracketingBar]",
   RowBox[{RowBox[{FractionBox["1", "N"],
       UnderoverscriptBox["\[Sum]", RowBox[{"n", "=", "1"}], "N"],
       RowBox[{"f", "(", SubscriptBox["x", "n"], ")"}]}], "-",
     RowBox[{"\[Integral]", RowBox[{"f", "(", "x", ")"}],
       RowBox[{"\[DifferentialD]", "x"}]}]}],
   "\[RightBracketingBar]", "\[LessEqual]",
   RowBox[{"V", "(", "f", ")"}], " ", SubsuperscriptBox["D", "N", "*"]}]],

para[{"With ", imath@RowBox[{SubsuperscriptBox["D", "N", "*"], "=",
     RowBox[{"O", "(", SuperscriptBox["N", RowBox[{"-", "1"}]],
       SuperscriptBox[RowBox[{"(", "log", " ", "N", ")"}], "d"], ")"}]}],
   ", QMC inherits that rate \[LongDash] ", ital["provided"], " the \
integrand is smooth enough to have finite variation. That proviso drives \
every modelling choice below."}]
}];


(* ==================================================================
   3. THE MODEL WE FIT
   ================================================================== *)
writeAll[{

section["3.  The model: a drug moving through the body"],

para[{ital["What happens when you take a pill."], " The drug dissolves in \
the gut and is absorbed into the bloodstream; the blood carries it to the \
tissues, where some of it parks reversibly; meanwhile the liver and kidneys \
clear it away. Plot the blood concentration against time and you get a \
familiar shape: a fast rise to a peak, then a slow decline. ",
   bold["Pharmacokinetics"], " is the quantitative description of that \
curve, and it is the backbone of every dosing decision \[LongDash] how \
much, how often, and how confident we are."}],

para[{"We use the canonical teaching data set, ", bold["Theophylline"],
   " (an anti-asthma drug, from Boeckmann, Sheiner & Beal): 12 subjects, \
each a single oral dose, serum concentration sampled 11 times over ~25 h. \
Theophylline has a narrow therapeutic window (about 10\[Dash]20 mg/L; \
toxicity climbs above it), so getting the curve \[LongDash] and its \
uncertainty \[LongDash] right genuinely matters."}],

subsec["3.1  The two-compartment system"],

para[{"The standard structural model splits the body into two \
well-mixed ", ital["compartments"], ": a ", bold["central"], " one (blood \
and richly-perfused organs, volume ", sub["V", "1"], ") where we actually \
measure concentration, and a ", bold["peripheral"], " one (slower tissues, \
volume ", sub["V", "2"], "). The swallowed dose sits in a gut depot ",
   sub["A", "g"], " and is absorbed first-order into the central \
compartment; drug shuttles between central and peripheral, and is cleared \
from the central compartment. Writing the amounts in each compartment, the \
dynamics are a linear system of ODEs:"}],

displayMath[RowBox[{FractionBox[RowBox[{"\[DifferentialD]", SubscriptBox["A", "g"]}], RowBox[{"\[DifferentialD]", "t"}]],
   "=", RowBox[{RowBox[{"-", SubscriptBox["k", "a"]}], " ", SubscriptBox["A", "g"]}]}]],
displayMath[RowBox[{FractionBox[RowBox[{"\[DifferentialD]", SubscriptBox["A", "1"]}], RowBox[{"\[DifferentialD]", "t"}]],
   "=", RowBox[{
     RowBox[{SubscriptBox["k", "a"], " ", SubscriptBox["A", "g"]}], "-",
     RowBox[{FractionBox["CL", SubscriptBox["V", "1"]], " ", SubscriptBox["A", "1"]}], "-",
     RowBox[{FractionBox["Q", SubscriptBox["V", "1"]], " ", SubscriptBox["A", "1"]}], "+",
     RowBox[{FractionBox["Q", SubscriptBox["V", "2"]], " ", SubscriptBox["A", "2"]}]}]}]],
displayMath[RowBox[{FractionBox[RowBox[{"\[DifferentialD]", SubscriptBox["A", "2"]}], RowBox[{"\[DifferentialD]", "t"}]],
   "=", RowBox[{
     RowBox[{FractionBox["Q", SubscriptBox["V", "1"]], " ", SubscriptBox["A", "1"]}], "-",
     RowBox[{FractionBox["Q", SubscriptBox["V", "2"]], " ", SubscriptBox["A", "2"]}]}]}]],

para[{"with the oral dose as initial condition ",
   imath@RowBox[{SubscriptBox["A", "g"], "(", "0", ")", "=", "D"}],
   ", ", imath@RowBox[{SubscriptBox["A", "1"], "(", "0", ")", "=",
     RowBox[{SubscriptBox["A", "2"], "(", "0", ")"}], "=", "0"}],
   ", and the measured concentration ",
   imath@RowBox[{"C", "(", "t", ")", "=",
     RowBox[{SubscriptBox["A", "1"], "/", SubscriptBox["V", "1"]}]}], "."}],

subsec["3.2  The six parameters, one by one"],

para[{"These are the unknowns we infer \[LongDash] each a physical \
quantity a clinician can interpret:"}],

item[{mono["ka"], " \[LongDash] ", bold["absorption rate"], " (1/h). How \
fast the drug leaves the gut for the blood; it sets the steepness of the \
rising limb and the time-to-peak."}],
item[{mono["CL"], " \[LongDash] ", bold["clearance"], " (L/h). Volume of \
blood completely cleared of drug per hour. It alone fixes the total \
exposure ", imath@RowBox[{"AUC", "=", RowBox[{"D", "/", "CL"}]}],
   " \[LongDash] the single most important dosing parameter."}],
item[{mono["V1"], " \[LongDash] ", bold["central volume"], " (L). Relates \
dose to the initial concentration; small ", sub["V", "1"],
   " means a high peak."}],
item[{mono["Q"], " \[LongDash] ", bold["inter-compartmental clearance"],
   " (L/h). How fast drug exchanges with the tissues; it shapes the bend \
between the fast and slow phases."}],
item[{mono["V2"], " \[LongDash] ", bold["peripheral volume"], " (L). How \
much drug the tissues hold; it stretches the slow elimination tail."}],
item[{mono["\[Sigma]"], " \[LongDash] ", bold["residual scatter"], " \
(mg/L). The combined assay-plus-model noise on each measurement \[LongDash] \
itself an unknown to be estimated."}],

para[{"From these follow the quantities dosing actually turns on: \
half-life ", imath@RowBox[{SubscriptBox["t", RowBox[{"1", "/", "2"}]], "\[Proportional]",
     RowBox[{SubscriptBox["V", "1"], "/", "CL"}]}],
   ", exposure ", imath@RowBox[{"D", "/", "CL"}], ", peak concentration, and \
the steady state reached under repeated dosing."}],

subsec["3.3  A closed form \[LongDash] no solver needed"],

para[{"Because the system is linear, it has an exact solution: a sum of \
three exponentials in the macro-rate constants ", math["\[Alpha]"], ", ",
   math["\[Beta]"], " (the eigenvalues of the compartment matrix) and ",
   sub["k", "a"], ". This makes each likelihood evaluation a handful of ",
   mono["Exp"], "s rather than an ODE solve:"}],

displayMath[RowBox[{RowBox[{"C", "(", "t", ")"}], "=",
   FractionBox[RowBox[{SubscriptBox["k", "a"], " ", "D"}], SubscriptBox["V", "1"]],
   RowBox[{"(",
     RowBox[{RowBox[{SubscriptBox["A", "\[Alpha]"], SuperscriptBox["e", RowBox[{"-", "\[Alpha]", " ", "t"}]]}], "+",
       RowBox[{SubscriptBox["A", "\[Beta]"], SuperscriptBox["e", RowBox[{"-", "\[Beta]", " ", "t"}]]}], "+",
       RowBox[{SubscriptBox["A", "k"], SuperscriptBox["e", RowBox[{"-", SubscriptBox["k", "a"], " ", "t"}]]}]}],
   ")"}]}]],

wlIn["(* The closed-form concentration; ka, CL, V1, Q, V2, dose. *)
Plot[PKConcentration[t, {1.5, 2.8, 30., 2.0, 20.}, 320.], {t, 0, 25},
   Frame -> True, FrameLabel -> {\"time (h)\", \"conc (mg/L)\"},
   PlotStyle -> RGBColor[0.153, 0.51, 0.64]]"],

callout[bold["Why bother with six parameters? "],
   "A simpler one-compartment model has three. The second compartment is \
what lets the curve bend \[LongDash] a fast distribution phase then a slow \
elimination phase \[LongDash] which real drugs do. The cost is that the two \
tissue parameters (", mono["Q"], ", ", mono["V2"], ") are only weakly \
pinned down by a single subject\[CloseCurlyQuote]s sparse samples. Holding \
honest uncertainty about them, rather than a false-precision point \
estimate, is exactly where the Bayesian treatment earns its keep."]
}];


(* ==================================================================
   4. THE BAYESIAN MODEL
   ================================================================== *)
writeAll[{

section["4.  The Bayesian model, and why it is all integrals"],

para[{"Each measurement is the model prediction plus Gaussian scatter, ",
   imath@RowBox[{SubscriptBox["c", "i"], "=",
     RowBox[{RowBox[{"C", "(", SubscriptBox["t", "i"], ")"}], "+", SubscriptBox["\[Epsilon]", "i"]}]}],
   " with ", imath@RowBox[{SubscriptBox["\[Epsilon]", "i"], "\[Tilde]",
     RowBox[{"N", "(", "0", ",", SuperscriptBox["\[Sigma]", "2"], ")"}]}],
   ". All six parameters are positive, so each gets a ", bold["lognormal"],
   " prior (a normal prior on its logarithm). Bayes\[CloseCurlyQuote] rule \
gives the posterior:"}],

displayMath[RowBox[{RowBox[{"p", "(", "\[Theta]", "\[VerticalSeparator]", "c", ")"}],
   "\[Proportional]",
   RowBox[{UnderoverscriptBox["\[Product]", RowBox[{"i", "=", "1"}], "n"],
     RowBox[{"N", "(", RowBox[{SubscriptBox["c", "i"], "\[VerticalSeparator]",
       RowBox[{"C", "(", SubscriptBox["t", "i"], ")"}], ",", SuperscriptBox["\[Sigma]", "2"]}], ")"}]}],
   " ", RowBox[{"\[Pi]", "(", "\[Theta]", ")"}]}]],

para[{"Now the key point. Everything we want from this posterior is an ",
   bold["expectation"], " \[LongDash] an integral over the six-dimensional \
parameter space:"}],

displayMath[RowBox[{RowBox[{SubscriptBox["E", "post"], "[", RowBox[{"g", "(", "\[Theta]", ")"}], "]"}],
   "=", RowBox[{"\[Integral]", RowBox[{"g", "(", "\[Theta]", ")"}], " ",
     RowBox[{"p", "(", "\[Theta]", "\[VerticalSeparator]", "c", ")"}],
     RowBox[{"\[DifferentialD]", "\[Theta]"}]}]}]],

para[{"Posterior mean clearance? Take ", imath@RowBox[{"g", "=", "CL"}],
   ". Its uncertainty? ", imath@RowBox[{"g", "=", SuperscriptBox["CL", "2"]}],
   ". Probability the half-life exceeds a threshold? ", math["g"],
   " an indicator. The predicted concentration at the next clinic visit? ",
   imath@RowBox[{"g", "=", RowBox[{"C", "(", SubscriptBox["t", "*"], ")"}]}],
   ". Six-dimensional integrals of a smooth density, with no closed form: \
the exact setting where the choice of integration rule \[LongDash] \
Monte-Carlo or quasi-Monte-Carlo \[LongDash] decides the cost."}]
}];


(* ==================================================================
   5. CLASSICAL vs BAYESIAN
   ================================================================== *)
writeAll[{

section["5.  Classical curve-fitting versus the Bayesian posterior"],

para[{ital["The classical reflex."], " Hand this data to a statistician and \
the first move is ", bold["nonlinear least squares"], " \[LongDash] in \
Wolfram Language, ", mono["NonlinearModelFit"], " \[LongDash] which slides \
the six parameters until the squared residuals are smallest. It is fast and \
gives a tidy best-fit curve:"}],

wlIn["model = PKConcentration[t, {ka, CL, V1, Q, V2}, subj1[\"dose\"]];
nlm = NonlinearModelFit[Transpose[{subj1[\"t\"], subj1[\"c\"]}], model,
   {{ka, 1.5}, {CL, 2.8}, {V1, 30}, {Q, 2.0}, {V2, 20}}, t];
{nlm[\"BestFitParameters\"], nlm[\"ParameterErrors\"]}"],

para[{ital["And the trouble it hides."], " The point estimate is \
fine, but ask for its error bars and the asymptotic standard errors come \
back enormous \[LongDash] of order ",
   imath@SuperscriptBox["10", "6"], "\[Dash]", imath@SuperscriptBox["10", "7"],
   " on ", math["CL"], ", ", sub["V", "1"], ", ", sub["V", "2"],
   ", with ", sub["V", "2"], " driven to essentially zero. The reason is \
real: a two-compartment model is ", bold["not identifiable"], " from one \
subject\[CloseCurlyQuote]s eleven points, so the least-squares surface is \
nearly flat in some directions and the linearized covariance blows up. \
Classical fitting gives you a curve but no trustworthy uncertainty."}],

imgCell["classical.png"],
caption["Figure 3.  Same data, two philosophies. ", bold["Left:"], " \
least squares returns a single best-fit curve \[LongDash] and, because the \
model is unidentifiable here, no usable error band. ", bold["Right:"], " \
the Bayesian posterior (sampled by QMC) delivers a full predictive band; \
the lognormal priors gently regularize the directions the data \
cannot constrain."],

para[{"The Bayesian posterior fixes this honestly: the priors keep the \
weakly-identified directions finite, and the answer is a full joint \
distribution \[LongDash] every parameter, every prediction, with calibrated \
uncertainty. The catch is the one from \[Section]4: you must integrate. The \
options:"}],

item[{bold["Grid quadrature"], " \[LongDash] exact-ish in 1\[Dash]2 \
dimensions, hopeless in six (", imath@RowBox[{"10", " ", "points", "/",
     "dim"}], " is ", imath@SuperscriptBox["10", "6"], " evaluations, and \
the accuracy is still poor)."}],
item[{bold["Markov-chain Monte Carlo (MCMC)"], " \[LongDash] the standard \
Bayesian workhorse; it samples the posterior directly but inherits the ",
   powN[RowBox[{"-", "1", "/", "2"}]], " Monte-Carlo rate (worse, after \
autocorrelation) and needs tuning and convergence checks."}],
item[{bold["Laplace approximation"], " \[LongDash] fit a Gaussian at the \
posterior mode; cheap, and the basis we build on here."}],
item[{bold["Quasi-Monte Carlo"], " \[LongDash] deterministic \
low-discrepancy points, near ", powN[RowBox[{"-", "1"}]],
   " for smooth integrands. For moderate-dimensional, smooth posteriors it \
is the most evaluations-efficient of the lot \[LongDash] and it is a \
drop-in replacement for the random draws."}],

para[{"QMC does not compete with MCMC so much as complement it: where you \
can reparameterize the posterior to be smooth and roughly Gaussian \
(Bayesian asymptotics says you usually can), QMC integration buys back most \
of the lost power. That is the recipe we follow next."}]
}];


(* ==================================================================
   6. THE FIT
   ================================================================== *)
writeAll[{

section["6.  The fit"],

para[{ital["From posterior to a unit-cube integral."], " We build the ",
   bold["Laplace\[Dash]Gaussian"], " approximation \[LongDash] posterior \
mode plus curvature in log-parameter space, a multivariate normal ",
   imath@RowBox[{"N", "(", "\[Mu]", ",", "\[CapitalSigma]", ")"}], " on ",
   imath@RowBox[{"\[CurlyPhi]", "=", RowBox[{"log", " ", "\[Theta]"}]}],
   " \[LongDash] and map the unit cube onto it with the inverse-CDF \
transform ", imath@RowBox[{"\[CurlyPhi]", "(", "u", ")", "=",
     RowBox[{"\[Mu]", "+", RowBox[{"L", " ",
       SuperscriptBox["\[CapitalPhi]", RowBox[{"-", "1"}]], "(", "u", ")"}]}]}],
   ", ", imath@RowBox[{"L", SuperscriptBox["L", "T"], "=", "\[CapitalSigma]"}],
   ". A posterior expectation becomes an integral over ",
   imath@SuperscriptBox[RowBox[{"[", "0", ",", "1", "]"}], "6"],
   " of a smooth, weight-free integrand \[LongDash] ready for QMC."}],

wlIn["(* Draw from the posterior with a Sobol' set; form the predictive band *)
draws  = PosteriorDraws[LDPoints[\"Sobol\", 4096, 6], pkLaplace];
tGrid  = Subdivide[0., 25., 200];
curves = ConcentrationCurves[draws, tGrid, subj1[\"dose\"]];
band   = Transpose[curves];
{Quantile[#, 0.05] & /@ band, Quantile[#, 0.95] & /@ band};"],

imgCell["pk_fit.png"],
caption["Figure 4.  Theophylline Subject 1: the 11 measurements with the \
90% posterior-predictive band. The fit captures the absorption peak near \
2 h and the slow elimination tail."],

imgCell["posterior.png"],
caption["Figure 5.  Posterior marginals. Clearance ", math["CL"],
   ", central volume ", sub["V", "1"], ", and absorption ", sub["k", "a"],
   " are tightly determined; ", math["Q"], " and ", sub["V", "2"],
   " stay broad \[LongDash] honest uncertainty where the data are thin."],

callout[bold["Is the Gaussian approximation good enough? "],
   "We check against the full posterior via importance sampling (",
   mono["FitProposal"], " + ", mono["ImportanceEstimate"], "): the \
well-identified parameters agree to ~10%. And the convergence demonstration \
does not depend on the approximation being perfect \[LongDash] only on the \
integrand we actually integrate being smooth, which it is."]
}];


(* ==================================================================
   7. THE CONVERGENCE LAW
   ================================================================== *)
writeAll[{

section["7.  The convergence law"],

para[{"Now the measurement. We estimate two posterior expectations and \
watch the error fall with the number of points, for Monte Carlo, \
Sobol\[CloseCurlyQuote], and Halton: the ", bold["mean clearance"], " ",
   imath@RowBox[{SubscriptBox["E", "post"], "[", "CL", "]"}],
   " (which has an analytic value under the Gaussian posterior, so we know \
the truth exactly) and the ", bold["predicted concentration at 6 h"],
   " (no closed form \[LongDash] each evaluation a model solve). Errors are \
averaged over independent randomizations (a digital shift for \
Sobol\[CloseCurlyQuote], a Cranley\[Dash]Patterson rotation for Halton):"}],

wlIn["Nlist = 2^Range[7, 15];
refCL = LaplaceMeanExact[pkLaplace][[2]];          (* exact E[CL] *)
gCL   = #[[All, 2]] &;
rmseCL = Association[# -> ExpectationRMSE[#, Nlist, 200, pkLaplace, gCL, refCL] & /@
   {\"MonteCarlo\", \"Sobol\", \"Halton\"}];
ListLogLogPlot[Table[Transpose[{Nlist, rmseCL[m]}],
   {m, {\"MonteCarlo\", \"Sobol\", \"Halton\"}}], Joined -> True, Mesh -> All]"],

imgCell["convergence.png"],
caption["Figure 6.  RMSE versus ", math["N"], " (log-log). ", bold["Left:"],
   " posterior mean clearance (analytic reference). ", bold["Right:"],
   " posterior-predictive concentration at 6 h. Monte Carlo follows ",
   math["N\^(-1/2)"], " (slope \[TildeTilde] ", math["-0.50"],
   "); Sobol\[CloseCurlyQuote] and Halton follow ", math["N\^(-1)"],
   " (slopes \[TildeTilde] ", math["-0.95"], "). At ", math["N = 32768"],
   " Sobol\[CloseCurlyQuote] is ~50\[Times] more accurate, and the gap keeps \
widening."],

para[{"That is the headline, on real data: the same number of model solves \
buys nearly twice the correct digits with Sobol\[CloseCurlyQuote] as with \
Monte Carlo."}]
}];


(* ==================================================================
   8. DIMENSION
   ================================================================== *)
writeAll[{

section["8.  Why dimension matters"],

para[{"The ", imath@SuperscriptBox[RowBox[{"(", "log", " ", "N", ")"}], "d"],
   " factor is not cosmetic: filling a cube evenly gets exponentially harder \
as the dimension grows, so the QMC advantage erodes. Fix the budget ",
   math["N"], " and measure how many times more accurate \
Sobol\[CloseCurlyQuote] is than MC on a smooth test integral, across \
dimension:"}],

wlIn["cAmp  = 0.6;
fMean = Function[pts, Mean[Times @@@ (1 + cAmp Cos[2 Pi pts])]];   (* exact 1 *)
rmseAt[meth_, d_, n_] := Sqrt@Mean@Table[
   (fMean[RandomizedLDPoints[meth, n, d, 53 r + d]] - 1.)^2, {r, 120}];
gain[n_] := Table[rmseAt[\"MonteCarlo\", d, n]/rmseAt[\"Sobol\", d, n], {d, 2, 12}];
{gain[4096], gain[65536]}"],

imgCell["dimension.png"],
caption["Figure 7.  Sobol\[CloseCurlyQuote] efficiency gain (MC RMSE / \
Sobol\[CloseCurlyQuote] RMSE, fixed budget) versus dimension. Tens to \
hundreds of times more accurate in low dimension, decaying toward parity \
as the ", math["(log N)\^d"], " penalty bites. The six-parameter PK problem \
sits where the gain is still large."]
}];


(* ==================================================================
   9. SYNTHETIC RECOVERY
   ================================================================== *)
writeAll[{

section["9.  Does it recover the truth?"],

para[{"A fast wrong answer is worthless, so we generate data from known \
parameters, fit, and check the posterior brackets them:"}],

wlIn["thetaTrue = {1.20, 2.50, 30.0, 1.80, 22.0, 0.45};
{synProblem, synConc} = SyntheticProblem[thetaTrue, 320.0,
   {0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 20, 24, 30},
   priorMedians, priorLogSD, 4242];
synDraws = PosteriorDraws[LDPoints[\"Sobol\", 8192, 6],
   LaplacePosterior[synProblem]];
Mean /@ Transpose[synDraws]"],

imgCell["synthetic.png"],
caption["Figure 8.  Posterior estimate / true value, with 90% credible \
intervals (red line = truth). The well-identified parameters (",
   sub["k", "a"], ", ", math["CL"], ", ", sub["V", "1"],
   ") land on the truth; ", math["Q"], " and ", sub["V", "2"],
   " carry wide intervals that still cover it. Correct, and honest about \
what the data cannot say."]
}];


(* ==================================================================
   10. PAY-OFF
   ================================================================== *)
writeAll[{

section["10.  The practical pay-off"],

para[{"Slopes are abstract; model solves are money. Inverting the fitted \
error laws \[LongDash] ", imath@RowBox[{"N", "\[Proportional]", SuperscriptBox["tol", RowBox[{"-", "2"}]]}],
   " for Monte Carlo, ", imath@RowBox[{"N", "\[Proportional]", SuperscriptBox["tol", RowBox[{"-", "1"}]]}],
   " for QMC \[LongDash] gives the number of forward-model evaluations \
needed for a target accuracy, and the ratio is the speed-up:"}],

wlIn["fitPow[e_] := Module[{c = Fit[Transpose[{Log[Nlist], Log[e]}], {1, x}, x]},
   {Exp[c /. x -> 0], Coefficient[c, x]}];
nForTol[e_, tol_] := With[{cp = fitPow[e]}, (cp[[1]]/tol)^(1/(-cp[[2]]))];
nForTol[rmsePred[\"MonteCarlo\"], 0.001 refPred] /
   nForTol[rmsePred[\"Sobol\"], 0.001 refPred]"],

imgCell["payoff.png"],
caption["Figure 9.  Forward-model solves to reach a target accuracy on the \
predicted concentration. Because MC needs ", math["O(tol\^(-2))"],
   " and QMC ", math["O(tol\^(-1))"], ", the gap widens with the accuracy \
demanded: ~2\[Times] at 1%, ~4\[Times] at 0.5%, ~18\[Times] at 0.1%."],

para[{"That is the value proposition in one figure. QMC changes neither the \
model nor the estimand \[LongDash] only ", ital["which points you evaluate \
at"], " \[LongDash] and that single swap buys back most of an order of \
magnitude in expensive model runs."}]
}];


(* ==================================================================
   11. THE HERO ANIMATION (code)
   ================================================================== *)
writeAll[{

section["11.  The cover animation"],

para[{"For completeness, here is the code behind the animation at the top \
\[LongDash] the same Monte-Carlo and Sobol\[CloseCurlyQuote] point sets \
accumulating, with the running integration error traced beneath. It is the \
whole post compressed into ", math["1/N"], " versus ", math["1/\[Sqrt]N"],
   ":"}],

codeIn["frameNs = Round[2^Range[3, 12]];
fInt[pt_] := Exp[Total[pt]];   exact = (E - 1.)^2;
mcAll  = BlockRandom[SeedRandom[7]; RandomReal[1, {4096, 2}]];
sobAll = LDPoints[\"Sobol\", 4096, 2];
runErr[pts_] := Abs[Mean[fInt /@ pts] - exact];
(* each frame: MC cloud | Sobol cloud, with the error-vs-N race below *)
frames = Table[ Rasterize @ Column[{
     Row[{ListPlot[mcAll[[1 ;; n]]], ListPlot[sobAll[[1 ;; n]]]}],
     ListLogLogPlot[{ {#, runErr[mcAll[[1 ;; #]]]} & /@ Select[frameNs, # <= n &],
                      {#, runErr[sobAll[[1 ;; #]]]} & /@ Select[frameNs, # <= n &] },
        Joined -> True] }], {n, frameNs}];
Export[\"hero.gif\", AnimatedImage[frames, FrameRate -> 1.4]]"]
}];


(* ==================================================================
   12. CONCLUSIONS
   ================================================================== *)
writeAll[{

section["12.  Conclusions"],

para[{"On a real six-parameter pharmacokinetic inference, deterministic \
low-discrepancy sampling beats plain Monte Carlo by close to a full power \
of ", math["N"], " \[LongDash] measured slopes near ",
   imath@RowBox[{"-", "0.95"}], " against ", imath@RowBox[{"-", "0.5"}],
   ", and an order-of-magnitude saving in model evaluations at useful \
accuracies. Against classical least squares, the Bayesian treatment adds \
honest, regularized uncertainty where an unidentifiable model would \
otherwise report nonsense error bars \[LongDash] and QMC is what makes \
computing that uncertainty cheap."}],

para[{ital["When QMC helps, and when it does not."], " It helps when the \
dimension is low-to-moderate or the effective dimension is small, the \
integrand is smooth, and evaluations are the bottleneck. It does not help \
for rough or discontinuous integrands, very high dimension, or \
heavy-tailed importance weights \[LongDash] which is why we integrate \
against a smooth Gaussianized posterior rather than a spiky ratio."}],

para[{ital["The portable recipe."], " (1) reparameterize to an \
unconstrained space and build a Gaussian approximation of the posterior; \
(2) map the unit cube through it with the inverse-CDF transform; (3) swap \
your pseudo-random draws for a randomized Sobol\[CloseCurlyQuote] or Halton \
set; (4) average over a few randomizations for an error bar. Two lines of \
change, most of an order of magnitude back."}]
}];


(* ==================================================================
   13. REFERENCES + REPRODUCIBILITY
   ================================================================== *)
writeAll[{

section["13.  References & reproducibility"],

para[{bold["Halton, J. H."], " (1960). ", ital["Numerische Mathematik"],
   " 2, 84\[Dash]90.  \[Bullet]  ", bold["Sobol\[CloseCurlyQuote], I. M."],
   " (1967). ", ital["USSR Comp. Math. Math. Phys."], " 7, 86\[Dash]112.  \
\[Bullet]  ", bold["Joe, S., & Kuo, F. Y."], " (2008). ", ital["SIAM J. \
Sci. Comput."], " 30, 2635\[Dash]2654 (the direction numbers used here)."}],

para[{bold["Niederreiter, H."], " (1992). ", ital["Random Number \
Generation and Quasi-Monte Carlo Methods"], ", SIAM.  \[Bullet]  ",
   bold["Caflisch, R. E."], " (1998). ", ital["Acta Numerica"], " 7, 1\
\[Dash]49 (effective dimension).  \[Bullet]  ", bold["Owen, A. B."],
   " (1998). ", ital["J. Complexity"], " 14, 466\[Dash]489 (randomized \
QMC).  \[Bullet]  ", bold["Gerber, M., & Chopin, N."], " (2015). ",
   ital["JRSS B"], " 77, 509\[Dash]579 (QMC in Bayesian computation)."}],

para[{bold["Boeckmann, Sheiner & Beal"], " (1994). ", ital["NONMEM \
Users Guide"], " (the Theophylline data).  \[Bullet]  ",
   bold["Gabrielsson & Weiner"], " (2016). ", ital["Pharmacokinetic and \
Pharmacodynamic Data Analysis"], " (the two-compartment model)."}],

para[{"All code is pure Wolfram Language at ",
   link["github.com/mthiel74/QuasiMonteCarloBayesianEstimation",
     "https://github.com/mthiel74/QuasiMonteCarloBayesianEstimation"],
   ". The companion package ", mono["qmc_bayes_helpers.wl"], " is the single \
source of truth; the figure scripts and this notebook both load it. To \
rebuild:"}],

codeIn["wolframscript -file wolfram/fetch_theoph.wls   # real data -> data/
wolframscript -file wolfram/run_all.wls        # all figures -> docs/images/
wolframscript -file community/build_notebook.wls"],

para[{Style["Generated " <> DateString[Now, {"MonthName", " ", "DayShort",
    ", ", "Year"}] <> ".", FontSlant -> Italic, FontColor -> GrayLevel[0.45]]}]
}];


(* ---- assemble + save ---- *)
Print["assembled ", Length[$allCells], " cells"];

TimeConstrained[
   UsingFrontEnd[
      nbObj = CreateDocument[$allCells, Visible -> False,
         WindowSize -> {1000, 820}, StyleDefinitions -> styleDefs];
      NotebookSave[nbObj, outNb];
      Print["wrote ", outNb, " (", FileByteCount[outNb], " B)"];
      Quiet @ Check[Export[outPdf, nbObj];
         Print["wrote ", outPdf, " (", FileByteCount[outPdf], " B)"],
         Print["warn: PDF export failed"]];
      NotebookClose[nbObj];
   ], 700, Print["warn: notebook save timed out"]];
