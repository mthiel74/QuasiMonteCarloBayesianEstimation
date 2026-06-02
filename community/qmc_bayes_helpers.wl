(* ::Package:: *)

(*  qmc_bayes_helpers.wl
    ===================================================================
    Companion package for the Wolfram Community notebook
    "Quasi-Monte Carlo for a real Bayesian estimation".

    Upload this file as an attachment alongside the .nb.  The notebook's
    Setup cell does:

        SetDirectory[NotebookDirectory[]];
        Get["qmc_bayes_helpers.wl"];

    after which every Input cell in the post is runnable.  Each long
    function below is *called* from the notebook with all of its
    arguments spelled out inline, so a reader sees the full call in the
    main notebook and the implementation here.

    Contents
    --------
      Low-discrepancy point sets
            LDPoints["Sobol"|"Halton"|"MonteCarlo", n, d]
            RandomizedLDPoints[...]   (digital shift / CP rotation / reseed)
            StarDiscrepancy2D
      Pharmacokinetics
            PKConcentration[t, {ka,CL,V1,Q,V2}, dose]   (t may be a vector)
      Bayesian problem
            BayesProblem[dose, tObs, cObs, priorMedians, priorLogSD]
            LogPosterior[theta, problem]                 (scalar)
            FitProposal[problem]                         (adaptive Gaussian)
      Estimators / experiments
            ImportanceEstimate[points, proposal, problem]
            ConvergenceStudy[method, Nlist, R, proposal, problem, ref]
            ReferenceValue[proposal, problem, nRef]
      Data
            LoadTheoph[]   SelectSubject[theoph, id]
*)

BeginPackage["QMCBayes`"];

LDPoints::usage = "LDPoints[\"Sobol\"|\"Halton\"|\"MonteCarlo\", n, d] \
returns an n*d matrix of points in [0,1]^d.  Sobol' uses Joe-Kuo \
direction numbers, Halton uses radical inverses in the first d primes, \
MonteCarlo uses pseudo-random points (with the current random state).";

RandomizedLDPoints::usage = "RandomizedLDPoints[method, n, d, seed] \
returns one randomized realization of an n*d point set: Sobol' gets a \
random digital shift (XOR), Halton a Cranley-Patterson rotation, \
MonteCarlo a reseeded pseudo-random draw.";

StarDiscrepancyL2::usage = "StarDiscrepancyL2[pts] is the L2 star \
discrepancy of an n*d point set, computed exactly by Warnock's formula \
(O(n^2 d)).  Lower means more uniform; it decays like N^-1/2 for random \
points and ~N^-1 for low-discrepancy sequences.";

PKConcentration::usage = "PKConcentration[t, {ka,CL,V1,Q,V2}, dose] is the \
plasma concentration of a single oral dose under a two-compartment model \
with first-order absorption (closed-form sum of three exponentials). \
t may be a scalar or a vector.";

ConcentrationCurves::usage = "ConcentrationCurves[paramMat, tVec, dose] is \
the vectorized predictive: for an n*5 (or n*6) matrix of parameter draws it \
returns an n*Length[tVec] matrix of concentrations.  Used for posterior- \
predictive bands and the predictive convergence study.";

BayesProblem::usage = "BayesProblem[dose, tObs, cObs, priorMedians, \
priorLogSD] packages the data and the lognormal priors into an \
association passed to the estimators.  priorMedians and priorLogSD are \
length-6 lists for {ka, CL, V1, Q, V2, sigma}.";

LogPosterior::usage = "LogPosterior[theta, problem] is the unnormalized \
log-posterior at the 6-vector theta (natural parameters).";

FitProposal::usage = "FitProposal[problem] returns an adaptive Gaussian \
proposal in log-parameter space: Laplace mode, then a short \
population-Monte-Carlo refinement of the mean and covariance.  Returns \
<|\"mean\",\"chol\",\"cholInv\",\"mode\",\"cov\",\"ess\"|>.";

LaplacePosterior::usage = "LaplacePosterior[problem] returns the Laplace \
(Gaussian) approximation to the posterior in log-parameter space: \
<|\"mean\",\"chol\",\"cov\",\"sd\",\"mode\"|>.  Posterior expectations of \
smooth functionals computed against this Gaussian are the integrals whose \
QMC vs MC convergence the notebook measures.";

PosteriorDraws::usage = "PosteriorDraws[points, laplace] maps a [0,1]^6 \
point set through the Laplace-Gaussian posterior and returns an n*6 matrix \
of parameter draws theta = {ka,CL,V1,Q,V2,sigma} (natural scale).";

LaplaceMeanExact::usage = "LaplaceMeanExact[laplace] returns the exact \
posterior means E[theta_i] = exp(mu_i + Sigma_ii/2) under the \
Laplace-Gaussian approximation (the analytic reference for the \
convergence study).";

ExpectationRMSE::usage = "ExpectationRMSE[method, Nlist, R, laplace, gFun, \
gRef] returns, for each N in Nlist, the RMSE over R randomizations of the \
QMC/MC estimate of E[gFun[theta]] against the reference value gRef.  gFun \
maps an n*6 draw matrix to a length-n vector.";

ImportanceEstimate::usage = "ImportanceEstimate[points, proposal, problem] \
maps a [0,1]^6 point set through the proposal and returns the \
importance-sampling estimate <|\"evidence\",\"logEvidence\",\"postMean\", \
\"ess\",\"essFraction\"|>.";

ConvergenceStudy::usage = "ConvergenceStudy[method, Nlist, R, proposal, \
problem, ref] returns, for each N in Nlist, the RMSE (over R independent \
randomizations) of the log-evidence and the posterior-mean vector, \
relative to the reference `ref`.";

ReferenceValue::usage = "ReferenceValue[proposal, problem, nRef] returns a \
high-accuracy reference <|\"logEvidence\",\"postMean\"|> from a single \
large Sobol' point set.";

LoadTheoph::usage = "LoadTheoph[] loads the Theophylline data set from \
data/theoph.csv if present, otherwise live-fetches it.  Returns a list of \
rows {subject, weight, dosePerKg, time, conc}.";

SelectSubject::usage = "SelectSubject[theoph, id] returns \
<|\"dose\",\"t\",\"c\"|> for one subject (total dose in mg).";

SyntheticProblem::usage = "SyntheticProblem[thetaTrue, dose, tObs, \
priorMedians, priorLogSD, seed] generates noisy concentrations from \
thetaTrue and returns {problem, cObs}.";

paramNames::usage = "paramNames -- {ka, CL, V1, Q, V2, sigma}.";

Begin["`Private`"];

paramNames = {"ka", "CL", "V1", "Q", "V2", "\[Sigma]"};

(* =================================================================
   1.  LOW-DISCREPANCY POINT SETS
   ================================================================= *)

(* ---- Halton: radical inverse in prime bases ---- *)
firstPrimes = Prime[Range[20]];

radicalInverse[nVec_List, b_] := Module[{f, r, i, active},
   f = ConstantArray[1./b, Length[nVec]];
   r = ConstantArray[0., Length[nVec]];
   i = nVec;
   active = True;
   While[Max[i] > 0,
     r += f*Mod[i, b];
     i = Quotient[i, b];
     f /= b];
   r];

haltonMatrix[n_, d_] := haltonMatrix[n, d] = Transpose[
   radicalInverse[Range[n], firstPrimes[[#]]] & /@ Range[d]];

(* ---- Sobol': Joe-Kuo direction numbers, dims 1..20 ---- *)
sobolBits = 31;

(* {s, a, {m_1..m_s}} for dimensions 2..20 (new-joe-kuo-6.21201). *)
jkTable = <|
   2 -> {1, 0, {1}},            3 -> {2, 1, {1, 3}},
   4 -> {3, 1, {1, 3, 1}},      5 -> {3, 2, {1, 1, 1}},
   6 -> {4, 1, {1, 1, 3, 3}},   7 -> {4, 4, {1, 3, 5, 13}},
   8 -> {5, 2, {1, 1, 5, 5, 17}}, 9 -> {5, 4, {1, 1, 5, 5, 5}},
   10 -> {5, 7, {1, 1, 7, 11, 19}}, 11 -> {5, 11, {1, 1, 5, 1, 1}},
   12 -> {5, 13, {1, 1, 1, 3, 11}}, 13 -> {5, 14, {1, 3, 5, 5, 31}},
   14 -> {6, 1, {1, 3, 3, 9, 7, 49}}, 15 -> {6, 13, {1, 1, 1, 15, 21, 21}},
   16 -> {6, 16, {1, 3, 1, 13, 27, 49}}, 17 -> {6, 19, {1, 1, 1, 15, 7, 5}},
   18 -> {6, 22, {1, 3, 1, 15, 13, 25}}, 19 -> {6, 25, {1, 1, 5, 5, 19, 61}},
   20 -> {7, 1, {1, 3, 7, 11, 23, 15, 103}}|>;

directionNumbers[dim_] := Module[{s, a, m, polyBits, k, val},
   If[dim == 1,
      Return[Table[BitShiftLeft[1, sobolBits - k], {k, sobolBits}]]];
   {s, a, m} = jkTable[dim];
   m = PadRight[m, sobolBits];
   polyBits = IntegerDigits[a, 2, s - 1];   (* a_1 .. a_{s-1} *)
   Do[
     val = BitXor[BitShiftLeft[m[[k - s]], s], m[[k - s]]];
     Do[If[polyBits[[i]] == 1,
          val = BitXor[val, BitShiftLeft[m[[k - i]], i]]], {i, s - 1}];
     m[[k]] = val,
    {k, s + 1, sobolBits}];
   Table[BitShiftLeft[m[[k]], sobolBits - k], {k, sobolBits}]];

$dirCache = <||>;
dirFor[d_] := Lookup[$dirCache, d,
   $dirCache[d] = Association[# -> directionNumbers[#] & /@ Range[d]]];

(* Integer Sobol' coordinates for indices i = 1..n (origin i=0 omitted
   so no coordinate is exactly 0).  Vectorized over the <=31 bit planes
   instead of looping over points: for each bit k, every index whose
   k-th bit is set gets its coordinate XORed with direction number k.
   Memoized, because the convergence study asks for the same n many
   times (once per randomization). *)
sobolIntMatrix[n_, d_] := sobolIntMatrix[n, d] =
   Module[{V = dirFor[d], idx = Range[n], cols},
      cols = Table[
         Module[{x = ConstantArray[0, n], on},
            Do[on = Pick[Range[n], BitGet[idx, k - 1], 1];
               If[on =!= {}, x[[on]] = BitXor[x[[on]], V[j][[k]]]],
             {k, sobolBits}];
            x],
         {j, d}];
      Transpose[cols]];

(* Centered scaling: (m + 1/2)/2^bits keeps every coordinate inside the
   open cube (0,1), so the inverse-normal transform never hits +-Inf. *)
sobolMatrix[n_, d_] := (sobolIntMatrix[n, d] + 0.5)/2.^sobolBits;

LDPoints["Sobol", n_, d_]      := sobolMatrix[n, d];
LDPoints["Halton", n_, d_]     := haltonMatrix[n, d];
LDPoints["MonteCarlo", n_, d_] := RandomReal[1, {n, d}];

(* ---- Randomized realizations ---- *)
RandomizedLDPoints["Sobol", n_, d_, seed_] := Module[{ints, shift},
   ints = sobolIntMatrix[n, d];   (* cached base net *)
   BlockRandom[SeedRandom[seed];
      shift = RandomInteger[{0, 2^sobolBits - 1}, d]];
   (BitXor[ints, ConstantArray[shift, n]] + 0.5)/2.^sobolBits];

RandomizedLDPoints["Halton", n_, d_, seed_] := Module[{pts, shift},
   pts = haltonMatrix[n, d];
   BlockRandom[SeedRandom[seed]; shift = RandomReal[1, d]];
   Mod[pts + ConstantArray[shift, n], 1.]];

RandomizedLDPoints["MonteCarlo", n_, d_, seed_] :=
   BlockRandom[SeedRandom[seed]; RandomReal[1, {n, d}]];

(* ---- L2 star discrepancy via Warnock's closed form (any dimension) ----
   (D*_2)^2 = 3^-d - (2^{1-d}/n) Sum_i prod_k (1 - x_ik^2)
             + (1/n^2) Sum_{i,j} prod_k (1 - max(x_ik, x_jk)).
   The double sum is built with Outer over each coordinate, O(n^2 d). *)
StarDiscrepancyL2[pts_] := Module[{n = Length[pts], d = Last@Dimensions[pts],
    term1, term2, prodMax},
   term1 = 3.^-d - (2.^(1 - d)/n) Total[Times @@@ (1 - pts^2)];
   prodMax = Times @@ Table[
      1 - Outer[Max, pts[[All, k]], pts[[All, k]]], {k, d}];
   term2 = Total[prodMax, 2]/n^2;
   Sqrt @ Max[term1 + term2, 0.]];

(* =================================================================
   2.  PHARMACOKINETICS  (two-compartment, first-order absorption)
   ================================================================= *)

PKConcentration[t_, {ka_, CL_, V1_, Q_, V2_}, dose_] := Module[
   {k10 = CL/V1, k12 = Q/V1, k21 = Q/V2, sm, df, al, be},
   sm = k10 + k12 + k21;
   df = Sqrt[sm^2 - 4 k10 k21];
   al = (sm + df)/2; be = (sm - df)/2;
   (ka dose/V1) (
      (k21 - al)/((ka - al) (be - al)) Exp[-al t] +
      (k21 - be)/((ka - be) (al - be)) Exp[-be t] +
      (k21 - ka)/((al - ka) (be - ka)) Exp[-ka t])];

ConcentrationCurves[paramMat_, tVec_, dose_] := pkBatch[
   paramMat[[All, 1 ;; 5]], N[tVec],
   If[ListQ[dose], N[dose], ConstantArray[N[dose], Length[tVec]]]];

(* Vectorized over an M*5 matrix of parameter rows; tVec length nt;
   doseVec length nt (per-observation dose, so several subjects with
   different doses can be pooled).  Returns an M*nt matrix. *)
pkBatch[paramMat_, tVec_, doseVec_] := Module[
   {ka, CL, V1, Q, V2, k10, k12, k21, sm, df, al, be, cA, cB, cK, pre},
   {ka, CL, V1, Q, V2} = Transpose[paramMat];
   k10 = CL/V1; k12 = Q/V1; k21 = Q/V2;
   sm = k10 + k12 + k21;
   df = Sqrt[Clip[sm^2 - 4 k10 k21, {0., Infinity}]];
   al = (sm + df)/2; be = (sm - df)/2;
   cA = (k21 - al)/((ka - al) (be - al));
   cB = (k21 - be)/((ka - be) (al - be));
   cK = (k21 - ka)/((al - ka) (be - ka));
   pre = Outer[Times, ka/V1, doseVec];   (* M*nt: ka_m dose_i / V1_m *)
   pre (cA Exp[-Outer[Times, al, tVec]] +
        cB Exp[-Outer[Times, be, tVec]] +
        cK Exp[-Outer[Times, ka, tVec]])];

(* =================================================================
   3.  BAYESIAN PROBLEM  (lognormal priors, Gaussian residual)
   ================================================================= *)

BayesProblem[dose_, tObs_, cObs_, priorMedians_, priorLogSD_] := <|
   (* dose may be a scalar (single dose for all samples) or a vector
      aligned with tObs (per-observation dose, for pooled data). *)
   "dose" -> If[ListQ[dose], N[dose], ConstantArray[N[dose], Length[tObs]]],
   "t" -> N[tObs], "c" -> N[cObs],
   "nt" -> Length[tObs],
   "priorLogMean" -> N[Log[priorMedians]],
   "priorLogSD"   -> N[priorLogSD]|>;

(* Unnormalized log-posterior in NATURAL parameters theta (length 6). *)
LogPosterior[theta_, problem_] :=
   First @ logPostBatchPhi[{Log[theta]}, problem];

(* Vectorized unnormalized log-posterior in LOG parameters phi (M*6).
   Returns a length-M vector; invalid rows -> -10^7. *)
logPostBatchPhi[phiMat_, problem_] := Module[
   {th, pred, resid, sse, sig, ll, lp, lm, ls, bad},
   th = Exp[phiMat];
   sig = th[[All, 6]];
   pred = pkBatch[th[[All, 1 ;; 5]], problem["t"], problem["dose"]];
   resid = pred - ConstantArray[problem["c"], Length[phiMat]];
   sse = Total[resid^2, {2}];
   ll = -sse/(2 sig^2) - problem["nt"] Log[sig];
   lm = problem["priorLogMean"]; ls = problem["priorLogSD"];
   lp = -Total[((phiMat - ConstantArray[lm, Length[phiMat]])/
                 ConstantArray[ls, Length[phiMat]])^2, {2}]/2;
   bad = Map[! (NumberQ[#] && Element[#, Reals]) &, ll + lp];
   MapThread[If[#2, -10.^7, #1] &, {ll + lp, bad}]];

(* =================================================================
   4.  ADAPTIVE GAUSSIAN PROPOSAL  (Laplace + population Monte Carlo)
   ================================================================= *)

laplaceMode[problem_] := Module[
   {f, x1, x2, x3, x4, x5, x6, vars, start, sol, phiStar, hess, sig0, h},
   (* NumericQ-guarded objective so FindMaximum skips symbolic
      preprocessing and only ever calls the batch evaluator on numbers. *)
   f[a_?NumericQ, b_?NumericQ, c_?NumericQ, dd_?NumericQ, e_?NumericQ,
      g_?NumericQ] := First @ logPostBatchPhi[{{a, b, c, dd, e, g}}, problem];
   vars = {x1, x2, x3, x4, x5, x6};
   start = problem["priorLogMean"];
   sol = Quiet @ FindMaximum[f @@ vars, Transpose[{vars, start}],
      MaxIterations -> 800, Method -> "QuasiNewton"];
   phiStar = vars /. sol[[2]];
   h = 0.01;
   hess = Table[
      With[{ei = UnitVector[6, i], ej = UnitVector[6, j]},
       (First@logPostBatchPhi[{phiStar + h ei + h ej}, problem]
        - First@logPostBatchPhi[{phiStar + h ei - h ej}, problem]
        - First@logPostBatchPhi[{phiStar - h ei + h ej}, problem]
        + First@logPostBatchPhi[{phiStar - h ei - h ej}, problem])/(4 h^2)],
      {i, 6}, {j, 6}];
   sig0 = Inverse[-(hess + Transpose[hess])/2];
   {phiStar, sig0}];

FitProposal[problem_] := Module[
   {phiStar, sig0, mu, cov, chol, cholInv, us, phis, lw, w, ess, np = 8000},
   {phiStar, sig0} = laplaceMode[problem];
   mu = phiStar; cov = 2.0^2 sig0;
   Do[
     chol = Transpose @ CholeskyDecomposition[cov];
     cholInv = Inverse[chol];
     BlockRandom[SeedRandom[2024 + it]; us = RandomReal[1, {np, 6}]];
     phis = ConstantArray[mu, np] +
        InverseCDF[NormalDistribution[], us] . Transpose[chol];
     lw = logPostBatchPhi[phis, problem] - mvnLogPDF[phis, mu, chol, cholInv];
     w = Exp[lw - Max[lw]]; w = w/Total[w];
     ess = 1./Total[w^2];
     mu = w . phis;
     cov = 1.3^2 Transpose[(phis - ConstantArray[mu, np])] .
              ((phis - ConstantArray[mu, np]) w),
    {it, 4}];
   chol = Transpose @ CholeskyDecomposition[cov];
   <|"mean" -> mu, "chol" -> chol, "cholInv" -> Inverse[chol],
     "mode" -> phiStar, "cov" -> cov, "ess" -> ess|>];

(* Multivariate-normal log pdf at rows of phiMat. *)
mvnLogPDF[phiMat_, mu_, chol_, cholInv_] := Module[{z, logdet},
   z = (phiMat - ConstantArray[mu, Length[phiMat]]) . Transpose[cholInv];
   logdet = 2 Total[Log[Abs[Diagonal[chol]]]];
   -Total[z^2, {2}]/2 - (6 Log[2 Pi] + logdet)/2];

(* =================================================================
   4b. LAPLACE-GAUSSIAN POSTERIOR  (the smooth-integrand target used
       for the clean convergence-rate demonstration)
   ================================================================= *)

LaplacePosterior[problem_] := Module[{phiStar, sig0, chol},
   {phiStar, sig0} = laplaceMode[problem];
   chol = Transpose @ CholeskyDecomposition[sig0];
   <|"mean" -> phiStar, "cov" -> sig0, "chol" -> chol,
     "sd" -> Sqrt[Diagonal[sig0]], "mode" -> phiStar|>];

(* Map a [0,1]^6 point set through the Gaussian posterior, no weights. *)
PosteriorDraws[points_, laplace_] := Exp[
   ConstantArray[laplace["mean"], Length[points]] +
   InverseCDF[NormalDistribution[], points] . Transpose[laplace["chol"]]];

LaplaceMeanExact[laplace_] :=
   Exp[laplace["mean"] + Diagonal[laplace["cov"]]/2];

ExpectationRMSE[method_, Nlist_, R_, laplace_, gFun_, gRef_] := Table[
   Sqrt @ Mean @ Table[
      (Mean[gFun[PosteriorDraws[
           RandomizedLDPoints[method, n, 6, 41 r + n], laplace]]] - gRef)^2,
      {r, R}],
   {n, Nlist}];

(* =================================================================
   5.  IMPORTANCE-SAMPLING ESTIMATOR  (full posterior; used to validate
       the Laplace approximation and for the practical pay-off)
   ================================================================= *)

(* Map a [0,1]^6 point set through the proposal, return log-weights,
   theta rows, and the proposal-mapped phi rows. *)
mapThroughProposal[points_, proposal_, problem_] := Module[{phis, lw, th},
   phis = ConstantArray[proposal["mean"], Length[points]] +
      InverseCDF[NormalDistribution[], points] . Transpose[proposal["chol"]];
   lw = logPostBatchPhi[phis, problem] -
        mvnLogPDF[phis, proposal["mean"], proposal["chol"], proposal["cholInv"]];
   th = Exp[phis];
   {lw, th}];

ImportanceEstimate[points_, proposal_, problem_] := Module[
   {lw, th, mx, w, sw, logZ, postMean, ess},
   {lw, th} = mapThroughProposal[points, proposal, problem];
   mx = Max[lw];
   w = Exp[lw - mx];
   sw = Total[w];
   logZ = mx + Log[sw/Length[points]];
   postMean = (w . th)/sw;
   ess = sw^2/Total[w^2];
   <|"logEvidence" -> logZ, "evidence" -> Exp[logZ],
     "postMean" -> postMean, "ess" -> ess,
     "essFraction" -> ess/Length[points]|>];

ReferenceValue[proposal_, problem_, nRef_] := Module[{est},
   est = ImportanceEstimate[LDPoints["Sobol", nRef, 6], proposal, problem];
   <|"logEvidence" -> est["logEvidence"], "postMean" -> est["postMean"]|>];

(* =================================================================
   6.  CONVERGENCE STUDY
   ================================================================= *)

ConvergenceStudy[method_, Nlist_, R_, proposal_, problem_, ref_] := Module[
   {rmseLogZ, rmsePM},
   {rmseLogZ, rmsePM} = Transpose @ Table[
      Module[{logZerr = {}, pmErr = {}, est},
        Do[
          est = ImportanceEstimate[
             RandomizedLDPoints[method, n, 6, 17 r + n], proposal, problem];
          AppendTo[logZerr, (est["logEvidence"] - ref["logEvidence"])^2];
          AppendTo[pmErr,
             Total[((est["postMean"] - ref["postMean"])/ref["postMean"])^2]/6],
         {r, R}];
        {Sqrt[Mean[logZerr]], Sqrt[Mean[pmErr]]}],
      {n, Nlist}];
   <|"method" -> method, "N" -> Nlist,
     "rmseLogEvidence" -> rmseLogZ, "rmsePostMeanRel" -> rmsePM|>];

(* =================================================================
   7.  DATA
   ================================================================= *)

LoadTheoph[] := Module[{file, raw, url, resp},
   file = FileNameJoin[{Directory[], "data", "theoph.csv"}];
   If[! FileExistsQ[file],
      file = FileNameJoin[{NotebookDirectory[], "theoph.csv"}]];
   If[FileExistsQ[file],
      Rest @ Import[file, "CSV"],
      url = "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/Theoph.csv";
      resp = URLRead[HTTPRequest[url]];
      If[resp["StatusCode"] =!= 200, Return[$Failed]];
      raw = Rest @ ImportString[resp["Body"], "CSV"];
      {ToExpression[ToString[#[[2]]]], #[[3]], #[[4]], #[[5]], #[[6]]} & /@ raw]];

SelectSubject[theoph_, id_] := Module[{rows},
   rows = SortBy[Select[theoph, #[[1]] == id &], #[[4]] &];
   <|"dose" -> rows[[1, 2]] rows[[1, 3]],
     "t" -> rows[[All, 4]], "c" -> rows[[All, 5]]|>];

SyntheticProblem[thetaTrue_, dose_, tObs_, priorMedians_, priorLogSD_, seed_] :=
   Module[{clean, cObs},
   clean = PKConcentration[tObs, thetaTrue[[1 ;; 5]], dose];
   BlockRandom[SeedRandom[seed];
      cObs = clean + RandomVariate[NormalDistribution[0, thetaTrue[[6]]],
                Length[tObs]]];
   {BayesProblem[dose, tObs, cObs, priorMedians, priorLogSD], cObs}];

End[];
EndPackage[];

Print["QMCBayes loaded: LDPoints, RandomizedLDPoints, PKConcentration, ",
   "BayesProblem, FitProposal, ImportanceEstimate, ConvergenceStudy, LoadTheoph"];
