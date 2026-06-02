#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  tests/sanity.wls — fast invariants for the package.  Exits nonzero on
    any failure.  Run: wolframscript -file tests/sanity.wls *)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "community", "qmc_bayes_helpers.wl"}];

fails = 0;
check[name_, cond_] := (
   Print[If[TrueQ[cond], "  ok  ", " FAIL "], name];
   If[! TrueQ[cond], fails++]);

(* 1. Sobol' / Halton points live strictly inside the open cube. *)
sp = LDPoints["Sobol", 4096, 6];
hp = LDPoints["Halton", 4096, 6];
check["Sobol points in (0,1)", Min[sp] > 0 && Max[sp] < 1];
check["Halton points in (0,1)", Min[hp] > 0 && Max[hp] < 1];

(* 2. Equidistribution: each coordinate mean ~ 1/2. *)
check["Sobol coordinate means ~ 0.5", Max[Abs[Mean[sp] - 0.5]] < 0.01];

(* 3. Sobol' integrates exp(sum x) over [0,1]^6 far better than its own
   discrepancy floor; here just check it beats 1e-3 at N=2^14. *)
exact = (E - 1.)^6;
check["Sobol 6-D integral accurate",
   Abs[Mean[Exp[Total[#]] & /@ LDPoints["Sobol", 2^14, 6]] - exact]/exact < 2.*^-3];

(* 4. PK concentration is ~0 at t=0 (only roundoff) and positive after. *)
c0 = PKConcentration[0., {1.5, 2.8, 30., 2., 20.}, 320.];
cT = PKConcentration[{1., 4., 12.}, {1.5, 2.8, 30., 2., 20.}, 320.];
check["PK conc ~0 at t=0", Abs[c0] < 1.*^-6];
check["PK conc positive for t>0", AllTrue[cT, Positive]];

(* 5. Laplace mean exact agrees with a large-N Sobol' estimate. *)
prob = BayesProblem[320., {0.5, 1, 2, 4, 8, 12, 24},
   {3.5, 7.0, 9.5, 8.0, 6.0, 4.5, 2.0}, {1.5, 3., 35., 2., 25., 1.},
   {0.6, 0.5, 0.5, 0.7, 0.7, 0.5}];
lap = LaplacePosterior[prob];
estCL = Mean[PosteriorDraws[LDPoints["Sobol", 2^15, 6], lap][[All, 2]]];
check["Laplace mean = QMC estimate (CL)",
   Abs[estCL - LaplaceMeanExact[lap][[2]]]/LaplaceMeanExact[lap][[2]] < 0.01];

(* 6. QMC convergence is faster than MC on E[CL]. *)
Nl = {256, 1024, 4096, 16384};
slp[meth_] := Coefficient[Fit[Transpose[{Log[Nl],
   Log[Table[Sqrt@Mean@Table[(Mean[PosteriorDraws[
      RandomizedLDPoints[meth, n, 6, 7 r + n], lap][[All, 2]]]
      - LaplaceMeanExact[lap][[2]])^2, {r, 40}], {n, Nl}]]}], {1, x}, x], x];
sS = slp["Sobol"]; sM = slp["MonteCarlo"];
Print["   slopes: Sobol=", NumberForm[sS, {3, 2}], "  MC=", NumberForm[sM, {3, 2}]];
check["Sobol slope steeper than MC by >= 0.3", sS < sM - 0.3];

Print[If[fails == 0, "ALL TESTS PASSED", ToString[fails] <> " TEST(S) FAILED"]];
If[fails > 0, Exit[1]];
