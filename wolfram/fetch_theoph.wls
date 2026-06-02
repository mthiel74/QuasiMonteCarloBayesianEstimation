#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  fetch_theoph.wls — pull the classic Theophylline pharmacokinetic
    data set and tidy it into data/theoph.csv.

    Source: the `Theoph` data frame distributed with R's `datasets`
    package (originally Boeckmann, Sheiner & Beal, NONMEM user guide).
    12 subjects given a single oral dose of theophylline; serum
    concentration measured at 11 time points over ~25 h.

    Mirror: Vincent Arel-Bundock's Rdatasets (public, stable URLs).
    We use URLRead + status-code check (URLDownload silently writes
    server error pages on 4xx/5xx).

    Columns written:  subject, weight_kg, dose_mg_per_kg, time_h, conc_mg_per_L
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
outCsv   = FileNameJoin[{repoRoot, "data", "theoph.csv"}];

url = "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/Theoph.csv";

Print["Fetching ", url];
resp = URLRead[HTTPRequest[url]];
If[resp["StatusCode"] =!= 200,
   Print["FATAL: HTTP ", resp["StatusCode"]]; Exit[1]];

raw = ImportString[resp["Body"], "CSV"];
header = First[raw];
Print["columns: ", header];

(* Rdatasets layout: {rownames, Subject, Wt, Dose, Time, conc}.
   Subject is a factor 1..12 (ordered by a weird level order in R; we
   coerce to the integer label). *)
rows = Rest[raw];

tidy = ({
    ToExpression[ToString[#[[2]]]],   (* Subject *)
    #[[3]],                            (* Wt (kg) *)
    #[[4]],                            (* Dose (mg/kg) *)
    #[[5]],                            (* Time (h) *)
    #[[6]]                             (* conc (mg/L) *)
   } & /@ rows);

(* Sort by subject then time so each subject is a contiguous block. *)
tidy = SortBy[tidy, {#[[1]], #[[4]]} &];

out = Prepend[tidy,
   {"subject", "weight_kg", "dose_mg_per_kg", "time_h", "conc_mg_per_L"}];

Export[outCsv, out, "CSV"];
Print["wrote ", outCsv, "  (", Length[tidy], " rows, ",
   Length @ DeleteDuplicates[tidy[[All, 1]]], " subjects)"];
