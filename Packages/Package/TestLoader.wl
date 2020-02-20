(* ::Package:: *)

(* ::Title:: *)
(*Test Loader*)


(* ::Section:: *)
(*Exposed*)


LoadTests::usage="LoadTests[...] loads tests from a directory"; 


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"]


(* ::Subsection:: *)
(*LoadTests*)


Options[LoadTests]=
  {
    "TestFilePattern"->"*Tests."~~("wl"|"m"),
    "SearchDepth"->1,
    "LoadingFunction"->Get
    };
LoadTests[files:{___String?FileExistsQ}, ops:OptionsPattern[]]:=
  Module[{results, get = OptionValue["LoadingFunction"], suites},
    results = 
      Reap[
        get/@files,
        TestSuite
        ];
    suites = Flatten@results[[2]]
    ];
LoadTests[dir_String?DirectoryQ, ops:OptionsPattern[]]:=
  Module[{files, pat = OptionValue["TestFilePattern"]},
    files = 
      FileNames[
        Replace[
          OptionValue["TestFilePattern"],
          All->"*."~~("m"|"wl")
          ],
        If[DirectoryQ@FileNameJoin@{dir, "Tests"}, FileNameJoin@{dir, "Tests"}, dir], 
        OptionValue["SearchDepth"]
        ];
    LoadTests[files, ops]
    ];


(* ::Subsection:: *)
(*End*)


End[]
