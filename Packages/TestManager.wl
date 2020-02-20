(* ::Package:: *)

(* ::Title:: *)
(*Testing Framework*)


(* ::Text:: *)
(*A simple Mathematica testing framework that mimics the python testing framework*)


(* ::Section:: *)
(*Exposed*)


TestManager::usage="TestManager[...] manages the set of tests";
RunTests::usage="RunTests[runner, ...] runs tests";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"]


(* ::Subsection:: *)
(*TestManager*)


(* ::Subsubsection:: *)
(*TestManager*)


TestManager//Clear
Options[TestManager]=
  {
    "TestsDirectory"->Automatic,
    "TestLoader"->Automatic
    };
TestManager[ops:OptionsPattern[]]:=
  Module[
    {
      root,
      loader,
      suites,
      data
      },
    root = 
      Replace[
        OptionValue["TestsDirectory"],
        Automatic:>DirectoryName[$InputFileName]
        ];
    loader = Replace[OptionValue["TestLoader"], Automatic:>LoadTests];
    suites = 
      Internal`WithLocalSettings[
        System`Private`NewContextPath[
          {"System`"}~Join~$PackageObject@"Contexts"[]
          ];,
        loader[root],
        System`Private`RestoreContextPath[]
        ];
    data = 
      <|
        "Root"->root,
        "Suites"->suites
        |>;
    With[{data=data},
      System`Private`SetValid[Unevaluated[TestManager[data]]]
      ]
    ];


TestManager[dir_String?DirectoryQ, ops:OptionsPattern[]]:=
  TestManager["TestsDirectory"->dir, ops];


TestManager[pac:_PacletManager`Paclet|PacletObject, ops:OptionsPattern[]]:=
  TestManager["TestsDirectory"->pac["Location"], ops];


TestManager[ctx:_String?(Not@*DirectoryQ), ops:OptionsPattern[]]:=
  With[{pf=PacletManager`PacletFind[ctx]},
    TestManager[pf[[1]], ops]/;Length[pf]>0
    ];


(* ::Subsubsection::Closed:: *)
(*Accessor*)


(TestManager[a_]?TestManagerQ)[k_]:=a[k];


(* ::Subsubsection::Closed:: *)
(*TestManagerQ*)


TestManagerQ[t:TestManager[_Association]]:=
  System`Private`ValidQ[t];
TestManagerQ[s_Symbol]:=
  If[MatchQ[OwnValues[s], {_:>_TestManager}],
    TestManagerQ[Evaluate[s]],
    False
    ];
TestManagerQ[e_]:=False;
TestManagerQ~SetAttributes~HoldFirst


(* ::Subsubsection:: *)
(*Format*)


Format[TestManager[a_Association]?TestManagerQ, StandardForm]:=
  RawBoxes@BoxForm`ArrangeSummaryBox[
    TestManager,
    TestManager,
    None,
    {
      BoxForm`MakeSummaryItem[{"Suites:", #["Tag"]&/@a["Suites"]}, StandardForm]
      },
    {
      
      },
    StandardForm
    ]


(* ::Subsection:: *)
(*RunTests*)


(* ::Subsubsection:: *)
(*RunTests*)


Options[RunTests]=
  {
    Format->"Text"
    }
RunTests[
  TestManager[a_]?TestManagerQ, 
  test:{(_String->_String)..}|All,
  ops:OptionsPattern[{RunTests, RunTestSuite}]
  ]:=
  Module[
    {
      suites,
      suitePatterns,
      testPatterns,
      timing, results,
      result
      },
    suites = a["Suites"];
    If[test=!=All,
      suitePatterns = Append[Thread[test, List], _->None];
      testPatterns = Replace[#["Tag"]&/@suites, suitePatterns, 1];
      suites = Pick[suites, #=!=None&/@testPatterns];
      testPatterns = Select[testPatterns, #=!=None&],
      testPatterns = None
      ];
    {timing, results} = AbsoluteTiming[
      AssociationThread[
        #["Tag"]&/@suites,
       If[testPatterns=!=None,
         MapThread[
           RunTestSuite[#, #2, ops]&, 
           {
             suites,
             testPatterns
             }
           ],
         RunTestSuite[#, All, FilterRules[{ops}, Options[RunTestSuite]]]&/@suites
         ]
       ]
      ];
    result = TestResults@<|
      "Results"->results,
      "Timing"->Quantity[timing, "Seconds"]
      |>;
    Replace[
      FormatTestResults[result, OptionValue[Format]],
      _FormatTestResults->result
      ]
    ]


RunTests[
  e_, 
  test:{(_String->_String)..}|All,
  ops:OptionsPattern[{RunTests, RunTestSuite}]
  ]:=
  With[{m=TestManager[e]},
    RunTests[m, test, ops]/;TestManagerQ[m]
    ]


(* ::Subsection:: *)
(*End*)


End[]
