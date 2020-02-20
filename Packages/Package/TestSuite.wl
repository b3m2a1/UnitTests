(* ::Package:: *)

(* ::Title:: *)
(*Test Suites / Cases*)


(* ::Text:: *)
(*Defines the test suite object and construction interface*)


(* ::Section:: *)
(*Exposed*)


TestSuite::usage="TestSuite[name, tests, ops] defines a suite of tests that can be run";
TestCase::usage="TestCase[name, test, input, ops] defines a single test case that runs inside a suite";


BeginTestSuite::usage="BeginTestSuite[name, ctx, pkgs, ops] defines a TestSuite that uses pkgs in ctx";
SetTestSuiteOptions::usage="SetTestSuiteOptions[ops] sets options on the current TestSuite";
EndTestSuite::usage="EndTestSuite[] finalizes and constructs a TestSuite";


RunTestCase::usage="RunTestCase[case, ops] runs a test case in a TestSuite";
RunTestSuite::usage="RunTestSuite[suite, ops] runs the tests in a TestSuite";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"]


(* ::Subsection:: *)
(*TestSuite*)


(* ::Subsubsection::Closed:: *)
(*TestSuite*)


(* ::Text:: *)
(*We'll make this more sophisticated later when we get to formatting stuff and generating a test report*)


Options[TestSuite]=
  {
    "Needs"->{},
    "Context"->None,
    "Teardown"->None,
    "Setup"->None
    };
TestSuite[tag_, tests_List, ops:OptionsPattern[]]:=
  TestSuite[
    <|
      "Tag"->tag,
      "Tests"->tests,
      "Context"->OptionValue["Context"],
      "Needs"->OptionValue["Needs"],
      "Setup"->OptionValue["Setup"],
      "Teardown"->OptionValue["Teardown"]
      |>
    ];
t:TestSuite[a_Association]?NotTestSuiteQ:=
  System`Private`SetValid[Unevaluated[t]];
TestSuite[a_Association][k_]:=a[k];


(* ::Subsubsection::Closed:: *)
(*TestSuiteQ*)


TestSuiteQ[t:TestSuite[_Association]]:=
  System`Private`ValidQ[Unevaluated[t]];
TestSuiteQ[t_]:=False;
TestSuiteQ~SetAttributes~HoldFirst;


(* ::Subsubsection::Closed:: *)
(*NotTestSuiteQ*)


NotTestSuiteQ[t_]:=
  Not@TestSuiteQ[t];
NotTestSuiteQ~SetAttributes~HoldFirst;


(* ::Subsubsection::Closed:: *)
(*Format*)


Format[TestSuite[a_Association]?TestSuiteQ, StandardForm]:=
  RawBoxes@BoxForm`ArrangeSummaryBox[
    TestSuite,
    TestSuite,
    None,
    {
      BoxForm`MakeSummaryItem[{"Name:", a["Tag"]}, StandardForm]
      },
    {
      
      },
    StandardForm
    ]


(* ::Subsubsection::Closed:: *)
(*$TestSuiteStack / $TestSuiteData*)


If[!ValueQ[$TestSuiteStack], $TestSuiteStack={}];
If[!ValueQ[$TestSuiteData], $TestSuiteData=None]


(* ::Subsubsection:: *)
(*BeginTestSuite*)


BeginTestSuite[tag_, 
  ctx:_String|None|Automatic:Automatic, 
  needs:{___String}:{}, 
  ops:OptionsPattern[]
  ]:= 
  With[
    {
      oldCX=$Context, 
      ct = Replace[ctx, 
              Automatic:>StringDelete[tag, Except[WordCharacter|"`"]]<>"`"
              ]
      },
    If[ctx=!=None,
      System`Private`NewContextPath[
        Join[{"System`", ct}, $PackageObject@"Contexts"[]]
        ];
      $Context = ct,
      System`Private`NewContextPath[
        Join[{"System`"}, $PackageObject@"Contexts"[]]
        ]
      ];
    Needs/@Flatten@{needs};
    If[!ListQ@$TestSuiteStack, $TestSuiteStack={}];
    $TestSuiteStack = {$TestSuiteData, <|"Context"->oldCX|>, $TestSuiteStack};
    $TestSuiteData =
      <|
        "Tag"->tag,
        "Needs"->needs,
        "Context"->ct,
        "Tests"->{},
        ops
        |>
    ]


(* ::Subsubsection:: *)
(*SetTestSuiteOptions*)


SetTestSuiteOptions[ops_]:=
  If[$TestSuiteData=!=None,
    AssociateTo[$TestSuiteData, ops]
    ]


(* ::Subsubsection:: *)
(*EndTestSuite*)


EndTestSuite[]:=
  ( 
    If[$TestSuiteData=!=None,
      System`Private`RestoreContextPath[];
      Sow[TestSuite[$TestSuiteData], TestSuite];
      $TestSuiteData = $TestSuiteStack[[1]];
      $Context = $TestSuiteStack[[2, "Context"]];
      $TestSuiteStack = $TestSuiteStack[[3]];
      ];
    )


(* ::Subsubsection:: *)
(*RunTestSuite*)


RunTestSuite//Clear
Options[RunTestSuite]=
  {
    "TestTypes"->All
    };
RunTestSuite[
  s:TestSuite[a_]?TestSuiteQ, 
  testPatterns:{__String}|_?StringPattern`StringPatternQ|All:All, 
  ops:OptionsPattern[]
  ]:=
  Module[
    {
      tests, tag, types,
      context, needs, setup, teardown,
      result, timing
      },
    tests = a["Tests"];
    tag = a["Tag"];
    types = OptionValue["TestTypes"];
    Which[
      ListQ@types,
        tests = Select[tests, MemberQ[types, #["Type"]&]],
      types=!=All,
        tests = Select[tests, StringMatchQ[#["Type"], types]&]
      ];
    Which[
      ListQ@testPatterns,
        tests = Select[tests, MemberQ[testPatterns, #["Tag"]&]],
      testPatterns=!=All,
        tests = Select[tests, StringMatchQ[#["Tag"], testPatterns]&]
      ];
    context = a["Context"];
    needs = a["Needs"];
    setup = a["Setup"];
    teardown = a["Teardown"];
    $TestSuiteName; (* invoke the autoloader if needed *)
    result = 
      Block[{$TestSuiteName=tag, ctx = $Context},
        Internal`WithLocalSettings[
          If[context=!=None, 
            System`Private`NewContextPath[{"System`", context}];
            $Context = context;
            ];
          If[needs=!=None, Needs/@needs];
          setup[s],
          result = 
            AbsoluteTiming[
              AssociationThread[#["Tag"]&/@tests, Map[RunTestCase, tests]]
              ],
          teardown[s];
          If[context=!=None, 
            $Context = ctx;
            System`Private`RestoreContextPath[];
            ];
          ]
        ];
    {timing, result} = result;
    TestSuiteResult@<|
      "Tag"->tag,
      "Results"->result,
      "Timing"->Quantity[timing, "Seconds"]
      |>
    ]


(* ::Subsection:: *)
(*TestCase*)


(* ::Subsubsection:: *)
(*TestCase*)


Options[TestCase]=
  {
    "Tag"->None,
    "Test"->None,
    "Inputs"->None,
    "Type"->None
    };
TestCase[tag_, test_, inputs:_List|None, ops:OptionsPattern[]]:=
  Module[
    {case},
    case = 
      TestCase[<|"Tag"->tag, "Test"->test, "Inputs"->inputs, ops|>];
    If[ListQ@$TestSuiteData["Tests"],
      $TestSuiteData["Tests"]=
        Append[$TestSuiteData["Tests"], case]
      ];
    case
    ];
TestCase[tag_, test_, inputs:Except[OptionsPattern[]|None|_List]..., ops:OptionsPattern[]]:=
  TestCase[tag, test, {inputs}, ops];
t:TestCase[a_Association]?NotTestCaseQ:=
  System`Private`SetValid[Unevaluated[t]];
TestCase[a_Association][k_]:=a[k];


(* ::Subsubsection::Closed:: *)
(*TestCaseQ*)


TestCaseQ[t:TestCase[_Association]]:=
  System`Private`ValidQ[Unevaluated[t]];
TestCaseQ[t_]:=False;
TestCaseQ~SetAttributes~HoldFirst;


(* ::Subsubsection::Closed:: *)
(*NotTestCaseQ*)


NotTestCaseQ[t_]:=
  Not@TestCaseQ[t];
NotTestCaseQ~SetAttributes~HoldFirst;


(* ::Subsubsection::Closed:: *)
(*Format*)


Format[TestCase[a_Association]?TestCaseQ, StandardForm]:=
  RawBoxes@BoxForm`ArrangeSummaryBox[
    TestCase,
    TestCase,
    None,
    {
      BoxForm`MakeSummaryItem[{"Name:", a["Tag"]}, StandardForm]
      },
    {
      
      },
    StandardForm
    ]


(* ::Subsubsection:: *)
(*RunTestCase*)


getMessageData[Hold[Message[msg:MessageName[symbol_, name_], args___], _]]:=
   <|
     "Tag"->name, 
     "MessageTemplate"->
       Replace[msg, HoldPattern[MessageName[s_, n_]]:>MessageName[General, n]],
     "MessageParameters"->{args}
     |>


Clear[RunTestCase]
Options[RunTestCase]=
  {
    "FailedIfMessages"->True
    };
RunTestCase[TestCase[a_]?TestCaseQ, ops:OptionsPattern[]]:=
  Module[
    {
      test, input, tag, 
      result, 
      assertions, timing, mem,
      messages = {}, status
      },
    test = a["Test"];
    input = a["Inputs"];
    tag = a["Tag"];
    $TestCaseName; (* invoke the autoloader if needed *)
    {mem, assertions} = 
      Quiet@Reap[
        Block[{$TestCaseName=tag},
          Internal`HandlerBlock[
            {"Message", Function[Null, messages = {getMessageData[#], messages}, HoldFirst]},
            MaxMemoryUsed[
              result = 
                AbsoluteTiming[
                  Catch[If[input===None, test[], test@@input], tag]
                  ]
              ]
            ]
          ],
        tag
        ];
    {timing, result} = result;
    assertions = Flatten@assertions;
    status = AllTrue[assertions, Head[#]===Success&];
    messages = Flatten[messages];
    If[Length@messages>0&&OptionValue["FailedIfMessages"],
      status = False
      ];
    TestCaseResult@<|
      "Tag"->tag,
      "Tests"->assertions,
      "Status"->If[status, "Success", "Failure"],
      "Messages"->messages,
      "Result"->result,
      "Timing"->Quantity[timing, "Seconds"],
      "Memory"->Quantity[mem, "Bytes"]
      |>
    ]


(* ::Subsection:: *)
(*End*)


End[]
