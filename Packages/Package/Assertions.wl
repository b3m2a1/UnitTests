(* ::Package:: *)

(* ::Title:: *)
(*Test Assertions*)


(* ::Section:: *)
(*Exposed*)


AssertTest::usage="AssertTest[obj, test, ops] general assertion that test[obj] is True";
AssertApplyTest::usage="AssertApplyTest[obj, test, ops] general assertion that test@@obj is True";
AssertAllTest::usage="AssertAllTest[obj, test, ops] general assertion that AllTrue[test, obj] is True";
AssertAnyTest::usage="AssertAnyTest[obj, test, ops] general assertion that AnyTrue[test, obj] is True";


$TestSuiteName::usage="The name of the suite from which the test is being run";
$TestCaseName::usage="The name of the case which is being run";
$TestAssertionMessages::usage="The messages to be used by default for tests";


(* ::Section:: *)
(*Private*)


(* ::Subsection::Closed:: *)
(*Begin*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
(*GetTestResult*)


(* ::Subsubsection::Closed:: *)
(*Success*)


GetTestResult["Success", tag_, suite_, case_, msg_, params_]:=
  Success[
    tag,
    <|
      "TestSuite"->suite,
      "TestCase"->case,
      "MessageTemplate"->msg,
      "MessageParameters"->params
      |>
    ]


(* ::Subsubsection::Closed:: *)
(*Failure*)


GetTestResult["Failure", tag_, suite_, case_, msg_, params_]:=
  Failure[
    tag,
    <|
      "TestSuite"->suite,
      "TestCase"->case,
      "MessageTemplate"->msg,
      "MessageParameters"->params
      |>
    ]


(* ::Subsubsection::Closed:: *)
(*Inconclusive*)


GetTestResult["Inconclusive", tag_, suite_, case_, msg_, params_]:=
  Failure[
    tag,
    <|
      "TestSuite"->suite,
      "TestCase"->case,
      "MessageTemplate"->msg,
      "MessageParameters"->params
      |>
    ]


(* ::Subsection::Closed:: *)
(*GetTestMessage*)


(* ::Subsubsection::Closed:: *)
(*$TestAssertionMessages*)


$TestAssertionMessages=
  <|
    "Success"->"Ok.",
    "Failure"->"Failed.",
    "Inconclusive"->"Inconclusive."
    |>


(* ::Subsubsection::Closed:: *)
(*GetTestMessage*)


GetTestMessage[k_String, Automatic]:=
  Lookup[$TestAssertionMessages, k];
GetTestMessage[k_String, msg_]:=
  msg


(* ::Subsection:: *)
(*AssertTest*)


Clear[AssertTest]
Options[AssertTest]=
  {
    "SuiteName"->Automatic,
    "CaseName"->Automatic,
    "Tag"->Automatic,
    "SuccessMessage"->Automatic,
    "FailureMessage"->Automatic,
    "InconclusiveMessage"->Automatic,
    "MessageParameters"->Automatic,
    "AbortOnFailure"->True,
    "AbortOnInconclusive"->False
    };
AssertTest~SetAttributes~HoldFirst;
AssertTest[input_, function:Except[OptionsPattern[]], ops:OptionsPattern[]]:=
  Module[
    {
      suite, case,
      tag,
      rcode, msg, result, params
      },
    suite = Replace[OptionValue[AssertTest, {ops}, "SuiteName"], Automatic:>$TestSuiteName];
    case = Replace[OptionValue[AssertTest, {ops}, "CaseName"], Automatic:>$TestCaseName];
    tag = Replace[OptionValue[AssertTest, {ops}, "Tag"], Automatic:>ToString[function]];
    {rcode, msg} = 
      If[Catch[function[input]]&&Length[$MessageList]===0,
        {
          "Success", 
          GetTestMessage["Success", OptionValue["SuccessMessage"]]
          },
        {
          "Failure", 
          GetTestMessage["Failure", OptionValue["FailureMessage"]]
          },
        {
          "Inconclusive", 
          GetTestMessage["Inconclusive", OptionValue["InconclusiveMessage"]]
          }
        ];
    params = OptionValue["MessageParameters"];
    If[!AssociationQ@Association[params],
      params = Join[params, {tag, suite, case}],
      params = Join[Association[params], <|"tag"->tag, "suite"->suite, "case"->case|>]
      ];
    result = GetTestResult[rcode, tag, suite, case, msg, params];
    Sow[result, case];
    If[rcode==="Failure"&&OptionValue["AbortOnFailure"],
      Throw[result, case]
      ];
    If[rcode==="Inconclusive"&&OptionValue["AbortOnInconclusive"],
      Throw[result, case]
      ];
    ]


AssertTest[function:Except[OptionsPattern[]]][input_, ops:OptionsPattern[]]:=
  AssertTest[input, function, ops]


(* ::Subsection:: *)
(*AssertApplyTest*)


Clear[AssertApplyTest]
Options[AssertApplyTest]=Options[AssertTest];
AssertApplyTest~SetAttributes~HoldFirst;
AssertApplyTest[input_, function:Except[OptionsPattern[]], ops:OptionsPattern[]]:=
  AssertTest[
    input,
    Apply[function],
    FilterRules[
      {
        "Tag"->Replace[OptionValue[AssertApplyTest, {ops}, "Tag"], Automatic:>ToString[function]],
        ops
        },
      Options[AssertTest]
      ]
    ];
AssertApplyTest[function:Except[OptionsPattern[]]][input__, ops:OptionsPattern[]]:=
  AssertApplyTest[{input}, function, ops]


(* ::Subsection:: *)
(*AssertAllTest*)


Clear[AssertAllTest]
Options[AssertAllTest]=Options[AssertTest];
AssertAllTest~SetAttributes~HoldFirst;
AssertAllTest[input_, function:Except[OptionsPattern[]], ops:OptionsPattern[]]:=
  AssertTest[
    input,
    AllTrue[function],
    FilterRules[
      {
        "Tag"->Replace[OptionValue[AssertAllTest, {ops}, "Tag"], Automatic:>ToString[function]],
        ops
        },
      Options[AssertTest]
      ]
    ];
AssertAllTest[function:Except[OptionsPattern[]]][input_, ops:OptionsPattern[]]:=
  AssertAllTest[input, function, ops]


(* ::Subsection:: *)
(*AssertAnyTest*)


Clear[AssertAnyTest]
Options[AssertAnyTest]=Options[AssertTest];
AssertAnyTest~SetAttributes~HoldFirst;
AssertAnyTest[input_, function:Except[OptionsPattern[]], ops:OptionsPattern[]]:=
  AssertTest[
    input,
    AnyTrue[function],
    FilterRules[
      {
        "Tag"->Replace[OptionValue[AssertAnyTest, {ops}, "Tag"], Automatic:>ToString[function]],
        ops
        },
      Options[AssertTest]
      ]
    ];
AssertAnyTest[function:Except[OptionsPattern[]]][input_, ops:OptionsPattern[]]:=
  AssertAnyTest[input, function, ops]


(* ::Subsection::Closed:: *)
(*End*)


End[]
