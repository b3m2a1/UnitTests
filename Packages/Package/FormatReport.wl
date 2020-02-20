(* ::Package:: *)

(* ::Title:: *)
(*Results Formatting*)


(* ::Text:: *)
(*Formats a report of the test results*)


(* ::Section:: *)
(*Exposed*)


FormatTestResults::usage="FormatTestResults[res, ...] formats test results";


TestResults::usage="TestResults[...] is a wrapper for the results of RunTests";
TestCaseResult::usage="TestCaseResult[...] is a wrapper for the results of a RunTestCase";
TestSuiteResult::usage="TestSuiteResult[...] is a wrapper for the results of a RunTestSuite";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"]


(* ::Subsection:: *)
(*FormatTestResults*)


(* ::Subsubsection:: *)
(*getIndent*)


getIndent[n_, char:_String:"  "]:=
  StringRepeat[char, n];


(* ::Subsubsection:: *)
(*TestResults*)


(* ::Subsubsubsection::Closed:: *)
(*Text*)


$testResultsTemplate="Test Results
`div`
Timing: `timing`

`suites`
";
FormatTestResults[TestResults[a_], "Text"]:=
  TemplateApply[
    $testResultsTemplate,
    <|
      "timing"->a["Timing"],
      "suites"->
        StringRiffle[
          Map[FormatTestResults[#, "Text"]&, Values@a["Results"]],
          "\n"
          ],
      "div"->StringRepeat["-", 50]
      |>
    ]


(* ::Subsubsubsection:: *)
(*Dataset*)


FormatTestResults[TestResults[a_], "Dataset"]:=
    Dataset[
      Map[FormatTestResults[#, "Dataset"]&, a["Results"]]
      ]


(* ::Subsubsection:: *)
(*TestSuite*)


(* ::Subsubsubsection::Closed:: *)
(*Text*)


$testSuiteResultsTemplate="`indent``suite`:
`div`
`indent1`Overall Timing: `timing`
`cases`
`div`
";
FormatTestResults[TestSuiteResult[a_], "Text"]:=
  TemplateApply[
    $testSuiteResultsTemplate,
    <|
      "suite"->a["Tag"],
      "div"->getIndent[1]<>StringRepeat["-", 35],
      "indent"->getIndent[1],
      "indent1"->getIndent[2],
      "timing"->a["Timing"],
      "cases"->
        StringRiffle[Map[FormatTestResults[#, "Text"]&, Values@a["Results"]], "\n"]
      |>
    ]


(* ::Subsubsubsection:: *)
(*Dataset*)


FormatTestResults[TestSuiteResult[a_], "Dataset"]:=
  Join[
    <|"Cases"->Map[FormatTestResults[#, "Dataset"]&, a["Results"]]|>,
    a[[{"Timing"}]]
    ]


(* ::Subsubsection:: *)
(*TestCase*)


(* ::Subsubsubsection::Closed:: *)
(*Text*)


$testCaseResultsTemplate="`indent``case`: `status`
`indent1`Timing: `timing`
`indent1`Memory: `mem`
`tests`
`msgs`";
FormatTestResults[TestCaseResult[a_], "Text"]:=
  StringReplace["\n"..->"\n"]@TemplateApply[
    $testCaseResultsTemplate,
    <|
      "case"->a["Tag"],
      "status"->a["Status"],
      "indent"->getIndent[2],
      "indent1"->getIndent[3],
      "timing"->a["Timing"],
      "mem"->a["Memory"],
      "tests"->StringRiffle[Map[FormatTestResults[#, "Text"]&, a["Tests"]], "\n"],
      "msgs"->StringRiffle[
        Map[
          getIndent[3]<>TemplateApply[#["MessageTemplate"], #["MessageParameters"]]&,
          a["Messages"]
          ],
          "\n"
        ]
      |>
    ]


(* ::Subsubsubsection:: *)
(*Dataset*)


FormatTestResults[TestCaseResult[a_], "Dataset"]:=
  Join[
    a[[{"Status", "Timing", "Memory"}]],
    <|"Tests"->Map[FormatTestResults[#, "Dataset"]&, a["Tests"]]|>,
    Map[TemplateApply[#["MessageTemplate"], #["MessageParameters"]]&]/@a[[{"Messages"}]],
    a[[{"Result"}]]
    ]


(* ::Subsubsection:: *)
(*Success/Failure*)


(* ::Subsubsubsection::Closed:: *)
(*Text*)


$testCaseSingleTestTemplate="`indent``tag`: `message`";
FormatTestResults[(Success|Failure)[tag_, data_], "Text"]:=
  TemplateApply[
    $testCaseSingleTestTemplate,
    <|
      "indent"->getIndent[3],
      "tag"->tag,
      "message"->TemplateApply[data["MessageTemplate"], data["MessageParameters"]]
      |>
    ]


(* ::Subsubsubsection:: *)
(*Dataset*)


FormatTestResults[(h:Success|Failure)[tag_, data_], "Dataset"]:=
   <|
    "Tag"->tag,
    "Status"->h,
    "Message"->TemplateApply[data["MessageTemplate"], data["MessageParameters"]]
    |>


(* ::Subsection:: *)
(*End*)


End[]
