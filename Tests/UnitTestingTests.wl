(* ::Package:: *)

(* ::Title:: *)
(*Testing Framework Tests*)


(* ::Text:: *)
(*Tests how wells the testing framework works (basically by its mere existence)*)


(* ::Subsection:: *)
(*TestingFrameworkTests*)


(* ::Subsubsection:: *)
(*BeginTestSuite*)


BeginTestSuite["TestingFrameworkTests"]


(* ::Subsubsection::Closed:: *)
(*Setup*)


SetTestSuiteOptions[
  "Setup"->(PackageFramework`LoadPackage["UnitTest`", "Reload"->True]&)
  ]


(* ::Subsubsection::Closed:: *)
(*AssertTrue*)


TestCase["AssertTrue", AssertTest[TrueQ], True]


(* ::Subsubsection::Closed:: *)
(*AssertSame*)


TestCase["AssertSame", AssertApplyTest[SameQ], {2, 2}]


(* ::Subsubsection::Closed:: *)
(*AssertAllGreater*)


TestCase["AssertAllGreater", AssertAllTest[GreaterThan[2]], {{3, 3}}]


(* ::Subsubsection::Closed:: *)
(*CatchMessages*)


TestCase["CatchMessages", (Plot[]&), None]


(* ::Subsubsection:: *)
(*End*)


EndTestSuite[]


(* ::Section:: *)
(*Run Tests*)


(* ::Input:: *)
(*RunTests["UnitTest", All]*)
