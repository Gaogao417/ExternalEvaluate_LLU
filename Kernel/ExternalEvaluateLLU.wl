(* ::Package:: *)

BeginPackage["ExternalEvaluateLLU`"]
targetDirectory
sourceDirectory
debug
(*\:5220\:9664targetDirectory\sourceDirectory\:6587\:4ef6\:5939\:4e0b\:6587\:4ef6\:540d\:4e3a\:7eaf\:6570\:5b57\:7684\:4efb\:4f55\:6587\:4ef6*)
clean
(*\:6253\:5f00Demo.nb*)
demo
(*\:7528\:4e86GeneralUtilities`AssociationTranspose*)
Needs["GeneralUtilities`"]


Begin["`Private`"]

$Options = <|"IncludeDirectories"->{PacletManager`PacletResource["ExternalEvaluate_LLU", "IncludeDirectory"]}, "Head"->
"#include \"LLU.h\"
",
"CompileOptions"->" /std:c++latest /EHsc",
"ExtraObjectFiles" -> "",
"Libraries"->{"LLU", "wstp64i4", "wstp64i4m", "wstp64i4s"},
"LibraryDirectories"->{PacletManager`PacletResource["ExternalEvaluate_LLU", "LibraryResources"]}|>;

targetDirectory = "D:/ExternalEvaluate_LLU/Library";
sourceDirectory = "D:/ExternalEvaluate_LLU/Source";
debug["vs"] := ExternalEvaluate["Shell", "devenv "<>sourceDirectory]
debug["vscode"] := ExternalEvaluate["Shell", "code "<>sourceDirectory]
Map[If[!DirectoryQ@#, CreateDirectory@#]&]@{targetDirectory, sourceDirectory};


clean[] := ExternalEvaluate["Python", 
"import os, re
dir = 'D:/ExternalEvaluate_LLU/'
for f in os.listdir(dir):
    print(f)
    for dirpath, _, filenames in os.walk(os.path.join(dir, f)):
        for filename in filter(lambda x: re.match('[0-9]+\\..*', x), filenames):
            os.remove(os.path.join(dirpath, filename))"]


demo[] := SystemOpen[PacletManager`PacletResource["ExternalEvaluate_LLU", "Demo"]]


cmake := StringTemplate["cmake_minimum_required(VERSION 3.22)
set(CMAKE_CXX_FLAGS \"${CMAKE_CXX_FLAGS} /std:c++latest /MTd\")
project(DEBUG)
set(LIBRARY_OUTPUT_PATH <*targetDirectory*>/Debug)
include_directories(
	<*ToString[FileNameJoin[{$InstallationDirectory, \"SystemFiles\", \"IncludeFiles\", \"C\"}], InputForm]*>
	`IncludeDirectories`
)
link_directories(
	`LibraryDirectories`
)

file(GLOB USER_LIBS_PATH CONFIGURE_DEPENDS *.cpp)
foreach(_filename ${USER_LIBS_PATH})
	string(REGEX MATCHALL \"[0-9|a-z|A-Z]*.cpp\" tmp1 ${_filename})
	string(REGEX REPLACE \".cpp\" \"\" _libname ${tmp1})
	add_library(${_libname} SHARED ${_filename})
	set_property(TARGET ${_libname} PROPERTY MSVC_RUNTIME_LIBRARY \"MultiThreaded$<$<CONFIG:Debug>:Debug>\")
	target_link_libraries(${_libname} PRIVATE `Libraries`)
endforeach()"]@
	Map[StringRiffle/*StringReplace[{"\\"->"/", "Release"->"Debug"}]]@KeyTake[{"IncludeDirectories", "LibraryDirectories", "Libraries"}]@$Options;


cmakeFile = FileNameJoin[{sourceDirectory, "CMakeLists.txt"}];
If[!FileExistsQ@cmakeFile, WriteString[cmakeFile, cmake]; Close@cmakeFile]


test["code"] := 
"
<||>
Boolean __test__(Integer a)
{
	Return !LLU::LibraryData::uncheckedAPI();
}"


build[Dynamic[src_], Dynamic[libName_]] := build[src, libName]
build[src_, libName_] :=
Module[{lib},
	lib = FileNameJoin[{targetDirectory, libName<>".dll"}];
	If[FileExistsQ@lib, LibraryUnload[FileNameJoin[{targetDirectory, libName<>".dll"}]]];
	Echo[libName, "Compiling"];
	lib = CCompilerDriver`CreateLibrary[
 								File@src,
 								libName,
 								Language -> "C++",
 								"CompileOptions" -> $Options["CompileOptions"],
 								"TargetDirectory" -> targetDirectory,
 								"IncludeDirectories" -> $Options["IncludeDirectories"],
 								"LibraryDirectories" -> $Options["LibraryDirectories"],
 								"ExtraObjectFiles" -> $Options["ExtraObjectFiles"],
 								"Libraries" -> $Options["Libraries"]
 							];
	If[lib === $Failed,
		Throw[$Failed],
		Echo[lib, "Done"]
	]
]

(*load: \:548cLibraryFunctionLoad\:3001LibraryFunctionDeclaration\:76f8\:5173\:7684GUI*)

(*\:8c03\:7528\:ff1a
	GetLibraryFunctionDeclarations-\:5728Cpp\:6e90\:6587\:4ef6\:4e2d\:627e\:5230\:6240\:6709\:53ef\:88abLibraryFunctionDeclaration\:4f7f\:7528\:7684\:51fd\:6570\:ff08\:5305\:542bEXTERN C DLLEXPORT\:ff0c\:4e14\:8f93\:5165\:53d8\:91cf\:4e2d\:4e0d\:5305\:542bWolframLibrary\:ff09*)
load := {Src, Functions, LibName} |->
DynamicModule[{debuglib, a, lib, src = Src, functions = Functions, libName = LibName, load},
	lib = FileNameJoin[{targetDirectory, libName<>".dll"}];(*
	Quiet@LibraryUnload[lib];*)
	debuglib = StringReplace["\\Library\\"->"\\Library\\Debug\\"]@lib;
	(*If[!FileExistsQ[lib], Failure, Success]
	[
		lib, *)
		Panel@TableForm@
		{
			{Style["Export Function", Bold],
				Column@
				Map[
					OpenerView[{Button[Style[StringTrim[#Name, "\""], RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10, Underlined], ToExpression@#Load, Appearance -> Frameless], 
						Panel@TableForm@{
							{Button[Style["Load LibraryFunction(Release)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10], CopyToClipboard@(load = ToExpression@#Load); Echo[load, StringTemplate["Function `1` Loaded:"][#Name]];],
							Button[Style["Load LibraryFunction(Debug)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10], CopyToClipboard@(load = ToExpression@StringReplace["\\Library\\"->"\\Library\\\\Debug\\"]@#Load); Echo[load, StringTemplate["Function `1` Loaded:"][#Name]];]},
							{Button[Style["Copy Load Input(Release)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10], CopyToClipboard@#Load],
							Button[Style["Copy Load Input(Debug)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10], CopyToClipboard@StringReplace["\\Library\\"->"\\Library\\\\Debug\\"]@#Load]}
							
						}
					}]&
				]@functions},
			{Style["Copy LibraryFunctionDeclaration", Bold],
				(Panel@TableForm@
				{
					Button[
						Style["Release", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
						ToExpression@(#Load&)@First@Select[#Name=="\"__test__\""&]@functions; 
						Echo[ToExpression@("!$$test$$[1]"), "libData Initialization:"];	 
						GetLibraryFunctionDeclarations[File@#1, #2],
						Method->"Queued"
					],
					Button[
						Style["Debug", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10], 
						ToExpression@StringReplace["\\Library\\"->"\\Library\\Debug\\"]@(#Load&)@First@Select[#Name=="\"__test__\""&]@functions; 
						Echo[ToExpression@("$$"<>libName<>"$$[1]"), "libData Initialization:"];
						GetLibraryFunctionDeclarations[File@#1, #3],
						Method->"Queued"
					]
				}&)[src, lib, debuglib]}
		}
(*	]*)
];

ExternalEvaluate`RegisterSystem["LLU",
	<|
		"ProgramFileFunction"->Function[{x}, x],

		"ScriptExecCommandFunction"->Function[{x}, x],
		
		"SessionProlog"->None,
		"NonZMQInitializeFunction"-> False,
		"NonZMQEvaluationFunction" -> Function[
			{session$, input$},
			DynamicModule[{stream, src0 = "", input = input$["input"], libName = DateString[{"Year", "Month", "Day", "Hour", "Minute", "Second"}], src, head, functions, filename, lib, path, a, debuglib},
				Needs["CCompilerDriver`"];
				{libName, input} = If[StringContainsQ[RegularExpression["((?m)^`)|(`(?m)$)"]]@input, StringSplit[input, RegularExpression["((?m)^`)|(`(?m)$)"]], {libName, input}];
				filename = FileNameJoin[{sourceDirectory, libName<>".cpp"}];
				If[!FileExistsQ@filename, 
					Quiet@Close@filename;input = $Options["Head"]<>input
				];
				stream = If[FileExistsQ@filename, src0 = ReadString[filename]; OpenAppend@Echo[filename, "Source File:"], Echo[CreateFile[filename], "Source File:"]];
				If[!StringContainsQ["DLLEXPORT __"<>libName<>"__"]@src0, input = input<>test["code"]];
				src = If[StringContainsQ[RegularExpression["(?m)^<\\|[^<|]*\\|>"]]@input
					,
					{head, functions} = Preprocessor[input, FileNameJoin[{targetDirectory, libName<>".dll"}]];
					StringRiffle[Flatten@{head, Map[#Code&]@functions}, "\n"]
					,
					input];
				WriteString[stream, src];
				Quiet@Close[stream];
				lib = build[filename, libName];
				debuglib = StringReplace["\\Library\\"->"\\Library\\Debug\\"]@lib;
				Panel@TableForm@
				{
					{
						Style["Open source file", Bold],
						Button[
							Style[filename, RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10, Underlined],
							If[FileExistsQ@filename, ExternalEvaluate["Shell", StringTemplate["code `1`"][filename]]],
							Appearance -> Frameless,
							Method -> "Queued"
						]
					},
					{
						Style["Build", Bold],
						Button[
							Style[libName, RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10, Underlined],
							build[Dynamic@filename, Dynamic@libName],
							Appearance -> Frameless,
							Method -> "Queued"
						]
					},
					{
						Style["Load/Unload", Bold],
						Panel@TableForm@Transpose@
						{
							{
								Button[
									Style["Load Library(Release)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
									Echo[
										LibraryLoad[lib], "Load"],
									Method -> "Queued"
								],
								Button[
									Style["Unload Library(Release)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
									Echo[LibraryUnload[lib], "Unload"],
									Method -> "Queued"
								],
								Button[
									Style["Load All Functions", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
									Echo[TableForm@Map[{#Name, ToExpression@#Load}&]@functions, "LoadResult"]
								]	
							},
							{
								Button[
									Style["Load Library(Debug)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
									Echo[
										LibraryLoad[debuglib], "Loaded"],
									Method -> "Queued"
								],
								Button[
									Style["Unload Library(Debug)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
									Echo[LibraryUnload[debuglib], "Unloaded"],
									Method -> "Queued"
								],
								Button[
									Style["Load All Functions", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
									Echo[TableForm@Map[{#Name, ToExpression@StringReplace["\\Library\\"->"\\Library\\Debug\\"]@#Load}&]@functions, "LoadResult"]
								]
							}
						}
					},
					{
						Style["Load Function(Release/Debug)", Bold],
						load[filename, functions, libName]
					},
					{
						Style["File Operations", Bold],
						{
						Button[
							Style["Append to Source File", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
							CellPrint@Cell[StringJoin["`", libName, "`"], "ExternalLanguage", CellEvaluationLanguage->"LLU"]
						],
						Button[
							Style["Rewrite Source File", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
							DeleteFile[filename];
							CellPrint@Cell[StringJoin["`", libName, "`"], "ExternalLanguage", CellEvaluationLanguage->"LLU"]
						],
						Button[
							Style["Rollback Source File", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
							DeleteFile[filename];
							WriteString[filename, src0]; Close[filename]
						]
						}
					}
				}
			]	
		]
	|>
];


(* ::Section:: *)
(*Parser*)


(* ::Subsection:: *)
(*GetFunctions\:ff1a\:7528regex_\:5206\:5272\:8f93\:5165\:ff0cGet\:6240\:6709\:8981\:7f16\:8bd1\:4e3aLibraryLink\:7684Function*)


GetFunctions[regex_, argumentSplitFlag_] := {input, libDirectory} |->
Module[{metadatas, metadataPos, functions, functionPos, head},
	metadataPos = StringPosition[regex]@input;
	metadatas = Map[pos |-> ToExpression@StringTake[input, pos]] @ metadataPos;

	functionPos = ArrayReshape[Join[(Flatten@metadataPos)[[2;;]], {StringLength@input+1}], {Length@metadataPos, 2}];
	functionPos[[;;, 1]] = functionPos[[;;, 1]]+1;
	functionPos[[;;, 2]] = functionPos[[;;, 2]]-1;
	functions = Map[pos |-> StringTake[input, pos]] @ (functionPos);
	functions = Map[ParseFunction[argumentSplitFlag]]@functions;
	{
		StringTake[input, {1, metadataPos[[1, 1]]-1}],
		MapThread[
			{metadata, function} |-> Association[{First@function, "metadata"->metadata}, Rest@function, "libDirectory"->libDirectory],
			{metadatas, functions}
		]
	}
]


(* ::Subsection:: *)
(*ParseFunction\:ff1a\:83b7\:5f97\:51fd\:6570\:7684\:51fd\:6570\:540d\:3001\:8f93\:5165\:8f93\:51fa\:53d8\:91cf\:3001\:8f93\:5165\:8f93\:51fa\:7c7b\:578b\:53cametadata\:7b49\:4fe1\:606f*)


ParseFunction[argumentSplitFlag_] := function |->
Module[{declLoc, parenthesisLoc, braceLoc, decl, input, src, outType, funcName, inputVars, inType, inName, Code, outName},
	braceLoc = Flatten@StringPosition[function, Longest["{"~~___~~"}"], Overlaps->False];
	parenthesisLoc = Flatten@StringPosition[function, "("~~Shortest[___]~~")", 1];
	declLoc = {1, parenthesisLoc[[1]]-1};
    {decl, input, src} = Map[pos |-> StringTake[function, pos]]@{declLoc, {parenthesisLoc[[1]]+1, parenthesisLoc[[2]]-1}, {braceLoc[[1]]+1, braceLoc[[2]]-1}};
    {outType, funcName} = StringSplit[decl][[-2;;-1]];
    inputVars = If[!StringMatchQ[input, RegularExpression["\\s*"]], StringSplit[input, argumentSplitFlag], {"Null", "Null"}];
    {inType, inName} = Transpose@Map[
         Flatten@*StringCases[{x__~~RegularExpression["\\s+"]~~Longest[y__]:>{x, y}, "Null":>{"Null", ""}}, Overlaps->False],
        inputVars];
    src = If[StringContainsQ[src, "Return"], StringSplit[src, RegularExpression["\\s*Return\\s*"]], {src, ""}];
	{Code, outName} = If[Length@src == 1, {"", src//First}, src];
	Code = StringReplace["\n"->"\n\t"]@Code;
    outName = StringDelete[outName, ";"|"\n"];
    {"Name"->funcName, "inType"->inType, "outType"->outType, "inName"->inName, "outName"->outName, "Code"->Code}
]


(* ::Subsection:: *)
(*LoadFunction\:ff1a\:751f\:6210LibraryFunctionLoad\:4ee3\:7801\:ff0c\:4ee5\:5b9e\:73b0LibraryFunction\:7684\:81ea\:52a8\:5bfc\:5165*)


LoadFunction := functionInfo |->
Module[{func = functionInfo},
	func["loadName"] = StringReplace["_"->"$"]@func["Name"];
	If[Quiet@KeyExistsQ["Type"]@func["metadata"],
		{func["loadIn"], func["loadOut"]} = List@@func["metadata"]["Type"],
		{func["loadIn"], func["loadOut"]} = Map[cTypeToLibraryType]@{func["inType"], func["outType"]}
	];
	func["libDirectory"] = ToString[func["libDirectory"], InputForm];
	func["Name"] = ToString[func["Name"], InputForm];
	
	func["Load"] = StringTemplate["ClearAll[`loadName`]\n(*LoadFunction*)\n `loadName` = \n LibraryFunctionLoad[\n\t`libDirectory`, \n\t`Name`, \n\t`loadIn`, \n\t`loadOut`]"]@
		<|KeyTake[{"Name", "loadName", "loadIn", "loadOut", "libDirectory"}]@func|>;
	func
]


(* ::Subsection:: *)
(*GenerateCFunction\:ff1a\:6839\:636eGetFunctions\:5f97\:5230\:7684\:51fd\:6570\:7684\:6240\:6709\:4fe1\:606f\:ff0c\:751f\:6210C++\:4ee3\:7801*)


GenerateCFunction := functionInfo |->
Module[{func = functionInfo, inputDeclaration},
	func["inMember"] = cTypeToArgumentMember@functionInfo["inType"];
	func["outMember"] = cTypeToArgumentMember@functionInfo["outType"];
	func["inCoercion"] = generateInCoercion@functionInfo["inType"];
    func["outCoercion"] = generateOutCoercion@functionInfo["outType"];
    
    inputDeclaration = 
    StringJoin@MapThread[
        generateInputDecls[<|"i"-> #1-1, #2|>]&,
        {Range@Length@functionInfo["inType"], If[Length@functionInfo["inType"]>1, AssociationTranspose, ReplaceAll[{x_}:>x]/*List]@KeyTake[func, {"inType","inName","inMember", "inCoercion"}]}
    ];
    
    func["Code"] = generateFunction[<|"inputDeclaration"->StringTrim[inputDeclaration, "\n"|"\n\t"], KeyTake[func, {"Name", "outName","outMember", "outCoercion", "Code"}]|>];
    func
]


(* ::Subsection:: *)
(*Preprocessor\:ff1aExternalEvaluate\:7684\:65f6\:5019\:ff0c\:53ea\:8c03\:7528\:8fd9\:4e00\:4e2a\:51fd\:6570*)


Preprocessor := {input, libDirectory} |->
Module[{head, functions},
	{head, functions} = GetFunctions[RegularExpression["(?m)^<\\|[^<|]*\\|>"], RegularExpression["\\s*;\\s*"]][input, libDirectory];
	{
		head, 
		functions//
			Map[GenerateCFunction]//
			Map[LoadFunction]//
			Map[KeyTake[{"Name","Load","Code", "inType","outType","inName","outName","inMember","outMember","inCoercion","outCoercion"}]]
	}
]


(* ::Subsection:: *)
(*FunctionCompile\:5bfc\:5165\:76f8\:5173*)


selectFunctions[functions_] := Select[functions, !ContainsQ[#inType, "WolframLibraryData"|"Null"]&]

GetLibraryFunctionDeclarations[filename:File[_], lib_String] := 
DynamicModule[{input = ReadString[filename], head, functions, decl},
	functions = GetFunctions[RegularExpression["(?m)EXTERN_C DLLEXPORT[\\s]*"], RegularExpression["\\s*,\\s*"]][input, lib][[2]]//selectFunctions;
	CopyToClipboard@If[functions != {},
		LL`def = Map[
				LibraryFunctionDeclaration[ToExpression["LL`"<>StringReplace["_"->"$"]@#Name]->#Name, lib, cTypeToFunctionCompileTypes[#inType, #outType]]&
		]@functions,
		Echo["There are no functions to FunctionCompile", "Copy LibraryFunctionDeclaration:"];
	]
]

GetLibraryFunctionDeclarations[src_String, lib_String] := 
Module[{input = src//Echo, head, functions, decl},
	functions = GetFunctions[RegularExpression["(?m)EXTERN_C DLLEXPORT[\\s]*"], RegularExpression["\\s*,\\s*"]][input, lib][[2]]//selectFunctions;
	If[functions != {},
		CopyToClipboard@Map[
				ToString@LibraryFunctionDeclaration[ToExpression["LL`"<>StringReplace["_"->"$"]@#Name]->#Name, lib, cTypeToFunctionCompileTypes[#inType, #outType]]&
		]@functions,
		Echo["There are no functions to FunctionCompile", "Copy LibraryFunctionDeclaration:"];
	]
]

GetLibraryFunctionDeclarations[___] := $Failed


(* ::Subsection:: *)
(*\:4ee3\:7801\:751f\:6210\:7528\:7684StringTemplate*)


generateInputDecls := StringTemplate[
    "auto `inName` = mngr.get`inType`(`i`);\n\t"
];

generateFunction[x_/;x["outName"] != ""] := StringTemplate[
"EXTERN_C DLLEXPORT int `Name`(WolframLibraryData libData, mint Argc, 
MArgument *Args, MArgument Res){
	auto err = LLU::ErrorCode::NoError;
	LLU::MArgumentManager mngr {libData, Argc, Args, Res};
	`inputDeclaration``Code`
	mngr.set(`outName`);
	return err;
}"][x];

generateFunction[x_/;x["outName"] == ""] := StringTemplate[
"EXTERN_C DLLEXPORT int `Name`(WolframLibraryData libData, mint Argc, 
MArgument *Args, MArgument Res){
	auto err = LLU::ErrorCode::NoError;
	LLU::MArgumentManager mngr {libData, Argc, Args, Res};
	`inputDeclaration``Code`
	return err;
}"][x];


(* ::Subsection:: *)
(*\:7528\:4e8e\:4ee3\:7801\:751f\:6210\:53caLibraryFunctionLoad/Declaration\:7684\:5404\:79cd\:66ff\:6362\:89c4\:5219*)


cTypeToFunctionCompileTypes[inType_, outType_] := 
Module[{t, r, replaceRule, in, out},
	replaceRule = ReplaceAll[
		{
			"char*" -> "CString",
    		x__ ~~ "*" :> TypeSpecifier["CArray"][x],
    		"mint" -> "MachineInteger",
    		"mreal" | "double" -> "MachineReal",
    		"MTensor" -> TypeSpecifier["PackedArray"]["MachineReal",2],
    		"MNumericArray" -> TypeSpecifier["NumericArray"]["MachineReal", 2],
    		"MSparseArray" -> TypeSpecifier["SparseArray"]["MachineReal", 2],
    		"mcomplex" -> "ComplexReal64",
    		"bool" -> "Boolean",
    		"int" -> "CInt"
		}
	];
	in = replaceRule@inType;
	out = replaceRule@outType;
	Rule[in, out]
]

generateInCoercion := 
StringReplace[
{
	a__~~RegularExpression["\\*"]:> 
		"("~~a~~"*)", 
	Alternatives["mbool","mint","mreal","mcomplex","MTensor","MSparseArray","MNumericArray","MImage","char*"]:>"",
	x__:>"("~~x~~")"
}];
	
generateOutCoercion := 
StringReplace[
	{a__~~RegularExpression["\\*"]:> "(mint)", Alternatives["mbool","mint","mreal","mcomplex","MTensor","MSparseArray","MNumericArray","MImage","char*"]:>"", __->"(mint)"}
];

cTypeToArgumentMember := 
StringReplace[{
    x__~~RegularExpression["\\*"]/;x!="char"->"integer",
    "mbool"->"boolean",
    "mint"->"integer",
    "mreal"->"real",
    "mcomplex"->"cmplex",
    "MTensor"->"tensor",
    "MSparseArray"->"sparse",
    "MNumericArray"->"numeric",
    "MImage"->"image",
    "char*"->"utf8string",
    __->"integer"
}];

inputTypeToInput := 
StringReplace[{
    "Integer"->"RandomInteger[{0, 100}]//Echo",
    "\"Boolean\""->"True",
    "Real"->"RandomReal[]//Echo",
    "Complex"->"RandomComplex[]//Echo",
    "{_, _}"->"Table[RandomReal[], 10, 10]//Echo",
    "LibraryDataType[SparseArray]"->"SparseArray@Table[RandomReal[], 10, 10]//Echo",
    "LibraryDataType[NumericArray]"->"NumericArray@Table[RandomReal[], 10, 10]//Echo",
    "LibraryDataType[Image3D]"->"Image@Table[RandomReal[], 10, 10]//Echo",
    "UTF8String"->"ToString@RandomInteger[{0, 100}]//Echo"
    }];

cTypeToLibraryType := 
StringReplace[{(*
	x__~~RegularExpression["\\*"]/;x!="char":>"Integer",*)
    "Boolean":>"\"Boolean\"",
    StringExpression["Integer<", __, ">"]|"Integer" :>"Integer",
    "Real":>"Real",
    "Complex":>"Complex",
    StringExpression["Tensor<", ___,  "Passing::"|"LLU::Passing::", x__, ">"]:>StringExpression["{_, _, \"", x, "\"}"],
    StringExpression["Tensor<", __, ">"]|"MTensor"|"Tensor"|"GenericTensor":>"{_, _}",
    StringExpression["SparseArray<", ___,  "Passing::"|"LLU::Passing::", x__, ">"]:>StringExpression["{LibraryDataType[SparseArray], \"", x, "\"}"],
    "MSparseArray"|StringExpression["SparseArray<", __, ">"]|"GenericSparseArray":>"LibraryDataType[SparseArray]",
    StringExpression["NumericArray<", ___,  "Passing::"|"LLU::Passing::", x__, ">"]:>StringExpression["{LibraryDataType[NumericArray], \"", x, "\"}"],
    "MNumericArray"|StringExpression["NumericArray<", __, ">"]|"GenericNumericArray":>"LibraryDataType[NumericArray]",
    StringExpression["Image<", ___,  "Passing::"|"LLU::Passing::", x__, ">"]:>StringExpression["{LibraryDataType[Image|Image3D], \"", x, "\"}"],
    "MImage"|StringExpression["Image<", __, ">"]|"GenericImage":>"LibraryDataType[Image|Image3D]",
    "CString"|"String":>"UTF8String",
    StringExpression["DataList<", ___,  "Passing::"|"LLU::Passing::", x__, ">"]:>StringExpression["{\"DataStore\", \"", x, "\"}"],
    "DataStore"|StringExpression["DataList<", __, ">"]|"DataList"|"GenericDataList":>"DataStore",
    "WSLINK":>"LinkObject"
}];



(* ::Section:: *)
(*Test*)


(* ::Subsection:: *)
(*\:6d4b\:4e86\:4e2a\:5bc2\:5bde\:3002\:3002\:6d4b\:8bd5\:4ee5Demo.nb\:4e3a\:51c6*)


CCompilerDriver`CreateLibrary[
 								File@src,
 								libName,
 								Language -> "C++",
 								"CompileOptions" -> $Options["CompileOptions"],
 								"TargetDirectory" -> targetDirectory,
 								"IncludeDirectories" -> $Options["IncludeDirectories"],
 								"LibraryDirectories" -> $Options["LibraryDirectories"],
 								"ExtraObjectFiles" -> $Options["ExtraObjectFiles"],
 								"Libraries" -> $Options["Libraries"]
 							]


ExternalEvaluateLLU`Private`GetFunctions[RegularExpression["(?m)EXTERN_C DLLEXPORT[\\s]*"], RegularExpression["\\s*,\\s*"]]["#include <LLU/LLU.h>\r\n\r\nEXTERN_C DLLEXPORT mint WolframLibrary_getVersion(){\r\n  return WolframLibraryVersion;\r\n}\r\n\r\nEXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {\r\n\tLLU::LibraryData::setLibraryData(libData);\r\n    return 0;\r\n}\r\n\r\nEXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {\r\n    return;\r\n}\r\n\r\nEXTERN_C DLLEXPORT int test(WolframLibraryData libData, mint Argc, \r\nMArgument *Args, MArgument Res){\r\n\tauto err = LLU::ErrorCode::NoError;\r\n\tLLU::MArgumentManager mngr {libData, Argc, Args, Res};\r\n\tauto a = mngr.getInteger(0);\r\n\tmngr.set(a);\r\n\treturn err;\r\n}", "d"]


#Name&@<|"Name"->"WolframLibrary_getVersion","metadata"->DLLEXPORT EXTERN_C,"inType"->{"Null","Null"},"outType"->"mint","inName"->{"",""},"outName"->"","Code"->"\\n/\\n\\t  return WolframLibraryVersion;\\n\\n\\t","libDirectory"->"d"|>


End[] (* End `Private` *)

EndPackage[]
