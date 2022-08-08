(* ::Package:: *)

BeginPackage["ExternalEvaluateLLU`"]
targetDirectory
sourceDirectory
debug
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
						GetLibraryFunctionDeclarations[File@#1, #2]
					],
					Button[
						Style["Debug", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10], 
						GetLibraryFunctionDeclarations[File@#1, #3]
					]
				}&)[src, lib, debuglib]}
		}
(*	]*)
];

ExternalEvaluate`RegisterSystem["LLU",
	<|
		(*standard paths are possible install locations *)
		"StringTemplateSupportQ" -> True,


        "TargetDiscoveryFunction" -> 
            Function @ FileNames[
				With[
					{fileExtension = Switch[$OperatingSystem, "Windows", ".exe", _, ""]},
					Alternatives[
						StartOfString~~"python"~~fileExtension~~EndOfString,
						StartOfString~~"python"~~(DigitCharacter..|(DigitCharacter~~("."~~DigitCharacter)...))~~fileExtension~~EndOfString
					]
				], 
				Switch[$OperatingSystem,
					"Windows",
					With[{maindrive = First[FileNameSplit[Environment["PROGRAMFILES"]]]},
						{
							FileNameJoin[{maindrive, "Python"}],
							FileNameJoin[{Environment["LOCALAPPDATA"], "Programs", "Python", "Python"}],
							FileNameJoin[{Environment["PROGRAMFILES"], "Python"}],
							FileNameJoin[{$UserBaseDirectory, "ApplicationData", "SystemInstall", "Python"}],

							(*versioned folder names*)
							FileNameJoin[{maindrive, "Python*"}],
							FileNameJoin[{Environment["LOCALAPPDATA"], "Programs", "Python", "Python*"}],
							FileNameJoin[{Environment["PROGRAMFILES"], "Python*"}],

							(*32-bit executables have -32 at the end*)
							FileNameJoin[{Environment["PROGRAMFILES(X86)"], "Python-32"}],
							FileNameJoin[{Environment["LOCALAPPDATA"], "Programs", "Python", "Python*-32"}],
							FileNameJoin[{Environment["PROGRAMFILES(X86)"], "Python*-32"}],
							FileNameJoin[{$HomeDirectory, "anaconda3"}]
						}
					],
					"MacOSX", Join[
						{
							"/usr/bin",
							"/usr/local/bin/",
							"/usr/local/sbin",
							"/usr/local/Cellar/bin",
							ExpandFileName["~/opt/anaconda3/bin"]
						},
						(*this next path(s) is from the pkg installer on mac*)
						FileNames["/Library/Frameworks/Python.framework/Versions/*/bin"]
					],
					"Unix",{"/usr/bin", "/usr/local/bin/", "/usr/local/sbin"}
				]
            ],

		(*the program file is what is actually run as a repl - this is a function that takes as it's argument the version string*)
		(*specified by the user*)
		"ProgramFileFunction"->Function[{version},PacletManager`PacletResource["ExternalEvaluate_Python", "REPL"]],

		(*we use the -u flag to stop python from buffering output so that Mathematica can read from the process immediately*)
		(*in addition we provide the location of where to find the wolframclient package that can be used to export python types to wl*)
		"ScriptExecCommandFunction"->makePythonCommandFunction[],



		(*we can avoid any customization of the --version output screen by just evaluating the python code that tells us the version - this code works in all python versions*)
		(*for example anaconda will customize the --version so it's not easily parsable, but this sidesteps all of that*)
		"VersionExecCommandFunction"-> Function[{exec},{exec, "-c", "import sys; v=sys.version_info; print('%s.%s.%s' % (v.major, v.minor, v.micro))"}],
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
				src = If[StringContainsQ[RegularExpression["(?m)^<\\|[^<|]*\\|>"]]@input
					,
					{head, functions} = Preprocessor[input, FileNameJoin[{targetDirectory, libName<>".dll"}]];
					StringReplace["DLLEXPORT"->"EXTERN_C DLLEXPORT"]@StringRiffle[Flatten@{head, Map[#Code&]@functions}, "\n"]
					,
					input];
					
				stream = If[FileExistsQ@filename, src0 = ReadString[filename]; OpenAppend@Echo[filename, "Source File:"], Echo[CreateFile[filename], "Source File:"]];
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
							If[FileExistsQ@filename, ExternalEvaluate["Shell", StringTemplate["code `1`\n code `2`"][sourceDirectory, filename]]],
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
									Echo[LibraryLoad[lib], "Load"],
									Method -> "Queued"
								],
								Button[
									Style["Unload Library(Release)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
									Echo[LibraryUnload[lib], "Unload"],
									Method -> "Queued"
								]
							},
							{
								Button[
									Style["Load Library(Debug)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
									Echo[LibraryLoad[debuglib], "Loaded"],
									Method -> "Queued"
								],
								Button[
									Style["Unload Library(Debug)", RGBColor[0.25, 0.48, 1], "FontFamily" -> "Courier", FontSize -> 10],
									Echo[LibraryUnload[debuglib], "Unloaded"],
									Method -> "Queued"
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

(*\:6211\:6ca1\:641e\:61c2ExternalEvaluate\:91ccRegister\:76f8\:5173\:7684\:673a\:5236\:ff0c\:6211\:53ea\:77e5\:9053R.exe\:53ef\:7528*)
(*Quiet@RegisterExternalEvaluator["LLU", "D:\\Program Files\\R\\R-4.1.2\\bin\\R.exe"];*)


(* ::Section:: *)
(*Parser*)


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


selectFunctions[functions_] := Select[functions, !ContainsQ[#inType, "WolframLibraryData"|"Null"]&]

GetLibraryFunctionDeclarations[filename:File[_], lib_String] := 
DynamicModule[{input = ReadString[filename], head, functions, decl},
	functions = GetFunctions[RegularExpression["(?m)EXTERN_C DLLEXPORT[\\s]*"], RegularExpression["\\s*,\\s*"]][input, lib][[2]]//selectFunctions;
	CopyToClipboard@If[functions != {},
		Map[
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
    		"int" -> "CInt"
		}
	];
	in = replaceRule@inType;
	out = replaceRule@outType;
	Rule[in, out]
]

generateInputDecls := StringTemplate[
    "auto `inName` = mngr.get`inType`(`i`);\n\t"
];

generateFunction[x_/;x["outName"] != ""] := StringTemplate[
"DLLEXPORT int `Name`(WolframLibraryData libData, mint Argc, 
MArgument *Args, MArgument Res){
	auto err = LLU::ErrorCode::NoError;
	LLU::MArgumentManager mngr {libData, Argc, Args, Res};
	`inputDeclaration``Code`
	mngr.set(`outName`);
	return err;
}"][x];

generateFunction[x_/;x["outName"] == ""] := StringTemplate[
"DLLEXPORT int `Name`(WolframLibraryData libData, mint Argc, 
MArgument *Args, MArgument Res){
	auto err = LLU::ErrorCode::NoError;
	LLU::MArgumentManager mngr {libData, Argc, Args, Res};
	`inputDeclaration``Code`
	return err;
}"][x];

generateInType := 
StringReplace[
{
	a__~~RegularExpression["\\*"]:> 
		a~~"*",
	x__:>x
}];

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


ExternalEvaluateLLU`Private`GetFunctions[RegularExpression["(?m)EXTERN_C DLLEXPORT[\\s]*"], RegularExpression["\\s*,\\s*"]]["#include <LLU/LLU.h>\r\n\r\nEXTERN_C DLLEXPORT mint WolframLibrary_getVersion(){\r\n  return WolframLibraryVersion;\r\n}\r\n\r\nEXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {\r\n\tLLU::LibraryData::setLibraryData(libData);\r\n    return 0;\r\n}\r\n\r\nEXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {\r\n    return;\r\n}\r\n\r\nEXTERN_C DLLEXPORT int test(WolframLibraryData libData, mint Argc, \r\nMArgument *Args, MArgument Res){\r\n\tauto err = LLU::ErrorCode::NoError;\r\n\tLLU::MArgumentManager mngr {libData, Argc, Args, Res};\r\n\tauto a = mngr.getInteger(0);\r\n\tmngr.set(a);\r\n\treturn err;\r\n}", "d"]


End[] (* End `Private` *)

EndPackage[]
