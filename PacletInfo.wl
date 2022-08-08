(* ::Package:: *)

Paclet[
    "Name" -> "ExternalEvaluate_LLU",
    "Version" -> "0.0.2",
    "MathematicaVersion" -> "12.3+",
    "AutoUpdating" -> True,
    "Extensions" -> {
        {
            "ExternalEvaluate", 
            "System" -> {"LLU" -> {"Ordering" -> "1102"}}, 
            "Context" -> {"ExternalEvaluateLLU`"}
        },
        {   
            "Kernel",
            "Root" -> "Kernel",
            "HiddenImport" -> True,
            "Context" -> {"ExternalEvaluateLLU`"}
        },
        {
            "Resource",
            "Root" -> ".",
            "Resources" -> {
                {"Icon",     "Resources/IconNormal.wxf"},
                {"IconCell", "Resources/IconGray.wxf"},
                {"LibraryResources", "Resources/Library/Release"},
                {"IncludeDirectory", "Resources/IncludeDirectory"}
            }
        },
        {"LibraryLink"}
    }
]
