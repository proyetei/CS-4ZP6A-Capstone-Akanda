package cmd

import "bytes"

type Overview struct {
	Testcases []Test `json:"testcases"`
}

type Testcase struct {
	id        int
	desc      string
	file_name string
	max_range int
}

type Language struct {
	name           string
	file_extension string
	cmd            string
}

type Test struct {
	Name        string         `json:"name"`
	Description string         `json:"description"`
	Languages   []LanguageJSON `json:"languages"`
}

type LanguageJSON struct {
	Name        string `json:"name"`
	Tests       []Data `json:"tests"`
	Exit_status string `json:"exit_status"`
}

type Data struct {
	Size        float64 `json:"size"`
	Memory      float64 `json:"memory"`
	Real_time   float64 `json:"real_time"`
	User_time   float64 `json:"user_time"`
	System_time float64 `json:"system_time"`
}

type cmdResult struct {
	outb bytes.Buffer
	errb bytes.Buffer
	err  error
}

var Case_list = map[int]Testcase{
	1: {
		1,
		"Generate Nested Let Statements",
		"LetExample",
		5000,
	},
	2: {
		2,
		"<TODO>",
		"LetAddExample",
		5000,
	},
	3: {
		3,
		"Generate Nested Functions",
		"NestedFunction",
		5000,
	},
	4: {
		4,
		"<TODO>",
		"DataSimpleDeclarations",
		5000,
	},
	5: {
		5,
		"<TODO>",
		"LongIdentifier",
		5000,
	},
	6: {
		6,
		"A record declaration with N dependent fields",
		"Fields_DependentRecordModule",
		5000,
	},
	7: {
		7,
		"Generate a very long chain (N) of dependent record definitions ",
		"ChainDef_DependentRecordModule",
		5000,
	},
	8: {
		8,
		"Generate record with N parameters ",
		"Parameters_DependentRecordModule",
		5000,
	},
	9: {
		9,
		"<TODO>",
		"NewlineFile",
		100000,
	},
	10: {
		10,
		"<TODO>",
		"Fields_NonDependentRecordModule",
		5000,
	},
	11: {
		11,
		"<TODO>",
		"ChainDefFields_NonDependentRecordModule",
		5000,
	},
	12: {
		12,
		"<TODO>",
		"Constructors_Datatypes",
		5000,
	},
	13: {
		13,
		"<TODO>",
		"Parameters_Datatypes",
		5000,
	},
	14: {
		14,
		"Defines lots of dependent variables (10 at each level of dependency) and then use the most nested ones in a declaration",
		"FirstLast_VariableModule",
		5000,
	},
	15: {
		15,
		"Defines lots of dependent variables (10 at each level of dependency) and then use the most nested ones in a declaration",
		"DeepDependency_VariableModule",
		5000,
	},
}

var Language_list = []Language{
	{
		"Coq",
		".v",
		"coqc",
	},
	{
		"Agda",
		".agda",
		"agda",
	},
	{
		"Idris",
		".idr",
		"idris2 --check",
	},
	{
		"Lean",
		".lean",
		"lean",
	},
}

var StdMsg = "command could not complete successfully, check logs for more details"
