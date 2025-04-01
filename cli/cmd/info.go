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
	Interval    string         `json:"interval"`
	LowerBound  int            `json:"lower_bound"`
	Languages   []LanguageJSON `json:"languages"`
}

type LanguageJSON struct {
	Name        string `json:"name"`
	Tests       []Data `json:"tests"`
	Exit_status string `json:"exit_status"`
	Exit_point  int    `json:"exit_point"`
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
		"A series of N nested let statements",
		"LetExample",
		5000,
	},
	2: {
		2,
		"A series of N nested let statements that define and use sequential variables based on previous definitions",
		"LetAddExample",
		5000,
	},
	3: {
		3,
		"A series of N nested functions",
		"NestedFunction",
		5000,
	},
	4: {
		4,
		"A specified number of simple datatype declarations",
		"DataSimpleDeclarations",
		5000,
	},
	5: {
		5,
		"Variable declaration with an identifier of a specified length",
		"LongIdentifier",
		1000000,
	},
	6: {
		6,
		"A record declaration with N dependent fields",
		"Fields_DependentRecordModule",
		5000,
	},
	7: {
		7,
		"A very long chain (N) of dependent record definitions",
		"ChainDef_DependentRecordModule",
		100000,
	},
	8: {
		8,
		"A record with N parameters",
		"Parameters_DependentRecordModule",
		5000,
	},
	9: {
		9,
		"A file with N newlines",
		"NewlineFile",
		500000,
	},
	10: {
		10,
		"A record declaration with N independent fields",
		"Fields_NonDependentRecordModule",
		5000,
	},
	11: {
		11,
		"Generate a very long chain (N) of independent record definitions",
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
		"creates a datatype with a single constructor accepting N parameters",
		"Parameters_Datatypes",
		5000,
	},
	14: {
		14,
		"defines N variables, and uses both the first and last one in a declaration, N>=2",
		"FirstLast_VariableModule",
		5000,
	},
	15: {
		15,
		"Defines a series of dependent variables, with 10 variables at each level of dependency, and then utilizes the innermost variables in a subsequent expression",
		"DeepDependency_VariableModule",
		5000,
	},
	16: {
		16,
		"A simple datatype declaration with a specified number of indices, defined implicitly",
		"DataImplicitIndices",
		5000,
	},
	17: {
		17,
		"A file consisting of a single long line with N characters",
		"SingleLongLine",
		5000,
	},
	18: {
		18,
		"A single datatype where N represents the number of 'Type' parameters, all needed for N constructors",
		"ConstructorsParameters_Datatypes",
		5000,
	},
	19: {
		19,
		"A single datatype where N represents the number of indices, all needed for N constructors",
		"IndicesConstructors_Datatypes",
		5000,
	},
	20: {
		20,
		"A single datatype where N represents the number of 'Type' parameters as well as the number of indices",
		"IndicesParameters_Datatypes",
		5000,
	},
	21: {
		21,
		"A function pattern matching on N constructors of a datatype",
		"Pattern_Matching_Datatypes",
		5000,
	},
}

var Language_list = []Language{
	{
		"Rocq",
		".v",
		"coqc",
	},
	{
		"Agda",
		".agda",
		"agda +RTS -M3.0G -RTS",
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
