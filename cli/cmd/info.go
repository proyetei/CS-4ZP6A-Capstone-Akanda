package cmd

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

// Language represents a programming language structure
type LanguageJSON struct {
	Name  string `json:"name"`
	Tests []Data `json:"tests"`
}

// Test represents a test case for a specific language
type Data struct {
	Size        int     `json:"size"`
	Memory      int     `json:"memory"`
	Real_time   float64 `json:"real_time"`
	User_time   float64 `json:"user_time"`
	System_time float64 `json:"system_time"`
}

var Case_list = map[int]Testcase{
	1: {
		1,
		"Function with nested Let statements",
		"LetExample",
		10000,
	},
	2: {
		2,
		"Function with nested statements",
		"LetAddExample",
		20000,
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
		"agda --compile",
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
