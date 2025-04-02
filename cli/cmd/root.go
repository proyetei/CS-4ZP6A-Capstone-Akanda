package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var maxPoint int = 150
var caseID int
var maxID int = 21
var output_folder string = "out"
var webpage bool
var verbose bool
var starting_time map[int]map[string][]Data
var active_languages int = 4
var max_memory int = 3

var rootCmd = &cobra.Command{
	Use: "mhpgeez [sub_command] [flags]",
	Short: "CLI tool for generating and analyzing test cases of varying sizes across Lean, Idris, Agda, and Rocq. " +
		"Note: If you are running the tool with Docker, ignore the 'mhpgeez' prefix when using the help section. For example, you can get the help section for generate-list by appending 'generate-list --help' to the end of the docker run command",
	CompletionOptions: cobra.CompletionOptions{
		HiddenDefaultCmd: true, // hides cmd
	},
	Run: func(cmd *cobra.Command, args []string) {
		os.Exit(1)

	},
}

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func init() {

}

//-D maxRecDepth=50
