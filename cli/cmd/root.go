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
	Use:   "mhpgeez [sub_command] [flags]",
	Short: "CLI tool for generating and analyzing test cases of varying sizes across Lean, Idris, Agda, and Rocq. *If running with docker, ignore mhppgeez in help section run as eg 'generate-list --help'",
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
	//rootCmd.PersistentFlags().StringVar(&testcase, "testcase", "LetExample", "Select a test case to translate")

}
