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

var rootCmd = &cobra.Command{
	Use:   "cli",
	Short: "translate + run test cases + generate graphs",
	Long:  `allow the user to select a test case and a range to control the number of operations.....`,
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
