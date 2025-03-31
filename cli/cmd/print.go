package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var printTestsCmd = &cobra.Command{
	Use:   "print",
	Short: "print available test cases",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("##### The testcases ####")
		for i := 1; i <= maxID; i++ {
			fmt.Println(i, "- ", Case_list[i].desc)
		}
		os.Exit(1)
	},
}

func init() {
	rootCmd.AddCommand(printTestsCmd)

}
