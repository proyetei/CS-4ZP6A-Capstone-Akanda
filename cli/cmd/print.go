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
		for _, element := range Case_list {
			fmt.Println(element.id, "- ", element.desc)
		}
		os.Exit(1)
		},
	 
   }


func init() {
	rootCmd.AddCommand(printTestsCmd)

}