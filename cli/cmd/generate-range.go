package cmd

import (
	"fmt"
	"os"
	"github.com/spf13/cobra"
	"math"
	"encoding/json"
)

var interval int
var lowerBound int
var upperBound int

var generateRangeCmd = &cobra.Command{
	Use:   "generate-range -t [testcase] -l [lower-bound] -u [upper-bound] -i [interval]",
	Short: "translate + run test cases + generate graphs",
	Long: `allow the user to select a test case and a range to control the number of operations.....`,
	Run: func(cmd *cobra.Command, args []string) {
	 testcase := rangeInputValidation(caseID)
	 data := dataTemplate(testcase)
	 dataMap := map[string][]Data{
		"Coq": {}, 
		"Agda":{}, 
		"Idris":{}, 
		"Lean":{},
	}
	 for i := lowerBound; i < upperBound; i += interval {
		translateTest(testcase, i)
		dataMap = run_test(testcase, dataMap, i)
	 }
	 for j := 0; j < len(data[0].Languages); j++ {
		data[0].Languages[j].Tests = append(data[0].Languages[j].Tests, dataMap[data[0].Languages[j].Name]...)
	 }
	 json_data, err := json.MarshalIndent(data, "", "  ")
	 if err != nil {
		 panic(err)
	 }
	 fmt.Println(string(json_data))

	 

	},
   }

func rangeInputValidation(input int) Testcase {
	testcase, found := Case_list[input]
	if !found {
		fmt.Println("available testcases have IDs between 1 and", maxID)
        os.Exit(1)
	} 
	if lowerBound < 1 {
		fmt.Println("The lower bound cannot be less than 1")
		os.Exit(1)
	}
	if upperBound > testcase.max_range {
		fmt.Println("The upper bound cannot be greater than", testcase.max_range)
		os.Exit(1)
	}
	if upperBound < lowerBound {
		fmt.Println("The upper bound cannot be less than the lower bound")
		os.Exit(1)
	}
	min_interval := math.Ceil(float64(upperBound - lowerBound) / 500) 
	if interval < int(min_interval) {
		fmt.Println("The interval can be no less than ", min_interval)
	}


	return testcase

}

func init() {
	rootCmd.AddCommand(generateRangeCmd)
	generateRangeCmd.PersistentFlags().IntVarP(&caseID, "testcase", "t", 1, "generates data for selected testcase")
	generateRangeCmd.PersistentFlags().IntVarP(&lowerBound, "lower", "l", 1, "The lower bound for the generated datapoints")
	generateRangeCmd.PersistentFlags().IntVarP(&upperBound, "upper", "u", 500, "the upper bound for the generated datapoints")
	generateRangeCmd.PersistentFlags().IntVarP(&interval, "interval", "i", 10, "the interval between the generated datapoints")
	
}