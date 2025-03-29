package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"slices"

	"github.com/spf13/cobra"
)

var interval string
var lowerBound int
var upperBound int
var num_points int
var steps []string = []string{"linear", "log", "quadratic"}

var generateRangeCmd = &cobra.Command{
	Use:   "generate-range -t [testcase] -l [lower-bound] -u [upper-bound] -i [interval]",
	Short: "translate + run test cases + generate graphs",
	Long:  `allow the user to select a test case and a range to control the number of operations.....`,
	Run: func(cmd *cobra.Command, args []string) {
		if !verbose {
			log.SetOutput(io.Discard)
		}
		starting_time = startupTimes()
		testcase := rangeInputValidation(caseID)
		data := dataTemplate(testcase)
		dataMap := map[string][]Data{
			"Rocq":  {},
			"Agda":  {},
			"Idris": {},
			"Lean":  {},
		}
		exit_status := map[string]string{
			"Rocq":  "OK",
			"Agda":  "OK",
			"Idris": "OK",
			"Lean":  "OK",
		}
		i, point, linear_inc := lowerBound, 1, 1
		if num_points != 1 {
			linear_inc = int(math.Floor((float64(upperBound) - float64(lowerBound)) / float64(num_points-1)))
		}
		if linear_inc == 0 {
			linear_inc = 1
		}
		if interval == "log" {
			i = int(math.Pow(2, math.Ceil(math.Log2(float64(i)))))
		}
		if interval == "quadratic" {
			i = int(math.Pow(math.Ceil(math.Sqrt(float64(i))), 2))
		}
		data.Testcases[0].LowerBound = i
		data.Testcases[0].Interval = interval

		for i <= upperBound {
			if point > num_points {
				break
			}
			log.Printf("datapoint %d out of %d\n", point, num_points)
			translateTest(testcase, i)
			if point == 1 {
				loadAgdalib(testcase)
			}
			dataMap, exit_status = run_test(testcase, dataMap, exit_status, i)

			if interval == "linear" {
				i += linear_inc
			}
			if interval == "log" {
				i *= 2
			}
			if interval == "quadratic" {
				i = int(math.Pow(math.Sqrt(float64(i))+1, 2))
			}
			point += 1

		}

		for j := 0; j < len(data.Testcases[0].Languages); j++ {
			data.Testcases[0].Languages[j].Tests = append(data.Testcases[0].Languages[j].Tests, dataMap[data.Testcases[0].Languages[j].Name]...)
			data.Testcases[0].Languages[j].Exit_status = exit_status[data.Testcases[0].Languages[j].Name]
		}
		json_data, err := json.MarshalIndent(data, "", "  ")
		if err != nil {
			if !verbose {
				fmt.Println(StdMsg)
			}
			log.Fatalln("Error marshalling json data", err)
		}

		log.Println("Creating JSON file")
		file, err := os.Create("data.json")
		if err != nil {
			if !verbose {
				fmt.Println(StdMsg)
			}
			log.Fatalln("Error creating json file:", err)
		}
		defer file.Close()

		log.Println("Writing data to JSON file")
		_, err = file.Write(json_data)
		if err != nil {
			if !verbose {
				fmt.Println(StdMsg)
			}
			log.Fatalln("Error writing to json file:", err)

		}

		if webpage {
			log.Println("Generating webpage")
			generateGraphs()

		} else {
			return
		}

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
	if upperBound < lowerBound || upperBound > testcase.max_range {
		fmt.Printf("The upper bound must be between %d and %d\n", lowerBound, testcase.max_range)
		os.Exit(1)
	}
	if num_points < 1 || num_points > maxPoint {
		fmt.Println("The number of datapoints must be between 1 and", maxPoint)
		os.Exit(1)
	}
	if !slices.Contains(steps, interval) {
		fmt.Printf("Please provide a valid interval. Options are: %v\n", steps)
		os.Exit(1)
	}

	return testcase

}

func init() {
	rootCmd.AddCommand(generateRangeCmd)
	generateRangeCmd.PersistentFlags().IntVarP(&caseID, "testcase", "t", 1, "generates data for selected testcase")
	generateRangeCmd.PersistentFlags().IntVarP(&lowerBound, "lower", "l", 1, "The lower bound for the generated datapoints")
	generateRangeCmd.PersistentFlags().IntVarP(&upperBound, "upper", "u", 500, "the upper bound for the generated datapoints")
	generateRangeCmd.PersistentFlags().StringVarP(&interval, "interval", "i", "linear", "the interval between the generated datapoints")
	generateRangeCmd.PersistentFlags().IntVarP(&num_points, "datapoints", "d", 5, "the number of generated datapoints")
	generateRangeCmd.PersistentFlags().BoolVarP(&webpage, "webpage", "w", true, "generates webpage with graph visualizations")
	generateRangeCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "Enable detailed output for debugging and progress tracking")

}
