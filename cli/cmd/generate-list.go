package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"slices"

	"github.com/spf13/cobra"
)

var datapoints []int

var generateListCmd = &cobra.Command{
	Use:   "generate-list [flags]",
	Short: "Generates + translates + type checks a testcase at specific sizes, returns URL to webpage with results",
	Long:  "Generates and type checks a selected test case at specific sizes in Agda, Idris, Lean, and Rocq, and provides a URL where users can access the webpage with the time and memory results",
	Run: func(cmd *cobra.Command, args []string) {
		if !verbose {
			log.SetOutput(io.Discard)
		}
		set_max_memory()
		starting_time = startupTimes()
		testcase := listInputValidation(caseID)
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
		exit_point := map[string]int{
			"Rocq":  -1,
			"Agda":  -1,
			"Idris": -1,
			"Lean":  -1,
		}
		datapoints = remove_duplicate_int(datapoints)
		slices.Sort(datapoints)
		data.Testcases[0].LowerBound = datapoints[0]
		for i := 0; i < len(datapoints); i++ {
			if active_languages == 0 {
				break
			}
			log.Printf("datapoint %d out of %d\n", i+1, len(datapoints))
			translateTest(testcase, datapoints[i])
			if i == 0 {
				loadAgdalib(testcase)
			}
			dataMap, exit_status, exit_point = run_test(testcase, dataMap, exit_status, exit_point, datapoints[i])
		}
		for j := 0; j < len(data.Testcases[0].Languages); j++ {
			data.Testcases[0].Languages[j].Tests = append(data.Testcases[0].Languages[j].Tests, dataMap[data.Testcases[0].Languages[j].Name]...)
			data.Testcases[0].Languages[j].Exit_status = exit_status[data.Testcases[0].Languages[j].Name]
			data.Testcases[0].Languages[j].Exit_point = exit_point[data.Testcases[0].Languages[j].Name]
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

func listInputValidation(input int) Testcase {
	testcase, found := Case_list[input]
	if !found {
		fmt.Println("available testcases have IDs between 1 and", maxID)
		os.Exit(1)
	}
	if len(datapoints) > maxPoint {
		fmt.Printf("Maximum number of datapoints [%d] exceeded\n", maxPoint)
		os.Exit(1)
	}
	if len(datapoints) != 0 {
		for i := 0; i < len(datapoints); i++ {
			if datapoints[i] < 0 || datapoints[i] > testcase.max_range {
				fmt.Println("One of the datapoints you provided was not in the acceptable range 0 -", testcase.max_range)
				os.Exit(1)
			}
		}
		return testcase
	} else {
		fmt.Println("datapoints list empty")
		os.Exit(1)
	}
	if max_memory < 1 || max_memory > 15 {
		fmt.Printf("The memory limit for the type checking commands must be between 1GB and 15GB \n")
		os.Exit(1)

	}

	return testcase

}

func init() {
	rootCmd.AddCommand(generateListCmd)
	generateListCmd.PersistentFlags().IntVarP(&caseID, "testcase", "t", 1, "Specifies which test case the data is generated for")
	generateListCmd.PersistentFlags().IntSliceVarP(&datapoints, "datapoints", "d", []int{}, "List of up to 150 sizes (≥ 1) in comma-separated format (eg 1,2,3)")
	generateListCmd.PersistentFlags().BoolVarP(&webpage, "webpage", "w", true, "Generates webpage with graph visualizations")
	generateListCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "Enable detailed output for debugging and progress tracking")
	generateListCmd.PersistentFlags().IntVarP(&max_memory, "max-memory", "m", 3, "Memory limit for the type checking commands in GB (between 1 to 15)")
}
