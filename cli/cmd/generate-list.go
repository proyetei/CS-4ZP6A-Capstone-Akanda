package cmd

import (
	"fmt"
	"os"
	"os/exec"
	"io"
	"bytes"
	"encoding/json"
	"github.com/spf13/cobra"
)

var generateListCmd = &cobra.Command{
	Use:   "generate-list -t [testcase] -d [datapoints]",
	Short: "translate + run test cases + generate graphs",
	Long: `allow the user to select a test case and a range to control the number of operations.....`,
	Run: func(cmd *cobra.Command, args []string) {
	 testcase := listInputValidation(caseID)
	 data := dataTemplate(testcase)
	 dataMap := map[string][]Data{
		"Coq": {}, 
		"Agda":{}, 
		"Idris":{}, 
		"Lean":{},
	}
	 for i := 0; i < len(datapoints); i++ {
		translateTest(testcase, datapoints[i])
		dataMap = run_test(testcase, dataMap, datapoints[i])
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

func listInputValidation(input int) Testcase {
	testcase, found := Case_list[input]
	if !found {
		fmt.Println("available testcases have IDs between 1 and", maxID)
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

	return testcase

}

func dataTemplate(test Testcase) []Test {
	data := []Test{
		{
			Name:        test.file_name,
			Description: test.desc,
			Languages: []LanguageJSON{
				{
					Name: "Coq",
					Tests: []Data{
					},
				},
				{
					Name: "Agda",
					Tests: []Data{
					},
				},
				{
					Name: "Idris",
					Tests: []Data{
					},
				},
				{
					Name: "Lean",
					Tests: []Data{
					},
				},
			},
		},
	}
	return data

}

func translateTest(test Testcase, operations int) {
	cmd_string := fmt.Sprintf("rm -rf %s && mkdir %s", output_folder, output_folder)
	setup_cmd := exec.Command("bash", "-c", cmd_string)

	err := setup_cmd.Run()
	if err != nil {
		fmt.Println("Error executing the output folder setup commands", err)
		os.Exit(1)
	}


	translate_cmd := exec.Command("bash", "-c", "./main")
	stdin, err := translate_cmd.StdinPipe()
	if err != nil {
		os.Exit(1)
	}
	stdin_string := fmt.Sprintf("%d\n%d\n", test.id, operations)
	go func() {
		defer stdin.Close()
		io.WriteString(stdin, stdin_string)
	}()

	_, err = translate_cmd.CombinedOutput()
	if err != nil {
		fmt.Println("Could not translate the testcases", err)
		os.Exit(1)
	}


}

func run_test(test Testcase, dataMap map[string][]Data, operations int) map[string][]Data {
	for i := 0; i < len(Language_list); i++ {
		time_str := `/usr/bin/time --format='"real_time": %e, "user_time": %U, "system_time": %S, "memory": %M}' `
		cmd_str := time_str + Language_list[i].cmd + " ./" + output_folder + "/" + test.file_name + Language_list[i].file_extension 
		cmd := exec.Command("bash", "-c", cmd_str)
		var outb, errb bytes.Buffer
		cmd.Stdout = &outb
		cmd.Stderr = &errb
		err := cmd.Run()
		var test_data Data
		if err != nil {
			fmt.Println(Language_list[i].name, "file has an error")
		} else {
			jsonstr := fmt.Sprintf(`{"size": %d, `, operations) + errb.String()
			err = json.Unmarshal([]byte(jsonstr), &test_data)
			if err != nil {
				fmt.Println("Could not unmarshal test data", err)
			} else {
				dataMap[Language_list[i].name] = append(dataMap[Language_list[i].name], test_data)
			}

		}
	}
	return dataMap
	
}



func init() {
	rootCmd.AddCommand(generateListCmd)
	generateListCmd.PersistentFlags().IntVarP(&caseID, "testcase", "t", 1, "generates data for selected testcase")
	generateListCmd.PersistentFlags().IntSliceVarP(&datapoints, "datapoints", "d", []int{}, "List of datapoints (comma separated no spaces) eg 1,2,3")

}