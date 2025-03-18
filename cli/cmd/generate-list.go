package cmd

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"regexp"

	"github.com/spf13/cobra"
)

var generateListCmd = &cobra.Command{
	Use:   "generate-list -t [testcase] -d [datapoints]",
	Short: "translate + run test cases + generate graphs",
	Long:  `allow the user to select a test case and a range to control the number of operations.....`,
	Run: func(cmd *cobra.Command, args []string) {
		testcase := listInputValidation(caseID)
		data := dataTemplate(testcase)
		dataMap := map[string][]Data{
			"Coq":   {},
			"Agda":  {},
			"Idris": {},
			"Lean":  {},
		}
		for i := 0; i < len(datapoints); i++ {
			translateTest(testcase, datapoints[i])
			dataMap = run_test(testcase, dataMap, datapoints[i])
		}
		for j := 0; j < len(data.Testcases[0].Languages); j++ {
			data.Testcases[0].Languages[j].Tests = append(data.Testcases[0].Languages[j].Tests, dataMap[data.Testcases[0].Languages[j].Name]...)
		}
		json_data, err := json.MarshalIndent(data, "", "  ")
		if err != nil {
			panic(err)
		}

		file, err := os.Create("data.json")
		if err != nil {
			fmt.Println("Error creating json file:", err)
			return
		}
		defer file.Close()

		_, err = file.Write(json_data)
		if err != nil {
			fmt.Println("Error writing to json file:", err)
			return
		}

		if webpage {
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

func dataTemplate(test Testcase) Overview {
	data := Overview{
		Testcases: []Test{
			{
				Name:        test.file_name,
				Description: test.desc,
				Languages: []LanguageJSON{
					{
						Name:  "Coq",
						Tests: []Data{},
					},
					{
						Name:  "Agda",
						Tests: []Data{},
					},
					{
						Name:  "Idris",
						Tests: []Data{},
					},
					{
						Name:  "Lean",
						Tests: []Data{},
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
	originalDir, err := os.Getwd()
	if err != nil {
		fmt.Println("Error getting current directory:", err)
		os.Exit(1)
	}
	err = os.Chdir(output_folder)
	if err != nil {
		fmt.Println("Error changing directory:", err)
		os.Exit(1)
	}

	agda_str := Language_list[1].cmd + " ./" + test.file_name + Language_list[1].file_extension
	agda_cmd := exec.Command("bash", "-c", agda_str)
	_ = agda_cmd.Run()
	pattern := `(?s)"real_time": ([0-9]*\.[0-9]+), "user_time": ([0-9]*\.[0-9]+), "system_time": ([0-9]*\.[0-9]+), "memory": ([0-9]+)}`
	re := regexp.MustCompile(pattern)

	for i := 0; i < len(Language_list); i++ {
		time_str := `/usr/bin/time --format='"real_time": %e, "user_time": %U, "system_time": %S, "memory": %M}' `
		cmd_str := time_str + Language_list[i].cmd + " ./" + test.file_name + Language_list[i].file_extension
		cmd := exec.Command("bash", "-c", cmd_str)
		var outb, errb bytes.Buffer
		cmd.Stdout = &outb
		cmd.Stderr = &errb
		err = cmd.Run()
		var test_data Data
		fmt.Println(errb.String())
		if err != nil {
			fmt.Println(Language_list[i].name, "file has an error")
			fmt.Println(errb.String(), outb.String())
		} else {
			matches := re.FindStringSubmatch(errb.String())
			if matches == nil {
				fmt.Println("could not record the time")
				os.Exit(1)
			}
			jsonstr := fmt.Sprintf(`{"size": %d, `, operations) + matches[0]
			err = json.Unmarshal([]byte(jsonstr), &test_data)
			if err != nil {
				fmt.Println(Language_list[i].name, errb.String())
				fmt.Println("Could not unmarshal test data", err)
			} else {
				dataMap[Language_list[i].name] = append(dataMap[Language_list[i].name], test_data)
			}

		}

	}
	err = os.Chdir(originalDir)
	if err != nil {
		fmt.Println("Error changing directory:", err)
		os.Exit(1)
	}

	return dataMap

}

func generateGraphs() {
	setup_str := "rm -rf ./static/graphs/ && mkdir -p ./static/graphs"
	setup_cmd := exec.Command("bash", "-c", setup_str)
	err := setup_cmd.Run()
	if err != nil {
		fmt.Println("Error executing the setup commands for the graph folder", err)
		os.Exit(1)
	}

	app_str := "python3.12 app.py"
	app_cmd := exec.Command("bash", "-c", app_str)
	go func() {
		err := app_cmd.Start()
		if err != nil {
			fmt.Println("Error starting Python script:", err)
			return
		}

		// Wait for the command to finish
		err = app_cmd.Wait()
		if err != nil {
			fmt.Println("Error waiting for Python script:", err)
		}
	}()
	err = exec.Command("xdg-open", "http://127.0.0.1:5001").Start()
	if err != nil {
		fmt.Println("Error opening browser:", err)
	}
	select {}
}

func init() {
	rootCmd.AddCommand(generateListCmd)
	generateListCmd.PersistentFlags().IntVarP(&caseID, "testcase", "t", 1, "generates data for selected testcase")
	generateListCmd.PersistentFlags().IntSliceVarP(&datapoints, "datapoints", "d", []int{}, "List of datapoints (comma separated no spaces) eg 1,2,3")
	generateListCmd.PersistentFlags().BoolVarP(&webpage, "webpage", "w", true, "generates webpage with graph visualizations")
}
