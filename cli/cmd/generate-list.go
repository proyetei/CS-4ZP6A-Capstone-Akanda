package cmd

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"os/exec"
	"regexp"
	"slices"
	"syscall"
	"time"

	"github.com/savioxavier/termlink"
	"github.com/spf13/cobra"
)

var datapoints []int

var generateListCmd = &cobra.Command{
	Use:   "generate-list -t [testcase] -d [datapoints]",
	Short: "translate + run test cases + generate graphs",
	Long:  `allow the user to select a test case and a range to control the number of operations.....`,
	Run: func(cmd *cobra.Command, args []string) {
		if !verbose {
			log.SetOutput(io.Discard)
		}
		testcase := listInputValidation(caseID)
		data := dataTemplate(testcase)
		dataMap := map[string][]Data{
			"Coq":   {},
			"Agda":  {},
			"Idris": {},
			"Lean":  {},
		}
		exit_status := map[string]string{
			"Coq":   "OK",
			"Agda":  "OK",
			"Idris": "OK",
			"Lean":  "OK",
		}

		slices.Sort(datapoints)
		for i := 0; i < len(datapoints); i++ {
			log.Printf("datapoint %d out of %d\n", i+1, len(datapoints))
			translateTest(testcase, datapoints[i])
			if i == 0 {
				loadAgdalib(testcase)
			}
			dataMap, exit_status = run_test(testcase, dataMap, exit_status, datapoints[i])
		}
		for j := 0; j < len(data.Testcases[0].Languages); j++ {
			data.Testcases[0].Languages[j].Tests = append(data.Testcases[0].Languages[j].Tests, dataMap[data.Testcases[0].Languages[j].Name]...)
			data.Testcases[0].Languages[j].Exit_status = exit_status[data.Testcases[0].Languages[j].Name]
		}
		json_data, err := json.MarshalIndent(data, "", "  ")
		if err != nil {
			fmt.Println(StdMsg)
			log.Fatalln("Error marshalling json data", err)
		}

		log.Println("Creating JSON file")
		file, err := os.Create("data.json")
		if err != nil {
			fmt.Println(StdMsg)
			log.Fatalln("Error creating json file:", err)
		}
		defer file.Close()

		log.Println("Writing data to JSON file")
		_, err = file.Write(json_data)
		if err != nil {
			fmt.Println(StdMsg)
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
						Name:        "Coq",
						Tests:       []Data{},
						Exit_status: "OK",
					},
					{
						Name:        "Agda",
						Tests:       []Data{},
						Exit_status: "OK",
					},
					{
						Name:        "Idris",
						Tests:       []Data{},
						Exit_status: "OK",
					},
					{
						Name:        "Lean",
						Tests:       []Data{},
						Exit_status: "OK",
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
		fmt.Println(StdMsg)
		log.Fatalln("Error executing the output folder setup commands", err)

	}

	translate_cmd := exec.Command("bash", "-c", "./main")
	translate_cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}

	cmdDone := make(chan error, 1)
	go func() {
		stdin, err := translate_cmd.StdinPipe()
		if err != nil {
			fmt.Println(StdMsg)
			log.Fatalln("Could not create Stdin pipe", err)
		}
		stdin_string := fmt.Sprintf("%d\n%d\n", test.id, operations)
		io.WriteString(stdin, stdin_string)
		stdin.Close()
		log.Printf("translating test case %d into the 4 proof assistant languages", test.id)
		_, err = translate_cmd.CombinedOutput()
		cmdDone <- err
		close(cmdDone)
	}()

	select {
	case <-time.After(20 * time.Second):
		syscall.Kill(-translate_cmd.Process.Pid, syscall.SIGKILL)
		fmt.Println(StdMsg)
		log.Fatalln("Process killed, context deadline exceeded")
	case result := <-cmdDone:
		if result != nil {
			fmt.Println(StdMsg)
			log.Fatalln("Could not translate the testcase", result)
		}
	}

}

func run_test(test Testcase, dataMap map[string][]Data, exit_status map[string]string, operations int) (map[string][]Data, map[string]string) {
	originalDir, err := os.Getwd()
	if err != nil {
		fmt.Println(StdMsg)
		log.Fatalln("Error getting current directory:", err)
	}
	err = os.Chdir(output_folder)
	if err != nil {
		fmt.Println(StdMsg)
		log.Fatalln("Error changing directory:", err)
	}

	pattern := `(?s)"real_time": ([0-9]*\.[0-9]+), "user_time": ([0-9]*\.[0-9]+), "system_time": ([0-9]*\.[0-9]+), "memory": ([0-9]+)}`
	re := regexp.MustCompile(pattern)

	for i := 0; i < len(Language_list); i++ {
		var test_data Data
		var final_result cmdResult
		if exit_status[Language_list[i].name] != "OK" {
			continue
		}

		log.Printf("Type-checking %s file: testcase %d, size %d\n", Language_list[i].name, test.id, operations)
		time_str := `/usr/bin/time --format='"real_time": %e, "user_time": %U, "system_time": %S, "memory": %M}' `
		cmd_str := time_str + Language_list[i].cmd + " ./" + test.file_name + Language_list[i].file_extension
		cmd := exec.Command("bash", "-c", cmd_str)
		cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}

		cmdDone := make(chan cmdResult, 1)
		go func() {
			// Could cause memory issues if output size is large
			var outb, errb bytes.Buffer
			cmd.Stdout = &outb
			cmd.Stderr = &errb
			err = cmd.Run()
			cmdDone <- cmdResult{outb, errb, err}
			close(cmdDone)
		}()

		select {
		case <-time.After(30 * time.Second):
			syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
			log.Println("Process killed, context deadline exceeded")
			exit_status[Language_list[i].name] = "time"
			final_result = cmdResult{bytes.Buffer{}, bytes.Buffer{}, nil}
		case result := <-cmdDone:
			final_result = result
		}

		if final_result.err != nil {
			log.Printf("Type-checking stderr message:\n%s\nType-checking stdout message:\n%s\n", final_result.errb.String(), final_result.outb.String())
			exit_status[Language_list[i].name] = "memory"

		}
		if exit_status[Language_list[i].name] == "OK" {
			matches := re.FindStringSubmatch(final_result.errb.String())
			if matches == nil {
				log.Println("Could not record the space and time data")
			}
			jsonstr := fmt.Sprintf(`{"size": %d, `, operations) + matches[0]
			err = json.Unmarshal([]byte(jsonstr), &test_data)
			if err != nil {
				log.Println("Could not unmarshal test data", err)
			} else {
				if interval == "log" {
					test_data.Memory = safe_log(test_data.Memory)
					test_data.Real_time = safe_log(test_data.Real_time)
					test_data.System_time = safe_log(test_data.System_time)
					test_data.User_time = safe_log(test_data.User_time)
					test_data.Size = safe_log(test_data.Size)

				}
				dataMap[Language_list[i].name] = append(dataMap[Language_list[i].name], test_data)
			}
		}

	}
	err = os.Chdir(originalDir)
	if err != nil {
		fmt.Println(StdMsg)
		log.Fatalln("Error changing directory:", err)

	}

	return dataMap, exit_status

}

func loadAgdalib(test Testcase) {
	log.Println("Type-checking the Agda file once to load the stdlib")
	originalDir, err := os.Getwd()
	if err != nil {
		fmt.Println(StdMsg)
		log.Fatalln("Error getting current directory:", err)
	}
	err = os.Chdir(output_folder)
	if err != nil {
		fmt.Println(StdMsg)
		log.Fatalln("Error changing directory:", err)
	}
	agda_str := Language_list[1].cmd + " ./" + test.file_name + Language_list[1].file_extension
	agda_cmd := exec.Command("bash", "-c", agda_str)
	agda_cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}

	cmdDone := make(chan cmdResult, 1)
	go func() {
		// Could cause memory issues if output size is large
		var outb, errb bytes.Buffer
		agda_cmd.Stdout = &outb
		agda_cmd.Stderr = &errb
		err = agda_cmd.Run()
		cmdDone <- cmdResult{outb, errb, err}
		close(cmdDone)
	}()

	select {
	case <-time.After(30 * time.Second):
		syscall.Kill(-agda_cmd.Process.Pid, syscall.SIGKILL)
		log.Println("Process killed, context deadline exceeded")
	case <-cmdDone:
		log.Printf("Agda stdlib Loaded\n")

	}

	err = os.Chdir(originalDir)
	if err != nil {
		fmt.Println(StdMsg)
		log.Fatalln("Error changing directory:", err)

	}
}

func safe_log(value float64) float64 {
	if value <= 0 {
		return 0
	} else {
		return math.Log2(value)
	}

}

func generateGraphs() {
	setup_str := "rm -rf ./static/graphs/ && mkdir -p ./static/graphs"
	setup_cmd := exec.Command("bash", "-c", setup_str)
	err := setup_cmd.Run()
	if err != nil {
		fmt.Println(StdMsg)
		log.Fatalln("Error executing the setup commands for the graph folder", err)

	} // ask proyetei if still need this

	app_str := "python3.12 app.py"
	app_cmd := exec.Command("bash", "-c", app_str)
	go func() {
		err := app_cmd.Start()
		if err != nil {
			fmt.Println(StdMsg)
			log.Fatalln("Error starting Python script:", err)

		}

		// Wait for the command to finish
		err = app_cmd.Wait()
		if err != nil {
			fmt.Println(StdMsg)
			log.Fatalln("Error waiting for Python script:", err)
		}
	}()
	err = exec.Command("xdg-open", "http://127.0.0.1:5001").Start()
	if err != nil {
		fmt.Println(StdMsg)
		log.Fatalln("Error opening webpage:", err)
	}
	fmt.Println(termlink.ColorLink("Results", "http://127.0.0.1:5001", "italic green"))
	select {}

}

func init() {
	rootCmd.AddCommand(generateListCmd)
	generateListCmd.PersistentFlags().IntVarP(&caseID, "testcase", "t", 1, "generates data for selected testcase")
	generateListCmd.PersistentFlags().IntSliceVarP(&datapoints, "datapoints", "d", []int{}, "List of datapoints (comma separated no spaces) eg 1,2,3")
	generateListCmd.PersistentFlags().BoolVarP(&webpage, "webpage", "w", true, "generates webpage with graph visualizations")
	generateListCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "Enable detailed output for debugging and progress tracking")
}
