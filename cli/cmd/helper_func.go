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
	"syscall"
	"time"

	"github.com/savioxavier/termlink"
)

func startupTimes() map[int]map[string][]Data {
	var startup_times map[int]map[string][]Data
	jsonFile, err := os.Open("startup.json")
	if err != nil {
		if !verbose {
			fmt.Println(StdMsg)

		}
		log.Fatalln("Could not open JSON file", err)
	}
	defer jsonFile.Close()
	json_bytes, _ := io.ReadAll(jsonFile)

	err = json.Unmarshal(json_bytes, &startup_times)
	if err != nil {
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Error marshalling json data", err)
	}

	return startup_times

}

func dataTemplate(test Testcase) Overview {
	data := Overview{
		Testcases: []Test{
			{
				Name:        test.file_name,
				Description: test.desc,
				Interval:    "None",
				LowerBound:  1,
				Languages: []LanguageJSON{
					{
						Name:        "Rocq",
						Tests:       []Data{},
						Exit_status: "OK",
						Exit_point:  -1,
					},
					{
						Name:        "Agda",
						Tests:       []Data{},
						Exit_status: "OK",
						Exit_point:  -1,
					},
					{
						Name:        "Idris",
						Tests:       []Data{},
						Exit_status: "OK",
						Exit_point:  -1,
					},
					{
						Name:        "Lean",
						Tests:       []Data{},
						Exit_status: "OK",
						Exit_point:  -1,
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
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Error executing the output folder setup commands", err)

	}

	translate_cmd := exec.Command("bash", "-c", "./translators")
	translate_cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}

	cmdDone := make(chan error, 1)
	go func() {
		stdin, err := translate_cmd.StdinPipe()
		if err != nil {
			if !verbose {
				fmt.Println(StdMsg)
			}
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
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Process killed, context deadline exceeded")
	case result := <-cmdDone:
		if result != nil {
			if !verbose {
				fmt.Println(StdMsg)
			}
			log.Fatalln("Could not translate the testcase", result)
		}
	}

}

func run_test(test Testcase, dataMap map[string][]Data, exit_status map[string]string, exit_point map[string]int, operations int) (map[string][]Data, map[string]string, map[string]int) {
	originalDir, err := os.Getwd()
	if err != nil {
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Error getting current directory:", err)
	}
	err = os.Chdir(output_folder)
	if err != nil {
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Error changing directory:", err)
	}

	pattern := `"real_time": ([0-9]*\.[0-9]+), "user_time": ([0-9]*\.[0-9]+), "system_time": ([0-9]*\.[0-9]+), "memory": ([0-9]+)}`
	re := regexp.MustCompile(pattern)

	for i := 0; i < len(Language_list); i++ {
		var test_data Data
		var final_result cmdResult
		var type_check_time string
		if exit_status[Language_list[i].name] != "OK" {
			continue
		}

		log.Printf("Type-checking %s file: testcase %d, size %d\n", Language_list[i].name, test.id, operations)
		time_str := `/usr/bin/time -o time.json --format='"real_time": %e, "user_time": %U, "system_time": %S, "memory": %M}' `
		cmd_str := fmt.Sprintf("%s %s ./%s%s", time_str, Language_list[i].cmd, test.file_name, Language_list[i].file_extension)
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
		case <-time.After(120 * time.Second):
			syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
			log.Println("Process killed, context deadline exceeded")
			exit_status[Language_list[i].name] = "time"
			exit_point[Language_list[i].name] = operations
			final_result = cmdResult{bytes.Buffer{}, bytes.Buffer{}, nil}
		case result := <-cmdDone:
			final_result = result
		}

		if final_result.err != nil && test.id != 17 {
			exit_point[Language_list[i].name] = operations
			log.Printf("Type-checking stderr message:\n%s\nType-checking stdout message:\n%s\n", final_result.errb.String(), final_result.outb.String())
			exit_status[Language_list[i].name] = "memory"

		}
		if exit_status[Language_list[i].name] == "OK" {
			type_check_time = get_time()
			matches := re.FindStringSubmatch(type_check_time)
			if matches == nil {
				log.Println("Could not record the space and time data")
			}
			jsonstr := fmt.Sprintf(`{"size": %d, `, operations) + matches[0]
			err = json.Unmarshal([]byte(jsonstr), &test_data)
			if err != nil {
				log.Println("Could not unmarshal test data", err)
			} else {
				test_data.Memory = test_data.Memory / 1000
				if starting_time != nil {
					test_data.Real_time = safe_time(test_data.Real_time, starting_time[test.id][Language_list[i].name][0].Real_time)
					test_data.System_time = safe_time(test_data.System_time, starting_time[test.id][Language_list[i].name][0].System_time)
					test_data.User_time = safe_time(test_data.User_time, starting_time[test.id][Language_list[i].name][0].User_time)

				}

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
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Error changing directory:", err)

	}

	return dataMap, exit_status, exit_point

}

func remove_duplicate_int(intSlice []int) []int {
	allKeys := make(map[int]bool)
	list := []int{}
	for _, item := range intSlice {
		if _, value := allKeys[item]; !value {
			allKeys[item] = true
			list = append(list, item)
		}
	}
	return list
}

func get_time() string {
	dat, err := os.ReadFile("./time.json")
	if err != nil {
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Could not read time file", err)

	}
	return string(dat)

}
func safe_time(type_check float64, start_time float64) float64 {
	real_time := type_check - start_time
	if real_time < 0 {
		log.Println("There is an artificial 0 at this point")
		return -10
	} else {
		return real_time
	}

}
func loadAgdalib(test Testcase) {
	log.Println("Type-checking the Agda file once to load the stdlib")
	originalDir, err := os.Getwd()
	if err != nil {
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Error getting current directory:", err)
	}
	err = os.Chdir(output_folder)
	if err != nil {
		if !verbose {
			fmt.Println(StdMsg)
		}
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
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Error changing directory:", err)

	}
}

func safe_log(value float64) float64 {
	if value <= 0 {
		if value == -10 {
			return -10
		} else {
			return math.Log2(0.01)
		}
	} else {
		return math.Log2(value)
	}

}

func set_agda_memory() {
	if agda_memory != 3 {
		agda_cmd := fmt.Sprintf("agda +RTS -M%dG -RTS", agda_memory)
		Language_list[1].cmd = agda_cmd
	}
}

func generateGraphs() {
	setup_str := "rm -rf ./static/graphs/ && mkdir -p ./static/graphs"
	setup_cmd := exec.Command("bash", "-c", setup_str)
	err := setup_cmd.Run()
	if err != nil {
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Error executing the setup commands for the graph folder", err)

	}

	app_str := "python3.12 app.py"
	app_cmd := exec.Command("bash", "-c", app_str)
	go func() {
		err := app_cmd.Start()
		if err != nil {
			if !verbose {
				fmt.Println(StdMsg)
			}
			log.Fatalln("Error starting Python script:", err)

		}

		// Wait for the command to finish
		err = app_cmd.Wait()
		if err != nil {
			if !verbose {
				fmt.Println(StdMsg)
			}
			log.Fatalln("Error waiting for Python script:", err)
		}
	}()
	err = exec.Command("xdg-open", "http://127.0.0.1:5001").Start()
	if err != nil {
		if !verbose {
			fmt.Println(StdMsg)
		}
		log.Fatalln("Error opening webpage:", err)
	}
	fmt.Println(termlink.ColorLink("Results", "http://127.0.0.1:5001", "italic green"))
	select {}

}
