package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"

	"github.com/spf13/cobra"
)

var StartTimeCmd = &cobra.Command{
	Use:   "startup-time [flags]",
	Short: "Records the startup time for each test case in Agda, Idris, Lean, and Rocq, saving the data to a JSON file",
	Run: func(cmd *cobra.Command, args []string) {
		if !verbose {
			log.SetOutput(io.Discard)
		}
		startup_times := make(map[int]map[string][]Data)

		for _, test := range Case_list {
			startup_times[test.id] = make(map[string][]Data)
			for _, lang := range Language_list {
				startup_times[test.id][lang.name] = []Data{{0, 0, 0, 0, 0}}

			}
			translateTest(test, 0)
			loadAgdalib(test)

			for i := 0; i < 10; i++ {
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
				exit_point := map[string]int{
					"Rocq":  -1,
					"Agda":  -1,
					"Idris": -1,
					"Lean":  -1,
				}

				dataMap, _, _ = run_test(test, dataMap, exit_status, exit_point, 0)
				for language, data := range dataMap {
					if len(data) != 0 {
						if i == 0 {
							startup_times[test.id][language][0].System_time = data[0].System_time
							startup_times[test.id][language][0].Real_time = data[0].Real_time
							startup_times[test.id][language][0].User_time = data[0].User_time

						} else {
							if data[0].System_time < startup_times[test.id][language][0].System_time {
								startup_times[test.id][language][0].System_time = data[0].System_time
							}
							if data[0].User_time < startup_times[test.id][language][0].User_time {
								startup_times[test.id][language][0].User_time = data[0].User_time
							}
							if data[0].Real_time < startup_times[test.id][language][0].Real_time {
								startup_times[test.id][language][0].Real_time = data[0].Real_time
							}

						}

					}

				}

			}

		}
		json_data, err := json.MarshalIndent(startup_times, "", "  ")
		if err != nil {
			if !verbose {
				fmt.Println(StdMsg)
			}
			log.Fatalln("Error marshalling to JSON:", err)
		}

		log.Println("Creating JSON file")
		file, err := os.Create("startup.json")
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

	},
}

func init() {
	rootCmd.AddCommand(StartTimeCmd)
	StartTimeCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "Enable detailed output for debugging and progress tracking")

}
