package cmd

import (
	"encoding/json"
	"fmt"
	"log"
	"os"

	"github.com/spf13/cobra"
)

var StartTimeCmd = &cobra.Command{
	Use:   "startup-time",
	Short: "Gets the startup time for each testcase and each language and returns a JSON file",
	Run: func(cmd *cobra.Command, args []string) {
		startup_times := make(map[int]map[string][]Data)

		for _, test := range Case_list {
			cumulative_times := make(map[int]map[string][]Data)
			cumulative_times[test.id] = make(map[string][]Data)
			startup_times[test.id] = make(map[string][]Data)
			for _, lang := range Language_list {
				cumulative_times[test.id][lang.name] = []Data{{0, 0, 0, 0, 0}}
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

				dataMap, _ = run_test(test, dataMap, exit_status, 0)
				for language, data := range dataMap {
					if len(data) != 0 {
						cumulative_times[test.id][language][0].System_time += data[0].System_time
						cumulative_times[test.id][language][0].Real_time += data[0].Real_time
						cumulative_times[test.id][language][0].User_time += data[0].User_time
						cumulative_times[test.id][language][0].Memory += data[0].Memory

					}

				}

			}
			for pal, info := range cumulative_times[test.id] {
				if len(info) != 0 {
					startup_times[test.id][pal][0].System_time = info[0].System_time / 10
					startup_times[test.id][pal][0].User_time = info[0].User_time / 10
					startup_times[test.id][pal][0].Real_time = info[0].Real_time / 10
					startup_times[test.id][pal][0].Memory = info[0].Memory / 10

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

}
