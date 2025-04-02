package cmd

import (
	"fmt"
	"os"
	"strings"

	"github.com/jedib0t/go-pretty/v6/table"
	"github.com/jedib0t/go-pretty/v6/text"

	"github.com/spf13/cobra"
)

var printTestsCmd = &cobra.Command{
	Use:   "print",
	Short: "Prints the available test cases and their maximum upper bounds",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("Available Test Cases:")
		tw := table.NewWriter()
		tw.SetColumnConfigs([]table.ColumnConfig{
			{Number: 1, VAlign: text.VAlignMiddle, Align: text.AlignCenter, WidthMax: 10},
			{Number: 2, AlignHeader: text.AlignCenter, WidthMax: 60},
			{Number: 3, VAlign: text.VAlignMiddle, Align: text.AlignCenter, WidthMax: 15},
		})

		tw.AppendHeader(table.Row{"Test Case", "Description", "Upper Bound"})
		tw.AppendSeparator()
		for i := 1; i <= maxID; i++ {
			wrapped_desc := wrap_text(Case_list[i].desc, 60)
			tw.AppendRow(table.Row{i, wrapped_desc, Case_list[i].max_range})
			tw.AppendSeparator()

		}
		fmt.Println(tw.Render())
		os.Exit(1)
	},
}

func wrap_text(str string, width int) string {
	new_str := ""
	current_width := 0
	words := strings.Fields(str)
	for _, word := range words {
		if current_width+len(word) >= width {
			new_str += "\n" + word + " "
			current_width = len(word) + 1
		}
		current_width += len(word) + 1
		new_str += word + " "

	}
	return new_str

}
func init() {
	rootCmd.AddCommand(printTestsCmd)

}
