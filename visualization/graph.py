import json
import matplotlib.pyplot as plt

with open('/Users/Proyetei/Desktop/GitHub/CS-4ZP6A-Capstone-Akanda/visualization/dummy.json', 'r') as f:
    data = json.load(f)

# Function to plot size vs space and size vs time on the same graph
def plot_size_vs_space_and_time(test_case):
    test_case_name = test_case["name"]
    languages = test_case["languages"]
    title = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"Space and Time Complexity for {title} with increasing sizes")
    plt.xlabel("Size")

    # Create a primary y-axis for space
    plt.ylabel("Space (MB)")
    
    # Plot each language's data for space complexity vs n 
    for language_data in languages:
        # From the JSON file extract info -> languages, points, x values and space values
        language = language_data["name"]
        points = language_data["tests"] 
        x_values = [point["size"] for point in points] 
        space_values = [point["space"] for point in points] 
        
        # Plot space complexity marked by solid line and o
        plt.plot(x_values, space_values, marker='o', label=f'{language} - Space')
    
    #Add legend on the upper left for space complexity vs n 
    plt.legend(loc='upper left')

    # Create a secondary y-axis for time
    ax2 = plt.gca().twinx()  # This creates a second y-axis
    ax2.set_ylabel("Time (ms)")  # Label for the time axis

    # Plot each language's data for time complexity vs n 
    for language_data in languages:
        # From the JSON file extract info -> languages, points, x values and space values
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        time_values = [point["time"] for point in points]
        
        # Plot time complexity marked by dotted line and x
        ax2.plot(x_values, time_values, marker='x', linestyle='--', label=f'{language} - Time')
        
    #Add legend on the lower right for time complexity vs n 
    ax2.legend(loc='lower right')

    # Show grid, layout and plot
    plt.grid(True)
    plt.tight_layout()
    plt.show()

# Iterate over every test case and plot size n vs time complexity vs space complexity 
for test_case in data["testcases"]:
    plot_size_vs_space_and_time(test_case)