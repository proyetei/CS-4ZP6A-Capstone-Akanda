import json
import matplotlib.pyplot as plt
import os

# Construct the path dynamically
json_path = os.path.join('visualization', 'dummy.json')

# Load the JSON file
with open(json_path, 'r') as f:
    data = json.load(f)

# Function to plot size vs space
def plot_size_vs_space(test_case):
    test_case_name = test_case["name"]
    languages = test_case["languages"]
    title = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"Space Complexity for {title} with increasing sizes")
    plt.xlabel("Size")
    plt.ylabel("Space (MB)")

    # Plot each language's data for space complexity vs size
    for language_data in languages:
        language = language_data["name"]
        points = language_data["tests"] 
        x_values = [point["size"] for point in points] 
        space_values = [point["space"] for point in points] 
        
        # Plot space complexity marked by solid line and o
        plt.plot(x_values, space_values, marker='o', label=f'{language} - Space')
    
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to the static/graphs directory
    graph_filename = f"/Users/Proyetei/Desktop/CS-4ZP6A-Capstone-Akanda/visualization/static/graphs/{test_case_name}_space_graph.png"
    plt.savefig(graph_filename)
    plt.close()

    return graph_filename

# Function to plot size vs time
def plot_size_vs_time(test_case):
    test_case_name = test_case["name"]
    languages = test_case["languages"]
    title = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"Time Complexity for {title} with increasing sizes")
    plt.xlabel("Size")
    plt.ylabel("Time (ms)")

    # Plot each language's data for time complexity vs size
    for language_data in languages:
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        time_values = [point["time"] for point in points]
        
        # Plot time complexity marked by dotted line and x
        plt.plot(x_values, time_values, marker='x', linestyle='--', label=f'{language} - Time')
        
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to the static/graphs directory
    graph_filename = f"/Users/Proyetei/Desktop/CS-4ZP6A-Capstone-Akanda/visualization/static/graphs/{test_case_name}_time_graph.png"
    plt.savefig(graph_filename)
    plt.close()

    return graph_filename

# Generate graphs
for test_case in data["testcases"]:
    plot_size_vs_space(test_case)
    plot_size_vs_time(test_case)