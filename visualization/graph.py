
import json
import matplotlib.pyplot as plt
import os

# Construct the path dynamically
cwd = os.getcwd()
json_path = os.path.join(cwd, 'data.json')

# Load the JSON file
with open(json_path, 'r') as f:
    data = json.load(f)

# Function to plot size vs real time
def plot_size_vs_real_time(test_case):
    test_case_name = test_case["name"]
    languages = test_case["languages"]
    title = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"Real Time Complexity for {title} with increasing sizes")
    plt.xlabel("Size")
    plt.ylabel("Real Time (s)")

    # Plot each language's data for real time complexity vs size
    for language_data in languages:
        language = language_data["name"]
        points = language_data["tests"] 
        x_values = [point["size"] for point in points] 
        real_time_values = [point["real_time"] for point in points] 
        
        # Plot real time complexity marked by solid line and o
        plt.plot(x_values, real_time_values, marker='o', label=f'{language} - Real Time')
    
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to the static/graphs directory
    graph_filename = os.path.join(cwd, f"static/graphs/{test_case_name}_real_time_graph.png")
    plt.savefig(graph_filename)
    plt.close()

    return graph_filename

# Function to plot size vs user time
def plot_size_vs_user_time(test_case):
    test_case_name = test_case["name"]
    languages = test_case["languages"]
    title = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"User Time Complexity for {title} with increasing sizes")
    plt.xlabel("Size")
    plt.ylabel("User Time (s)")

    # Plot each language's data for user time complexity vs size
    for language_data in languages:
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        user_time_values = [point["user_time"] for point in points]
        
        # Plot user time complexity marked by dotted line and x
        plt.plot(x_values, user_time_values, marker='x', linestyle='--', label=f'{language} - User Time')
        
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to the static/graphs directory
    graph_filename = os.path.join(cwd, f"static/graphs/{test_case_name}_user_time_graph.png")
    plt.savefig(graph_filename)
    plt.close()

    return graph_filename

# Function to plot size vs system time
def plot_size_vs_system_time(test_case):
    test_case_name = test_case["name"]
    languages = test_case["languages"]
    title = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"System Time Complexity for {title} with increasing sizes")
    plt.xlabel("Size")
    plt.ylabel("System Time (s)")

    # Plot each language's data for system time complexity vs size
    for language_data in languages:
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        system_time_values = [point["system_time"] for point in points]
        
        # Plot system time complexity marked by dotted line and x
        plt.plot(x_values, system_time_values, marker='x', linestyle='--', label=f'{language} - System Time')
        
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to the static/graphs directory
    graph_filename = os.path.join(cwd, f"static/graphs/{test_case_name}_system_time_graph.png")
    plt.savefig(graph_filename)
    plt.close()

    return graph_filename

# Function to plot size vs memory
def plot_size_vs_memory(test_case):
    test_case_name = test_case["name"]
    languages = test_case["languages"]
    title = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"Memory Usage for {title} with increasing sizes")
    plt.xlabel("Size")
    plt.ylabel("Memory (MB)")

    # Plot each language's data for memory usage vs size
    for language_data in languages:
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        memory_values = [point["memory"] for point in points]
        
        # Plot memory usage marked by dotted line and x
        plt.plot(x_values, memory_values, marker='x', linestyle='--', label=f'{language} - Memory')
        
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to the static/graphs directory
    graph_filename = os.path.join(cwd, f"static/graphs/{test_case_name}_memory_graph.png")
    plt.savefig(graph_filename)
    plt.close()

    return graph_filename

# Generate graphs
for test_case in data["testcases"]:
    plot_size_vs_real_time(test_case)
    plot_size_vs_user_time(test_case)
    plot_size_vs_system_time(test_case)
    plot_size_vs_memory(test_case)