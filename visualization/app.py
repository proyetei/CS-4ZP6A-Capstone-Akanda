from flask import Flask, render_template
import os
import json
import matplotlib
matplotlib.use('Agg')  # Set the backend to 'Agg' to avoid GUI issues
import matplotlib.pyplot as plt
import io
import base64
from matplotlib.ticker import ScalarFormatter

# LOGIC: Since Vercel is a serverless platform, it doesn't support saving static files
# To avoid issues with static images not appearing on Vercel hosted website, the graphs are first saved to a BytesIO object instead of a file, and then converted to base64

app = Flask(__name__)

# Returns the current directory
curr_directory = os.path.dirname(__file__)
json_path = os.path.join(curr_directory, 'data.json')

# Load the JSON file into a variable called `data` to be used later
try:
    with open(json_path, 'r') as f:
        data = json.load(f)
# file error exception message
except FileNotFoundError:
    print(f"Error: The file {json_path} does not exist. Please ensure 'data.json' is in the correct location.")
    exit(1)


# METHOD CHECK EXIT STATUS LOGIC: Find the exit status, if its not OK, meaning its memory or time, then put a marker on the exact coordinates

def checkExitStatus(plt, language_data, lower_bound, x_values, y_values):
    if language_data["exit_status"] != "OK":
        if len(x_values) == 0:
            max_size = lower_bound
            y_value = 0
        else:
            max_size = max(x_values)
            index = x_values.index(max_size)
            y_value = y_values[index]
        # if the exit status is memory exceeded then its marked by red cross, if its time limit exceeded, its marked by blue cross
        if language_data["exit_status"] == "memory":
            plt.plot(max_size, y_value, marker='x', markersize=12, color='red', markeredgewidth=2)
        else:
            plt.plot(max_size, y_value, marker='x', markersize=12, color='blue', markeredgewidth=2)

# METHOD USE LOG SCALE LOGIC: for the testcase interval, take the log of lower bound and upper bound as the y axis
# If that field is equal to string log, allow for negtiave values for y axis of the graph
# def should_use_log_scale(test_case):
#     """Check if test case should use log scale on y-axis"""
#     return test_case.get("interval", "").lower() == "log"

# Function to plot size vs real time
def plot_size_vs_real_time(test_case):
    test_case_name = test_case["name"]
    test_case_lower = test_case.get("lower_bound")
    languages = test_case["languages"]

    # Create a new figure
    plt.figure(figsize=(7, 5))
    plt.title(f"Real Time Complexity for {test_case_name}")
    plt.xlabel("Size")
    plt.ylabel("Real Time (s)")

    # Set y-axis scale based on test case interval
    # if should_use_log_scale(test_case):
    #     plt.yscale('symlog', linthresh=1e-3)
    #     plt.gca().yaxis.set_major_formatter(ScalarFormatter())
    #     plt.gca().yaxis.set_minor_formatter(ScalarFormatter())
    # else:
    #     plt.ylim(bottom=0)

    # Plot each language's data for real time complexity vs size
    for language_data in languages:
        language = language_data["name"]
        points = language_data["tests"] 
        x_values = [point["size"] for point in points] 
        real_time_values = [point["real_time"] for point in points] 

        # To avoid confusion with the red marker, change leans pointer to purple
        color = 'purple' if language == "Lean" else None
        
        # Plot real time complexity marked by solid line and o
        plt.plot(x_values, real_time_values, marker='o', label=f'{language} - Real Time', color = color)

        # Call exit status marker
        checkExitStatus(plt, language_data, test_case_lower, x_values, real_time_values)
    
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to a BytesIO object
    buf = io.BytesIO()
    plt.savefig(buf, format='png')
    plt.close()
    buf.seek(0)

    # Encode the image as base64
    image_base64 = base64.b64encode(buf.read()).decode('utf-8')
    buf.close()

    return image_base64

# Function to plot size vs user time
def plot_size_vs_user_time(test_case):
    test_case_name = test_case["name"]
    test_case_lower = test_case.get("lower_bound")
    languages = test_case["languages"]
    # description = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(7, 5))
    plt.title(f"User Time Complexity for {test_case_name} ")
    plt.xlabel("Size")
    plt.ylabel("User Time (s)")

    # Set y-axis scale based on test case interval
    # if should_use_log_scale(test_case):
    #     plt.yscale('symlog', linthresh=1e-3)
    #     plt.gca().yaxis.set_major_formatter(ScalarFormatter())
    #     plt.gca().yaxis.set_minor_formatter(ScalarFormatter())
    # else:
    #     plt.ylim(bottom=0)

    # Plot each language's data for user time complexity vs size
    for language_data in languages:
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        user_time_values = [point["user_time"] for point in points]

        color = 'purple' if language == "Lean" else None
        
        # Plot user time complexity marked by dotted line and x
        plt.plot(x_values, user_time_values, marker='x', linestyle='--', label=f'{language} - User Time', color = color)
        # Check exit status and plot marker if not OK
        checkExitStatus(plt, language_data, test_case_lower, x_values, user_time_values)
        
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to a BytesIO object
    buf = io.BytesIO()
    plt.savefig(buf, format='png')
    plt.close()
    buf.seek(0)

    # Encode the image as base64
    image_base64 = base64.b64encode(buf.read()).decode('utf-8')
    buf.close()

    return image_base64

# Function to plot size vs system time
def plot_size_vs_system_time(test_case):
    test_case_name = test_case["name"]
    test_case_lower = test_case.get("lower_bound")
    languages = test_case["languages"]
    # description = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(7, 5))
    plt.title(f"System Time Complexity for {test_case_name}")
    plt.xlabel("Size")
    plt.ylabel("System Time (s)")

    # Set y-axis scale based on test case interval
    # if should_use_log_scale(test_case):
    #     plt.yscale('symlog', linthresh=1e-3)
    #     plt.gca().yaxis.set_major_formatter(ScalarFormatter())
    #     plt.gca().yaxis.set_minor_formatter(ScalarFormatter())
    # else:
    #     plt.ylim(bottom=0)

    # Plot each language's data for system time complexity vs size
    for language_data in languages:
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        system_time_values = [point["system_time"] for point in points]

        color = 'purple' if language == "Lean" else None
        
        # Plot system time complexity marked by dotted line and x
        plt.plot(x_values, system_time_values, marker='x', linestyle='--', label=f'{language} - System Time', color = color)

        # Check exit status and plot marker if not OK
        checkExitStatus(plt, language_data, test_case_lower, x_values, system_time_values)
        
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to a BytesIO object
    buf = io.BytesIO()
    plt.savefig(buf, format='png')
    plt.close()
    buf.seek(0)

    # Encode the image as base64
    image_base64 = base64.b64encode(buf.read()).decode('utf-8')
    buf.close()

    return image_base64

# Function to plot size vs memory
def plot_size_vs_memory(test_case):
    test_case_name = test_case["name"]
    test_case_lower = test_case.get("lower_bound")
    languages = test_case["languages"]
    # description = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(7, 5))
    plt.title(f"Memory Usage for {test_case_name}")
    plt.xlabel("Size")
    plt.ylabel("Memory (KB)")

    # Set y-axis scale based on test case interval
    # if should_use_log_scale(test_case):
    #     plt.yscale('symlog', linthresh=1e-3)
    #     plt.gca().yaxis.set_major_formatter(ScalarFormatter())
    #     plt.gca().yaxis.set_minor_formatter(ScalarFormatter())
    # else:
    #     plt.ylim(bottom=0)

    # Plot each language's data for memory usage vs size
    for language_data in languages:
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        memory_values = [point["memory"] for point in points]

        color = 'purple' if language == "Lean" else None
        
        # Plot memory usage marked by dotted line and x
        plt.plot(x_values, memory_values, marker='x', linestyle='--', label=f'{language} - Memory', color = color )
        
        # Check exit status and plot marker if not OK
        checkExitStatus(plt, language_data, test_case_lower, x_values, memory_values)
    # Add legend
    plt.legend(loc='upper left')
    plt.grid(True)
    plt.tight_layout()

    # Save the plot to a BytesIO object
    buf = io.BytesIO()
    plt.savefig(buf, format='png')
    plt.close()
    buf.seek(0)

    # Encode the image as base64
    image_base64 = base64.b64encode(buf.read()).decode('utf-8')
    buf.close()

    return image_base64

# Generate graphs from the JSON data and return base64 images
def generate_graphs(data):
    graphs = []
    for test_case in data["testcases"]:
        real_time_graph = plot_size_vs_real_time(test_case)
        user_time_graph = plot_size_vs_user_time(test_case)
        system_time_graph = plot_size_vs_system_time(test_case)
        memory_graph = plot_size_vs_memory(test_case)
        graphs.extend([real_time_graph, user_time_graph, system_time_graph, memory_graph])
    return graphs

# Route to serve the images
@app.route('/')
def index():
    # Generate graphs before displaying the page
    graphs = generate_graphs(data)

    # Get test case
    test_case = data["testcases"][0]

    # Return a simple HTML page to display the images
    # Extract name and description to reference on the website
    return render_template('index.html',  test_case_name=test_case["name"], 
                           test_case_desc=test_case["description"], graphs=graphs)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5001, debug=True)

    