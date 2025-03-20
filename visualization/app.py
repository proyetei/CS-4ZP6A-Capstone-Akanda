from flask import Flask, render_template
import os
import json
import matplotlib
matplotlib.use('Agg')  # Set the backend to 'Agg' to avoid GUI issues
import matplotlib.pyplot as plt
import io
import base64


# LOGIC: Since Vercel is a serverless platform, it doesn't support saving static files
# To avoid issues with static images not appearing on Vercel hosted website
# The graphs are first saved to a BytesIO object instead of a file, and then converted to base64

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

# Function to plot size vs real time
def plot_size_vs_real_time(test_case):
    test_case_name = test_case["name"]
    languages = test_case["languages"]
    description = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"Real Time Complexity for {description}")
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
    languages = test_case["languages"]
    description = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"User Time Complexity for {description} ")
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
    languages = test_case["languages"]
    description = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"System Time Complexity for {description}")
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
    languages = test_case["languages"]
    description = test_case["description"]

    # Create a new figure
    plt.figure(figsize=(8, 6))
    plt.title(f"Memory Usage for {description}")
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

    # Return a simple HTML page to display the images
    return render_template('index.html', graphs=graphs)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5001, debug=True)