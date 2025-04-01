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

# Setting colors for each language
colors = {"Rocq": "blue", "Agda": "orange", "Lean": "purple", "Idris":"green"}

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

def plotArtificialZeros(plt, artificial_zero_x, artificial_zero):
    for i in range(0, len(artificial_zero_x)):
        plt.plot(artificial_zero_x[i], artificial_zero, marker='s', markersize=8, color='cyan', markeredgewidth=1.5)

def get_label_position(plt, end_y_values):
    min_y, max_y = plt.ylim()
    ratio_denominator = max_y - min_y
    label_positions = {}
    last_position = 0
    ratio_dict = {key: (value - min_y) / ratio_denominator for key, value in end_y_values.items()}
    ratio_dict_sorted = {k: v for k, v in sorted(ratio_dict.items(), key=lambda item: item[1], reverse=False)}
    for i, (key, value) in enumerate(ratio_dict_sorted.items()):
        if i == 0:
            label_positions[key] = value
            last_position = value
        else:
            difference = value - last_position
            if  difference < 0.03:
                offset = 0.04 - difference
                label_positions[key] = value + offset
                last_position = value + offset
            else:
                label_positions[key] = value
                last_position = value


    return label_positions

    

def adding_annotations(plt, all_points, field, lower_bound, end_values):
    label_positions = get_label_position(plt, end_values)
    print(field)
    for language_data in all_points:
        language = language_data["name"]
        plt.text(1.01, label_positions[language], language, color = colors[language], transform=plt.gca().transAxes)



# Function to plot size vs real time
def plot_size_vs_real_time(test_case):
    test_case_name = test_case["name"]
    test_case_lower = test_case["lower_bound"]
    languages = test_case["languages"]

    if test_case["interval"] == "log":
        xlabel = "Log(Size)"
        ylabel = "Log(Real Time [s])"
        artificial_zero = -6.64
    else:
        xlabel = "Size"
        ylabel = "Real Time [s]"
        artificial_zero = 0

    # Create a new figure
    plt.figure(figsize=(7, 5))
    plt.title(f"Real Time Complexity for {test_case_name}")
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)

    # Set y-axis scale based on test case interval

    max_y_values = {}
    # Plot each language's data for real time complexity vs size
    for language_data in languages:
        artificial_zero_x = []
        language = language_data["name"]
        points = language_data["tests"] 
        x_values = [point["size"] for point in points] 
        real_time_values = [point["real_time"] for point in points] 
        
        if len(x_values) == 0:
            max_y_values[language] = 0
        else:
            end_value = real_time_values[x_values.index(max(x_values))]
            if end_value == -10:
                max_y_values[language] = 0
            else:
                max_y_values[language] = end_value

        for i in range(0, len(real_time_values)):
            if real_time_values[i] == -10:
                real_time_values[i] = 0
                artificial_zero_x += [x_values[i]]
        # Plot real time complexity marked by solid line and o
        plt.plot(x_values, real_time_values, marker='o', label=f'{language} - Real Time', color = colors[language])
        plotArtificialZeros(plt, artificial_zero_x, artificial_zero)

        # Call exit status marker
        checkExitStatus(plt, language_data, test_case_lower, x_values, real_time_values)
    
    adding_annotations(plt, languages, "real_time", test_case_lower, max_y_values)
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
    test_case_lower = test_case["lower_bound"]
    languages = test_case["languages"]
    # description = test_case["description"]

    if test_case["interval"] == "log":
        xlabel = "Log(Size)"
        ylabel = "Log(User Time [s])"
        artificial_zero = -6.64
    else:
        xlabel = "Size"
        ylabel = "User Time [s]"
        artificial_zero = 0

    # Create a new figure
    plt.figure(figsize=(7, 5))
    plt.title(f"User Time Complexity for {test_case_name} ")
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)


    max_y_values = {}

    # Plot each language's data for user time complexity vs size
    for language_data in languages:
        artificial_zero_x = []
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        user_time_values = [point["user_time"] for point in points]

        if len(x_values) == 0:
            max_y_values[language] = 0
        else:
            end_value = user_time_values[x_values.index(max(x_values))]
            if end_value == -10:
                max_y_values[language] = 0
            else:
                max_y_values[language] = end_value
        
        for i in range(0, len(user_time_values)):
            if user_time_values[i] == -10:
                user_time_values[i] = 0
                artificial_zero_x += [x_values[i]]
        # Plot user time complexity marked by dotted line and x
        plt.plot(x_values, user_time_values, marker='x', linestyle='--', label=f'{language} - User Time', color = colors[language])
        plotArtificialZeros(plt, artificial_zero_x, artificial_zero)
        # Check exit status and plot marker if not OK
        checkExitStatus(plt, language_data, test_case_lower, x_values, user_time_values)
        
    adding_annotations(plt, languages, "user_time", test_case_lower, max_y_values)
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
    test_case_lower = test_case["lower_bound"]
    languages = test_case["languages"]
    # description = test_case["description"]

    if test_case["interval"] == "log":
        xlabel = "Log(Size)"
        ylabel = "Log(System Time [s])"
        artificial_zero = -6.64
    else:
        xlabel = "Size"
        ylabel = "System Time [s]"
        artificial_zero = 0

    # Create a new figure
    plt.figure(figsize=(7, 5))
    plt.title(f"System Time Complexity for {test_case_name}")
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)

    max_y_values = {}
    # Plot each language's data for system time complexity vs size
    for language_data in languages:
        artificial_zero_x = []
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        system_time_values = [point["system_time"] for point in points]

        if len(x_values) == 0:
            max_y_values[language] = 0
        else:
            end_value = system_time_values[x_values.index(max(x_values))]
            if end_value == -10:
                max_y_values[language] = 0
            else:
                max_y_values[language] = end_value

        for i in range(0, len(system_time_values)):
            if system_time_values[i] == -10:
                system_time_values[i] = 0
                artificial_zero_x += [x_values[i]]
        
        # Plot system time complexity marked by dotted line and x
        plt.plot(x_values, system_time_values, marker='x', linestyle='--', label=f'{language} - System Time', color = colors[language])
        plotArtificialZeros(plt, artificial_zero_x, artificial_zero)

        # Check exit status and plot marker if not OK
        checkExitStatus(plt, language_data, test_case_lower, x_values, system_time_values)
    
    adding_annotations(plt, languages, "system_time", test_case_lower, max_y_values)
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
    test_case_lower = test_case["lower_bound"]
    languages = test_case["languages"]
    # description = test_case["description"]

    if test_case["interval"] == "log":
        xlabel = "Log(Size)"
        ylabel = "Log(Memory [MB])"
    else:
        xlabel = "Size"
        ylabel = "Memory [MB]"

    # Create a new figure
    plt.figure(figsize=(7, 5))
    plt.title(f"Memory Usage for {test_case_name}")
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)


    max_y_values = {}
    # Plot each language's data for memory usage vs size
    for language_data in languages:
        language = language_data["name"]  
        points = language_data["tests"]  
        x_values = [point["size"] for point in points]  
        memory_values = [point["memory"] for point in points]

        if len(x_values) == 0:
            max_y_values[language] = 0
        else:
            end_value = memory_values[x_values.index(max(x_values))]
            if end_value == -10:
                max_y_values[language] = 0
            else:
                max_y_values[language] = end_value
        
        # Plot memory usage marked by dotted line and x
        plt.plot(x_values, memory_values, marker='x', linestyle='--', label=f'{language} - Memory', color = colors[language] )
        
        # Check exit status and plot marker if not OK
        checkExitStatus(plt, language_data, test_case_lower, x_values, memory_values)

    adding_annotations(plt, languages, "memory", test_case_lower, max_y_values)
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


def get_error_messages(language_data, interval, lower_bound, x_values, y_values):
    errors = []
    if language_data["exit_status"] != "OK":
        if len(x_values) == 0:
            max_size = lower_bound
            y_value = 0
        else:
            max_size = max(x_values)
            index = x_values.index(max_size)
            y_value = y_values[index]
        if interval == "log":
            max_size = 2 ** max_size

            
        errors.append({
            "language": language_data["name"],
            "size": language_data["exit_point"],
            "type_of_error": "memory" if language_data["exit_status"] == "memory" else "time"
        })
    return errors


# Route to serve the images
@app.route('/')
def index():
    # Generate graphs before displaying the page
    graphs = generate_graphs(data)

    # Get test case
    test_case = data["testcases"][0]
    
    # Extract error information for each language
    errors = []
    for language_data in test_case["languages"]:
        x_values = [point["size"] for point in language_data["tests"]]
        # Using real_time values for error calculation; change to another metric if needed
        y_values = [point["real_time"] for point in language_data["tests"]]
        errors.extend(get_error_messages(language_data, test_case["interval"], test_case["lower_bound"], x_values, y_values))


    # Return a simple HTML page to display the images
    # Extract name and description to reference on the website
    return render_template('index.html',  test_case_name=test_case["name"], errors=errors, 
                           test_case_desc=test_case["description"], graphs=graphs)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5001, debug=True)

