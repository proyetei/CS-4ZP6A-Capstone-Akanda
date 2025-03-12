from flask import Flask, render_template
import os

app = Flask(__name__)

# Path to the directory where images are stored
GRAPH_DIR = os.path.join(os.getcwd(), 'static/graphs')

# Route to serve the images
@app.route('/')
def index():
    # Get all image files in the directory
    image_files = [f for f in os.listdir(GRAPH_DIR) if f.endswith('.png') or f.endswith('.jpg')]

    # Return a simple HTML page to display the images
    return render_template('index.html', images=image_files)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)
