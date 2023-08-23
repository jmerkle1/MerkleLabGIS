import os
import subprocess
source_folder = 'C:/Users/cowboy/Documents/Cropscape'
destination_folder = 'C:/Users/cowboy/Documents/Cropscape/COGS'

# Iterate through all the files in the source folder
for filename in os.listdir(source_folder):
    if filename.endswith('.tif'):
        print({filename})
        source_path = os.path.join(source_folder, filename)
        destination_path = os.path.join(destination_folder, filename)

        command = f'rio cogeo create "{source_path}" "{destination_path}"'
        subprocess.run(command, shell=True)

        print(f"Converted: {destination_path}")
