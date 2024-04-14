# This is a simple program that computes the color reduced histogram of
# the images in a directory
#
# (c) Robert Laganiere, CSI2510 2023

from PIL import Image
import os

def saveHistogram(image_path, depth):
    # Open the image
    with Image.open(image_path) as img:
        # Convert the image to RGB mode
        img_rgb = img.convert('RGB')
        print(image_path)
        # Get the image data as a list of tuples
        data = list(img_rgb.getdata())

        # Get the size of the image
        width, height = img.size        
        hsize= 2**(3*depth)
        
        histogram = [0] * hsize

        # Loop through each pixel and update the corresponding histogram bin
        for red, green, blue in data:

            red= red>>(8-depth)
            green= green>>(8-depth)
            blue= blue>>(8-depth)
            index= (red<<(2*depth))+(green<<depth)+blue
            histogram[index]+= 1

        # save histogram
        hist_name = image_path + '.txt' 
      
        with open(hist_name, 'w') as file:
            file.write(str(hsize)+'\n')        
            for freq in histogram:
                file.write(str(freq) + ' ')        
            file.write('\n')        
        
    return width*height

def saveHistograms_in_directory(input_directory, depth):
    total_count = 0

    # Get a list of all files in the input directory
    files = os.listdir(input_directory)

    # Filter out only the JPG files
    jpg_files = [file for file in files if file.lower().endswith('.jpg')]

    total_count=0 
    for jpg_file in jpg_files:
        # Construct the input file path
        input_path = os.path.join(input_directory, jpg_file)

        # Create histogram for the current image
        total_pix = saveHistogram(input_path, depth)

        # Accumulate the count
        total_count += 1

    return total_count

if __name__ == "__main__":
    # Specify the input directory
    input_directory = "."


    # Call the function to generate histograms
    # must provide the directory name and 
    # the number of bits per channel (for color reduction)
    total_count = saveHistograms_in_directory(input_directory, 3)

    print(f"Total number of images: {total_count}")
