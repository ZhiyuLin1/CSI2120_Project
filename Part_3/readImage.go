package main

import (
	"fmt"
	"image"
	"os"
	_ "image/jpeg"
)

type Histo struct {
    Name string
    H []int
}

// adapted from: first example at pkg.go.dev/image
func computeHistogram(imagePath string, depth int) (Histo, error) {
	// Open the JPEG file
	file, err := os.Open(imagePath)
	if err != nil {
		return Histo{"",nil},err
	}
	defer file.Close()

	// Decode the JPEG image
	img, _, err := image.Decode(file)
	if err != nil {
		return Histo{"",nil},err
	}

	// Get the dimensions of the image
	bounds := img.Bounds()
	width, height := bounds.Max.X, bounds.Max.Y

	// Display RGB values for the first 5x5 pixels
	// remove y < 5 and x < 5  to scan the entire image
	for y := 0; y < height && y < 5; y++ {
		for x := 0; x < width && x < 5; x++ {
		
			// Convert the pixel to RGBA
			red, green, blue, _ := img.At(x, y).RGBA() 
			// A color's RGBA method returns values in the range [0, 65535].
			// Shifting by 8 reduces this to the range [0, 255].
			red>>=8
			blue>>=8
			green>>=8

			// Display the RGB values
			fmt.Printf("Pixel at (%d, %d): R=%d, G=%d, B=%d\n", x, y, red, green, blue)
		}
	}

    h:= Histo{imagePath, make([]int,depth)}
	return h,nil
}

func main() {
	// read the image name from command line
    args := os.Args

	// Call the function to display RGB values of some pixels
	_,err := displayRGBValues(args[1],10)
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		return
	}
}
