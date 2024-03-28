package main

import (
    "fmt"
    "io/ioutil"
	"os"
    "log"
    "strings"
)

func main() {

	// read the directory name from command line
    args := os.Args

    files, err := ioutil.ReadDir(args[1])
    if err != nil {
        log.Fatal(err)
    }
	
	// Create an array to store filenames
	var filenames []string

    // get the list of jpg files	
    for _, file := range files {
		if strings.HasSuffix(file.Name(), ".jpg") {
		    filenames = append(filenames, file.Name())
		}
    }
	
	// Print the array of filenames
	fmt.Println("List of jpg images:")
	for _, filename := range filenames {
		fmt.Println(filename)
	}
}