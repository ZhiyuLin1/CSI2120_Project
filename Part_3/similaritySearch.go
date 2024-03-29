package main

import (
    "fmt"
    "image"
    _ "image/jpeg"
    "log"
    "os"
    "path/filepath"
    "sort"
    "sync"
)

type Histo struct {
    Name string
    H    []int
}

func computeHistogram(imagePath string, depth int) (Histo, error) {
    file, err := os.Open(imagePath)
    if err != nil {
        return Histo{}, err
    }
    defer file.Close()

    img, _, err := image.Decode(file)
    if err != nil {
        return Histo{}, err
    }

    bounds := img.Bounds()
    histogram := make([]int, 1<<(depth*3))

    for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
        for x := bounds.Min.X; x < bounds.Max.X; x++ {
            r, g, b, _ := img.At(x, y).RGBA()
            index := (((r >> 8) >> (8 - depth)) << (2 * depth)) + (((g >> 8) >> (8 - depth)) << depth) + ((b >> 8) >> (8 - depth))
            histogram[index]++
        }
    }

    return Histo{Name: filepath.Base(imagePath), H: histogram}, nil
}

func computeHistograms(imagePaths []string, depth int, hChan chan<- Histo, wg *sync.WaitGroup) {
    defer wg.Done()
    for _, imagePath := range imagePaths {
        histo, err := computeHistogram(imagePath, depth)
        if err != nil {
            log.Printf("Error computing histogram for %s: %v", imagePath, err)
            continue
        }
        hChan <- histo
    }
}

func similarity(h1, h2 []int) float64 {
    var dH1H2 float64
    var sumAllHistNumbers float64

    for i := 0; i < len(h1); i++ {
        dH1H2 += float64(min(h1[i], h2[i]))
        sumAllHistNumbers += float64(h1[i])
    }

    if sumAllHistNumbers == 0 {
        return 0
    }
    return dH1H2 / sumAllHistNumbers
}

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}

func main() {
    if len(os.Args) != 3 {
        log.Fatalf("Usage: go run similaritySearch.go <queryImageFilename> <imageDatasetDirectory>")
    }

    queryImageName := os.Args[1]
    datasetDir := os.Args[2]
    depth := 3

    queryImagePath := filepath.Join("queryImages", queryImageName)

    queryHisto, err := computeHistogram(queryImagePath, depth)
    if err != nil {
        log.Fatalf("Failed to compute histogram for query image: %v", err)
    }

    datasetPath := filepath.Join(datasetDir, "*.jpg")
    filepaths, err := filepath.Glob(datasetPath)
    if err != nil {
        log.Fatalf("Failed to list dataset images: %v", err)
    }

    var K int = 4
    slices := make([][]string, K)
    for i, path := range filepaths {
        sliceIndex := i % K
        slices[sliceIndex] = append(slices[sliceIndex], path)
    }

    hChan := make(chan Histo, len(filepaths))
    var wg sync.WaitGroup

    for _, slice := range slices {
        wg.Add(1)
        go computeHistograms(slice, depth, hChan, &wg)
    }

    go func() {
        wg.Wait()
        close(hChan)
    }()

    similarityScores := make(map[string]float64)
    for histo := range hChan {
        similarityScores[histo.Name] = similarity(queryHisto.H, histo.H)
    }

    var ss []kv
    for k, v := range similarityScores {
        ss = append(ss, kv{k, v})
    }

    sort.Slice(ss, func(i, j int) bool {
        return ss[i].Value > ss[j].Value
    })

    fmt.Println("5 most similar images to the query image:")
    for i := 0; i < 5 && i < len(ss); i++ {
        fmt.Printf("Top %d = %s | Similarity: %.12f\n", i+1, ss[i].Key, ss[i].Value)
    }
}

type kv struct {
    Key   string
    Value float64
}
