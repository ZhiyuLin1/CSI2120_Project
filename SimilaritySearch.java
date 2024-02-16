// Zhiyu Lin 300255509
// yitao CUI 300345709

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class SimilaritySearch{
    public static void main(String[] args){
        String queryImageName = args[0];
        String datasetFolder = args[1];
        int depth = 3;

        long startTime = System.nanoTime(); // start time


        ColorImage queryImage = new ColorImage("queryImages/" + queryImageName); // load query image

       
        ColorHistogram queryHistogramDone = new ColorHistogram(depth); // color depth d = 3
        
        

        ColorImage queryHistogram = new ColorImage("queryImages/" + queryImageName);
        queryHistogram.reduceColor(depth); // depth = 3
        queryHistogramDone.setImage(queryHistogram); 




        File datasetDir = new File(datasetFolder);
        File[] datasetFiles = datasetDir.listFiles((dir, name) -> name.endsWith(".jpg.txt")); // read histogram


        Map<String, Double> similarityData = new HashMap<>();
        
        for (int i = 0; i < datasetFiles.length; i++) {
            File file = datasetFiles[i];
            ColorHistogram datasetHistogram = new ColorHistogram(file.getAbsolutePath());
            double similarity = queryHistogramDone.compare(datasetHistogram);
            similarityData.put(file.getName(), similarity);
        }

        // sort similarityData
        List<Map.Entry<String, Double>> finalOuputSorted = new ArrayList<>(similarityData.entrySet());

        for (int i = 0; i < finalOuputSorted.size() - 1; i++) {
            for (int j = 0; j < finalOuputSorted.size() - i - 1; j++) {
                if (finalOuputSorted.get(j).getValue() < finalOuputSorted.get(j + 1).getValue()) {
                    Map.Entry<String, Double> temp = finalOuputSorted.get(j);
                    finalOuputSorted.set(j, finalOuputSorted.get(j + 1));
                    finalOuputSorted.set(j + 1, temp);
                }
            }
        }

        System.out.println("5 most similar images to the query image: \n");

        for (int i = 0; i < 5 && i < finalOuputSorted.size(); i++) {
            Map.Entry<String, Double> entry = finalOuputSorted.get(i);
            System.out.println("Top " + (i+1) + " = " + entry.getKey() + " | Similarity: " + entry.getValue());
        }


        // output query image histogram
        String histogramFilename = queryImageName + "_normalized" + ".hist.txt"; 
        queryHistogramDone.saveHistogram(histogramFilename);
        System.out.println("\nQuery image normalized histogram saved to: " + histogramFilename);

        
        // running time
        long endTime = System.nanoTime();
        System.out.println("\nrunning time is "+ (endTime- startTime)/1000000+" ms");
        
    }
}

