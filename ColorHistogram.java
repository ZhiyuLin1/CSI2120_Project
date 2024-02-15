// Zhiyu Lin 300255509
// yitao CUI 300345709

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;

public class ColorHistogram {
    private int depth;
    private double[] histogram;
    private int totalPixels = 480*360;

    public ColorHistogram(int d) {
        this.depth = d;
        this.histogram = new double[(int) Math.pow(2, d * 3)]; 
    }

    public ColorHistogram(String filename) {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            reader.readLine();
            String line = reader.readLine();

            String[] histData;
            histData = line.trim().split("\\s+"); // spilit spaces in histogram
            histogram = new double[histData.length];
            for (int i = 0; i < histData.length; i++) {
                histogram[i] = Double.parseDouble(histData[i]);
            }
            
        } catch (IOException e) {
            System.out.println(e);
        }
    }

    public void setImage(ColorImage image) {

        histogram = image.histogram;

    }
    
    public void saveHistogram(String filename) {

        try (FileWriter writer = new FileWriter(filename)) {
            writer.write(histogram.length + "\n");

            for (int i=0 ; i<histogram.length; i++) {
                writer.write(((double)histogram[i]) + " ");
            }

            writer.write("\n");
        } catch (IOException e) {
            System.out.println(e);
        }
    }
    

    public double compare(ColorHistogram toCompare) {
        double[] histToCompare = toCompare.getHistogram();
        double d_H1H2 = 0; 
        double sum_all_hist_number = 0; 
        double similarityResult = 0;
        
        for (int i = 0; i < histogram.length; i++) {
            d_H1H2 += Math.min(histogram[i], histToCompare[i]); 
            
            sum_all_hist_number += histogram[i]; // calculate the sum of all the numbers in the query image histogram 
        }
        similarityResult = d_H1H2 / sum_all_hist_number;  // similarity result [0,1]
        
        return similarityResult;
        

    }

    
    public double[] getHistogram() {

        
        for (int i=0; i < histogram.length; i++){
            
            double temp = 0.0;
            temp = (histogram[i])/totalPixels;
            histogram[i] = temp;
            
        }

        return histogram;

    }

}
