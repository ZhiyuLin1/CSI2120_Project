// Zhiyu Lin 300255509

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;

public class ColorHistogram {
    private int depth;
    private double[] histogram;

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
        /* 
        Arrays.fill(histogram, 0);
    
        int width = image.getWidth();
        int height = image.getHeight();
        int shift = 8 - depth;
    
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                int[] pixel = image.getPixel(j, i);
                int r = pixel[0] >> shift;
                int g = pixel[1] >> shift;
                int b = pixel[2] >> shift;
                int index = (r << (2 * depth)) + (g << depth) + b;
                histogram[index]++;
            }
        }
        */

        histogram = image.histogram;

    }
    
    public void saveHistogram(String filename) {

        try (FileWriter writer = new FileWriter(filename)) {
            writer.write(histogram.length + "\n");

            for (int i=0 ; i<histogram.length; i++) {
                writer.write(((int)histogram[i]) + " ");
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

        System.out.println(histogram.length);
        
        for (int i = 0; i < histogram.length; i++) {
            d_H1H2 += Math.min(histogram[i], histToCompare[i]); 
            
            sum_all_hist_number += histogram[i]; // calculate the sum of all the numbers in the query image histogram 
        }
        similarityResult = d_H1H2 / sum_all_hist_number;  // normalized similarity result [0,1]
        
       
        System.out.println("//////////////////////////////////////////////////////////////////////");
        System.out.println(similarityResult);
        return similarityResult;
        

    }

    
    public double[] getHistogram() {

        return histogram;

    }

}
