// Zhiyu Lin 300255509
// yitao CUI 300345709

import java.io.File;
import java.io.IOException;
import java.util.Scanner;

public class ColorImage{
    public int[][][] pixels; //[width][height][depth]
    public int width;
    public int height;
    public int depth;
    public double totalPixels;
    
    public int d=3;
    public double[] histogram = new double[(int) Math.pow(2, d*3)]; 

    public ColorImage(String filename){
        try{
            Scanner imageValue = new Scanner(new File(filename));  //read .ppm file

            imageValue.nextLine(); // skip 1st line
            imageValue.nextLine(); // skip 2nd line
    
            width = imageValue.nextInt(); // read 3rd line 1
            height = imageValue.nextInt(); // read 3rd line 2
    
            imageValue.nextInt(); // skip 4th line
            

            pixels = new int[width][height][3]; // store pixels data
    
            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    pixels[j][i][0] = imageValue.nextInt(); // R 
                    pixels[j][i][1] = imageValue.nextInt(); // G
                    pixels[j][i][2] = imageValue.nextInt(); // B
                }
            }
    
            imageValue.close();

        } catch (IOException e) {
            System.out.println("Error occured");
        }   
    }

    public int[] getPixel(int i, int j){
        return pixels[i][j];
    }

    public int getWidth(){
        return width;
    }

    public int getHeight(){
        return height;
    }

    public int getDepth(){
        return depth;
    }


    public void reduceColor(int d){
    
        

        //bit shifts
        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                int pixels_R = pixels[i][j][0] >> (8 - d);
                int pixels_G = pixels[i][j][1] >> (8 - d);
                int pixels_B = pixels[i][j][2] >> (8 - d);

                int index = (pixels_R << (2 * d)) + (pixels_G << d) + pixels_B; 
                histogram[index] += 1;
            }
        }
       
        
        //Normalize H
        totalPixels = width*height;
        
        for (int i=0; i < histogram.length; i++){
            
            double temp = 0.0;
            temp = (histogram[i])/totalPixels;
            histogram[i] = temp;
            
        }
        
        

    }

}