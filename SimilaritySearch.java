public class SimilaritySearch{
    public static void main(String[] args){
        String image1 = args[0];
        String image2 = args[1];

    }
    
}

public class ColorImage{
    public int[][][] pixels;
    public int width;
    public int height;
    public int depth;

    public ColorImage(String filename){
        try{
            File file = new File(filename);
            
            if (filename.endsWith(".ppm")){
                readPPM(file);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        
    
}
}