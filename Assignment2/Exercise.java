import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

public class Exercise {
    private ApparatusType at;
    private Map<WeightPlateSize, Integer> weight;
    private int duration;
    private static final int MAX_DURATION = 25;

    public Exercise(ApparatusType at, Map<WeightPlateSize, Integer> weight, int duration){
        this.at = at;
        this.weight = weight;
        this.duration = duration;
    }

    public static Exercise generateRandom(){
        List<Exercise> exercises = new ArrayList<Exercise>();
        Map<WeightPlateSize, Integer> weights;
        Random random = new Random();
        int noOfExercises = 15 + random.nextInt(6); // nextInt() is not inclusive
        int duration = 1 + random.nextInt(MAX_DURATION);
        int noOf3KG, noOf5KG, noOf10KG;
        do {
            noOf3KG = random.nextInt(11);
            noOf5KG = random.nextInt(11);
            noOf10KG = random.nextInt(11);
        } while (noOf3KG + noOf5KG + noOf10KG == 0);

        weights = new HashMap<WeightPlateSize, Integer>();
        weights.put(WeightPlateSize.SMALL_3KG, noOf3KG);
        weights.put(WeightPlateSize.MEDIUM_5KG, noOf5KG);
        weights.put(WeightPlateSize.LARGE_10KG, noOf10KG);

        ApparatusType apparatusType = ApparatusType.values()[random.nextInt(ApparatusType.values().length)]; //exclusive upper bound 
        
        return new Exercise(apparatusType, weights, duration);
    }

    public ApparatusType getAt() {
        return at;
    }

    public void setAt(ApparatusType at) {
        this.at = at;
    }

    public Map<WeightPlateSize, Integer> getWeight() {
        return weight;
    }

    public void setWeight(Map<WeightPlateSize, Integer> weight) {
        this.weight = weight;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    @Override
    public String toString(){
        return at.name() + " " + weightToString(weight) + " " + duration + "MS";
    }

    private String weightToString(Map<WeightPlateSize, Integer> weights){
        StringBuilder sb = new StringBuilder();
        WeightPlateSize[] plates = WeightPlateSize.values();
        sb.append("[");
        for(int i =0; i < plates.length; i++){
            sb.append("(" + plates[i].name() + ", " + weights.get(plates[i]));
            if(i != plates.length - 1){
                sb.append("), ");
            }
        }
        sb.append(")]");
        return sb.toString();
    }
}