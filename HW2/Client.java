import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Client {
    private int id;
    private List<Exercise> routine;

    public Client(int id){
        this.id = id;
    }

    public void addExercise(Exercise e){
        this.routine.add(e);
    }

    public static Client generateRandom(int id){
        Client client = new Client(id);
        List<Exercise> exercises = new ArrayList<Exercise>();
        Random random = new Random();
        int noOfExercises = 15 + random.nextInt(6); // nextInt() is not inclusive
        
        for(int i = 0; i < noOfExercises; i++){
            exercises.add(Exercise.generateRandom());
        }

        client.routine = exercises;

        return client;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public List<Exercise> getRoutine() {
        return routine;
    }

    public void setRoutine(List<Exercise> routine) {
        this.routine = routine;
    }
}