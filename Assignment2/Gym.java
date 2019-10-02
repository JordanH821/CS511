import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

public class Gym implements Runnable {
    private static final int GYM_SIZE = 30;
    private static final int GYM_REGISTERED_CLIENTS = 10000;
    private Map<WeightPlateSize, Integer> noOfWeightPlates;
    private Set<Integer> clients;
    private ExecutorService executor;

    public static Semaphore[] apparatuses;
    public static Semaphore[] weights;

    public void run() {
        // setup the weights
        noOfWeightPlates = new HashMap<WeightPlateSize, Integer>();
        noOfWeightPlates.put(WeightPlateSize.SMALL_3KG, 110);
        noOfWeightPlates.put(WeightPlateSize.MEDIUM_5KG, 90);
        noOfWeightPlates.put(WeightPlateSize.LARGE_10KG, 75);

        // setup the apparatus semaphores
        int noOfApparatuses = ApparatusType.values().length;
        apparatuses = new Semaphore[noOfApparatuses];
        for (int i = 0; i < noOfApparatuses; i++) {
            apparatuses[i] = new Semaphore(5);
        }

        // setup the weight semaphores
        int noOfWeightTypes = WeightPlateSize.values().length;
        weights = new Semaphore[noOfWeightTypes];
        for (int i = 0; i < noOfWeightTypes; i++) {
            System.out.println(noOfWeightPlates.get(WeightPlateSize.values()[i]));
            weights[i] = new Semaphore(noOfWeightPlates.get(WeightPlateSize.values()[i]));
        }

        // create and workout clients
        executor = Executors.newFixedThreadPool(GYM_SIZE);
        clients = new HashSet<Integer>();
        Client client;
        int id;
        for (int i = 0; i < GYM_REGISTERED_CLIENTS; i++) {
            id = clients.size();
            client = Client.generateRandom(id);
            clients.add(clients.size());
            // executor.execute(new Workout(client));
            executor.execute(new Workout(client));
        }
        executor.shutdown();

    }

    public static class Workout implements Runnable {
        Client client;

        Workout(Client c) {
            this.client = c;
        }

        public void run() {
            List<Exercise> routine = client.getRoutine();
            Exercise currExercise;
            Map<WeightPlateSize, Integer> currWeight;
            int no3KG, no5KG, no10KG;
            for (int i = 0; i < routine.size(); i++) {
                currExercise = routine.get(i);
                currWeight = currExercise.getWeight();

                System.out.println("CLIENT #" + client.getId() + " is now starting " + currExercise.toString());

                // acquire machine
                try {
                    apparatuses[currExercise.getAt().ordinal()].acquire();
                } catch (InterruptedException e2) {
                    e2.printStackTrace();
                }
                System.out.println("CLIENT #" + client.getId() + " has acquired " + currExercise.getAt().name());

                // WeightPlateSize.SMALL_3KG
                no3KG = currWeight.get(WeightPlateSize.SMALL_3KG);
                for (int j = 0; j < no3KG; j++) {
                    try {
                        weights[0].acquire();
                    } catch (Exception e) {
                        System.err.println(e.toString());
                    }
                }
                // System.out.println("CLIENT #" + client.getId() + " has acquired 3KG weights");

                // WeightPlateSize.MEDIUM_5KG
                no5KG = currWeight.get(WeightPlateSize.MEDIUM_5KG);
                for (int j = 0; j < no5KG; j++) {
                    try {
                        weights[1].acquire();
                    } catch (Exception e) {
                        System.err.println(e.toString());
                    }
                }
                // System.out.println("CLIENT #" + client.getId() + " has acquired 5KG weights");

                // WeightPlateSize.LARGE_10KG
                no10KG = currWeight.get(WeightPlateSize.LARGE_10KG);
                for (int j = 0; j < no10KG; j++) {
                    try {
                        weights[2].acquire();
                    } catch (InterruptedException e) {
                        System.err.println(e.toString());
                    }
                }
                // System.out.println("CLIENT #" + client.getId() + " has acquired 10KG weights");

                System.out.println("CLIENT #" + client.getId() + " has acquired weights for " + currExercise.toString());
                // EXERCISE FOR DURATION
                try {
                    Thread.sleep(currExercise.getDuration());
                } catch (InterruptedException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                }

                // WeightPlateSize.SMALL_3KG
                for (int j = 0; j < no3KG; j++) {
                    weights[0].release();
                }

                // WeightPlateSize.MEDIUM_5KG
                for (int j = 0; j < no5KG; j++) {
                    weights[1].release();
                }

                // WeightPlateSize.LARGE_10KG
                for (int j = 0; j < no10KG; j++) {
                    weights[2].release();
                }
                System.out.println("CLIENT #" + client.getId() + " has finished " + currExercise.toString());

                System.out.println("CLIENT #" + client.getId() + " is releasing " + currExercise.getAt().name());
                apparatuses[currExercise.getAt().ordinal()].release();
            }
        }
    }
}