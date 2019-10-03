/** start the simulation */
public class Assignment2 {
    public static void main(String[] args) {
        Thread thread = new Thread(new Gym());
        thread.start();
        try {
            thread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}