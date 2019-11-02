import java.util.concurrent.Semaphore;

public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        Semaphore semaphore = new Semaphore(1);
        char[] contentChars = content.substring(interval.getX(), interval.getY()).toCharArray();
        try {
            semaphore.acquire();
            for(int i = 0; i < contentChars.length; i++){
                buffer[offset + i] = contentChars[i];
            }
            semaphore.release();
        } catch (InterruptedException e) {
            System.out.println("Problem acquiring the semaphore permissions, terminating...");
            System.exit(1);
        }
    }
}