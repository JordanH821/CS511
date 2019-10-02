import java.io.*;
import java.util.*;

public class TextSwap {

    private static String readFile(String filename) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null) {
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        Interval[] intervals = new Interval[numChunks];
        int chunkStart = 0;

        for (int i = 0; i < numChunks; i++) {
            intervals[i] = new Interval(chunkStart, chunkStart + chunkSize);
            chunkStart += chunkSize;
        }
        return intervals;
    }

    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        return labels;
    }

    private static char[] runSwapper(String content, int chunkSize, int numChunks) {
        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);
        Interval[] orderedIntervals = new Interval[intervals.length];
        for (int i = 0; i < labels.size(); i++) {
            orderedIntervals[i] = intervals[Character.getNumericValue(labels.get(i)) - 10];
        }

        Thread[] threads = new Thread[orderedIntervals.length];
        int offset = 0;
        char[] sharedBuffer = new char[content.length()];
        for (int i = 0; i < orderedIntervals.length; i++) {
            threads[i] = new Thread(new Swapper(orderedIntervals[i], content, sharedBuffer, offset));
            threads[i].start();
            offset += chunkSize;
        }

        for (Thread t : threads) {
            try {
                t.join();
            } catch (InterruptedException e) {
                System.out.println("Thread joing error, terminating...");
                System.exit(1);
            }
        }
        return sharedBuffer;
    }

    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }
        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);
        try {
            contents = readFile(args[1]);
            int numChunks = contents.length() / chunkSize;
            if(numChunks > 26){
                System.out.println("Chunk size too small");
                System.exit(1);
            } else if(contents.length() % chunkSize != 0){
                System.out.println("File does not evenly breakup into the given chunksize, terminating...");
                System.exit(1);
            }
            writeToFile(contents, chunkSize, numChunks);
        } catch (Exception e) {
            System.out.println("Error with IO.");
            return;
        }
    }
}