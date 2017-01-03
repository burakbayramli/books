/***********************************************************************************
 *  Compilation: javac LSD.java
 *  Execution:   java LSD < input.txt
 *
 *  LSD radix sort
 *
 *    - Sort a String[] array of N extended ASCII strings (R = 256), each of length W.
 *
 *    - Sort an int[] array of N 32-bit integers, treating each integer as 
 *      a sequence of W = 4 bytes (R = 256).
 *
 *  Uses extra space proportional to N + R.
 *
 *
 *  % java LSD < words3.txt
 *  all
 *  bad
 *  bed
 *  bug
 *  dad
 *  ...
 *  yes
 *  yet
 *  zoo
 *
 ***********************************************************************************/

public class LSD {
    private final static int BITS_PER_BYTE = 8;


    // LSD radix sort
    public static void sort(String[] a, int W) {
        int N = a.length;
        int R = 256;   // extend ASCII alphabet size
        String[] aux = new String[N];

        for (int d = W-1; d >= 0; d--) {
            // sort by key-indexed counting on dth character

            // compute frequency counts
            int[] count = new int[R+1];
            for (int i = 0; i < N; i++)
                count[a[i].charAt(d) + 1]++;

            // compute cumulates
            for (int r = 0; r < R; r++)
                count[r+1] += count[r];

            // move data
            for (int i = 0; i < N; i++)
                aux[count[a[i].charAt(d)]++] = a[i];

            // copy back
            for (int i = 0; i < N; i++)
                a[i] = aux[i];
        }
    }

    // LSD sort an array of integers, treating each int as 4 bytes
    // assumes integers are nonnegative
    // [ 2-3x faster than Arrays.sort() ]
    public static void sort(int[] a) {
        int BITS = 32;                 // each int is 32 bits 
        int W = BITS / BITS_PER_BYTE;  // each int is 4 bytes
        int R = 1 << BITS_PER_BYTE;    // each bytes is between 0 and 255
        int MASK = R - 1;              // 0xFF

        int N = a.length;
        int[] aux = new int[N];

        for (int d = 0; d < W; d++) {         

            // compute frequency counts
            int[] count = new int[R+1];
            for (int i = 0; i < N; i++) {           
                int c = (a[i] >> BITS_PER_BYTE*d) & MASK;
                count[c + 1]++;
            }

            // compute cumulates
            for (int r = 0; r < R; r++)
                count[r+1] += count[r];

            // for most significant byte, 0x80-0xFF comes before 0x00-0x7F
            if (d == W-1) {
                int shift1 = count[R] - count[R/2];
                int shift2 = count[R/2];
                for (int r = 0; r < R/2; r++)
                    count[r] += shift1;
                for (int r = R/2; r < R; r++)
                    count[r] -= shift2;
            }

            // move data
            for (int i = 0; i < N; i++) {
                int c = (a[i] >> BITS_PER_BYTE*d) & MASK;
                aux[count[c]++] = a[i];
            }

            // copy back
            for (int i = 0; i < N; i++)
                a[i] = aux[i];
        }
    }

    public static void main(String[] args) {
        String[] a = StdIn.readAllStrings();
        int N = a.length;

        // check that strings have fixed length
        int W = a[0].length();
        for (int i = 0; i < N; i++)
            assert a[i].length() == W : "Strings must have fixed length";

        // sort the strings
        sort(a, W);

        // print results
        for (int i = 0; i < N; i++)
            StdOut.println(a[i]);
    }
}
