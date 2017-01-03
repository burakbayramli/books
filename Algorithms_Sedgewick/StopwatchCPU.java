/*************************************************************************
 *  Compilation:  javac Stopwatch.java
 *
 *
 *************************************************************************/

import java.lang.management.ThreadMXBean;
import java.lang.management.ManagementFactory;

/**
 *  The <tt>StopwatchCPU</tt> data type is for measuring
 *  the CPU time used during a programming task.
 *
 *  See {@link Stopwatch} for a version that measures wall-clock time
 *  (the real time that elapses).
 *
 *  @author Josh Hug
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */

public class StopwatchCPU {
    private final ThreadMXBean threadTimer;
    private final long start;
    private static final double NANOSECONDS_PER_SECOND = 1000000000;
            
    /**
     * Initialize a stopwatch object.
     */
    public StopwatchCPU() {  
        threadTimer = ManagementFactory.getThreadMXBean();
        start = threadTimer.getCurrentThreadCpuTime();
    }   
        
    /**
     * Returns the elapsed CPU time (in seconds) since the object was created.
     */
    public double elapsedTime() {
        long now = threadTimer.getCurrentThreadCpuTime();
        return (now - start) / NANOSECONDS_PER_SECOND;
    }   
}
