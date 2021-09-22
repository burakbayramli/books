/**
 * Author: Rohan Ramasamy
 * Date: 03/03/2017
 */

#include <chrono>
#include <map>
#include <string>


namespace vof {
/**
 * A class to control the timing diagnostics. The Master timer is instantiated only once.
 */
class ScopedTimer
{
public:
    /**
     * Constructor initialises period to be timed
     */
    ScopedTimer(
        const std::string& timerName
        );

    /**
     * Destructor completes period o be timed and prints sub timer output
     */
    ~ScopedTimer();

    /**
     * Used by sub timers to store result times
     */
    void
    addSubTimerResult(
        const std::string& subTimerName,
        const double& timeTaken
        );

    static void
    getTimerStatistics();

private:
    std::string mName;
    std::chrono::time_point<std::chrono::steady_clock> mStart;
};

}
