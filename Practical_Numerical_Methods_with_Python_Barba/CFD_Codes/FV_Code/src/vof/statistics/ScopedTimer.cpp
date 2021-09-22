/**
 * Author: Rohan Ramasamy
 * Date: 03/03/2017
 */

#include <iostream>
#include <iomanip>
#include <cassert>
#include <map>
#include <vector>

#include <vof/statistics/ScopedTimer.h>


namespace vof
{
std::map<std::string, double> subtimerComponents;

ScopedTimer::
ScopedTimer(
    const std::string& timerName
    )
{
    mName = timerName;
    mStart = std::chrono::steady_clock::now();
}

ScopedTimer::
~ScopedTimer()
{
    auto totalTime = std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::steady_clock::now() - mStart);
    addSubTimerResult(mName, totalTime.count());
}

void
ScopedTimer::
addSubTimerResult(
    const std::string& subTimerName,
    const double& timeTaken
    )
{
    if (subtimerComponents.find(subTimerName) != subtimerComponents.end()) {
        subtimerComponents[subTimerName] += timeTaken;
    }
    else {
        subtimerComponents[subTimerName] = timeTaken;
    }
}

void
ScopedTimer::
getTimerStatistics()
{
    assert(subtimerComponents.size() > 0);
    // Set header
    int padding = 30;
    std::cout << "\n" << std::setw(padding) << std::right << "Timer Statistics:"
              << std::setw(padding) << std::right << "Time (microseconds)"
              << std::setw(padding) << std::right << "Percentage Runtime (%)" << "\n";

    // Convert subtimer into a vector and sort from longest to shortest component
    std::vector<std::pair<std::string, double> > results;
    for (auto stat : subtimerComponents) {
        results.push_back(stat);
    }
    std::sort(results.begin(), results.end(), [](std::pair<std::string, double>& a, std::pair<std::string, double>& b) {
        return a.second > b.second;
    });

    // Print results
    auto totalTime = results[0].second;
    double remainder = 200.0;
    double fraction;
    for (auto stat : results) {
        fraction = stat.second / totalTime * 100;
        std::cout << std::setw(padding) << std::right << stat.first
                  << std::setw(padding) << std::right << stat.second
                  << std::setw(padding) << std::right << fraction << "\n";
        remainder -= fraction;
    }
    std::cout << std::setw(padding) << std::right << "Unaccounted"
              << std::setw(padding) << std::right << "-"
              << std::setw(padding) << std::right << remainder << "\n";

    // Clear timer for next use
    subtimerComponents.clear();
}

}
