/**
 * Author: Rohan Ramasamy
 * Date: 11/02/2017
 */
 
 #pragma once
 
 #include <vof/data/grid/EulerianGrid.h>
 
 
 namespace vof {
 class FileWriter
 {
 public:
	static void
	writeGridToFile(
        const std::shared_ptr<EulerianGrid> &grid,
        const double &time
        );
 };
 }
