/**
 * Author: Rohan Ramasamy
 * Date: 11/02/2017
 */
 
 #include <fstream>
 #include <boost/lexical_cast.hpp>

 #include <vof/io/FileWriter.h>


 namespace vof {
	void
	FileWriter::
	writeGridToFile(
        const std::shared_ptr<EulerianGrid> &grid,
        const double &time
        )
	{
		std::string fName = boost::lexical_cast<std::string>(time);
		std::ofstream output(fName);

		auto cellResolution = grid->cellResolution();
		auto cellDimensions = grid->cellSize();
		const auto densities = grid->densities();
		const auto pressures = grid->pressures();
		const auto internalEnergies = grid->internalEnergies();
		const auto velocities = grid->velocities();

		double x, y, z;
        for (int k = 0; k < cellResolution[2]; ++k) {
            for (int j = 0; j < cellResolution[1]; ++j) {
                for (int i = 0; i < cellResolution[0]; ++i) {
                    x = (i + 0.5) * cellDimensions[0];
                    y = (j + 0.5) * cellDimensions[1];
                    z = (k + 0.5) * cellDimensions[2];
                    output << i << " " << j << " " << k << " " << x << " " << y << " " << z << " " << densities(i, j, k)
                           << " " << pressures(i, j, k) << " " << velocities(i, j, k)[0] << " "
                           << velocities(i, j, k)[1]
                           << " " << velocities(i, j, k)[2] << " " << internalEnergies(i, j, k) << "\n";
                }
            }
        }
        output.close();
	}
 }
