/**
 * Author: Rohan Ramasamy
 * Date: 17/06/2017
 */

#include <vof/io/VTKWriter.h>

#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkPoints.h>
#include <vtkSmartPointer.h>
#include <vtkHexahedron.h>
#include <vtkDoubleArray.h>
#include <vtkCellData.h>

namespace vof {
    void
    VTKWriter::
    writeStructuredGridToFile(
        const std::shared_ptr<EulerianGrid> &grid,
        const double& time,
        const int& timeStep
        )
    {
        // Get grid states and geometry
        auto cellResolution = grid->cellResolution();
        auto cellDimensions = grid->cellSize();
        const auto densities = grid->densities();
        const auto pressures = grid->pressures();
        const auto internalEnergies = grid->internalEnergies();
        const auto velocities = grid->velocities();
        
        // Set VTK writer classes
        vtkSmartPointer<vtkXMLUnstructuredGridWriter> vtkWriter = vtkXMLUnstructuredGridWriter::New();
        vtkSmartPointer<vtkUnstructuredGrid> vtkGrid = vtkUnstructuredGrid::New();
        
        // Define vtk points representing nodes of cells
        vtkSmartPointer<vtkPoints> points = vtkPoints::New();
        points->SetNumberOfPoints((cellResolution[0] + 1) * (cellResolution[1] + 1) * (cellResolution[2] + 1));
        
        // define vtk double arrays representing cell data
        vtkSmartPointer<vtkDoubleArray> rhoArray = vtkDoubleArray::New();
        rhoArray->SetNumberOfComponents(1);
        rhoArray->SetName("Density");
        vtkSmartPointer<vtkDoubleArray> pArray = vtkDoubleArray::New();
        pArray->SetNumberOfComponents(1);
        pArray->SetName("Pressure");
        vtkSmartPointer<vtkDoubleArray> eArray = vtkDoubleArray::New();
        eArray->SetNumberOfComponents(1);
        eArray->SetName("Internal_Energy");
        vtkSmartPointer<vtkDoubleArray> velArray = vtkDoubleArray::New();
        velArray->SetNumberOfComponents(3);
        velArray->SetName("Velocity");
        
        // Set time stamp
        vtkSmartPointer<vtkDoubleArray> timeArray = vtkDoubleArray::New();
        timeArray->SetName("TIME");
        timeArray->SetNumberOfTuples(1);
        timeArray->SetTuple1(0, time);
        vtkGrid->GetFieldData()->AddArray(timeArray);

        // Add point data to VTK Output
        int pointId = 0;
        double x, y, z;
        for (int k = 0; k < cellResolution[2] + 1; ++k) {
            for (int j = 0; j < cellResolution[1] + 1; ++j) {
                for (int i = 0; i < cellResolution[0] + 1; ++i) {
                    x = i * cellDimensions[0];
                    y = j * cellDimensions[1];
                    z = k * cellDimensions[2];
                    
                    points->SetPoint(pointId, x, y, z);
                    ++pointId;
                }
            }
        }

        // Define cells and cell data
        vtkSmartPointer<vtkHexahedron> cell = vtkHexahedron::New();
        for (int k = 0; k < cellResolution[2]; ++k) {
            for (int j = 0; j < cellResolution[1]; ++j) {
                for (int i = 0; i < cellResolution[0]; ++i) {
                    // Get node values for connectivity
                    int node1 = k * (cellResolution[1] + 1) * (cellResolution[0] + 1) + j * (cellResolution[0] + 1) + i;
                    int node2 = (k + 1) * (cellResolution[1] + 1) * (cellResolution[0] + 1) + j * (cellResolution[0] + 1) + i;
                    int node3 = (k + 1) * (cellResolution[1] + 1) * (cellResolution[0] + 1) + (j + 1) * (cellResolution[0] + 1) + i;
                    int node4 = k * (cellResolution[1] + 1) * (cellResolution[0] + 1) + (j + 1) * (cellResolution[0] + 1) + i;
                    int node5 = k * (cellResolution[1] + 1) * (cellResolution[0] + 1) + j * (cellResolution[0] + 1) + i + 1;
                    int node6 = (k + 1) * (cellResolution[1] + 1) * (cellResolution[0] + 1) + j * (cellResolution[0] + 1) + i + 1;
                    int node7 = (k + 1) * (cellResolution[1] + 1) * (cellResolution[0] + 1) + (j + 1) * (cellResolution[0] + 1) + i + 1;
                    int node8 = k * (cellResolution[1] + 1) * (cellResolution[0] + 1) + (j + 1) * (cellResolution[0] + 1) + i + 1;
                    
                    // Set nodes of cell                    
                    cell->GetPointIds()->SetId(0, node1);
                    cell->GetPointIds()->SetId(1, node2);
                    cell->GetPointIds()->SetId(2, node3);
                    cell->GetPointIds()->SetId(3, node4);
                    cell->GetPointIds()->SetId(4, node5);
                    cell->GetPointIds()->SetId(5, node6);
                    cell->GetPointIds()->SetId(6, node7);
                    cell->GetPointIds()->SetId(7, node8);

                    // Add cell to vtk
                    vtkGrid->InsertNextCell(cell->GetCellType(), cell->GetPointIds());
                    
                    // Add cell data to arrays
                    rhoArray->InsertNextTuple(&densities(i, j, k));
                    pArray->InsertNextTuple(&pressures(i, j, k));
                    velArray->InsertNextTuple3(velocities(i, j, k)[0],
                                               velocities(i, j, k)[1],
                                               velocities(i, j, k)[2]);
                    eArray->InsertNextTuple(&internalEnergies(i, j, k));
                }
            }
        }

        // Add to data to grid
        vtkGrid->SetPoints(points);
        vtkGrid->GetCellData()->AddArray(rhoArray);
        vtkGrid->GetCellData()->AddArray(pArray);
        vtkGrid->GetCellData()->AddArray(velArray);
        vtkGrid->GetCellData()->AddArray(eArray);

        // Write to file
        std::stringstream ss;
        ss << "ts" << timeStep << ".vtu";
        vtkWriter->SetInputData(vtkGrid);
        vtkWriter->SetFileName(ss.str().c_str());
        vtkWriter->SetDataModeToBinary();
        vtkWriter->Write();
    }
}
