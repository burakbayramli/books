#include "sw.h"

void write_results(sw &sw, int step)
{

    std::string file_name;

    file_name = sw.case_name + "_Asii." + std::to_string(step);

    sw.file.open(file_name + ".vtu", std::fstream::out);
    sw.file << std::fixed;
    sw.file << std::setprecision(10);
    sw.file << "<VTKFile type=\"UnstructuredGrid\" version=\"0.1\">\n";
    sw.file << "\t<UnstructuredGrid>\n";
    sw.file << "\t\t<Piece NumberOfPoints=\"" << sw.N_vertices << "\" NumberOfCells=\"" << sw.N_cells << "\">\n";
    sw.file << "\t\t\t<CellData>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Int32\" Name=\"type\" NumberOfComponents=\"1\" Format=\"ascii\">\n";
    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.file << sw.cells[i].type << "\n";
    }
    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Float64\" Name=\"h\" NumberOfComponents=\"1\" Format=\"ascii\">\n";
    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.file << sw.cells[i].Q(0) << "\n";
    }
    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Float64\" Name=\"u\" NumberOfComponents=\"1\" Format=\"ascii\">\n";
    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.file << sw.cells[i].Q(1) / sw.cells[i].Q(0) << "\n";
    }
    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Float64\" Name=\"v\" NumberOfComponents=\"1\" Format=\"ascii\">\n";
    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.file << sw.cells[i].Q(2) / sw.cells[i].Q(0) << "\n";
    }
    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Float64\" Name=\"R1\" NumberOfComponents=\"1\" Format=\"ascii\">\n";
    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.file << sw.cells[i].R(0) << "\n";
    }
    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Float64\" Name=\"R2\" NumberOfComponents=\"1\" Format=\"ascii\">\n";
    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.file << sw.cells[i].R(1) << "\n";
    }
    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Float64\" Name=\"R3\" NumberOfComponents=\"1\" Format=\"ascii\">\n";
    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.file << sw.cells[i].R(2) << "\n";
    }
    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t</CellData>\n";
    sw.file << "\t\t\t<Points>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Float64\" Name=\"position\" NumberOfComponents=\"3\" Format=\"ascii\">\n";
    for (int i = 0; i < sw.N_vertices; i++)
    {
        sw.file << sw.vertices[i](0) << "\t";
        sw.file << sw.vertices[i](1) << "\t";
        sw.file << sw.vertices[i](2) << "\n";
    }

    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t</Points>\n";
    sw.file << "\t\t\t<Cells>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Int32\" Name=\"connectivity\" Format=\"ascii\">\n";

    for (int i = 0; i < sw.N_cells; i++)
    {

        sw.file << sw.cells[i].vertex1 << "\t";
        sw.file << sw.cells[i].vertex2 << "\t";
        sw.file << sw.cells[i].vertex3 << "\n";
    }

    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Int32\" Name=\"offsets\" Format=\"ascii\">\n";

    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.file << 3 * (i + 1) << "\n";
    }

    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t\t<DataArray type=\"Int32\" Name=\"types\" Format=\"ascii\">\n";

    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.file << "5\n";
    }

    sw.file << "\t\t\t\t</DataArray>\n";
    sw.file << "\t\t\t</Cells>\n";
    sw.file << "\t\t</Piece>\n";
    sw.file << "\t</UnstructuredGrid>\n";
    sw.file << "</VTKFile>\n";
    sw.file.close();
}