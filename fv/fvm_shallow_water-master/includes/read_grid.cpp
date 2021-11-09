#include "sw.h"

//Read grid
void read_grid(sw &sw)
{
    // Read stl and remove duplicate vertices
    ////////////////////////////////////////////////////////////////////

    std::string file_line;
    std::smatch file_line_match;
    std::regex file_line_regex;

    file_line_regex.assign("\\s+vertex\\s(.+)\\s(.+)\\s(.+)");

    Eigen::Vector3d r1, r2, r3;

    std::vector<int> vertices;
    Eigen::Vector3d vertex;
    Eigen::Vector3d face;

    sw.file.open(sw.case_name + ".stl", std::fstream::in);

    int j = 0;
    if (sw.file.is_open())
    {

        while (std::getline(sw.file, file_line))
        {
            file_line_match.empty();

            if (std::regex_search(file_line, file_line_match, file_line_regex))
            {

                vertex(0) = std::stod(file_line_match[1]);
                vertex(1) = std::stod(file_line_match[2]);
                vertex(2) = std::stod(file_line_match[3]);

                bool vertex_exists = false;
                int vertex_i;
                for (int i = 0; i < sw.vertices.size(); i++)
                {
                    if ((sw.vertices[i] - vertex).norm() < 1e-6)
                    {
                        vertices.push_back(i);
                        vertex_exists = true;
                        break;
                    }
                }

                if (!vertex_exists)
                {
                    sw.vertices.push_back(vertex);
                    vertices.push_back(sw.vertices.size() - 1);
                }

                if (vertices.size() == 3) // If we have 3 vertex indices create cell
                {
                    cell cell(vertices[0], vertices[1], vertices[2]);
                    sw.cells.push_back(cell);
                    vertices.clear();
                }
            }
        }

        sw.file.close();

        sw.N_vertices = sw.vertices.size();
        sw.N_cells = sw.cells.size();

        // Remove duplicate edges
        ////////////////////////////////////////////////////////////////////

        //Create edges matrix
        Eigen::MatrixXd edges;
        edges = Eigen::MatrixXd::Zero(4, 3 * sw.N_cells);
        for (int i = 0; i < sw.N_cells; i++)
        {
            edges(0, 3 * (i + 1) - 3) = sw.cells[i].vertex1;
            edges(1, 3 * (i + 1) - 3) = sw.cells[i].vertex2;
            edges(2, 3 * (i + 1) - 3) = i;
            edges(3, 3 * (i + 1) - 3) = 0;

            edges(0, 3 * (i + 1) - 2) = sw.cells[i].vertex2;
            edges(1, 3 * (i + 1) - 2) = sw.cells[i].vertex3;
            edges(2, 3 * (i + 1) - 2) = i;
            edges(3, 3 * (i + 1) - 2) = 0;

            edges(0, 3 * (i + 1) - 1) = sw.cells[i].vertex3;
            edges(1, 3 * (i + 1) - 1) = sw.cells[i].vertex1;
            edges(2, 3 * (i + 1) - 1) = i;
            edges(3, 3 * (i + 1) - 1) = 0;
        }

        //Find duplicate edges in the edges matrix
        for (int i = 0; i < 3 * sw.N_cells; i++)
        {
            if (edges(3, i) == 0)
            {
                int vertex1l, vertex2l, celll, cellr;

                vertex1l = edges(0, i); //Left cell vertex1
                vertex2l = edges(1, i); //Left cell vertex2

                celll = edges(2, i); //Left cell index
                cellr = -1;          // Right cell index (-1) if cell is a boundary cell

                for (int j = 0; j < 3 * sw.N_cells; j++)
                {
                    int vertex1r, vertex2r;

                    vertex1r = edges(0, j); //Right cell vertex1
                    vertex2r = edges(1, j); //Right cell vertex2

                    if (vertex1l == vertex2r && vertex2l == vertex1r)
                    {
                        edges(3, j) = 1;     //Mark edge as processed
                        cellr = edges(2, j); //Set right cell index
                        break;
                    }
                }

                edges(3, i) = 1; //Mark edge as processed

                //Add edge
                edge edge(vertex1l, vertex2l, celll, cellr);
                sw.edges.push_back(edge);
            }
        }

        sw.N_edges = sw.edges.size();

        // Calculate cell normals, centroids, and area
        ////////////////////////////////////////////////////////////////////

        for (int i = 0; i < sw.N_cells; i++)
        {
            Eigen::Vector3d r1, r2, r3, d1, d2, n;

            r1 = sw.vertices[sw.cells[i].vertex1];
            r2 = sw.vertices[sw.cells[i].vertex2];
            r3 = sw.vertices[sw.cells[i].vertex3];
            d1 = r2 - r1;
            d2 = r3 - r1;
            n = d1.cross(d2);
            sw.cells[i].n = n / n.norm();
            sw.cells[i].S = 0.5 * n.norm();
            sw.cells[i].r = 1.0 / 3.0 * (r1 + r2 + r3);
        }

        // Calculate edge normals, centroids, and length
        ////////////////////////////////////////////////////////////////////

        for (int i = 0; i < sw.N_edges; i++)
        {
            Eigen::Vector3d r1, r2, d, n;

            r1 = sw.vertices[sw.edges[i].vertex1];
            r2 = sw.vertices[sw.edges[i].vertex2];
            d = r2 - r1;
            n = d.cross(sw.cells[sw.edges[i].celll].n);
            sw.edges[i].n = n / n.norm();
            sw.edges[i].l = d.norm();
            sw.edges[i].r = 0.5 * (r1 + r2);

            if (sw.edges[i].cellr == -1) //If there is no right cell attached to the edge, edge is at the boundary
            {
                sw.cells[sw.edges[i].celll].type = 1;
            }
        }
    }
    else
    {
        std::cout << "Failed reading grid file!" << std::endl;
        std::exit(-1);
    }
}
