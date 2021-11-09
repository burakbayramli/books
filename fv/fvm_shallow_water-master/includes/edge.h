#ifndef _edge_h
#define _edge_h

#include <Eigen/Dense>

// Edge class

class edge
{
public:
    int vertex1;
    int vertex2;
    int celll;
    int cellr;
    double l;
    Eigen::Vector3d n;
    Eigen::Vector3d r;
    Eigen::Vector3d Ql;
    Eigen::Vector3d Qr;
    Eigen::Vector3d Qf;
    edge(int, int, int, int);
    ~edge(void);
};
#endif
