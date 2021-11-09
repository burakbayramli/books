#ifndef _cell_h
#define _cell_h

#include <vector>
#include <Eigen/Dense>

// Cell class

class cell
{

public:
  int vertex1;
  int vertex2;
  int vertex3;
  double S;
  Eigen::Vector3d n;
  int type;
  Eigen::Vector3d Q;
  Eigen::Vector3d R;
  Eigen::Vector3d r;
  cell(int, int, int);
  ~cell(void);
};

#endif
