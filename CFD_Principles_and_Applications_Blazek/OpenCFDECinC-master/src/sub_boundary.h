//modules for Boundary layer condition

#pragma once
#ifndef _SUB_BOUNDARY_H_
#define _SUB_BOUNDARY_H_

void Update_coordinate_buffer();
void Update_coordinate_buffer_onemesh(int nMesh);

void Boundary_condition_onemesh(int Num_Mesh);
void update_buffer_onemesh(int nMesh);



#endif // !_SUB_BOUNDARY_H_
