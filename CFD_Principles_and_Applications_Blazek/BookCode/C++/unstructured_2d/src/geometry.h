/// @file geometry.h
///
/// Definition of the class related to grid geometry and to metrics.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 27, 2014
// 
//=============================================================================
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//
//*****************************************************************************

#ifndef GEOMETRY_H_INCLUDED
#define GEOMETRY_H_INCLUDED

#include <string>
#include "defs.h"

/// @class Geometry
/// Encompasses variables and functions related to the geometrical data
/// including the topology of the boundaries. It also contains edge list,
/// control volumes and face vectors. The class takes care of input,
/// initialization and computation of geometrical data.
///
class Geometry
{
public:

  int nNodes;   /**< total number of grid nodes (including dummy nodes).\n */
                /**< Dummy nodes are defined for inlet, outlet and far-field boundaries only. */
                /**< The variables cv[], dv[], coords[], edge[], vol[] and sij[] are divided into */
                /**< two parts. The first part is related to the physical grid (dimensions nndInt, */
                /**< nedInt). The second part refers to the dummy nodes. This approach makes it */
                /**< easier to loop either only over the physical grid nodes (edges), or over all */
                /**< nodes (edges)  using the same vector. */
  int nndInt,   /**< number of physical grid nodes (nNodes - dummy nodes) */
      nEdges,   /**< total number of edges (including edges between boundary and dummy nodes) */
      nedInt,   /**< number of edges excluding those to dummy nodes */
      nTria,    /**< number of triangles */
      nSegs,    /**< number of boundary segments (composed of boundary faces) */
      nBfaces,  /**< number of all boundary faces */
      nBnodes;  /**< total number of boundary nodes */

  int *btype;   /**< types of boundary conditions:\n */
                /**< 100-199 = inflow\n */
                /**< 200-299 = outflow\n */
                /**< 300-399 = viscous wall\n */
                /**< 400-499 = inviscid wall\n */
                /**< 500-599 = symmetry line\n */
                /**< 600-699 = far-field\n */
                /**< 700-799 = periodic boundary */

  /// Nodes of a triangle
  typedef struct T_TRIA
  {
    int node[3];
  } TRIA;
  TRIA *tria;  /**< node indexes of triangle elements */

  /// Nodes of an edge
  typedef struct T_EDGE
  {
    int i, j;
  } EDGE;
  EDGE *edge;  /**< edge list (node i, node j). For ie > nedInt, edge[ie] represents */
               /**< the edge from a boundary node (i) to a dummy node (used at inlet, */
               /**< outlet and far-field boundaries). */

  /// Nodes defining a face
  typedef struct T_BFACE
  {
    int node1,  /**< first node */
        node2;  /**< second node */
  } BFACE;
  BFACE *bface; /**< indexes of two nodes defining boundary faces (NOT for periodic boundaries!) */

  /// Data related to boundary nodes
  typedef struct T_BNODE
  {
    int node,   /**< index of the node itself */
        dummy,  /**< index of the related dummy node (inlet, outlet, far-field) */
                /**< or of the other periodic node; otherwise = -777 */
        edge;   /**< index of edge to the dummy node (edge[ie], ie > nedInt) */
  } BNODE;
  BNODE *bnode; /**< pointers from boundary nodes to related data */

  /// Pointer from boundary segment to its faces and nodes
  typedef struct T_IBOUND
  {
    int bfaceIndex,  /**< last index in bface[] */
        bnodeIndex;  /**< last index in bnode[] */
  } IBOUND;
  IBOUND *ibound;    /**< pointers from boundary segments to boundary faces and nodes */

  REAL xref,  /**< x-coordinate of the reference point */
       yref,  /**< y-coordinate of the reference point */
       cref;  /**< reference length or airfoil chord */

  NODE *coords;  /**< x- and y-coordinates */
  NODE *sij;     /**< x,y-components of the face vector (n*dS). */
                 /**< For ie > nedInt, sij[ie] represents the average face vector */
                 /**< at a boundary node (face between boundary and dummy node); */
                 /**< sij always points from node i to node j (see Fig. 5.9). */
  REAL *vol;     /**< median-dual control volume (shaded area in Fig. 5.8) */
  NODE *sbf;     /**< normal vector of boundary face (outward pointing, size equal to the edge */
                 /**< length); defined for all boundaries except for periodic ones. */
  NODE *sproj;   /**< projections of control volumes on the x- and y-axis */
                 /**< (used to compute the time step - see Eq. (6.22)). */

  std::string fnameGrid;  /**< file with grid and topology data */
  std::string *bname;     /**< names of boundary segments (used for plots) */

  // functions

  Geometry();
  ~Geometry();
  void ComputeMetrics();
  void GenerateEdgelist();
  int  GetNumberBoundNodes( int btypeFrom, int btypeTo ) const;
  void ReadGrid();

private:

  /// Edges sharing given node i
  struct EDGEI
  {
    int  j,       /**< node j of edge i->j */
         edge;    /**< pointer to the final edge list (Geometry::edge); it is used in */
                  /**< ComputeMetrics to associate face vector sij() with the correct edge */
    EDGEI *next;  /**< next edge to node i */
  };

  /// Pointer from node i to all nodes j connected to by an edge
  typedef struct T_EDGELIST
  {
    EDGEI *list;
  } EDGELIST;
  EDGELIST *tmpElist;  /**< temporary edge list (DO NOT overwrite */
                       /**< between GenerateEdges and ComputeMetrics) */

  // functions

  Geometry( const Geometry &geometry );             // override default copy constructor
  Geometry & operator = (const Geometry &geometry); // and assignment operator
  void CheckMetrics();
  void DeleteTmpElist();
  void DummyNodes();
  void FaceVectorsSymm();
  void FaceVectorsVolumes();
  void FaceVectorsVolumesBound();
  void VolumeProjections();
};

#endif // GEOMETRY_H_INCLUDED