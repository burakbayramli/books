/// @file deleteTmpElist.cpp
///
/// Deletion of temporary edge list.
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

#include "geometry.h"

/// Deletes the temporary edge list Geometry::tmpElist and all its linked entries.
///
void Geometry::DeleteTmpElist()
{
  int   i;
  EDGEI *point, *prev;

  // check if temp. edge list allocated

  if (tmpElist == nullptr) return;

  // clean the list

  for (i=0; i<nndInt; i++)
  {
    point = tmpElist[i].list;
    if (point != nullptr)
    {
      do
      {
        prev  = point;
        point = point->next;
        delete prev;
      } while (point != nullptr);
    }
  }
  delete[] tmpElist;
  tmpElist = nullptr;
}