//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// stream.c
//

void stream( lattice_ptr lattice)
{
  double  *f;

  int     i, j, n, a;

  int     nx, ny, ip, in, jp, jn;

  int     subs;

#if SAY_HI
  printf("stream() -- Hi!\n");
#endif /* SAY_HI */

// substance 0

	subs = 0;

#if 0
  // Get pointer to distribution functions.
  f = lattice->pdf[subs]->f;

  // Traverse nodes.
  for( n=0; n<lattice->NumNodes; n++)
  {
    // Copy the rest particle f[0] to ftemp[0].
    *(f+9) = *f++;

#if 0
    for( a=1; a<=8; a++)
    {
      // S T R E A M
      //
      //  - f[a] streams to ftemp[a] of its neighboring node.
      //
      if( *nn != INACTIVE_NODE)
      {
        pdf[ *nn].ftemp[a] = *f;

//printf("STREAM: %d from %d to %d.\n", a, n, *nn);

      } /* if( *nn != INACTIVE_NODE)  */
      else
      {
//printf("STREAM: Skipping %d at %d.\n", a, n);
      }

      // Move nn to neighbor of next element of f.
      nn++;

      // Move next element of f.
      f++;

    } /* for( a=1; a<=8; a++) */
#else
    pdf[].ftemp[ 1] = *f++;
    pdf[].ftemp[ 2] = *f++;
    pdf[].ftemp[ 3] = *f++;
    pdf[].ftemp[ 4] = *f++;
    pdf[].ftemp[ 5] = *f++;
    pdf[].ftemp[ 6] = *f++;
    pdf[].ftemp[ 7] = *f++;
    pdf[].ftemp[ 8] = *f++;
#endif

    // Advance nn to neighbor of first element of next node.
    nn+=3;

    // Advance to the next node.
    f+=18;

  } /* for( n=0; n<NumNodes; n++) */
#else

  nx = lattice->param.LX;
  ny = lattice->param.LY;

#if 0
//-------------------------------------------------------------[ TEST ]---------
  f = lattice->pdf[subs]->ftemp;
  for( j=0; j<ny; j++)
  {
    for( i=0; i<nx; i++)
    {
      for( a=0; a<9; a++)
      {
        *f++ = 0.;
      }
      f+=18;
    }
  }
//-------------------------------------------------------------[ TEST ]---------
#endif

#if 1
	//if(subs == 0 || lattice->time > 30){
  process_send_recv_begin( lattice, subs);

  n = 0;

  for( j=0; j<ny; j++)
  {
    jp = ( j<ny-1)?( j+1):( 0   );
    jn = ( j>0   )?( j-1):( ny-1);

    for( i=0; i<nx; i++)
    {
      ip = ( i<nx-1)?( i+1):( 0   );
      in = ( i>0   )?( i-1):( nx-1);

      f = lattice->pdf[subs][n].f;
      n++;

      lattice->pdf[subs][ j *nx + i ].ftemp[ 0] = f[ 0];

      lattice->pdf[subs][ j *nx + ip].ftemp[ 1] = f[ 1];
      lattice->pdf[subs][ jp*nx + i ].ftemp[ 2] = f[ 2];
      lattice->pdf[subs][ j *nx + in].ftemp[ 3] = f[ 3];
      lattice->pdf[subs][ jn*nx + i ].ftemp[ 4] = f[ 4];

      lattice->pdf[subs][ jp*nx + ip].ftemp[ 5] = f[ 5];
      lattice->pdf[subs][ jp*nx + in].ftemp[ 6] = f[ 6];
      lattice->pdf[subs][ jn*nx + in].ftemp[ 7] = f[ 7];
      lattice->pdf[subs][ jn*nx + ip].ftemp[ 8] = f[ 8];

    } /* if( i=0; i<nx; i++, n++) */
  } /* if( j=0; j<lattice->param.LY; j++) */

  process_send_recv_end( lattice, subs);

#endif

#endif


// substance 1
#if NUM_FLUID_COMPONENTS == 2
	subs = 1;

#if 0
  // Get pointer to distribution functions.
  f = lattice->pdf[subs]->f;

  // Traverse nodes.
  for( n=0; n<lattice->NumNodes; n++)
  {
    // Copy the rest particle f[0] to ftemp[0].
    *(f+9) = *f++;

#if 0
    for( a=1; a<=8; a++)
    {
      // S T R E A M
      //
      //  - f[a] streams to ftemp[a] of its neighboring node.
      //
      if( *nn != INACTIVE_NODE)
      {
        pdf[ *nn].ftemp[a] = *f;

//printf("STREAM: %d from %d to %d.\n", a, n, *nn);

      } /* if( *nn != INACTIVE_NODE)  */
      else
      {
//printf("STREAM: Skipping %d at %d.\n", a, n);
      }

      // Move nn to neighbor of next element of f.
      nn++;

      // Move next element of f.
      f++;

    } /* for( a=1; a<=8; a++) */
#else
    pdf[].ftemp[ 1] = *f++;
    pdf[].ftemp[ 2] = *f++;
    pdf[].ftemp[ 3] = *f++;
    pdf[].ftemp[ 4] = *f++;
    pdf[].ftemp[ 5] = *f++;
    pdf[].ftemp[ 6] = *f++;
    pdf[].ftemp[ 7] = *f++;
    pdf[].ftemp[ 8] = *f++;
#endif

    // Advance nn to neighbor of first element of next node.
    nn+=3;

    // Advance to the next node.
    f+=18;

  } /* for( n=0; n<NumNodes; n++) */
#else

//  nx = lattice->param.LX;
//  ny = lattice->param.LY;

#if 0
//-------------------------------------------------------------[ TEST ]---------
  f = lattice->pdf[subs]->ftemp;
  for( j=0; j<ny; j++)
  {
    for( i=0; i<nx; i++)
    {
      for( a=0; a<9; a++)
      {
        *f++ = 0.;
      }
      f+=18;
    }
  }
//-------------------------------------------------------------[ TEST ]---------
#endif

#if SIGMA_BULK_FLAG
	if(lattice->time > lattice->param.sigma_bulk_on)
	{
#endif
	//if(subs == 0 || lattice->time > 30){
  process_send_recv_begin( lattice, subs);

  n = 0;

  for( j=0; j<ny; j++)
  {
    jp = ( j<ny-1)?( j+1):( 0   );
    jn = ( j>0   )?( j-1):( ny-1);

    for( i=0; i<nx; i++)
    {
      ip = ( i<nx-1)?( i+1):( 0   );
      in = ( i>0   )?( i-1):( nx-1);

      f = lattice->pdf[subs][n].f;
      n++;

      lattice->pdf[subs][ j *nx + i ].ftemp[ 0] = f[ 0];

      lattice->pdf[subs][ j *nx + ip].ftemp[ 1] = f[ 1];
      lattice->pdf[subs][ jp*nx + i ].ftemp[ 2] = f[ 2];
      lattice->pdf[subs][ j *nx + in].ftemp[ 3] = f[ 3];
      lattice->pdf[subs][ jn*nx + i ].ftemp[ 4] = f[ 4];

      lattice->pdf[subs][ jp*nx + ip].ftemp[ 5] = f[ 5];
      lattice->pdf[subs][ jp*nx + in].ftemp[ 6] = f[ 6];
      lattice->pdf[subs][ jn*nx + in].ftemp[ 7] = f[ 7];
      lattice->pdf[subs][ jn*nx + ip].ftemp[ 8] = f[ 8];

    } /* if( i=0; i<nx; i++, n++) */
  } /* if( j=0; j<lattice->param.LY; j++) */

  process_send_recv_end( lattice, subs);

#if SIGMA_BULK_FLAG
	}
#endif

#endif

#endif

#if SAY_HI
  printf("stream() -- Bye!\n");
#endif /* SAY_HI */

} /* void stream( lattice_ptr lattice) */
