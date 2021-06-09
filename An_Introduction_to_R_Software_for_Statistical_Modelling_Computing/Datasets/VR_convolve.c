void VR_convolve(double *a, long *na, 
		 double *b, long *nb, 
		 double *ab) 
{
  int i, j, nab = *na + *nb - 1;

  for(i = 0; i < nab; i++)
    ab[i] = 0.0;
  for(i = 0; i < *na; i++) 
    for(j = 0; j < *nb; j++)
      ab[i + j] += a[i] * b[j];
}



