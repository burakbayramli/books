
#define IADD   453806245
#define IMUL   314159269
#define MASK   2147483647
#define SCALE  0.4656612873e-9

int randSeedP = 17;

void InitRand (int randSeedI)
{
  struct timeval tv;

  if (randSeedI != 0) randSeedP = randSeedI;
  else {
    gettimeofday (&tv, 0);
    randSeedP = tv.tv_usec;
  }
}

real RandR ()
{
  randSeedP = (randSeedP * IMUL + IADD) & MASK;
  return (randSeedP * SCALE);
}

#if NDIM == 2

void VRand (VecR *p)
{
  real s;

  s = 2. * M_PI * RandR ();
  p->x = cos (s);
  p->y = sin (s);
}

#elif NDIM == 3

void VRand (VecR *p)
{
  real s, x, y;

  s = 2.;
  while (s > 1.) {
    x = 2. * RandR () - 1.;
    y = 2. * RandR () - 1.;
    s = Sqr (x) + Sqr (y);
  }
  p->z = 1. - 2. * s;
  s = 2. * sqrt (1. - s);
  p->x = s * x;
  p->y = s * y;
}

#endif

