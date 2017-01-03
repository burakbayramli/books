#define DDx(u, i, j, dx) \
  (u(i+1,j) - 2*u(i,j) + u(i-1,j))/(dx*dx)
#define DDy(u, i, j, dy) \
  (u(i,j+1) - 2*u(i,j) + u(i,j-1))/(dy*dy)
