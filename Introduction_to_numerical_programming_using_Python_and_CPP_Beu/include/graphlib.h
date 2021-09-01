//------------------------------ graphlib.h ---------------------------------
// Contains C++ interface classes for the graphics library graphlib.py.
// Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _GRAPHLIB_
#define _GRAPHLIB_

#include <iostream>
#include <Python.h>

//===========================================================================
class Python                      // wrapper class for the Python interpreter
{
   public:
   Python(int argc, wchar_t** argv)         // constructor: initialize Python
   {
      Py_Initialize();    
      PySys_SetArgv(argc, argv);
   }

   ~Python() { Py_Finalize(); }                  // destructor: finish Python
};

//===========================================================================
class PyGraph                       // wrapper class for calls to graphlib.py
{
   Python* contxt;
   PyObject* pModule;

   public:
   PyGraph(int argc, wchar_t** argv)       // constructor: import graphlib.py
   {
      contxt = new Python(argc, argv);
      pModule = PyImport_Import(PyUnicode_FromString("graphlib"));
      if (!pModule) std::cout << "Failed to load <graphlib.py>";
   }
   virtual ~PyGraph()                      // destructor: release graphlib.py
   {
      if (pModule) Py_DECREF(pModule);
      if (contxt) delete contxt;
   }

//===========================================================================
   void MainLoop()                                   // creates Tk event loop
   {
      PyObject *pFunc, *pArgs;

      pFunc = PyObject_GetAttrString(pModule,"MainLoop");
      pArgs = PyTuple_New(0);
      Py_XINCREF(pArgs);

      PyObject_CallObject(pFunc, pArgs);

      Py_XDECREF(pArgs);
      Py_DECREF(pFunc);  
   }

//===========================================================================
   void GraphUpdate()                               // updates Tk root widget
   {
      PyObject *pFunc, *pArgs;

      pFunc = PyObject_GetAttrString(pModule,"GraphUpdate");
      pArgs = PyTuple_New(0);
      Py_XINCREF(pArgs);

      PyObject_CallObject(pFunc, pArgs);

      Py_XDECREF(pArgs);
      Py_DECREF(pFunc);  
   }

//===========================================================================
   void GraphInit(int nxwin, int nywin)              // creates Canvas widget
   {
      PyObject *pFunc, *pArgs;

      pFunc = PyObject_GetAttrString(pModule,"GraphInit");

      pArgs = PyTuple_New(2);
      PyTuple_SetItem(pArgs, 0, PyLong_FromLong(nxwin));
      PyTuple_SetItem(pArgs, 1, PyLong_FromLong(nywin));
      Py_XINCREF(pArgs);

      PyObject_CallObject(pFunc, pArgs);

      Py_XDECREF(pArgs);
      Py_DECREF(pFunc);  
   }

//===========================================================================
   void GraphClear()                      // deletes content of Canvas widget
   {
      PyObject *pFunc, *pArgs;

      pFunc = PyObject_GetAttrString(pModule,"GraphClear");
      pArgs = PyTuple_New(0);
      Py_XINCREF(pArgs);

      PyObject_CallObject(pFunc, pArgs);

      Py_XDECREF(pArgs);
      Py_DECREF(pFunc);  
   }

//===========================================================================
   void Plot(double x[], double y[], int n, const char *col, int sty,
             double fxmin, double fxmax, double fymin, double fymax,
             const char *xtext, const char *ytext, const char *title)
//---------------------------------------------------------------------------
// Plots a real function of one variable specified by a set of (x,y) points.
// The x and y-domains are extended to fit at most 10 intervals expressible
// as d * 10^p, with d = 1, 2, 5 and p integer.
//
// x[]   - abscissas of tabulation points (x[1] through x[n])
// y[]   - ordinates of tabulation points (y[1] through y[n])
// n     - number of tabulation points
// col   - plot color ("red", "green", "blue" etc.)
// sty   - plot style: 0 - scatter plot, 1 - line plot, 2 - polar plot,
//                     3 - drop lines, 4 - histogram
// fxmin - min fractional x-limit of viewport (0 < fxmin < fxmax < 1)
// fxmax - max fractional x-limit of viewport
// fymin - min fractional y-limit of viewport (0 < fymin < fymax < 1)
// fymax - max fractional y-limit of viewport
// xtext - x-axis title; for "None" - axis is not labeled
// ytext - y-axis title; for "None" - axis is not labeled
// title - plot title
//---------------------------------------------------------------------------
   {
      int i;
      PyObject *pFunc, *pArgs, *px, *py;

      pFunc = PyObject_GetAttrString(pModule,"Plot");

      px = PyList_New(n+1);                                // 0-offset arrays
      py = PyList_New(n+1);
      for (i=1; i<=n; i++) {
         PyList_SetItem(px, i, PyFloat_FromDouble(x[i]));
         PyList_SetItem(py, i, PyFloat_FromDouble(y[i]));
      }

      pArgs = PyTuple_New(12);
      PyTuple_SetItem(pArgs, 0, px);
      PyTuple_SetItem(pArgs, 1, py);
      PyTuple_SetItem(pArgs, 2, PyLong_FromLong(n));
      PyTuple_SetItem(pArgs, 3, PyUnicode_FromString(col));
      PyTuple_SetItem(pArgs, 4, PyLong_FromLong(sty));
      PyTuple_SetItem(pArgs, 5, PyFloat_FromDouble(fxmin));
      PyTuple_SetItem(pArgs, 6, PyFloat_FromDouble(fxmax));
      PyTuple_SetItem(pArgs, 7, PyFloat_FromDouble(fymin));
      PyTuple_SetItem(pArgs, 8, PyFloat_FromDouble(fymax));
      PyTuple_SetItem(pArgs, 9, PyUnicode_FromString(xtext));
      PyTuple_SetItem(pArgs,10, PyUnicode_FromString(ytext));
      PyTuple_SetItem(pArgs,11, PyUnicode_FromString(title));
      Py_XINCREF(pArgs);

      PyObject_CallObject(pFunc, pArgs);

      Py_XDECREF(pArgs);
      Py_DECREF(py);
      Py_DECREF(px);
      Py_DECREF(pFunc);
   }

//===========================================================================
   void MultiPlot(double x[], double y[], double sig[], int n[],
                  const char *col[], int sty[], int nplot, int maxint,
                  double xminp, double xmaxp, int ioptx,
                  double yminp, double ymaxp, int iopty,
                  double fxmin, double fxmax, double fymin, double fymax,
                  const char *xtext, const char *ytext, const char *title)
//---------------------------------------------------------------------------
// Plots nplot real functions of one variable given by sets of (x,y) points.
// The coordinate sets are stored contiguously in the arrays x[] and y[].
// The x and y-domains are extended to fit at most maxint intervals
// expressible as d * 10^p, with d = 1, 2, 5 and p integer.
//
// x[]    - abscissas of tabulation points for all functions
// y[]    - ordinates of tabulation points
// sig[]  - error bars of the tabulation points (useful for sty == 4)
// n[]    - ending index for the individual plots (nmax = n[nplot])
// col[]  - plot color ("red", "green", "blue" etc.)
// sty[]  - plot style 0 - scatter plot with squares
//                     1 - line plot;  -1 - dashed line
//                     2 - polar plot; -2 - dashed line
//                     3 - drop lines
//                     4 - error bars; -4 - including line plot
// nplot  - number of plots
// maxint - max. number of labeling intervals
// ioptx  - 0 - resize x-axis automatically
//          1 - resize x-axis based on user interval [xminp,xmaxp]
// iopty  - 0 - resize y-axis automatically
//          1 - resize y-axis based on user interval [yminp,ymaxp]
// fxmin  - min fractional x-limit of viewport (0 < fxmin < fxmax < 1)
// fxmax  - max fractional x-limit of viewport
// fymin  - min fractional y-limit of viewport (0 < fymin < fymax < 1)
// fymax  - max fractional y-limit of viewport
// xtext  - x-axis title; for "None" - axis is not labeled
// ytext  - y-axis title; for "None" - axis is not labeled
// title  - plot title
//---------------------------------------------------------------------------
   {
      int i;
      PyObject *pFunc, *pArgs, *px, *py, *ps, *pn, *pcolor, *pstyle;

      pFunc = PyObject_GetAttrString(pModule,"MultiPlot");

      px = PyList_New(n[nplot]+1);                         // 0-offset arrays
      py = PyList_New(n[nplot]+1);
      ps = PyList_New(n[nplot]+1);
      for (i=1; i<=n[nplot]; i++) {
         PyList_SetItem(px, i, PyFloat_FromDouble(x[i]));
         PyList_SetItem(py, i, PyFloat_FromDouble(y[i]));
         PyList_SetItem(ps, i, PyFloat_FromDouble(sig[i]));
      }

      pn     = PyList_New(nplot+1);                        // 0-offset arrays
      pcolor = PyList_New(nplot+1);
      pstyle = PyList_New(nplot+1);
      for (i=1; i<=nplot; i++) {
         PyList_SetItem(pn    , i, PyLong_FromLong(n[i]));
         PyList_SetItem(pcolor, i, PyUnicode_FromString(col[i]));
         PyList_SetItem(pstyle, i, PyLong_FromLong(sty[i]));
      }

      pArgs = PyTuple_New(21);
      PyTuple_SetItem(pArgs, 0, px);
      PyTuple_SetItem(pArgs, 1, py);
      PyTuple_SetItem(pArgs, 2, ps);
      PyTuple_SetItem(pArgs, 3, pn);
      PyTuple_SetItem(pArgs, 4, pcolor);
      PyTuple_SetItem(pArgs, 5, pstyle);
      PyTuple_SetItem(pArgs, 6, PyLong_FromLong(nplot));
      PyTuple_SetItem(pArgs, 7, PyLong_FromLong(maxint));
      PyTuple_SetItem(pArgs, 8, PyFloat_FromDouble(xminp));
      PyTuple_SetItem(pArgs, 9, PyFloat_FromDouble(xmaxp));
      PyTuple_SetItem(pArgs,10, PyLong_FromLong(ioptx));
      PyTuple_SetItem(pArgs,11, PyFloat_FromDouble(yminp));
      PyTuple_SetItem(pArgs,12, PyFloat_FromDouble(ymaxp));
      PyTuple_SetItem(pArgs,13, PyLong_FromLong(iopty));
      PyTuple_SetItem(pArgs,14, PyFloat_FromDouble(fxmin));
      PyTuple_SetItem(pArgs,15, PyFloat_FromDouble(fxmax));
      PyTuple_SetItem(pArgs,16, PyFloat_FromDouble(fymin));
      PyTuple_SetItem(pArgs,17, PyFloat_FromDouble(fymax));
      PyTuple_SetItem(pArgs,18, PyUnicode_FromString(xtext));
      PyTuple_SetItem(pArgs,19, PyUnicode_FromString(ytext));
      PyTuple_SetItem(pArgs,20, PyUnicode_FromString(title));
      Py_XINCREF(pArgs);

      PyObject_CallObject(pFunc, pArgs);

      Py_XDECREF(pArgs);
      Py_DECREF(ps);
      Py_DECREF(py);
      Py_DECREF(px);
      Py_DECREF(pFunc);
   }

//===========================================================================
   void Contour(double **z, int nx, int ny,
                double xmin, double xmax, double ymin, double ymax,
                double zmin, double zmax,
                double fxmin, double fxmax, double fymin, double fymax,
                const char *xtext, const char *ytext, const char *title)
//---------------------------------------------------------------------------
// Plots a function z(x,y) defined in [xmin,xmax] x [ymin,ymax] and tabulated
// on a regular Cartesian grid with (nx-1)x(ny-1) mesh cells as contour plot.
// The level curves result by inverse linear interpolation inside the cells.
// 
// z     - tabulated function values
// nx    - number of x-mesh points
// ny    - number of y-mesh points
// zmin  - minimum level considered
// zmin  - maximum level considered
// fxmin - minimum relative viewport abscissa (0 < fxmin < fxmax < 1)
// fxmax - maximum relative viewport abscissa
// fymin - minimum relative viewport ordinate (0 < fymin < fymax < 1)
// fymax - maximum relative viewport ordinate
// xtext - x-axis title; xtext = "" - axis is not labeled
// ytext - y-axis title; ytext = "" - axis is not labeled
// title - plot title
//---------------------------------------------------------------------------
   {
      int i, j;
      PyObject *pFunc, *pArgs, *pz, *pz1;

      pFunc = PyObject_GetAttrString(pModule,"Contour");

      pz = PyList_New(nx+1);                               // 0-offset arrays
      for (i=1; i<=nx; i++) {
         pz1 = PyList_New(ny+1);
         for (j=1; j<=ny; j++)
            PyList_SetItem(pz1, j, PyFloat_FromDouble(z[i][j]));
         PyList_SetItem(pz, i, pz1);
      }

      pArgs = PyTuple_New(16);
      PyTuple_SetItem(pArgs, 0, pz);
      PyTuple_SetItem(pArgs, 1, PyLong_FromLong(nx));
      PyTuple_SetItem(pArgs, 2, PyLong_FromLong(ny));
      PyTuple_SetItem(pArgs, 3, PyFloat_FromDouble(xmin));
      PyTuple_SetItem(pArgs, 4, PyFloat_FromDouble(xmax));
      PyTuple_SetItem(pArgs, 5, PyFloat_FromDouble(ymin));
      PyTuple_SetItem(pArgs, 6, PyFloat_FromDouble(ymax));
      PyTuple_SetItem(pArgs, 7, PyFloat_FromDouble(zmin));
      PyTuple_SetItem(pArgs, 8, PyFloat_FromDouble(zmax));
      PyTuple_SetItem(pArgs, 9, PyFloat_FromDouble(fxmin));
      PyTuple_SetItem(pArgs,10, PyFloat_FromDouble(fxmax));
      PyTuple_SetItem(pArgs,11, PyFloat_FromDouble(fymin));
      PyTuple_SetItem(pArgs,12, PyFloat_FromDouble(fymax));
      PyTuple_SetItem(pArgs,13, PyUnicode_FromString(xtext));
      PyTuple_SetItem(pArgs,14, PyUnicode_FromString(ytext));
      PyTuple_SetItem(pArgs,15, PyUnicode_FromString(title));
      Py_XINCREF(pArgs);

      PyObject_CallObject(pFunc, pArgs);

      Py_XDECREF(pArgs);
      Py_XDECREF(pz1);
      Py_XDECREF(pz);
      Py_XDECREF(pFunc);
   }

//===========================================================================
   void PlotParticles(double x[], double y[], double z[], double rad[],
                      char *col[], int n, double dmax,
                      double xminp, double xmaxp, int ioptx,
                      double yminp, double ymaxp, int iopty,
                      double fxmin, double fxmax, double fymin, double fymax,
                      const char *title)
//---------------------------------------------------------------------------
// Plots a system of particles as connected colored spheres
//
// x,y,z[] - coordinates of particles
// r[]     - radii of particles
// col[]   - colors of particles ("red", "green", "blue" etc.)
// n       - number of particles
// dmax    - max inter-distance for which particles are connected
// ioptx   - 0 - resize x-axis automatically
//           1 - resize x-axis to provided user interval (xminp,xmaxp)
// iopty   - 0 - resize y-axis automatically
//           1 - resize y-axis to provided user interval (yminp,ymaxp)
// fxmin   - min fractional x-limit of viewport (0 < fxmin < fxmax < 1)
// fxmax   - max fractional x-limit of viewport
// fymin   - min fractional y-limit of viewport (0 < fymin < fymax < 1)
// fymax   - max fractional y-limit of viewport
// title   - plot title
//---------------------------------------------------------------------------
   {
      int i;
      PyObject *pFunc, *pArgs, *px, *py, *pz, *pr, *pc;

      pFunc = PyObject_GetAttrString(pModule,"PlotParticles");

      px = PyList_New(n+1);                                // 0-offset arrays
      py = PyList_New(n+1);
      pz = PyList_New(n+1);
      pr = PyList_New(n+1);
      pc = PyList_New(n+1);
      for (i=0; i<=n; i++) {
         PyList_SetItem(px, i, PyFloat_FromDouble(x[i]));
         PyList_SetItem(py, i, PyFloat_FromDouble(y[i]));
         PyList_SetItem(pz, i, PyFloat_FromDouble(z[i]));
         PyList_SetItem(pr, i, PyFloat_FromDouble(rad[i]));
         PyList_SetItem(pc, i, PyUnicode_FromString(col[i]));
      }

      pArgs = PyTuple_New(18);
      PyTuple_SetItem(pArgs, 0, px);
      PyTuple_SetItem(pArgs, 1, py);
      PyTuple_SetItem(pArgs, 2, pz);
      PyTuple_SetItem(pArgs, 3, pr);
      PyTuple_SetItem(pArgs, 4, pc);
      PyTuple_SetItem(pArgs, 5, PyLong_FromLong(n));
      PyTuple_SetItem(pArgs, 6, PyFloat_FromDouble(dmax));
      PyTuple_SetItem(pArgs, 7, PyFloat_FromDouble(xminp));
      PyTuple_SetItem(pArgs, 8, PyFloat_FromDouble(xmaxp));
      PyTuple_SetItem(pArgs, 9, PyLong_FromLong(ioptx));
      PyTuple_SetItem(pArgs,10, PyFloat_FromDouble(yminp));
      PyTuple_SetItem(pArgs,11, PyFloat_FromDouble(ymaxp));
      PyTuple_SetItem(pArgs,12, PyLong_FromLong(iopty));
      PyTuple_SetItem(pArgs,13, PyFloat_FromDouble(fxmin));
      PyTuple_SetItem(pArgs,14, PyFloat_FromDouble(fxmax));
      PyTuple_SetItem(pArgs,15, PyFloat_FromDouble(fymin));
      PyTuple_SetItem(pArgs,16, PyFloat_FromDouble(fymax));
      PyTuple_SetItem(pArgs,17, PyUnicode_FromString(title));
      Py_XINCREF(pArgs);

      PyObject_CallObject(pFunc, pArgs);

      Py_XDECREF(pArgs);
      Py_DECREF(pFunc);
   }

//===========================================================================
   void PlotStruct(double x[], double y[], double z[], int n,
                   int ind1[], int ind2[], int ind3[], int n3,
                   double xminp, double xmaxp, int ioptx,
                   double yminp, double ymaxp, int iopty,
                   double fxmin, double fxmax, double fymin, double fymax,
                   const char *title)
//---------------------------------------------------------------------------
// Renders a 3D structure defined by nodes and triangular surfaces
//
// x,y,z[] - coordinates of nodes
// n       - number of nodes
// ind1[]  - index of 1st node of each triangle
// ind2[]  - index of 2nd node of each triangle
// ind3[]  - index of 3rd node of each triangle
// n3      - number of triangles
// ioptx   - 0 - resize x-axis automatically
//           1 - resize x-axis to provided user interval (xminp,xmaxp)
// iopty   - 0 - resize y-axis automatically
//           1 - resize y-axis to provided user interval (yminp,ymaxp)
// fxmin   - min fractional x-limit of viewport (0 < fxmin < fxmax < 1)
// fxmax   - max fractional x-limit of viewport
// fymin   - min fractional y-limit of viewport (0 < fymin < fymax < 1)
// fymax   - max fractional y-limit of viewport
// title   - plot title
//---------------------------------------------------------------------------
   {
      int i;
      PyObject *pFunc, *pArgs, *px, *py, *pz, *p1, *p2, *p3;

      pFunc = PyObject_GetAttrString(pModule,"PlotStruct");

      px = PyList_New(n+1);                                // 0-offset arrays
      py = PyList_New(n+1);
      pz = PyList_New(n+1);
      for (i=0; i<=n; i++) {
         PyList_SetItem(px, i, PyFloat_FromDouble(x[i]));
         PyList_SetItem(py, i, PyFloat_FromDouble(y[i]));
         PyList_SetItem(pz, i, PyFloat_FromDouble(z[i]));
      }
      p1 = PyList_New(n3+1);
      p2 = PyList_New(n3+1);
      p3 = PyList_New(n3+1);
      for (i=0; i<=n3; i++) {
         PyList_SetItem(p1, i, PyLong_FromLong(ind1[i]));
         PyList_SetItem(p2, i, PyLong_FromLong(ind2[i]));
         PyList_SetItem(p3, i, PyLong_FromLong(ind3[i]));
      }

      pArgs = PyTuple_New(19);
      PyTuple_SetItem(pArgs, 0, px);
      PyTuple_SetItem(pArgs, 1, py);
      PyTuple_SetItem(pArgs, 2, pz);
      PyTuple_SetItem(pArgs, 3, PyLong_FromLong(n));
      PyTuple_SetItem(pArgs, 4, p1);
      PyTuple_SetItem(pArgs, 5, p2);
      PyTuple_SetItem(pArgs, 6, p3);
      PyTuple_SetItem(pArgs, 7, PyLong_FromLong(n3));
      PyTuple_SetItem(pArgs, 8, PyFloat_FromDouble(xminp));
      PyTuple_SetItem(pArgs, 9, PyFloat_FromDouble(xmaxp));
      PyTuple_SetItem(pArgs,10, PyLong_FromLong(ioptx));
      PyTuple_SetItem(pArgs,11, PyFloat_FromDouble(yminp));
      PyTuple_SetItem(pArgs,12, PyFloat_FromDouble(ymaxp));
      PyTuple_SetItem(pArgs,13, PyLong_FromLong(iopty));
      PyTuple_SetItem(pArgs,14, PyFloat_FromDouble(fxmin));
      PyTuple_SetItem(pArgs,15, PyFloat_FromDouble(fxmax));
      PyTuple_SetItem(pArgs,16, PyFloat_FromDouble(fymin));
      PyTuple_SetItem(pArgs,17, PyFloat_FromDouble(fymax));
      PyTuple_SetItem(pArgs,18, PyUnicode_FromString(title));
      Py_XINCREF(pArgs);

      PyObject_CallObject(pFunc, pArgs);

      Py_XDECREF(pArgs);
      Py_DECREF(pFunc);
   }
};

//===========================================================================
void HistoBin(double xnew, double a, double b,
              double x[], double y[], int n, int iopt)
//---------------------------------------------------------------------------
// Bins data for a histogram to be plotted by function Plot (with sty = 4)
//
// xnew - new value to be binned
// a, b - limits of total binning interval
// x[]  - bin boundaries (x[1] = a, x[n] = b)
// y[]  - frequency of values in the bins: y[i] in [x[i],x[i+1]), i = 1,n-1
// n    - number of bin boundaries
// iopt - option: 0 - zeros bins
//                1 - bins new value xnew
//                2 - normalizes histogram
//---------------------------------------------------------------------------
{
   static double h, s;
   int i;

   if (iopt == 0) {                                        // initialize bins
      h = (b-a)/(n-1);                                            // bin size
      for (i=1; i<=n; i++) {
         x[i] = a + (i-1)*h;                                // bin boundaries
         y[i] = 0.e0;
      }
   } else if (iopt == 1) {                                  // bin new values
      i = (int) ((xnew-a)/h) + 1;                                // bin index
      if ((i >= 1) && (i < n)) y[i] += 1;              // increment bin value
   } else if (iopt == 2) {                             // normalize histogram
      s = 0e0;
      for (i=1;i<=n-1;i++) s += y[i];
      for (i=1;i<=n-1;i++) y[i] /= s;
   }
}

#endif
