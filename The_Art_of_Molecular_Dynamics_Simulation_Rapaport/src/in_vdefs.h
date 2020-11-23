
#ifndef V_DEFS

#define V_DEFS

#ifndef NDIM
#define NDIM  3
#endif

#define Sqr(x)     ((x) * (x))
#define Cube(x)    ((x) * (x) * (x))
#define Sgn(x, y)  (((y) >= 0) ? (x) : (- (x)))
#define IsEven(x)  ((x) & ~1)
#define IsOdd(x)   ((x) & 1)
#define Nint(x)                                             \
   (((x) < 0.) ? (- (int) (0.5 - (x))): ((int) (0.5 + (x))))
#define Min(x1, x2)  (((x1) < (x2)) ? (x1) : (x2))
#define Max(x1, x2)  (((x1) > (x2)) ? (x1) : (x2))
#define Min3(x1, x2, x3) \
   (((x1) < (x2)) ? (((x1) < (x3)) ? (x1) : (x3)) :         \
                    (((x2) < (x3)) ? (x2) : (x3)))
#define Max3(x1, x2, x3) \
   (((x1) > (x2)) ? (((x1) > (x3)) ? (x1) : (x3)) :         \
                    (((x2) > (x3)) ? (x2) : (x3)))
#define Clamp(x, lo, hi)                                    \
   (((x) >= (lo) && (x) <= (hi)) ? (x) :                    \
   (((x) < (lo)) ? (lo) : (hi)))

typedef struct {real x, y;} VecR2;
typedef struct {real x, y, z;} VecR3;
typedef struct {int x, y;} VecI2;
typedef struct {int x, y, z;} VecI3;

#if NDIM == 2

typedef VecR2 VecR;
typedef VecI2 VecI;

#define VSet(v, sx, sy)                                     \
   (v).x = sx,                                              \
   (v).y = sy
#define VCopy(v1, v2)                                       \
   (v1).x = (v2).x,                                         \
   (v1).y = (v2).y
#define VScale(v, s)                                        \
   (v).x *= s,                                              \
   (v).y *= s
#define VSCopy(v2, s1, v1)                                  \
   (v2).x = (s1) * (v1).x,                                  \
   (v2).y = (s1) * (v1).y
#define VAdd(v1, v2, v3)                                    \
   (v1).x = (v2).x + (v3).x,                                \
   (v1).y = (v2).y + (v3).y
#define VSub(v1, v2, v3)                                    \
   (v1).x = (v2).x - (v3).x,                                \
   (v1).y = (v2).y - (v3).y
#define VMul(v1, v2, v3)                                    \
   (v1).x = (v2).x * (v3).x,                                \
   (v1).y = (v2).y * (v3).y
#define VDiv(v1, v2, v3)                                    \
   (v1).x = (v2).x / (v3).x,                                \
   (v1).y = (v2).y / (v3).y
#define VSAdd(v1, v2, s3, v3)                               \
   (v1).x = (v2).x + (s3) * (v3).x,                         \
   (v1).y = (v2).y + (s3) * (v3).y
#define VSSAdd(v1, s2, v2, s3, v3)                          \
   (v1).x = (s2) * (v2).x + (s3) * (v3).x,                  \
   (v1).y = (s2) * (v2).y + (s3) * (v3).y
#define VDot(v1, v2)                                        \
   ((v1).x * (v2).x + (v1).y * (v2).y)
#define VWDot(v1, v2, v3)                                   \
   ((v1).x * (v2).x * (v3).x + (v1).y * (v2).y * (v3).y)
#define VCross(v1, v2)                                      \
   ((v1).x * (v2).y - (v1).y * (v2).x)
#define VProd(v)                                            \
   ((v).x * (v).y)
#define VGe(v1, v2)                                         \
   ((v1).x >= (v2).x && (v1).y >= (v2).y)
#define VLt(v1, v2)                                         \
   ((v1).x < (v2).x && (v1).y < (v2).y)
#define VLinear(p, s)                                       \
   ((p).y * (s).x + (p).x)
#define VSetAll(v, s)                                       \
   VSet (v, s, s)
#define VAddCon(v1, v2, s)                                  \
   (v1).x = (v2).x + (s),                                   \
   (v1).y = (v2).y + (s)
#define VComp(v, k)                                         \
   *((k == 0) ? &(v).x : &(v).y)
#define VToLin(a, n, v)                                     \
   a[(n) + 0] = (v).x,                                      \
   a[(n) + 1] = (v).y
#define VFromLin(v, a, n)                                   \
   VSet (v, a[(n) + 0], a[(n) + 1])
#define VCSum(v)                                            \
   ((v).x + (v).y)

#endif

#if NDIM == 3

typedef VecR3 VecR;
typedef VecI3 VecI;

#define VSet(v, sx, sy, sz)                                 \
   (v).x = sx,                                              \
   (v).y = sy,                                              \
   (v).z = sz
#define VCopy(v1, v2)                                       \
   (v1).x = (v2).x,                                         \
   (v1).y = (v2).y,                                         \
   (v1).z = (v2).z
#define VScale(v, s)                                        \
   (v).x *= s,                                              \
   (v).y *= s,                                              \
   (v).z *= s
#define VSCopy(v2, s1, v1)                                  \
   (v2).x = (s1) * (v1).x,                                  \
   (v2).y = (s1) * (v1).y,                                  \
   (v2).z = (s1) * (v1).z
#define VAdd(v1, v2, v3)                                    \
   (v1).x = (v2).x + (v3).x,                                \
   (v1).y = (v2).y + (v3).y,                                \
   (v1).z = (v2).z + (v3).z
#define VSub(v1, v2, v3)                                    \
   (v1).x = (v2).x - (v3).x,                                \
   (v1).y = (v2).y - (v3).y,                                \
   (v1).z = (v2).z - (v3).z
#define VMul(v1, v2, v3)                                    \
   (v1).x = (v2).x * (v3).x,                                \
   (v1).y = (v2).y * (v3).y,                                \
   (v1).z = (v2).z * (v3).z
#define VDiv(v1, v2, v3)                                    \
   (v1).x = (v2).x / (v3).x,                                \
   (v1).y = (v2).y / (v3).y,                                \
   (v1).z = (v2).z / (v3).z
#define VSAdd(v1, v2, s3, v3)                               \
   (v1).x = (v2).x + (s3) * (v3).x,                         \
   (v1).y = (v2).y + (s3) * (v3).y,                         \
   (v1).z = (v2).z + (s3) * (v3).z
#define VSSAdd(v1, s2, v2, s3, v3)                          \
   (v1).x = (s2) * (v2).x + (s3) * (v3).x,                  \
   (v1).y = (s2) * (v2).y + (s3) * (v3).y,                  \
   (v1).z = (s2) * (v2).z + (s3) * (v3).z
#define VDot(v1, v2)                                        \
   ((v1).x * (v2).x + (v1).y * (v2).y + (v1).z * (v2).z)
#define VWDot(v1, v2, v3)                                   \
   ((v1).x * (v2).x * (v3).x + (v1).y * (v2).y * (v3).y +   \
   (v1).z * (v2).z * (v3).z)
#define VCross(v1, v2, v3)                                  \
   (v1).x = (v2).y * (v3).z - (v2).z * (v3).y,              \
   (v1).y = (v2).z * (v3).x - (v2).x * (v3).z,              \
   (v1).z = (v2).x * (v3).y - (v2).y * (v3).x
#define MVMul(v1, m, v2)                                         \
   (v1).x = (m)[0] * (v2).x + (m)[3] * (v2).y + (m)[6] * (v2).z, \
   (v1).y = (m)[1] * (v2).x + (m)[4] * (v2).y + (m)[7] * (v2).z, \
   (v1).z = (m)[2] * (v2).x + (m)[5] * (v2).y + (m)[8] * (v2).z
#define MVMulT(v1, m, v2)                                        \
   (v1).x = (m)[0] * (v2).x + (m)[1] * (v2).y + (m)[2] * (v2).z, \
   (v1).y = (m)[3] * (v2).x + (m)[4] * (v2).y + (m)[5] * (v2).z, \
   (v1).z = (m)[6] * (v2).x + (m)[7] * (v2).y + (m)[8] * (v2).z
#define VProd(v)                                            \
   ((v).x * (v).y * (v).z)
#define VGe(v1, v2)                                         \
   ((v1).x >= (v2).x && (v1).y >= (v2).y && (v1).z >= (v2).z)
#define VLt(v1, v2)                                         \
   ((v1).x < (v2).x && (v1).y < (v2).y && (v1).z < (v2).z)
#define VLinear(p, s)                                       \
   (((p).z * (s).y + (p).y) * (s).x + (p).x)
#define VSetAll(v, s)                                       \
   VSet (v, s, s, s)
#define VAddCon(v1, v2, s)                                  \
   (v1).x = (v2).x + (s),                                   \
   (v1).y = (v2).y + (s),                                   \
   (v1).z = (v2).z + (s)
#define VComp(v, k)                                         \
   *((k == 0) ? &(v).x : ((k == 1) ? &(v).y : &(v).z))
#define VToLin(a, n, v)                                     \
   a[(n) + 0] = (v).x,                                      \
   a[(n) + 1] = (v).y,                                      \
   a[(n) + 2] = (v).z
#define VFromLin(v, a, n)                                   \
   VSet (v, a[(n) + 0], a[(n) + 1], a[(n) + 2])
#define VCSum(v)                                            \
   ((v).x + (v).y + (v).z)

#endif

#define VZero(v)  VSetAll (v, 0)
#define VLenSq(v)  VDot (v, v)
#define VWLenSq(v1, v2)  VWDot(v1, v2, v2)
#define VLen(v)  sqrt (VDot (v, v))
#define VVAdd(v1, v2)  VAdd (v1, v1, v2)
#define VVSub(v1, v2)  VSub (v1, v1, v2)
#define VVSAdd(v1, s2, v2) VSAdd (v1, v1, s2, v2)
#define VInterp(v1, s2, v2, v3)                             \
   VSSAdd (v1, s2, v2, 1. - (s2), v3)

typedef struct {
   real u1, u2, u3, u4;
} Quat;

#define QSet(q, s1, s2, s3, s4)                             \
   (q).u1 = s1,                                             \
   (q).u2 = s2,                                             \
   (q).u3 = s3,                                             \
   (q).u4 = s4
#define QZero(q)  QSet (q, 0, 0, 0, 0)
#define QScale(q, s)                                        \
   (q).u1 *= s,                                             \
   (q).u2 *= s,                                             \
   (q).u3 *= s,                                             \
   (q).u4 *= s
#define QSAdd(q1, q2, s3, q3)                               \
   (q1).u1 = (q2).u1 + (s3) * (q3).u1,                      \
   (q1).u2 = (q2).u2 + (s3) * (q3).u2,                      \
   (q1).u3 = (q2).u3 + (s3) * (q3).u3,                      \
   (q1).u4 = (q2).u4 + (s3) * (q3).u4
#define QLenSq(q)                                           \
   (Sqr ((q).u1) + Sqr ((q).u2) + Sqr ((q).u3) +            \
   Sqr ((q).u4))
#define QMul(q1, q2, q3)                                    \
   (q1).u1 =   (q2).u4 * (q3).u1 - (q2).u3 * (q3).u2 +      \
               (q2).u2 * (q3).u3 + (q2).u1 * (q3).u4,       \
   (q1).u2 =   (q2).u3 * (q3).u1 + (q2).u4 * (q3).u2 -      \
               (q2).u1 * (q3).u3 + (q2).u2 * (q3).u4,       \
   (q1).u3 = - (q2).u2 * (q3).u1 + (q2).u1 * (q3).u2 +      \
               (q2).u4 * (q3).u3 + (q2).u3 * (q3).u4,       \
   (q1).u4 = - (q2).u1 * (q3).u1 - (q2).u2 * (q3).u2 -      \
               (q2).u3 * (q3).u3 + (q2).u4 * (q3).u4

typedef struct {
  real R, I;
} Cmplx;

#define CSet(a, x, y)                                       \
   a.R = x,                                                 \
   a.I = y
#define CAdd(a, b, c)                                       \
   a.R = b.R + c.R,                                         \
   a.I = b.I + c.I
#define CSub(a, b, c)                                       \
   a.R = b.R - c.R,                                         \
   a.I = b.I - c.I
#define CMul(a, b, c)                                       \
  a.R = b.R * c.R - b.I * c.I,                              \
  a.I = b.R * c.I + b.I * c.R

#endif

