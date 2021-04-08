
typedef struct
{
  Window xwin;
  Display *xdisplay;
  XWindowAttributes xwa;
  Pixmap svimage;
  GC xgc;
  GC xgc_mono_fill;
  Pixmap gray[8];
  int num_fg_colors;
  int pixel_value_for_color[18];
  int current_x;
  int current_y;
  int current_pixel_value;
  int current_gray_value;
  int min_x;
  int max_x;
  int min_y;
  int max_y;
  int x_border;
  int y_border;
  int x_offset;
  int x_len;
  float x_scale;
  int y_offset;
  int y_len;
  float y_scale;
  int next_point;
  int npolylines;
  int nstrings;
  int *npoints;
  int *polyline_pixel_value;
  int *polyline_gray_value;
  int *strlen;
  int *strflg;
  int *textx;
  int *texty;
  char *strings;
  XPoint *points;
} DIGWin;


