/*-----[--.----+----.----+----.-----------------------------------------*/

/*      * * F E A P * * A Finite Element Analysis Program               */

/*....  Copyright (c) 1984-2020: Regents of the University of California*/
/*                               All rights reserved                    */
/*----[--.----+----.----+----.-----------------------------------------]
 *    Modification log                                Date (dd-mm-year)
 *      1. Add cinput()                                     23-10-2017
 */
/*-----[--.----+----.----+----.-----------------------------------------*/
/*     Purpose: FEAP driver for X windows Version 11, Release 6.        */

/*     Inputs:                                                          */

/*     Outputs:                                                         */
/*-----[--.----+----.----+----.-----------------------------------------*/

       /*********************************/
       /* Driver configuration          */
       /* X11 DEVICE DRIVER             */
       /* Change 6-places for EACH      */
       /* Type of workstation Search    */
       /* for string FORTRAN to change  */
       /* Currently set: Underscore     */
       /*   Version for: Underscore     */
       /*       GCC, INTEL, SUN & DEC   */
       /*   Version for: No Underscore  */
       /*       HP and IBM              */
       /*********************************/

/* This driver supports the traditional GIN mode, i.e. point the cursor */
/*  and strike a key! It also supports the use of the mouse buttons     */
/*  during GIN input. Below is the mouse button to "key" relationships. */

#define BUTTON1_KEY 'l'
#define BUTTON2_KEY 'm'
#define BUTTON3_KEY 'r'

/* This driver has the capability to store all the lines drawn into the */
/*  X Window and to use these saved lines to refresh the screen.        */
/*  Using this feature and the routine "gdx11_refresh_window" should    */
/*  make using FEAP under X11 easier than before!                       */
/* If you don't want/need this feature or if you can't afford the       */
/*  memory usage, set MAX_SEG to 1, MAX_POINTS to something around 100  */

#define MAX_POINTS 1000        /* Maximum points in all polylines stored */
#define MAX_SEG    100         /* Maximum polylines stored */
#define MAX_TEXT   100         /* Maximum 80-character text string */

#define DEBUG        0         /* 0 == No debugging. */
                               /* 1 == Soft X11 errors */
                               /* 2 == Above plus caller bugs */
                               /* 3 == Above plus Driver tracing messages */
                               /* 4 == Above plus all tracing messages */

       /****************************/
       /* Non-FEAP include files */
       /****************************/

#include <stdio.h>                /* Unix standard I/O definitions */
#include <stdlib.h>               /* Use "malloc", "calloc" and "free" */
#include <string.h>   
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

/* #include <sys/select.h>  Use this include instead of next 3
                            on POSIX systems                     */
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>

#include "digwin.h"
/*void jpgd(); */

/* Note: if you don't have the file "malloc.h", then use the following: */
/* extern char *malloc(); */
/* extern void free();    */

       /**************************/
       /* Gray scale definitions */
       /**************************/

#define gray1_width 16
#define gray1_height 16
static char gray1_bits [] = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
                             0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
                             0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
                             0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};

#define gray2_width 4
#define gray2_height 4
static char gray2_bits [] = {0x07, 0x0d, 0x07, 0x0d};

#define gray3_width 16
#define gray3_height 4
static char gray3_bits [] = {0x55, 0x55, 0xee, 0xee, 0x55, 0x55, 0xbb, 0xbb};

#define gray4_width 16
#define gray4_height 16
static char gray4_bits [] = {0xcc, 0xcc, 0x33, 0x33, 0xcc, 0xcc, 0x33, 0x33,
                             0xcc, 0xcc, 0x33, 0x33, 0xcc, 0xcc, 0x33, 0x33,
                             0xcc, 0xcc, 0x33, 0x33, 0xcc, 0xcc, 0x33, 0x33,
                             0xcc, 0xcc, 0x33, 0x33, 0xcc, 0xcc, 0x33, 0x33};

#define gray5_width 2
#define gray5_height 2
static char gray5_bits [] = {0x01, 0x02};

#define gray6_width 16
#define gray6_height 16
static char gray6_bits [] = {0x55, 0x55, 0x88, 0x88, 0x55, 0x55, 0x22, 0x22,
                             0x55, 0x55, 0x88, 0x88, 0x55, 0x55, 0x22, 0x22,
                             0x55, 0x55, 0x88, 0x88, 0x55, 0x55, 0x22, 0x22,
                             0x55, 0x55, 0x88, 0x88, 0x55, 0x55, 0x22, 0x22};

#define gray7_width 4
#define gray7_height 4
static char gray7_bits [] = {0x08, 0x02, 0x08, 0x02};

#define gray8_width 16
#define gray8_height 16
static char gray8_bits [] = {0x11, 0x11, 0x00, 0x00, 0x44, 0x44, 0x00, 0x00,
                             0x11, 0x11, 0x00, 0x00, 0x44, 0x44, 0x00, 0x00,
                             0x11, 0x11, 0x00, 0x00, 0x44, 0x44, 0x00, 0x00,
                             0x11, 0x11, 0x00, 0x00, 0x44, 0x44, 0x00, 0x00};

#define STROKE_DEVICE  0
#define RASTER_DEVICE  1
#define DVST_DEVICE    2
#define PLOTTER_DEVICE 3

#define CAN_DRAW_IN_BACKGROUND 4
#define HARDCOPY_DEVICE 8
#define SHARED_DEVICE 16
#define HLS_SETABLE_COLORS 32
#define RGB_SETABLE_COLORS 64
#define CAN_DO_GIN 128
#define CAN_DRAW_FILLED_POLYGONS 256
#define CONVEX_POLYGONS_ONLY 512
#define CAN_DO_LOCATOR_INPUT 1024

       /***************************/
       /* Some global definitions */
       /***************************/

#define MAX_OPCODE      16        /* number of op-code values */

#define DEFAULT_X      485        /* X Window Location on Screen */
#define DEFAULT_Y      165        /* Y Window Location on Screen */

static DIGWin *current_dw = NULL;
static DIGWin *default_dw = NULL;
static int default_width  = 768;  /* X Window width (default)   */
static int default_height = 600;  /* Y Window width (default)   */

#define Min(x,y) ( ((float)x < (float)y ) ? (float)x : (float)y )

       /*******************************************************************/
       /* Device coordinates <--> Virtual corrdinates, translation macros */
       /*******************************************************************/

#define x_translate(dw,x)    (int)((x)*dw->x_scale-1.5)+dw->x_offset
#define y_translate(dw,y)    (int)(dw->y_len-(y)*dw->y_scale-1.5)+dw->y_offset
#define x_untranslate(dw,x)  ((x)-dw->x_offset)/dw->x_scale
#define y_untranslate(dw,y)  (dw->y_len+dw->y_offset-(y))/dw->y_scale

       /*********************************/
       /*** Window Management Support ***/
       /*********************************/

       /********************/
       /* Clear the Pixmap */
       /********************/

void gdx11_clear_pixmap(DIGWin *dw)
{
  XSetForeground(dw->xdisplay, dw->xgc, 
                 (unsigned long)dw->pixel_value_for_color[0]);
  XFillRectangle(dw->xdisplay, dw->svimage, 
                 dw->xgc, 0, 0, dw->xwa.width, dw->xwa.height);
  XSetForeground(dw->xdisplay, dw->xgc, 
                 (unsigned long)dw->pixel_value_for_color[1]);
}

       /*******************************************************************/
       /* Adjust the scale of the diglib device to fit the current window */
       /*  given the user's guidelines for what part of the window to use.*/
       /*******************************************************************/

void gdx11_adjust_digwin(dw)
     DIGWin *dw;
{

      int x,y;
      unsigned int width, height, depth, bdw;
      Status status;
      Window root;

/*      static void gdx11_init_polylines(), gdx11_init_strings(); */
      void gdx11_init_polylines(), gdx11_init_strings();

      XGetWindowAttributes(dw->xdisplay,dw->xwin,&(dw->xwa));
      dw->x_offset = (dw->min_x*dw->xwa.width)/100+dw->x_border;
      dw->y_offset = (dw->min_y*dw->xwa.height)/100+dw->y_border;
      dw->x_len = ((dw->max_x-dw->min_x)*dw->xwa.width)/100-dw->x_border;
      dw->y_len = ((dw->max_y-dw->min_y)*dw->xwa.height)/100-dw->y_border;

      /* Check is the size has actually changed, if so reset pixmap */
      status = XGetGeometry(dw->xdisplay, dw->svimage,
                            &root, &x, &y, &width, &height, &bdw, &depth);

      if( width != dw->xwa.width || height != dw->xwa.height || status == 0) {
          dw->svimage    = XCreatePixmap(dw->xdisplay,dw->xwin,
                              dw->xwa.width,dw->xwa.height,
                              dw->xwa.depth);
          gdx11_clear_pixmap(dw);
          gdx11_init_polylines(dw);
          gdx11_init_strings(dw);
      }
}

       /*****************************/
       /* Refresh the FEAP window */
       /*****************************/

void gdx11_refresh_digwin(dw)
     DIGWin *dw;
{
  int i,j,x,y,l,flag,width ;
  int start = 0;
  Display *disp = dw->xdisplay;
  unsigned long fg_pv;
  unsigned long bg_pv;
  int fg_gray = 0;
  int pv;
  int gray_pv;
  int np;
  char text[80] ;
  Font nfont ;
  XFontStruct *nfont_struct ;

  fg_pv = (unsigned long)dw->polyline_pixel_value[0];
  XSetForeground(disp,dw->xgc,fg_pv);
  if(dw->num_fg_colors == 1) {
        fg_gray = dw->polyline_gray_value[0];
        XSetStipple(disp,dw->xgc_mono_fill,dw->gray[fg_gray]);
        }
  for (i=0;i<dw->npolylines;++i)
    {
      if ((pv=dw->polyline_pixel_value[i])!=(int)fg_pv)
        {
          fg_pv = (unsigned long)pv;
          XSetForeground(disp,dw->xgc,fg_pv);
        }
      np = dw->npoints[i];
      if (np<0)
        {
          np = -np;
          if (dw->num_fg_colors > 1){
               XFillPolygon(disp,dw->svimage,dw->xgc,&(dw->points[start]),np,
                       Complex,CoordModeOrigin);}

          /*Monochrome gray scale stuff */
          else {
                if( (gray_pv=dw->polyline_gray_value[i]) != fg_gray) {
                     fg_gray = gray_pv;
                     XSetStipple(disp,dw->xgc_mono_fill,dw->gray[fg_gray]);
                     }
                XFillPolygon(disp,dw->xwin,dw->xgc_mono_fill,
                        &(dw->points[start]),np,Complex,CoordModeOrigin);
                }
        }
      else {
            XDrawLines(disp,dw->svimage,dw->xgc,&(dw->points[start]),
                    np,CoordModeOrigin);}
      start = start+np;
    }

  if (dw->nstrings == 0) return ;

  fg_pv        = (unsigned long)dw->pixel_value_for_color[1];
  bg_pv        = (unsigned long)dw->pixel_value_for_color[0];

  XSetBackground(disp,dw->xgc,bg_pv);
  XSetForeground(disp,dw->xgc,fg_pv);

  if ( Min ( dw->x_len , 1.27*dw->y_len ) < 440 )
     nfont        = XLoadFont(disp,"*helvetica-medium-r-normal--8*"); 
  else if ( Min ( dw->x_len , 1.27*dw->y_len ) < 600. )
     nfont        = XLoadFont(disp,"*helvetica-bold-r-normal--10*");
  else if ( Min ( dw->x_len , 1.27*dw->y_len ) < 760. )
     nfont        = XLoadFont(disp,"*helvetica-bold-r-normal--12*");
  else if ( Min ( dw->x_len , 1.27*dw->y_len ) < 920. )
     nfont        = XLoadFont(disp,"*helvetica-bold-r-normal--14*");
  else if ( Min ( dw->x_len , 1.27*dw->y_len ) < 1080. )
     nfont        = XLoadFont(disp,"*helvetica-bold-r-normal--18*");
  else
     nfont        = XLoadFont(disp,"*helvetica-bold-r-normal--20*");

  nfont_struct = XQueryFont(disp,nfont);

  XSetFont(disp,dw->xgc,nfont);

  start = 0 ;

  for (i = 0 ; i < dw->nstrings ; ++i )
     {
       x    = dw->textx[i] ;
       y    = dw->texty[i] ;
       l    = dw->strlen[i] ;
       flag = dw->strflg[i] ;

       for (j = 0 ; j < l ; ++j )
         {
          text[j] = dw->strings[start+j] ;
         }
       if (flag == 1)
         {
           width = XTextWidth(nfont_struct,text,l) ;
           x     = x - width/2 ;
         }

       XDrawImageString(disp,dw->svimage,dw->xgc,x,y,text,l);

       start = start + 80 ;
     }
  XFreeFont(disp, nfont_struct );

  return;
}
  /* Color map for FEAPpv */

  static char colors[6][10] = { "RED"    ,
                                "GREEN"  ,
                                "BLUE"   ,
                                "YELLOW" ,
                                "CYAN"   ,
                                "MAGENTA"};

       /**********************************/
       /* Make XWindow into FEAP XWindow */
       /**********************************/

DIGWin *gdx11_make_digwin_from_xwin(display,xwindow,
                                    back_pixel,fore_pixel,
                                    x_border,y_border,
                                    min_x,max_x,min_y,max_y)
     Display *display;
     Window xwindow;
     unsigned long back_pixel, fore_pixel;
     int x_border, y_border;
     int min_x, max_x, min_y, max_y;
{
  DIGWin *dw;
  Screen *screen;
  Colormap cmap;
  XColor screen_def, exact_def;
  XGCValues setgc;
  int color;
  int colors_allocated;

  if ((dw = (DIGWin *)calloc(sizeof(DIGWin), sizeof(int)))==NULL)
    {
      fprintf(stderr,"FEAP X11 Driver unable to get window memory!\n");
      exit(0);
    }
  dw->npoints = (int  *)calloc(MAX_SEG+1  , sizeof(int));

  dw->strlen  = (int  *)calloc(MAX_TEXT   , sizeof(int));
  dw->strflg  = (int  *)calloc(MAX_TEXT   , sizeof(int));
  dw->strings = (char *)calloc(80*MAX_TEXT, sizeof(char));
  dw->textx   = (int  *)calloc(MAX_TEXT   , sizeof(int));
  dw->texty   = (int  *)calloc(MAX_TEXT   , sizeof(int));

  dw->polyline_pixel_value = (int *)calloc(MAX_SEG+1, sizeof(int));
  dw->polyline_gray_value  = (int *)calloc(MAX_SEG+1, sizeof(int));
  dw->points               = (XPoint *)calloc(MAX_POINTS, sizeof(XPoint));

  dw->xdisplay = display;
  dw->xwin     = xwindow;

  XGetWindowAttributes(display,xwindow,&(dw->xwa));
  screen = dw->xwa.screen;
  cmap = dw->xwa.colormap;

  /* Get default colors */
  dw->pixel_value_for_color[0] = (int)back_pixel;
  dw->pixel_value_for_color[1] = (int)fore_pixel;
  colors_allocated = 2;
  if (CellsOfScreen(screen)>=3)
   {
    for (color=2;color<8;++color)
     {
      if (XAllocNamedColor(display,cmap,colors[color-2],
                           &screen_def,&exact_def))
       {
        dw->pixel_value_for_color[colors_allocated++] = (int)screen_def.pixel;
        if (DEBUG>=3)
         fprintf(stderr,"Color %s is pixel %d\n",
                 colors[color-2],dw->pixel_value_for_color[color]);
       }
      else
       if (DEBUG>=2)
        fprintf(stderr,"FEAP-X11: Unable to allocate color %s\n",
                colors[color-2]);
    }
   }
  else
    if (DEBUG>=3) fprintf(stderr,"FEAP-X11: Monochrome window - no colors!\n");

        dw->gray[0] = XCreateBitmapFromData(display,xwindow,gray1_bits,
                                        gray1_width,gray1_height);
        dw->gray[1] = XCreateBitmapFromData(display,xwindow,gray2_bits,
                                        gray2_width,gray2_height);
        dw->gray[2] = XCreateBitmapFromData(display,xwindow,gray3_bits,
                                        gray3_width,gray3_height);
        dw->gray[3] = XCreateBitmapFromData(display,xwindow,gray4_bits,
                                        gray4_width,gray4_height);
        dw->gray[4] = XCreateBitmapFromData(display,xwindow,gray5_bits,
                                        gray5_width,gray5_height);
        dw->gray[5] = XCreateBitmapFromData(display,xwindow,gray6_bits,
                                        gray6_width,gray6_height);
        dw->gray[6] = XCreateBitmapFromData(display,xwindow,gray7_bits,
                                        gray7_width,gray7_height);
        dw->gray[7] = XCreateBitmapFromData(display,xwindow,gray8_bits,
                                        gray8_width,gray8_height);

  dw->num_fg_colors = colors_allocated - 1;
  dw->current_pixel_value = dw->pixel_value_for_color[1];

  /* Get scale factors for the screen (and therefore for the window) */
  dw->x_scale = ((double)WidthOfScreen(screen))/
    (((double)WidthMMOfScreen(screen))/10.0);
  dw->y_scale = ((double)HeightOfScreen(screen))/
    (((double)HeightMMOfScreen(screen))/10.0);

  /* Use portion of window user wants */
  dw->x_border = x_border;
  dw->y_border = y_border;
  dw->min_x = min_x;
  dw->max_x = max_x;
  dw->min_y = min_y;
  dw->max_y = max_y;

  /* Create a GC for this window */
  setgc.foreground = (unsigned long)dw->pixel_value_for_color[1];
  setgc.background = (unsigned long)dw->pixel_value_for_color[0];
  setgc.function = GXcopy;
  setgc.line_width = 0;
  setgc.line_style = LineSolid;
  setgc.fill_style = FillSolid;
  dw->xgc = XCreateGC(display,xwindow,
                      (GCForeground|GCBackground|GCFunction|GCLineWidth|
                       GCLineStyle|GCFillStyle),
                        &setgc);

  /* Monochrome gray scale stuff */
  if(dw->num_fg_colors == 1){
       setgc.fill_style = FillStippled;
       dw->xgc_mono_fill = XCreateGC(display,xwindow,
                      (GCForeground|GCBackground|GCFunction|GCLineWidth|
                       GCLineStyle|GCFillStyle),
                             &setgc);
       XSetFillStyle(display,dw->xgc_mono_fill,FillOpaqueStippled);
  }

  /* Set up Pixmap for the displayed image */
  XGetWindowAttributes(dw->xdisplay,dw->xwin,&(dw->xwa));
  dw->x_offset = (dw->min_x*dw->xwa.width)/100+dw->x_border;
  dw->y_offset = (dw->min_y*dw->xwa.height)/100+dw->y_border;
  dw->x_len = ((dw->max_x-dw->min_x)*dw->xwa.width)/100-dw->x_border;
  dw->y_len = ((dw->max_y-dw->min_y)*dw->xwa.height)/100-dw->y_border;

  dw->svimage    = XCreatePixmap(dw->xdisplay,dw->xwin,
                              dw->xwa.width,dw->xwa.height,
                              dw->xwa.depth);
  gdx11_clear_pixmap(dw);

  return(dw);
}

       /****************************************/
       /* Create a FEAP XWindow from scratch */
       /****************************************/

DIGWin *gdx11_create_digwin(xservername,window_width,window_height)
     char *xservername ;
     int window_width, window_height;
{
  Display *disp;
  Window xwin;
  Screen *def_screen;
  Colormap cmap;
  XColor screen_def, exact_def;
  XSetWindowAttributes setwin;
  XSizeHints size_hints;
  XEvent event;
  DIGWin *dw;
  unsigned long background_pixel, foreground_pixel;


/* Window Location on Screen */

  if (!(disp = XOpenDisplay(xservername)))
    {
      fprintf(stderr,
              "FEAP X11 Driver unable to open X windows connection.\n");
      exit(0);
    }
  if (DEBUG>=4)
    fprintf(stderr,"gdx11_init_device: Inform : X Display opened\n");

  /* Get screen for window */
  def_screen = DefaultScreenOfDisplay(disp);

  /* Get colors straight */
  cmap = DefaultColormapOfScreen(def_screen);
  if (XAllocNamedColor(disp,cmap,"BLACK",&screen_def,&exact_def))
    background_pixel = screen_def.pixel;
  else
    background_pixel = BlackPixelOfScreen(def_screen);
  if (XAllocNamedColor(disp,cmap,"WHITE",&screen_def,&exact_def))
    foreground_pixel = screen_def.pixel;
  else
    foreground_pixel = WhitePixelOfScreen(def_screen);

  /* Setup gray scale masks */

  /* Create an Xwindow */
  setwin.event_mask = (ButtonPressMask|ExposureMask|StructureNotifyMask|
                       KeyPressMask);
  setwin.background_pixel = background_pixel;
  setwin.backing_store    = Always;
/*  setwin.save_under       = True; */
  xwin = XCreateWindow(disp,
                       RootWindowOfScreen(def_screen),
                       DEFAULT_X,DEFAULT_Y,
                       window_width,window_height,
                       4,
                       DefaultDepthOfScreen(def_screen),
                       InputOutput,
                       DefaultVisualOfScreen(def_screen),
                       (CWBackPixel|CWEventMask|CWBackingStore),
                       &setwin);

  /* Map the window onto the display */
  size_hints.x      = 0             ;
  size_hints.y      = 0             ;
  size_hints.width  = window_width  ;
  size_hints.height = window_height ;
  size_hints.min_width  = 20;
  size_hints.min_height = 20;
  size_hints.flags = USPosition|PSize|PMinSize;
  XSetStandardProperties(disp,xwin,"FEAP Graphics Window","FEAP Win",
                         None,0,0,&size_hints);
  XMapWindow(disp,xwin);

  /* Wait for exposure event before proceeding - otherwise, some */
  /*  drawing command might get tossed                           */
  XWindowEvent(disp,xwin,ExposureMask,&event);

  /* Now turn the XWindow into a FEAP XWindow */
  dw = gdx11_make_digwin_from_xwin(disp,xwin,
                                   background_pixel,foreground_pixel,
                                   2,2,0,100,0,100);

  return(dw);
}

/* FORTRAN Interface */

/* HP  and IBM use :
DIGWin *gdx11cdw(xservername,widthptr,heightptr)
*/
/* GCC, INTEL, SUN and DEC use :
*/
DIGWin *gdx11cdw_(xservername,widthptr,heightptr)
     char *xservername;
     int *widthptr, *heightptr;
{
  return(gdx11_create_digwin(xservername,*widthptr,*heightptr));
}


       /*****************************/
       /* Set FEAP Drawing Window */
       /*****************************/

DIGWin *gdx11_set_current_digwin(dw)
     DIGWin *dw;
{
  DIGWin *old_dw = current_dw;

  current_dw = dw;

  /* Force FEAP to do equivalent of a DEVSEL! This is necessary to get */
  /*  FEAP to notice the change in the window. Unfortunately, it also  */
  /*  has the side effect of resetting all those things DEVSEL resets,   */
  /*  e.g. line type, clipping limits, etc.                              */

  /* Finally, return the old FEAP window */
  return(old_dw);
}

/* FORTRAN Interace */
/* HP  and IBM use :
DIGWin *gdx11setdw(dw)
*/
/* GCC, INTEL, SUN and DEC use :
*/
DIGWin *gdx11setdw_(dw)
     DIGWin **dw;
{
  return(gdx11_set_current_digwin(*dw));
}

       /****************************/
       /* Set Default Xwindow Size */
       /****************************/

void gdx11_set_def_window_size(width,height)
     int width, height;
{
  default_width = width;
  default_height = height;
}

/* FORTRAN Interface */
/* HP  and IBM use :
void gdx11setwindow(widthptr,heightptr)
*/
/* GCC, INTEL, SUN and DEC use :
*/
void gdx11setwindow_(widthptr,heightptr)
     int *widthptr, *heightptr;
{
  gdx11_set_def_window_size(*widthptr,*heightptr);
}

       /********************/
       /* Free FEAP Window */
       /********************/

void gdx11_free_digwin(dw)
     DIGWin *dw;
{
  if (current_dw == dw) current_dw = NULL;
  if (default_dw == dw) default_dw = NULL;

  free(dw->npoints);

  free(dw->polyline_pixel_value);
  free(dw->polyline_gray_value);
  free(dw->points);

  free(dw->strlen);
  free(dw->strflg);
  free(dw->strings);
  free(dw->textx);
  free(dw->texty);

  free(dw);
}



       /***********************************************************/
       /* Simple Function to Handle Single X Event in FEAP Window */
       /***********************************************************/

static XComposeStatus lookup_status;

int gdx11_handle_digwin_event(dw,eventptr,term_char,term_button)
     DIGWin *dw;
     XEvent *eventptr;
     char term_char;
     unsigned int term_button;
{
  int terminator = 0;
/* static void gdx11_flush(); */
  void gdx11_flush();

  switch (eventptr->type)
    {
    case MappingNotify:
      XRefreshKeyboardMapping((XMappingEvent *)eventptr);
      break;
    case Expose:
        gdx11_flush(dw);
      break;
    case ConfigureNotify:
        gdx11_adjust_digwin(dw);
        gdx11_flush(dw);
      break;
    case KeyPress:
      {
        XKeyEvent *key_event = (XKeyEvent *)eventptr;
        int length;
        char buf[8];
        KeySym ks;

        length = XLookupString(key_event,buf,8,&ks,&lookup_status);
        if (length > 0 && term_char != 0 && term_char == buf[0])
          terminator = 1;
        break;
      }
    case ButtonPress:
      {
        XButtonEvent *button_event = (XButtonEvent *)eventptr;

        if (term_button != 0 &&
            term_button == button_event->button) terminator = 1;
      }
    default:
      break;
    }
  return(terminator);
}

       /******************/
       /* String support */
       /******************/

/* static void gdx11_init_strings(dw) */
  void gdx11_init_strings(dw)
     DIGWin *dw;
{
  dw->nstrings = 0;
}

       /********************/
       /* Polyline support */
       /********************/

/* static void gdx11_init_polylines(dw) */
  void gdx11_init_polylines(dw)
     DIGWin *dw;
{
  dw->npolylines = 0;
  dw->npoints[0] = 0;
  dw->next_point = 0;
}

static void gdx11_term_polyline(dw)
     DIGWin *dw;
{
  /* Terminate previous polyline if necessary */

  if (dw->npoints[dw->npolylines]>=2) ++dw->npolylines;

  else dw->next_point -= dw->npoints[dw->npolylines];

  dw->npoints[dw->npolylines] = 0;
}

static void gdx11_end_polygon(dw)
     DIGWin *dw;
{
  int npoly = dw->npolylines;

  if (dw->npoints[npoly]>=2)
    {
      dw->npoints[npoly] = -dw->npoints[npoly];
      ++dw->npolylines;
    }
  else dw->next_point -= dw->npoints[dw->npolylines];

  dw->npoints[dw->npolylines] = 0;
}

static void gdx11_check_polyline_overflow(dw,n)
     DIGWin *dw;
     int n;
{
  if (dw->next_point+n > MAX_POINTS || dw->npolylines >=MAX_SEG)
    {
      gdx11_term_polyline(dw);
      gdx11_refresh_digwin(dw);
      gdx11_init_polylines(dw);

    }
}

static void gdx11_start_polyline(dw,x,y)
     DIGWin *dw;
     int x, y;
{
  gdx11_term_polyline(dw);
  gdx11_check_polyline_overflow(dw,2);

  /* Initialize current polyline */
  dw->points[dw->next_point].x = x;
  dw->points[dw->next_point++].y = y;
  dw->npoints[dw->npolylines] = 1;
  dw->polyline_pixel_value[dw->npolylines] = dw->current_pixel_value;
  if(dw->num_fg_colors == 1)
       dw->polyline_gray_value[dw->npolylines] = dw->current_gray_value;
  return;
}

static void gdx11_start_polygon(dw,n,x,y)
     DIGWin *dw;
     int n;
     int x, y;
{
  gdx11_term_polyline(dw);
  gdx11_check_polyline_overflow(dw,n);
  gdx11_start_polyline(dw,x,y);
  return;
}

static void gdx11_add_point_to_polyline(dw,x,y)
     DIGWin *dw;
     int x,y;
{
  if (dw->npoints[dw->npolylines]==0)
    gdx11_start_polyline(dw,dw->current_x,dw->current_y);

  gdx11_check_polyline_overflow(dw,1);

  if(dw->next_point == 0) {
      dw->polyline_pixel_value[dw->npolylines] = dw->current_pixel_value;
      dw->points[dw->next_point].x = dw->current_x;
      dw->points[dw->next_point++].y = dw->current_y;
      dw->npoints[dw->npolylines] = 1;
  }

  dw->points[dw->next_point].x = x;
  dw->points[dw->next_point++].y = y;
  ++dw->npoints[dw->npolylines];
}

       /**************************/
       /* 1. - INITIALIZE DEVICE */
       /**************************/

static void gdx11_init_device(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  /* If we do not yet have a diglib Xwindow get one */
  if (dw == NULL)
    {
      if (default_dw == NULL)
        dw = default_dw =
          gdx11_create_digwin("",default_width,default_height);
      else
        dw = default_dw;
    }

  /* Now fix the correct screen-size */

  current_dw = dw;
  gdx11_adjust_digwin(dw);
}

       /******************************************************/
       /* 2. - GET FRESH PLOTTING SURFACE, i.e. ERASE WINDOW */
       /******************************************************/

static void gdx11_clear_page(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  dw->current_pixel_value = dw->pixel_value_for_color[1];

  gdx11_init_polylines(dw);
  gdx11_init_strings(dw);

  if ((*x_data)[0] >= 0.0)
  {
    if (DEBUG>=4) fprintf(stderr,"Clearing Window ... ");
    XClearWindow(dw->xdisplay,dw->xwin);
    if (DEBUG>=4) fprintf(stderr,"done!\n");
    gdx11_adjust_digwin(dw);
    gdx11_clear_pixmap(dw);
  }
}

       /**********************/
       /* 3. - MOVE TO (X,Y) */
       /**********************/

static void gdx11_move_to(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{

  int x = x_translate(dw,(*x_data)[0]);
  int y = y_translate(dw,(*y_data)[0]);


  if (x==dw->current_x && y==dw->current_y) return;

  /* Start polyline */
  gdx11_start_polyline(dw,x,y);

  dw->current_x = x;
  dw->current_y = y;
}

       /**********************/
       /* 4. - DRAW TO (X,Y) */
       /**********************/

static void gdx11_draw_to(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  register int new_x, new_y;

  new_x = x_translate(dw,(*x_data)[0]);
  new_y = y_translate(dw,(*y_data)[0]);
  gdx11_add_point_to_polyline(dw,new_x,new_y);
  dw->current_x = new_x;
  dw->current_y = new_y;
}

       /******************************/
       /* 5. - FLUSH GRAPHICS BUFFER */
       /******************************/

/* static void gdx11_flush(dw,x_data,y_data) */
 void gdx11_flush(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  gdx11_term_polyline(dw);

  gdx11_refresh_digwin(dw);
  XCopyArea(dw->xdisplay, dw->svimage, dw->xwin, dw->xgc, 
            0, 0, dw->xwa.width,dw->xwa.height, 0 ,0);
  XFlush(dw->xdisplay);
}

       /***********************/
       /* 6. - RELEASE DEVICE */
       /***********************/

static void gdx11_release_device(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  /*
   *  This routine should really close out the window system,
   *  but we want to be able to get back to this same device if
   *  it gets reopened.  So just leave the window around and don't
   *  clear it.
   */
  return;
}

       /**************************************/
       /* 7. - RETURN DEVICE CHARACTERISTICS */
       /**************************************/

static void gdx11_return_device(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  (*x_data)[0] = 11.0;                      /* Nonzero device ID */
  if (dw!=NULL && dw->xwin != ((Window)NULL) )
    {
      (*x_data)[1] = dw->x_len/dw->x_scale; /* X Length in cm. */
      (*x_data)[2] = dw->y_len/dw->y_scale; /* Y Length in cm. */
      (*x_data)[3] = dw->x_scale;           /* X pixels per cm */
      (*x_data)[4] = dw->y_scale;           /* Y pixels per cm */
      (*x_data)[5] = dw->num_fg_colors;     /* Number of foreground colors */
    }
  else
    {
      (*x_data)[1] = 0.001;                 /* X Length in cm. */
      (*x_data)[2] = 0.001;                 /* Y Length in cm. */
      (*x_data)[3] = 1.0;                   /* X pixels per cm */
      (*x_data)[4] = 1.0;                   /* Y pixels per cm */
      (*x_data)[5] = 1.0;                   /* Number of foreground colors */
    }
  (*x_data)[6] = RASTER_DEVICE+CAN_DRAW_IN_BACKGROUND+
    CAN_DO_GIN+CAN_DRAW_FILLED_POLYGONS+
      CAN_DO_LOCATOR_INPUT;                 /* FEAP device capabilities */
  (*x_data)[7] = 1.0;                       /* Fill every line */
  return;
}

       /******************************/
       /* 8. - SELECT PLOTTING COLOR */
       /******************************/

static void gdx11_select_color(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  int new_pv = (int) (*x_data)[0];

  /* Set gray scale for monochrome monitors */
  if (dw->num_fg_colors == 1) {
       switch (new_pv) {
            case 0:
              dw->current_gray_value = 0;
              break;
            case 2:
              dw->current_gray_value = 1;
              break;
            case 7:
              dw->current_gray_value = 2;
              break;
            case 3:
              dw->current_gray_value = 3;
              break;
            case 5:
              dw->current_gray_value = 4;
              break;
            case 4:
              dw->current_gray_value = 5;
              break;
            case 6:
              dw->current_gray_value = 6;
              break;
            case 1:
              dw->current_gray_value = 7;
              break;
        }

       new_pv = (new_pv > 1) ? 1 : new_pv;

     }

     if (new_pv>=0 && new_pv<=dw->num_fg_colors &&
        (new_pv = dw->pixel_value_for_color[new_pv])
        !=dw->current_pixel_value)
      {
        gdx11_term_polyline(dw);
        dw->current_pixel_value = new_pv;
      }
}

static void gdx11_get_input(dw,cursor,allow_keys,left,middle,right,
                            press,x,y)
     DIGWin *dw;
     Cursor cursor;
     int allow_keys, left, middle, right;
     int *press, *x, *y;
{
  Display *disp = dw->xdisplay;
  Window w = dw->xwin;
  XEvent event;
  long int event_mask = (ButtonPressMask|ExposureMask|StructureNotifyMask|
                         KeyPressMask);
  int no_button = 1;

  /* First, toss all events on this window already in the queue */
  while (XCheckWindowEvent(disp,w,event_mask,&event))
    gdx11_handle_digwin_event(dw,&event,0,0);


  /* Put up cursor to signal we want input */
  XDefineCursor(disp,w,cursor);

  /* Now look for GIN event */
  while(no_button)
    {
      XWindowEvent(disp,w,event_mask,&event);
      switch((int)event.type)
        {
        case KeyPress:
          {
            XKeyEvent *key_event = (XKeyEvent *)(&event);
            int length;
            char buf[8];
            KeySym ks;

            /* Get the key pressed! */
            length = XLookupString(key_event,buf,8,&ks,&lookup_status);
            if (length>0)
              {
                *press = buf[0];
                *x = key_event->x;
                *y = key_event->y;
                no_button = 0;
              }
            break;
          }
        case ButtonPress:
          {
            XButtonEvent *DiglibEvent = (XButtonEvent *)(&event);

            if (DiglibEvent->button == Button3)
              *press = BUTTON3_KEY;
            else if (DiglibEvent->button == Button2)
              *press = BUTTON2_KEY;
            else if (DiglibEvent->button == Button1)
              *press = BUTTON1_KEY;
            else
              *press = 0;
            *x = DiglibEvent->x;
            *y = DiglibEvent->y;
            no_button = 0;
          }
        default:
          gdx11_handle_digwin_event(dw,&event,0,0);
          break;
        }
    }
  /* Remove cross_hair_cursor */
  XUndefineCursor(disp,w);
}

       /**********************/
       /* 9. - GET GIN INPUT */
       /**********************/

static void gdx11_gin(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  int key;
  int x,y;
  static Cursor gin_cursor;
  static int have_gin_cursor = 0;

  /* Make sure we have a cross hair cursor */
  if (!have_gin_cursor)
    {
      gin_cursor = XCreateFontCursor(dw->xdisplay,XC_crosshair);
      have_gin_cursor = 1;
    }

  gdx11_get_input(dw,gin_cursor,1,BUTTON1_KEY,BUTTON2_KEY,BUTTON3_KEY,
                  &key,&x,&y);
  (*x_data)[0] = key;
  (*x_data)[1] = x_untranslate(dw,x);
  (*x_data)[2] = y_untranslate(dw,y);
}

       /********************************/
       /* 10. - DEFINE COLOR USING RGB */
       /********************************/

/* Not yet implemented */
/* Because colors are allocated READ-ONLY, this will take some work! */
static void gdx11_set_color_map_rgb(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
/*  int color_index = (*x_data)[0]; */
}

       /********************************/
       /* 11. - DEFINE COLOR USING HLS */
       /********************************/

/* Not yet implemented, probably never will be */
static void gdx11_set_color_map_hls( op_code, x_data, y_data )
     int   *op_code;
     float (* x_data)[], (* y_data)[];
{
}

       /***************************/
       /* 12. - GET LOCATOR INPUT */
       /***************************/

static void gdx11_button_input(dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  int button;
  int x,y;
  static Cursor locator_cursor;
  static int have_locator_cursor = 0;

  /* Make sure we have a cross hair cursor */
  if (!have_locator_cursor)
    {
      locator_cursor = XCreateFontCursor(dw->xdisplay,XC_diamond_cross);
      have_locator_cursor = 1;
    }

  gdx11_get_input(dw,locator_cursor,0,1,2,4,&button,&x,&y);
  (*x_data)[0] = button;
  (*x_data)[1] = x_untranslate(dw,x);
  (*x_data)[2] = y_untranslate(dw,y);
}

       /*****************************/
       /*  13. - SET LINE STYLE     */
       /*****************************/
/*  Added by RLT on 1/22/93  */
static void gdx11_set_line (dw,x_data,y_data)
     DIGWin *dw;
     float (* x_data)[], (* y_data)[];
{
  unsigned int line_width;
  int          line_style;
  int          cap_style;
  int          join_style;
  int new_ln = (int) (*x_data)[0];
  int new_wd = (int) (*y_data)[0];

  /* Change GC for this window */

  XGCValues setgc;

  /* First flush buffer with any current events */

  gdx11_term_polyline(dw);
  gdx11_refresh_digwin(dw);

  XFlush(dw->xdisplay);

  gdx11_init_polylines(dw);
  gdx11_init_strings(dw); 

  setgc.foreground = (unsigned long)dw->pixel_value_for_color[1];
  setgc.background = (unsigned long)dw->pixel_value_for_color[0];
  setgc.function   = GXcopy;

  if  (new_ln <= 1)
      line_style = LineSolid;
  else if (new_ln == 2)
      line_style = LineOnOffDash;
  else
      line_style = LineDoubleDash;

  setgc.fill_style = FillSolid;

  if  (new_wd <= 1)
      line_width = 0;
  else if (new_wd == 2)
      line_width = 1;
  else if (new_wd == 3)
      line_width = 2;
  else if (new_wd == 4)
      line_width = 3;
  else
      line_width = 4;


  join_style = JoinMiter;
  cap_style  = CapButt;

  XSetLineAttributes( dw->xdisplay, dw->xgc, line_width, line_style,
                      cap_style, join_style);

  /* Monochrome gray scale stuff */

  if(dw->num_fg_colors == 1){
       XSetLineAttributes( dw->xdisplay, dw->xgc, line_width, line_style,
                           cap_style, join_style);
       XSetFillStyle(dw->xdisplay,dw->xgc_mono_fill,FillOpaqueStippled);
  }
}
       /*************************/
       /* 14. - Set Clip Window */
       /*************************/

static void gdx11_clip_mask(dw,x_data,y_data)
  DIGWin *dw;
  float (* x_data)[], (* y_data)[];
{
  register int new_x;
  XRectangle rectangles[1];
  int clip_x_origin, clip_y_origin;
  int nrects;
  int ordering;

  new_x = x_translate(dw,(*x_data)[2]);
  if (new_x == 0)
    XSetClipMask( dw->xdisplay, dw->xgc, None );
  else {
    clip_x_origin = x_translate(dw,(*x_data)[0]);
    clip_y_origin = y_translate(dw,(*y_data)[0]);
    rectangles->x = clip_x_origin;
    rectangles->y = clip_y_origin;
    rectangles->width  = x_translate(dw,(*x_data)[1]);
    rectangles->height = y_translate(dw,(*y_data)[1]);
    nrects = 1;
    ordering = Unsorted;
    XSetClipRectangles(dw->xdisplay, dw->xgc, clip_x_origin,
                       clip_y_origin, rectangles, nrects, ordering );

  }
}
       /**************************************/
       /*  MAX_OPCODE-1 - DRAW TEXT STRING   */
       /**************************************/

static void gdx11_draw_text(dw,n,x_data,y_data)
     DIGWin *dw;
     int n ;
     float (* x_data)[], (* y_data)[];
{
  int i ;
  dw->textx[dw->nstrings] = x_translate(dw,(*x_data)[0]);
  dw->texty[dw->nstrings] = y_translate(dw,(*y_data)[0]);
  dw->strflg[dw->nstrings] = (int)(*y_data)[1];
  dw->strlen[dw->nstrings] = n;
  for ( i = 1; i <= n; ++i)
    dw->strings[dw->nstrings*80+i-1]=(int)(*x_data)[i];

/* prepare for next text string */
  ++dw->nstrings;
  if(dw->nstrings==MAX_TEXT) {
    gdx11_refresh_digwin(dw);
    dw->nstrings = 0;
  }
}

       /************************************/
       /* MAX_OPCODE - DRAW FILLED POLYGON */
       /************************************/

static void gdx11_draw_polygon(dw,n,x_data,y_data)
     DIGWin *dw;
     int n;
     float (* x_data)[], (* y_data)[];
{
  int    i;
/*  Display *disp = dw->xdisplay;*/

  dw->current_x = x_translate(dw,(*x_data)[n-1]);
  dw->current_y = y_translate(dw,(*y_data)[n-1]);
  gdx11_start_polygon(dw,n+1,dw->current_x,dw->current_y);
  for ( i = 0; i < n; ++i)
    gdx11_add_point_to_polyline(dw,x_translate(dw,(*x_data)[i]),
                                y_translate(dw,(*y_data)[i]));
  gdx11_end_polygon(dw);
}


       /******************************************/
       /* X11 Window SAVE Options: Dummy routines*/
       /******************************************/

/*void gdx11_jpg(DIGWin* dw) {

    jpgd(dw);

} */

       /*********************/
       /* X11 DEVICE DRIVER */
       /*********************/

/* FORTRAN Interace */

/* HP  and IBM use :
void gdx11( op_code, x_data, y_data )
 */
/* GCC, INTEL, SUN and DEC use :
 */
void gdx11_( op_code, x_data, y_data )
     int    *op_code;        /* holds device independent op-code */
     float  (* x_data)[];    /* x coordinate data */
     float  (* y_data)[];    /* y coordinate data */
{
  /* An array of pointers to function */
  /*  i.e. a jump table that is global to this compilation unit */

  static void (* jump_table[MAX_OPCODE])() =
    {
      gdx11_init_device,           /*  1 = initialize new device */
      gdx11_clear_page,            /*  2 = erase the window      */
      gdx11_move_to,               /*  3 = move - no draw        */
      gdx11_draw_to,               /*  4 = draw - draw line      */
      gdx11_flush,                 /*  5 = flush buffer          */
      gdx11_release_device,        /*  6 = release device = NULL */
      gdx11_return_device,         /*  7 = return device feature */
      gdx11_select_color,          /*  8 = select colors         */
      gdx11_gin,                   /*  9 = gin input selection   */
      gdx11_set_color_map_rgb,     /* 10 = RGB color map = NULL  */
      gdx11_set_color_map_hls,     /* 11 = HLS color map = NULL  */
      gdx11_button_input,          /* 12 = locator input w/mouse */
      gdx11_set_line,              /* 13 = set line style        */
      gdx11_clip_mask,             /* 14 = set clip region       */
      gdx11_draw_text,             /* MAX_OP - 1 = draw text     */
      gdx11_draw_polygon           /* MAX_OP     = polygon fill  */
      };                           /*      MAX_OPCODE = 16       */

  if (DEBUG>=3) fprintf(stderr,"FEAP-X11 called, op_code = %d\n",*op_code);
  /* Check for correct op-code, and run */
  if (*op_code > 1024)
    jump_table[MAX_OPCODE-1](current_dw,*op_code-1024,x_data,y_data);
  else if (*op_code < -1024)
    jump_table[MAX_OPCODE-2](current_dw,-*op_code-1024,x_data,y_data);
  else if ((0 <= *op_code) && (*op_code <= MAX_OPCODE-1))
    jump_table[*op_code-1](current_dw,x_data,y_data);
  if (DEBUG>=3) fprintf(stderr,"FEAP-X11 done with op_code = %d\n",*op_code);
}

/*
 *  C-based input routines to allow for DIGWin Event Handeling if
 *  the graphics window has been created.
 *
 *  SG: 04/18/2005 (based upon DSB's GPL version)
 *
 */


/* FEAP's comrec common block */
struct {
          char record[256];
} comrec_;

/*
 * C stub for overloaded X11 input.
 *
 * Assumes that Fortran True == 1 and Fortran False == 0
 */


int cinput_nox()
{
        int stlen,j;

        for(j=0;j<256;j++) comrec_.record[j] = ' ';  /* Clear the record */

        /* Loop to get line, trap errors */
        while ( NULL == fgets(comrec_.record,256,stdin) ) {
          printf("\n *ERROR* EOF on input detected, please try again\n");
          clearerr(stdin);
        }

        stlen = strlen(comrec_.record);
        comrec_.record[stlen]   = ' '; /* zap /0 */
        comrec_.record[stlen-1] = ' '; /* zap carraige return */
        return 1;
}
/*
 * Select switch loop to look for X11 events and keyboard events
 */

int cinput_x()
{
    fd_set readfds;
    struct timeval timeout;

    int n, retv;
    int fd, cinput_x_unfinished;

    cinput_x_unfinished = 1;
    retv                = 0;

    fd = ConnectionNumber(current_dw->xdisplay);

    while (cinput_x_unfinished) {

        timeout.tv_sec  = 2;
        timeout.tv_usec = 0;

        FD_ZERO(&readfds);
        FD_SET (0,  &readfds);
        FD_SET (fd, &readfds);

        /* Select on fd's, process input as needed */
        n = select(fd + 1, &readfds, NULL, NULL, &timeout);

        /* Data on stdin */
        if (FD_ISSET(0, &readfds)) {
           retv = cinput_nox();
           cinput_x_unfinished = 0;
        }

        /* Data on X11 pipe */
        if(FD_ISSET(fd, &readfds)) {
            DIGWin *dw = current_dw;
            XEvent event;
            while (XCheckWindowEvent(dw->xdisplay,dw->xwin,
                                     dw->xwa.your_event_mask,&event))
                gdx11_handle_digwin_event(dw,&event,0,0);
        }

    }

    return retv;
}

/* Input routine for stdin via C */
int cinput_()
{
    if (current_dw == NULL)
        return cinput_nox();
    else
        return cinput_x();
}


