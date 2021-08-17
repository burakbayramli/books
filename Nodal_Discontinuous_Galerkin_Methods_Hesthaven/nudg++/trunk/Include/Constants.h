// Constants.h
// numeric and GUI constants
// 2006/12/12
//---------------------------------------------------------
#ifndef NDG__Constants_H__INCLUDED
#define NDG__Constants_H__INCLUDED



///////////////////////////////////////////////////////////
//
// Numeric constants
//
///////////////////////////////////////////////////////////

// some systems do not support M_PI in math.h etc:

#ifndef M_E
#define M_E         2.7182818284590452354       // e
#endif
#ifndef M_LOG2E
#define M_LOG2E     1.4426950408889634074       // log 2e
#endif
#ifndef M_LOG10E
#define M_LOG10E    0.43429448190325182765      // log 10e
#endif
#ifndef M_LN2
#define M_LN2       0.69314718055994530942      // log e2
#endif
#ifndef M_LN10
#define M_LN10      2.30258509299404568402      // log e10
#endif
#ifndef M_PI
#define M_PI        3.14159265358979323846      // pi
#endif
#ifndef M_PI_2
#define M_PI_2      1.57079632679489661923      // pi/2
#endif
#ifndef M_1_PI
#define M_1_PI      0.31830988618379067154      // 1/pi
#endif
#ifndef M_PI_4
#define M_PI_4      0.78539816339744830962      // pi/4
#endif
#ifndef M_2_PI
#define M_2_PI      0.63661977236758134308      // 2/pi
#endif

#undef  PI
//#define PI 3.141592653589793238462643383279502884197169399375105820974944592308
#define   PI 3.14159265358979323846
//#define SQUAREROOTTWO 1.4142135623730950488016887242096980785696718753769480732
#define   SQUAREROOTTWO 1.41421356237309504880

///////////////////////////////////////////////////////////////////////////////
// Constants
///////////////////////////////////////////////////////////////////////////////
const double TWOPI    =  2.0 * PI;
const double FOURPI   =  4.0 * PI;
const double HALFPI   =  0.5 * PI;
const double inv1pi   =  1.0 / PI;
const double inv2pi   =  1.0 / TWOPI;
const double inv4pi   =  1.0 / FOURPI;

const double SQROOTTWO = 1.414213562373095048801688724209698;
const double ONETHIRD  = 1.0/3.0;
const double TWOTHIRD  = 2.0/3.0;
const double ONESIXTH  = 1.0/6.0;

const double DM_E      = 2.7182818284590452354;   // e
const double DM_LOG2E  = 1.4426950408889634074;   // log 2e
const double DM_LOG10E = 0.43429448190325182765;  // log 10e
const double LOG10E    = DM_LOG10E;
const double DM_LN2    = 0.69314718055994530942;  // log e2
const double DM_LN10   = 2.30258509299404568402;  // log e10


// Adjust degenerate COLOR/CONTOUR ranges:
const double MIN_COLOR_delta   = 1.0e-10; // PageVizColormap.cpp (line 109)
const double MIN_CONTOUR_delta = 1.0e-5;  // PageVizContour.cpp  (line 100)


///////////////////////////////////////////////////////////
//
// enums for messages and control IDs
//
///////////////////////////////////////////////////////////

enum SLEDGE_WND_IDs
{
  // Window identifiers for FindWindowById()
  umID_WND_MAIN       = 101,
  umID_WND_HELP       = 102,
  umID_WND_PLOT       = 103,
  umID_WND_GEOM_2D    = 104,
  umID_WND_GEOM_3D    = 105,
  umID_WND_SIM_CTRL   = 106,
  umID_WND_AVI_CTRL   = 107,
  umID_WND_EXP_CTRL   = 108,
  // windows for custom GUIs
  umID_WND_USEME      = 201,
  umID_WND_GNUPLOT    = 202,
  umID_WND_BBH        = 210,
  umID_WND_WICK       = 211,
  umID_WND_DISC_CTRL  = 211
  // ...
  // ...
};

enum SLEDGE_ThreadEvents 
{
  // events passed from sim threads to GUI panels
  umID_WORKER_EVENT  = 301,
  umID_DOPLOT_EVENT  = 302,
  umID_RENDER_EVENT  = 303,

  umID_MENU_EVENT    = 306,   // [Apply] from Menu GUI
  umID_PDEWIZ_EVENT  = 307,   // [Apply] from PDEWiz GUI
  
  umID_LOAD_DOMAINS  = 310,   // [Load]  from Mesh GUI
  
  umID_SCURVE_EVENT  = 321,

  umID_UPDATE_GUI    = 350
};

enum SLEDGE_ThreadState 
{
  THREAD_INVALID    = -2,
  THREAD_ABORTED    = -1,
  THREAD_CREATED    =  0,
  THREAD_ON_ENTRY   =  1,
  THREAD_RUNNING    =  2,
  THREAD_PROGRESS   =  3,
  THREAD_RESULTS    =  4,
  THREAD_SIM_END    =  5,
  THREAD_PAUSED     =  6,
  THREAD_RESUMED    =  7,
  THREAD_STOPPED    =  8,
  THREAD_KILLED     =  9,
  THREAD_FINISHED   = 10,
  THREAD_ON_EXIT    = 11,
  THREAD_EXCEPTION  = 12
};

enum SLEDGE_ThreadMsgs
{
  umUSER_0                = 4000,
  umUSER_THREAD_CREATED   = (umUSER_0 + THREAD_CREATED),
  umUSER_THREAD_RUNNING   = (umUSER_0 + THREAD_RUNNING),
  umUSER_THREAD_PROGRESS  = (umUSER_0 + THREAD_PROGRESS),
  umUSER_THREAD_RESULTS   = (umUSER_0 + THREAD_RESULTS),
  umUSER_THREAD_FINISHED  = (umUSER_0 + THREAD_FINISHED),
  umUSER_THREAD_PAUSED    = (umUSER_0 + THREAD_PAUSED),
  umUSER_THREAD_RESUMED   = (umUSER_0 + THREAD_RESUMED),
  umUSER_THREAD_STOPPED   = (umUSER_0 + THREAD_STOPPED),
  umUSER_THREAD_KILLED    = (umUSER_0 + THREAD_KILLED),
  umUSER_THREAD_ABORTED   = (umUSER_0 + THREAD_ABORTED),
  umUSER_THREAD_EXCEPTION = (umUSER_0 + THREAD_EXCEPTION)
};


enum SLEDGE_PlotRequest
{
  PLOT_ERRORS      =  0,    // algebraic equation
  PLOT_R_PROFILE   =  1,    // disc profile
  PLOT_BVI_MAG     =  2,    // disc BVI magnitudes
  PLOT_V_STRUCT    =  3,    // Vertical structure
  PLOT_TLUSTY_DAT  =  4,    // TLusty input data
  PLOT_TLUSTY_FLUX =  5,    // TLusty output flux
  PLOT_SPECTRUM    =  6,    // disc spectrum
  //---------------------------------------------
  PLOT_CLAW1D      = 11,    // CLAWPack 1D
  PLOT_CLAW2D      = 12,    // CLAWPack 2D
  PLOT_CLAW3D      = 13,    // CLAWPack 3D
  //---------------------------------------------
  PLOT_TEST        = 17,    // testing...
  //---------------------------------------------
  PLOT_SHOW_AEQ    = 21,    // algebraic equation
  PLOT_SHOW_DAE    = 22,    // DAE convergence
  PLOT_SHOW_SCURVE = 23,    // disk S-curves
  PLOT_CANNIZZO    = 24,    // Canizzo disc state
  //---------------------------------------------
  PLOT_GAL_ORBIT   = 30,    // galactic orbit
  PLOT_SHOW_RCS    = 41,    // Hagstrom RCS
  //---------------------------------------------
  PLOT_MATRIX_COLS = 50,    // col-major data, e.g. PDE eigenvalues
  PLOT_MATRIX_ROWS = 51     // row-major data, e.g. PDE eigenvalues
};


enum SLEDGE_VtkRequest
{
  RENDER_VTK_GUI     =  0,
  RENDER_VTK_MESH    =  1,
  RENDER_VTK_FIELD   =  2,
  RENDER_VTK_SCALARS =  3,
  RENDER_VTK_VECTORS =  4,
  UPDATE_VTK_DATASET =  5,
  RENDER_VTK_C6464   = 10,
  RENDER_VS_COL      = 11,
  RENDER_CANNIZZO    = 12
};


enum SLEDGE_SimRequest
{
  // Rebuild Vtk objects to match Cannizzo grid
  SET_CANNIZZO_RADII  = 1,
  SET_CANNIZZO_MDOT   = 2,
  SET_WALID_RADII     = 3
};


enum SLEDGE_UserMode {
  USER_MODE_None      = 0,
  USER_MODE_Define    = 1,
  USER_MODE_Build     = 2,
  USER_MODE_Solve     = 3,
  USER_MODE_Zoom      = 4,
  USER_MODE_Explore   = 5,
  USER_MODE_Element   = 6,
  //-------------------------
  // 2D Geometry
  //-------------------------
  USER_MODE_Rect      = 20,
  USER_MODE_Ellipse   = 21,
  USER_MODE_Vertex    = 22,
  USER_MODE_Hole2D    = 25,
  USER_MODE_Region2D  = 26,
  USER_MODE_Select2D  = 27,
  USER_MODE_Adjust2D  = 28,
  USER_MODE_SetBC2D   = 29,
  //-------------------------
  // 3D Geometry
  //-------------------------
  USER_MODE_Hexagon   = 30,
  USER_MODE_Sphere    = 31,
  USER_MODE_Torus     = 32,
  USER_MODE_Hole3D    = 35,
  USER_MODE_Region3D  = 36,
  USER_MODE_Select3D  = 37,
  USER_MODE_Adjust3D  = 38,
  USER_MODE_SetBC3D   = 39
};


// Used as radio transfer values
enum SLEDGE_VideoMode
{
  MODE_RECORD_AVI   = 0,
  MODE_RECORD_MPEG  = 1,
  MODE_RECORD_WMV   = 2
};


enum SLEDGE_PlotMode2D
{
  MODE_Plot_SINGLE = 0,   // profiles : plot a single data set
  MODE_Plot_MULTI  = 1,   // S-curves : accumulate multiple data sets
  MODE_Plot_BUFFER = 2    // BVI Light: plot subset of data buffer
};



//---------------------------------------------------------
// Toolbar IDs
//---------------------------------------------------------
#define umID_TOOLBAR_MAIN   1100
#define umID_TOOLBAR_PLOT   1101
#define umID_TOOLBAR_WICK   1102
#define umID_TOOLBAR_BBH    1103
#define umID_TOOLBAR_MESH   1104
#define umID_TOOLBAR_USEME  1105


//---------------------------------------------------------
// IDs for Tools in umMainFrame (ID_TOOLBAR_MAIN)
//---------------------------------------------------------
#define umID_SIM            1110
#define umID_LOG            1111
#define umID_ENV            1112
#define umID_DISPL          1113
#define umID_GRAPH          1114
#define umID_EXTRA          1115
#define umID_ABOUT          1116
#define umID_HELP           1117

#define umID_WIZ            1120
#define umID_MENU           1121
#define umID_MOVIE          1122

#define umID_PLOT           1130
#define umID_PLOTS          1131
#define umID_PLOTBUF        1132

#define umID_GNUPLOT        1140
#define umID_USEMe          1141
#define umID_C6464          1142
#define umID_DISC           1143
#define umID_WICK           1144
#define umID_TEXTWIN        1145
#define umID_MESH           1146

#define umID_MENU_OPTIONS   1150
#define umID_MENU_RESET     1151


//---------------------------------------------------------
// IDs for Tools in umUSEMeView (ID_TOOLBAR_USEME)
//---------------------------------------------------------
#define umID_VIEW_RESET         1140
#define umID_SHOW_MESH_CTRLS    1141
#define umID_SHOW_VIZ_CTRLS     1142
#define umID_SHOW_LIGHT_TOOL    1143
#define umID_SHOW_DISC_CTRLS    1144

#define umID_SHOW_USEME_GEOM    1150
#define umID_SHOW_USEME_BDRY    1151
#define umID_SHOW_USEME_MESH    1152

#define umID_SHOW_USEME_FIELD   1155
#define umID_SHOW_USEME_CNTRS   1156
#define umID_SHOW_USEME_SLICE   1157

#define umID_SHOW_USEME_AXES    1160
#define umID_SHOW_USEME_CANVAS  1161
#define umID_SHOW_USEME_WORLD   1162

#define umID_SHOW_USEME_TEXT    1166

//---------------------------------------------------------
// ??
//---------------------------------------------------------
#define umID_BBH_VTK        1190
#define umID_MESH_VTK       1191
#define umID_WICK_VTK       1192
#define umID_USEME_VTK      1193
#define umID_FIELD_VTK      1194

//---------------------------------------------------------
// Common IDs for menus
//---------------------------------------------------------
#define umID_QUIT           1210
#define umID_SAVE           1211
#define umID_CLEAR_LOG      1212
#define umID_TOGGLE_LOG     1213
#define umID_TT_DELAY       1214
#define umID_TT_ENABLE      1215

#define umID_PREFER         1300
#define umID_PROPERTIES     1301
#define umID_TOOLBAR        1310
#define umID_STATBAR        1311

//---------------------------------------------------------
// IDs for PlotView
//---------------------------------------------------------
#define umID_SCALE_MINMAX   1400
#define umID_SCALE_AUTO     1401
#define umID_SCALE_FULL     1402

//---------------------------------------------------------
// IDs for Menu
//---------------------------------------------------------
#define umID_TREE_MENU      1430
#define umID_EDIT_COMMAND   1431
#define umID_EDIT_PROMPT    1432
#define umID_EDIT_VALID     1433
#define umID_COMBO_ANSWER   1434
#define umID_EDIT_CURVAL    1435
#define umID_BTN_ACCEPT     1436

//---------------------------------------------------------
// Common IDs for controls
//---------------------------------------------------------
#define umID_FIND           2000
#define umID_LOAD           2001
#define umID_EDIT           2002
#define umID_SAVEAS         2003
#define umID_WRITE          2004
#define umID_CLEAR          2005

#define umID_FIND_MENU      2010
#define umID_LOAD_MENU      2011
#define umID_EDIT_MENU      2012

#define umID_DELETE_ALL     2020
#define umID_ADD            2021
#define umID_DELETE         2022
#define umID_VERIFY         2023
#define umID_APPLY          2024
#define umID_RESET          2025

#define umID_SLIDER         2041

#define umID_SLIDER_1       2051
#define umID_SLIDER_2       2052
#define umID_SLIDER_3       2053
#define umID_SLIDER_4       2054
#define umID_SLIDER_5       2055

#define umID_COLOR_R        2071
#define umID_COLOR_G        2072
#define umID_COLOR_B        2073

// Standard camera settings
#define umID_CAM_POS_X      2081
#define umID_CAM_POS_Y      2082
#define umID_CAM_POS_Z      2083

#define umID_CAM_NEG_X      2084
#define umID_CAM_NEG_Y      2085
#define umID_CAM_NEG_Z      2086

// User-defined camera settings
#define umID_CAM_USER_1     2091
#define umID_CAM_USER_2     2092
#define umID_CAM_USER_3     2093
#define umID_CAM_USER_4     2094
#define umID_CAM_USER_5     2095
#define umID_CAM_USER_6     2096
#define umID_CAM_USER_7     2097
#define umID_CAM_USER_8     2098

#define umID_GRID_LIGHTS    2100

#define umID_LOG_NOTEBOOK   2300

#define umID_COMBO_BDRY     2510
#define umID_COMBO_SPP      2512
#define umID_COMBO_MENU     2513
#define umID_COMBO_SIM      2514
#define umID_COMBO_P        2515
#define umID_COMBO_Q        2516


// Simulation controls
#define umID_TEXT_PSTEP     2520
#define umID_TEXT_FTIME     2521
#define umID_TEXT_STATUS    2522
#define umID_TREE_FIELD     2525

// Recording sim as movie
#define umID_MOIVIEOPTS     2530
#define umID_MOVIE_NAME     2531
#define umID_SET_NAME       2532
#define umID_MOVIE_BPP      2533
#define umID_MOVIE_FPS      2534
#define umID_RADIO_MODE     2535

// Mesh options
#define umID_TREE_MESH      2540


// ...
// ... grid for BC/facetype colors (RGB )
// ...
// ...
// ...


// Atom grid
#define umID_COMBO_PROF     2550
#define umID_TEXT_TMIN      2551
#define umID_TEXT_TMAX      2552
#define umID_TEXT_WLO       2553
#define umID_TEXT_W_N       2554
#define umID_TEXT_WHI       2555


///////////////////////////////////////////////////////////
//
// Viz pages
//
///////////////////////////////////////////////////////////

#define umID_CHOICE_CATEGORY    2600
#define umID_CHK_VIZ_AUTO       2601
#define umID_CHK_VIZ_WIRE       2602
#define umID_CHK_VIZ_SHOW       2605

//---------------------------------------------------------
// IDD_PAGE_VIZ_ ?  GENERAL
//---------------------------------------------------------
#define umID_CHOICE_INTERP      2610
#define umID_CHOICE_ORDER       2611
#define umID_CHECK_INTERP_TRI   2612
#define umID_CHECK_WIREFRAME    2613
#define umID_CHECK_GRAYSCALE    2614
#define umID_CHECK_LEGEND       2615
#define umID_CHECK_OUTLINE      2616

//---------------------------------------------------------
// IDD_PAGE_VIZ_ ?  COLORMAP
//---------------------------------------------------------
#define umID_TEXT_CMIN      2621
#define umID_TEXT_CMAX      2622
#define umID_TEXT_OPACITY   2623

#define umID_CHK_VIZ_SCALE  2624
#define umID_SLIDER_SCALE   2625
#define umID_TEXT_SCALE     2626

//---------------------------------------------------------
// IDD_PAGE_VIZ_ ?  CONTOUR
//---------------------------------------------------------
#define umID_TEXT_NLEVELS   2631
#define umID_TEXT_CTRMIN    2632
#define umID_TEXT_CTRMAX    2633

//---------------------------------------------------------
// IDD_PAGE_VIZ_SLICE
//---------------------------------------------------------
#define umID_TEXT_X         2640
#define umID_TEXT_Y         2641
#define umID_TEXT_Z         2642
#define umID_TEXT_OPAC_X    2643
#define umID_TEXT_OPAC_Y    2644
#define umID_TEXT_OPAC_Z    2645

#define umID_CHECK_X        2650
#define umID_CHECK_Y        2651
#define umID_CHECK_Z        2652
#define umID_CHECK_CTR_X    2653
#define umID_CHECK_CTR_Y    2654
#define umID_CHECK_CTR_Z    2655


//---------------------------------------------------------
// Message to all Viz pages: HandleNewDataSet()
//---------------------------------------------------------
#define umID_NEW_DATASET    2690


///////////////////////////////////////////////////////////
//
// Mesh Control panel
//
///////////////////////////////////////////////////////////
#define umID_MESH_NOTEBOOK  2700

#define umID_MeshOpts_P1    2701
#define umID_MeshOpts_P2    2702
#define umID_MeshOpts_P3    2703
#define umID_MeshOpts_P4    2704

#define umID_MESHOPTS2D     2712
#define umID_MESHOPTS3D     2713

#define umID_VizOpts_CMAP   2721
#define umID_VizOpts_SLICE  2722
#define umID_VizOpts_P3     2723
#define umID_VizOpts_P4     2724

#define umID_VISIBLE        2730
#define umID_3DW_ON         2731
#define umID_COLOR_GRID     2732



///////////////////////////////////////////////////////////
//
// Simulation Control panel
//
///////////////////////////////////////////////////////////
#define umID_SIM_NOTEBOOK   2800

#define umID_SimCTRL_P1     2801
#define umID_SimCTRL_P2     2802
#define umID_SimCTRL_P3     2803
#define umID_SimCTRL_P4     2804

///////////////////////////////////////////////////////////
//
// Disc Control panel
//
///////////////////////////////////////////////////////////
#define umID_DISC_NOTEBOOK  2810

#define umID_DiscCTRL_P1    2811
#define umID_DiscCTRL_P2    2812


///////////////////////////////////////////////////////////
//
// PDE Wizard
//
///////////////////////////////////////////////////////////
#define umID_WIZ_NOTEBOOK         2900

#define umID_PDE_WIZ_1LAWS        2901
#define umID_PDE_WIZ_2DERIV       2902
#define umID_PDE_WIZ_3BCOND       2903
#define umID_PDE_WIZ_4INITC       2904
#define umID_PDE_WIZ_5PARAM       2905
#define umID_PDE_WIZ_6ANAL        2906

#define umID_CHOICE_WIZ_FILES     2910
#define umID_CHOICE_WIZ_SOLVER    2911
#define umID_CHOICE_WIZ_FLUX      2912
#define umID_CHOICE_WIZ_LIMITER   2913


///////////////////////////////////////////////////////////
//
// Mesh module
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
// Geometry module IDS
//---------------------------------------------------------
#define ID_MESH_VTK_VIEW          3001

#define umID_REFRESH              3010
#define umID_SOURCE               3011
#define umID_EXTRACT              3012

// editing modes
#define umID_MODE_NONE            3100
#define umID_MODE_DESIGN          3101
#define umID_MODE_ADJUST          3102
#define umID_MODE_ZOOM            3104
// 2D modes
#define umID_MODE_SELECT          3110
#define umID_MODE_CIRCLE          3111
#define umID_MODE_RECT            3112
#define umID_MODE_ELLIPSE         3113
#define umID_MODE_LINE            3114
#define umID_MODE_VERTEX          3115
#define umID_MODE_POLY            3116
#define umID_MODE_SPLINE          3117
#define umID_MODE_BEZIER          3118
// 3D modes
#define umID_MODE_HEXAGON         3121
#define umID_MODE_SPHERE          3122
#define umID_MODE_ELLIPSOID       3123
// spare mode IDs
#define umID_MODE_SPARE1          3131
#define umID_MODE_SPARE2          3132
#define umID_MODE_SPARE3          3133
#define umID_MODE_SPARE4          3134
#define umID_MODE_SPARE5          3135
// regions
#define umID_MODE_HOLE            3141
#define umID_MODE_REGION          3142

#define umID_TREE_BDRY_2D         3151
#define umID_TREE_BDRY_3D         3152


//---------------------------------------------------------
// Mesh module control IDS
//---------------------------------------------------------
#define umID_BH_SYS_RADIUS        3510
#define umID_BH_HOLE1_X           3511
#define umID_BH_HOLE2_X           3512
#define umID_GRUMMP_RESOLUTION    3513
#define umID_GRUMMP_GRADATION     3514

#define umID_GEN_MESH_SURF        3520
#define umID_GEN_MESH_GRUMMP      3521
#define umID_GEN_MESH_STOP        3522
#define umID_GEN_MESH_KILL        3523

#define umID_GRID_TREE            3531


///////////////////////////////////////////////////////////
//
// Field module
//
///////////////////////////////////////////////////////////

//---------------------------------------------------------
// Field module menu IDS
//---------------------------------------------------------
#define umID_FIELD_ID_1     4001

//---------------------------------------------------------
// Field module control IDS
//---------------------------------------------------------
#define umID_FIELD_IDC_1    4501


///////////////////////////////////////////////////////////
//
// Simulation module
//
///////////////////////////////////////////////////////////
// NOTE:  #define wxID_LOWEST   4999
// NOTE:  #define wxID_HIGHEST  5999


//---------------------------------------------------------
// Simulation module menu IDS
//---------------------------------------------------------
#define umID_SIM_ID_1       6101

//---------------------------------------------------------
// Simulation module control IDS
//---------------------------------------------------------
#define umID_SIM_WIZ        6501
#define umID_SIM_OPTS       6502
#define umID_SIM_AMR        6503

// simulation thread
#define umID_THREAD_START   6511
#define umID_THREAD_PAUSE   6512
#define umID_THREAD_STOP    6513
#define umID_THREAD_KILL    6514

// movie recording
#define umID_RECORD_START   6521
#define umID_RECORD_PAUSE   6522
#define umID_RECORD_STOP    6523


//---------------------------------------------------------
// Light kit control IDS
//---------------------------------------------------------
#define IDC_LIGHT_0         7000
#define IDC_LIGHT_1         7001
#define IDC_LIGHT_2         7002
#define IDC_LIGHT_3         7003
#define IDC_LIGHT_4         7004
#define IDC_LIGHT_5         7005
#define IDC_LIGHT_6         7006
#define IDC_LIGHT_7         7007

#define IDC_LIGHT_RESET     7011
#define IDC_LIGHT_COLOR     7012


///////////////////////////////////////////////////////////
//
// Wick Binary/Accretion module
//
///////////////////////////////////////////////////////////

//---------------------------------------------------------
// Wick Binary/Accretion module menu IDS
//---------------------------------------------------------

enum {
  SOLVER_Hameury = 0,   // building input for Hameury
  SOLVER_SCurve  = 1    // generating S-Curve data
};


enum {
  SPECTRUM_Walid  = 0,  // Wavelength vs Flux
  SPECTRUM_TLusty = 1   // Frequency vs Flux
};

enum {
  TL_TEST_wavl = 0,     // test plot is against wavelength
  TL_TEST_freq = 1      // test plot is against frequency
};


//---------------------------------------------------------
// Wick Binary/Accretion module control IDS
//---------------------------------------------------------
#define IDC_WICK_M1         7101
#define IDC_WICK_M2         7102
#define IDC_WICK_P          7103
#define IDC_WICK_Q          7104
#define IDC_WICK_RS         7110
#define IDC_WICK_R1         7111
#define IDC_WICK_R2         7112
#define IDC_WICK_NATOMS     7116
#define IDC_WICK_NREF       7117
#define IDC_WICK_GRID       7120

#define IDC_BBH_M1          7201
#define IDC_BBH_M2          7202
#define IDC_BBH_P           7203
#define IDC_BBH_Q           7204
#define IDC_BBH_GRID        7220


#endif  // NDG__Constants_H__INCLUDED
