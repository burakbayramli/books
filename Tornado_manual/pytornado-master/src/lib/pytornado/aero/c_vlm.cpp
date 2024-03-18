/*
  Copyright 2017-2020 Airinnova AB and the PyTornado authors

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/

/*******************************************************************************
PYTHON_C++ WRAPPER FOR VORTEX-LATTICE METHOD CODE

Authors:
* Alessandro Gastaldi
* Aaron Dettmann
*******************************************************************************/

/*
    On the Python/C and Numpy/C API documentation:

    - https://docs.scipy.org/doc/numpy/reference/c-api.html
    - https://docs.scipy.org/doc/numpy/reference/c-api.array.html#array-structure-and-data-access

    - https://docs.python.org/3.6/c-api/
    - https://docs.python.org/3.6/c-api/intro.html

    - here's a good explanation for some of the code that can seem rather magic:
    - http://scipy-cookbook.readthedocs.io/items/C_Extensions_NumPy_arrays.html
*/


#include "c_vlm.h"

// Interface between Python and NumPy
static PyObject* py2c_lattice( PyObject *self, PyObject *args );
static PyObject* py2c_downwash( PyObject *self, PyObject *args );
static PyObject* py2c_boundary( PyObject *self, PyObject *args );
static PyObject* py2c_results( PyObject *self, PyObject *args );

static int get_latticestruct( PyObject* py_lattice, latticestruct* lattice );
static int get_infostruct( PyObject* py_lattice, infostruct* info );
static int get_statestruct( PyObject* py_state, statestruct* state );
static int get_refstruct( PyObject* py_state, refstruct* refs );
static int get_resultstruct( PyObject* py_data, resultstruct* results );

static int set_infostruct( PyObject* py_lattice, infostruct* info );
static int set_resultstruct( PyObject* py_results, resultstruct* results );

// ===== Miscellaneous functions =====
double deg2rad(double deg)
{
    return deg*(PI/180.0);
}

// Normalise a vector v
double* normalise_vector(double* v)
{
    // Note: vector is passed by reference (changes seen outside function)
    double vector_len = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);

    for (int k = 0; k < 3; ++k) {
        v[k] /= vector_len;
    }

    return v;
}

// Return the freestream direction
double* get_freestream_direction(double* freestream_dir, double alpha, double beta)
{
    /*
        Args:
            :freestream_dir: (3x1) array for x, y and z component of freestream direction
            :alpha: angle of attack in RADIANS
            :beta: side slip angle in RADIANS

        Note:
        * See also Drela (2014) for formula
        * 'freestream_dir' always has length 1
     */

    freestream_dir[0] = cos(alpha)*cos(beta);
    freestream_dir[1] = -sin(beta);
    freestream_dir[2] = sin(alpha)*cos(beta);
    return freestream_dir;
}

// ===== LATTICE GENERATION =====
static PyObject* py2c_lattice( PyObject *self, PyObject *args )
{
    // Declare pointer to PYTHON objects (NUMPY arrays, dicts)
    PyObject* py_lattice = NULL;
    PyObject* py_state = NULL;
    PyArrayObject* py_pts = NULL;
    PyArrayObject* py_sym = NULL;
    PyArrayObject* py_pan = NULL;

    // PARSE INPUTS FROM PYTHON
    if (!PyArg_ParseTuple(args, "OOO!O!O!",
                &py_lattice,
                &py_state,
                &PyArray_Type, &py_pts,
                &PyArray_Type, &py_sym,
                &PyArray_Type, &py_pan))
        return NULL;

    latticestruct lattice;
    if (get_latticestruct(py_lattice, &lattice) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    infostruct info;
    if (get_infostruct(py_lattice, &info) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    statestruct state;
    if (get_statestruct(py_state, &state) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    /* GET SEGMENT VERTICES AND PANEL COUNTS *********************************/

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_pts))
    {
        std::cout << "ERROR: array_segments must have FLOAT data type" << std::endl;
    }
    double* pts = (double*)PyArray_DATA(py_pts);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISINTEGER(py_sym))
    {
        std::cout << "ERROR: array_symmetry must have INTEGER data type" << std::endl;
    }
    long int* sym = (long int*)PyArray_DATA(py_sym);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISINTEGER(py_pan))
    {
        std::cout << "ERROR: array_panels must have INTEGER data type" << std::endl;
    }
    long int* pan = (long int*)PyArray_DATA(py_pan);

    // std::cout << "NUMBER OF SEGMENTS : " << info.num_seg << std::endl;
    // std::cout << "NUMBER OF PANELS : " << info.num_pan << std::endl;

    // RUN LATTICE GENERATION ROUTINE
    // Send to actual C CODE
    vlm_lattice(&lattice, &info, &state, pts, sym, pan);

    // SET LATTICE QUALITY CONTROLS IN PYDICT
    if (set_infostruct(py_lattice, &info) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    // Clean up reference counts before leaving C
    Py_INCREF(Py_None);
    return Py_None;
}

// ===== DOWNWASH FACTOR CALCULATION =====
static PyObject* py2c_downwash( PyObject *self, PyObject *args )
{
    // Declare pointer to PYTHON objects (NUMPY arrays, dicts)
    PyObject* py_lattice = NULL;
    PyArrayObject* py_dw = NULL;

    // PARSE INPUTS FROM PYTHON
    if (!PyArg_ParseTuple(args, "OO!",
                &py_lattice,
                &PyArray_Type, &py_dw ))
        return NULL;

    double* dw = (double*)PyArray_DATA(py_dw);

    latticestruct lattice;
    if (get_latticestruct(py_lattice, &lattice) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    infostruct info;
    if (get_infostruct(py_lattice, &info) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    // Send to actual C CODE
    vlm_downwash(&lattice, dw, info.num_pan);

    // Clean up reference counts before leaving C
    Py_INCREF(Py_None);
    return Py_None;
}

// ===== BOUNDARY CONDITION (RHS) =====
static PyObject* py2c_boundary( PyObject *self, PyObject *args )
{
    // Declare pointer to PYTHON objects (NUMPY arrays, dicts)
    PyObject* py_lattice = NULL;
    PyObject* py_state = NULL;
    PyArrayObject* py_rhs = NULL;

    // PARSE INPUTS FROM PYTHON
    if (!PyArg_ParseTuple(args, "OOO!",
                &py_lattice,
                &py_state,
                &PyArray_Type, &py_rhs ))
        return NULL;

    double* rhs = (double*)PyArray_DATA(py_rhs);

    latticestruct lattice;
    if (get_latticestruct(py_lattice, &lattice) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    infostruct info;
    if (get_infostruct(py_lattice, &info) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    statestruct state;
    if (get_statestruct(py_state, &state) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    refstruct refs;
    if (get_refstruct(py_state, &refs) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    // Send to actual C CODE (no more magic required in the rest of the code)
    vlm_boundary(&lattice, &state, &refs, rhs, info.num_pan);

    // Clean up reference counts before leaving C
    Py_INCREF(Py_None);
    return Py_None;
}

// INWASH CALCULATION
static PyObject* py2c_results( PyObject *self, PyObject *args )
{
    // Declare pointer to PYTHON objects (NUMPY arrays, dicts)
    PyObject* py_lattice = NULL;
    PyObject* py_state = NULL;
    PyObject* py_results = NULL;

    // PARSE INPUTS FROM PYTHON
    if (!PyArg_ParseTuple(args, "OOO",
                &py_lattice,
                &py_state,
                &py_results ))
        return NULL;

    latticestruct lattice;
    if (get_latticestruct(py_lattice, &lattice) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    infostruct info;
    if (get_infostruct(py_lattice, &info) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    statestruct state;
    if (get_statestruct(py_state, &state) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    refstruct refs;
    if (get_refstruct(py_state, &refs) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    resultstruct results;
    if (get_resultstruct(py_results, &results) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    // Send to actual C CODE (no more magic required in the rest of the code)
    vlm_results(&lattice, &state, &refs, &results, info.num_pan);

    if (set_resultstruct(py_results, &results) != ERR_SUCCESS)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    // Clean up reference counts before leaving C
    Py_INCREF(Py_None);
    return Py_None;
}

// ===== PyOBJECT INTERPRETERS =====
static int get_latticestruct( PyObject* py_lattice, latticestruct* lattice )
{
    PyArrayObject* py_p = NULL;
    PyArrayObject* py_v = NULL;
    PyArrayObject* py_c = NULL;
    PyArrayObject* py_bound_leg_midpoints = NULL;
    PyArrayObject* py_n = NULL;
    PyArrayObject* py_a = NULL;
    double epsilon;

    // GET LATTICE ATTRIBUTES (NUMPY ARRAYS)

    // Get NUMPY array of panel corner points
    if (!PyObject_HasAttrString(py_lattice, "p"))
    {
        std::cout << "ERROR: lattice does not have attribute 'p'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    else if (!PyArray_Check(PyObject_GetAttrString(py_lattice, "p")))
    {
        std::cout << "ERROR: lattice.p is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_p = (PyArrayObject*)PyObject_GetAttrString(py_lattice, "p");

    // Get NUMPY array of panel vortex segment endpoints
    if (!PyObject_HasAttrString(py_lattice, "v"))
    {
        std::cout << "ERROR: lattice does not have attribute 'v'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    else if (!PyArray_Check(PyObject_GetAttrString(py_lattice, "v")))
    {
        std::cout << "ERROR: lattice.v is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_v = (PyArrayObject*)PyObject_GetAttrString(py_lattice, "v");

    // Get NUMPY array of panel collocation points
    if (!PyObject_HasAttrString(py_lattice, "c"))
    {
        std::cout << "ERROR: lattice does not have attribute 'c'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    else if (!PyArray_Check(PyObject_GetAttrString(py_lattice, "c")))
    {
        std::cout << "ERROR: lattice.c is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_c = (PyArrayObject*)PyObject_GetAttrString(py_lattice, "c");

    // Get NUMPY array of bound leg midpoint
    if (!PyObject_HasAttrString(py_lattice, "bound_leg_midpoints"))
    {
        std::cout << "ERROR: lattice does not have attribute 'bound_leg_midpoints'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    else if (!PyArray_Check(PyObject_GetAttrString(py_lattice, "bound_leg_midpoints")))
    {
        std::cout << "ERROR: lattice.bound_leg_midpoints is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_bound_leg_midpoints = (PyArrayObject*)PyObject_GetAttrString(py_lattice, "bound_leg_midpoints");

    // Get NUMPY array of panel normal vectors
    if (!PyObject_HasAttrString(py_lattice, "n"))
    {
        std::cout << "ERROR: lattice does not have attribute 'n'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    else if (!PyArray_Check(PyObject_GetAttrString(py_lattice, "n")))
    {
        std::cout << "ERROR: lattice.n is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_n = (PyArrayObject*)PyObject_GetAttrString(py_lattice, "n");

    // Get NUMPY array of panel corner points
    if (!PyObject_HasAttrString(py_lattice, "a"))
    {
        std::cout << "ERROR: lattice does not have attribute 'a'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    else if (!PyArray_Check(PyObject_GetAttrString(py_lattice, "a")))
    {
        std::cout << "ERROR: lattice.a is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_a = (PyArrayObject*)PyObject_GetAttrString(py_lattice, "a");

    // Get epsilon
    if (!PyObject_HasAttrString(py_lattice, "epsilon"))
    {
        std::cout << "ERROR: lattice does not have attribute 'epsilon'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    else if (!PyFloat_Check(PyObject_GetAttrString(py_lattice, "epsilon")))
    {
        std::cout << "ERROR: lattice.epsilon is not a FLOAT" << std::endl;
        return ERR_TYPE;
    }
    epsilon = PyFloat_AsDouble(PyObject_GetAttrString(py_lattice, "epsilon"));

    // ===== GET LATTICE ATTRIBUTES (DATA) =====

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_p))
    {
        std::cout << "ERROR: lattice.p must have FLOAT data type" << std::endl;
        return ERR_TYPE;
    }
    lattice->P = (double*)PyArray_DATA(py_p);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_v))
    {
        std::cout << "ERROR: lattice.v must have FLOAT data type" << std::endl;
        return ERR_TYPE;
    }
    lattice->V = (double*)PyArray_DATA(py_v);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_c))
    {
        std::cout << "ERROR: lattice.c must have FLOAT data type" << std::endl;
        return ERR_TYPE;
    }
    lattice->C = (double*)PyArray_DATA(py_c);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_bound_leg_midpoints))
    {
        std::cout << "ERROR: lattice.bound_leg_midpoints must have FLOAT data type" << std::endl;
        return ERR_TYPE;
    }
    lattice->BoundLegMidpoint = (double*)PyArray_DATA(py_bound_leg_midpoints);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_n))
    {
        std::cout << "ERROR: lattice.n must have FLOAT data type" << std::endl;
        return ERR_TYPE;
    }
    lattice->N = (double*)PyArray_DATA(py_n);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_a))
    {
        std::cout << "ERROR: lattice.a must have FLOAT data type" << std::endl;
        return ERR_TYPE;
    }
    lattice->A = (double*)PyArray_DATA(py_a);

    lattice->EPSILON = (double)(epsilon);

    return ERR_SUCCESS;
}

static int get_infostruct( PyObject* py_lattice, infostruct* info )
{
    PyObject* py_attr = NULL;
    PyObject* py_dict = NULL;
    PyObject* py_key = NULL;
    PyObject* py_val = NULL;

    // GET LATTICE CONTROLS DICT

    // Get dict attribute 'info' of VLMLattice
    if (!PyObject_HasAttrString(py_lattice, "info"))
    {
        std::cout << "ERROR: lattice does not have attribute 'info'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    py_attr = PyObject_GetAttrString(py_lattice, "info");
    py_dict = py_attr;

    // GET LATTICE CONTROLS DATA

    py_key = PyUnicode_FromString("num_panels");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'num_panels' in lattice.info" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyLong_Check(py_val))
    {
        std::cout << "ERROR: 'num_panels' in lattice.info must be INT" << std::endl;
        return ERR_TYPE;
    }

    // Get number of panels
    info->num_pan = PyLong_AsLong(py_val);

    py_key = PyUnicode_FromString("num_segments");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'num_segments' in lattice.info" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyLong_Check(py_val))
    {
        std::cout << "ERROR: 'num_segments' in lattice.info must be INT" << std::endl;
        return ERR_TYPE;
    }

    // Get number of segments
    info->num_seg = PyLong_AsLong(py_val);

    return ERR_SUCCESS;
}

static int set_infostruct( PyObject* py_lattice, infostruct* info )
{
    PyObject* py_attr = NULL;
    PyObject* py_dict = NULL;
    PyObject* py_key = NULL;

    // GET LATTICE CONTROLS DICT

    // Get dict attribute 'info' of VLMLattice
    if (!PyObject_HasAttrString(py_lattice, "info"))
    {
        std::cout << "ERROR: lattice does not have attribute 'info'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    py_attr = PyObject_GetAttrString(py_lattice, "info");
    py_dict = py_attr;

    // SET LATTICE CONTROLS DATA

    // Set minimum panel area
    py_key = PyUnicode_FromString("area_min");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'area_min' in lattice.info" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(info->area_min));

    // Set maximum panel area
    py_key = PyUnicode_FromString("area_max");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'area_max' in lattice.info" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(info->area_max));

    // Set average panel area
    py_key = PyUnicode_FromString("area_avg");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'area_avg' in lattice.info" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(info->area_avg));

    // Set minimum panel aspect ratio
    py_key = PyUnicode_FromString("aspect_min");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'aspect_min' in lattice.info" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(info->aspect_min));

    // Set maximum panel aspect ratio
    py_key = PyUnicode_FromString("aspect_max");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'aspect_max' in lattice.info" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(info->aspect_max));

    // Set average panel aspect ratio
    py_key = PyUnicode_FromString("aspect_avg");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'aspect_avg' in lattice.info" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(info->aspect_avg));

    return ERR_SUCCESS;
}

static int get_statestruct( PyObject* py_state, statestruct* state )
{
    PyObject* py_attr = NULL;
    PyObject* py_dict = NULL;
    PyObject* py_key = NULL;
    PyObject* py_val = NULL;

    // GET AERODYNAMIC DATA DICT

    // Get dict attribute 'aero' of FlightState
    if (!PyObject_HasAttrString(py_state, "aero"))
    {
        std::cout << "ERROR: state does not have attribute 'aero'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    py_attr = PyObject_GetAttrString(py_state, "aero");
    py_dict = py_attr;

    // GET AERODYNAMIC OPERATING CONDITIONS (PY DICT)

    py_key = PyUnicode_FromString("airspeed");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'airspeed' in state.aero" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'airspeed' in state.aero must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    // Get value of airspeed
    state->airspeed = PyFloat_AsDouble(py_val);

    py_key = PyUnicode_FromString("alpha");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'alpha' in state.aero" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'alpha' in state.aero must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    // Get value of alpha
    state->alpha = PyFloat_AsDouble(py_val);

    py_key = PyUnicode_FromString("beta");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'beta' in state.aero" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'beta' in state.aero must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    // Get value of beta
    state->beta = PyFloat_AsDouble(py_val);

    py_key = PyUnicode_FromString("rate_P");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'rate_P' in state.aero" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'rate_P' in state.aero must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    // Get value of alpha
    state->P = PyFloat_AsDouble(py_val);

    py_key = PyUnicode_FromString("rate_Q");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'rate_Q' in state.aero" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'rate_Q' in state.aero must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    // Get value of alpha
    state->Q = PyFloat_AsDouble(py_val);

    py_key = PyUnicode_FromString("rate_R");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'rate_R' in state.aero" << std::endl;
        return ERR_KEY;
    }

    // Get value of alpha
    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'rate_R' in state.aero must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    state->R = PyFloat_AsDouble(py_val);

    py_key = PyUnicode_FromString("density");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'density' in state.aero" << std::endl;
        return ERR_KEY;
    }

    // Get value of alpha
    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'density' in state.aero must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    state->rho = PyFloat_AsDouble(py_val);

    return ERR_SUCCESS;
}

static int get_refstruct( PyObject* py_state, refstruct* refs )
{
    PyObject* py_attr = NULL;
    PyObject* py_dict = NULL;

    PyObject* py_key = NULL;
    PyObject* py_val = NULL;

    /* GET REFERENCE VALUES (PY DICT) ****************************************/

    // Get dict attribute 'refs' of FlightState
    if (!PyObject_HasAttrString(py_state, "refs"))
    {
        std::cout << "ERROR: state does not have attribute 'refs'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    py_attr = PyObject_GetAttrString(py_state, "refs");
    py_dict = py_attr;

    /* GET REFERENCE POINT AND CENTER OF GRAVITY (NUMPY ARRAY) ***************/

    py_key = PyUnicode_FromString("area");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'area' in state.refs" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'area' in state.refs must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    // Get value of reference area
    refs->area = PyFloat_AsDouble(py_val);

    py_key = PyUnicode_FromString("span");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'span' in state.refs" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'span' in state.refs must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    // Get value of reference span
    refs->span = PyFloat_AsDouble(py_val);

    py_key = PyUnicode_FromString("chord");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'chord' in state.refs" << std::endl;
        return ERR_KEY;
    }

    py_val = PyDict_GetItem(py_dict, py_key);
    if (!PyFloat_Check(py_val))
    {
        std::cout << "ERROR: 'chord' in state.refs must be FLOAT" << std::endl;
        return ERR_TYPE;
    }

    // Get value of reference chord
    refs->chord = PyFloat_AsDouble(py_val);

    PyArrayObject* py_CG = NULL;
    PyArrayObject* py_RP = NULL;

    py_key = PyUnicode_FromString("gcenter");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'gcenter' in state.refs" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'gcenter' in state.refs is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_CG = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("rcenter");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'rcenter' in state.refs" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'rcenter' in state.refs is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_RP = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    /* GET REFERENCE POINT AND CENTER OF GRAVITY (DATA) **********************/

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_CG))
    {
        std::cout << "ERROR: state.gcenter must have FLOAT data type" << std::endl;
        return ERR_TYPE;
    }
    refs->CG = (double*)PyArray_DATA(py_CG);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_RP))
    {
        std::cout << "ERROR: state.rcenter must have FLOAT data type" << std::endl;
        return ERR_TYPE;
    }
    refs->RP = (double*)PyArray_DATA(py_RP);

    return ERR_SUCCESS;
}

static int get_resultstruct( PyObject* py_data, resultstruct* results )
{
    PyObject* py_attr = NULL;
    PyObject* py_dict = NULL;
    PyObject* py_key = NULL;

    // GET PANEL-WISE DATA DICT

    // Get dict attribute 'panelwise' of VLMData
    if (!PyObject_HasAttrString(py_data, "panelwise"))
    {
        std::cout << "ERROR: vlmdata does not have attribute 'panelwise'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    py_attr = PyObject_GetAttrString(py_data, "panelwise");
    py_dict =py_attr;

    // GET PANEL-WISE DATA (NUMPY ARRAYS)

    PyArrayObject* py_gamma = NULL;

    PyArrayObject* py_iwx = NULL;
    PyArrayObject* py_iwy = NULL;
    PyArrayObject* py_iwz = NULL;
    PyArrayObject* py_iwmag = NULL;

    PyArrayObject* py_fx = NULL;
    PyArrayObject* py_fy = NULL;
    PyArrayObject* py_fz = NULL;
    PyArrayObject* py_fmag = NULL;

    PyArrayObject* py_cp = NULL;

    py_key = PyUnicode_FromString("gamma");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'gamma' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'gamma' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_gamma = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("vx");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'vx' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'vx' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_iwx = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("vy");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'vy' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'vy' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_iwy = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("vz");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'vz' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'vz' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_iwz = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("vmag");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'vmag' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'vmag' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_iwmag = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("fx");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'fx' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'fx' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_fx = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("fy");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'fy' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'fy' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_fy = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("fz");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'fz' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'fz' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_fz = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("fmag");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'fmag' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'fmag' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_fmag = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    py_key = PyUnicode_FromString("cp");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'cp' in vlmdata.panelwise" << std::endl;
        return ERR_KEY;
    }
    else if (!PyArray_Check(PyDict_GetItem(py_dict, py_key)))
    {
        std::cout << "ERROR: 'cp' in vlmdata.panelwise is not a NUMPY array" << std::endl;
        return ERR_TYPE;
    }
    py_cp = (PyArrayObject*)PyDict_GetItem(py_dict, py_key);

    // GET PANEL-WISE DATA (DATA)

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_gamma))
    {
        std::cout << "ERROR: 'gamma' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->gamma = (double*)PyArray_DATA(py_gamma);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_iwx))
    {
        std::cout << "ERROR: 'vx' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->iw_x = (double*)PyArray_DATA(py_iwx);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_iwy))
    {
        std::cout << "ERROR: 'vy' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->iw_y = (double*)PyArray_DATA(py_iwy);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_iwz))
    {
        std::cout << "ERROR: 'vz' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->iw_z = (double*)PyArray_DATA(py_iwz);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_iwmag))
    {
        std::cout << "ERROR: 'vmag' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->iw_mag = (double*)PyArray_DATA(py_iwmag);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_fx))
    {
        std::cout << "ERROR: 'fx' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->fpan_x = (double*)PyArray_DATA(py_fx);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_fy))
    {
        std::cout << "ERROR: 'fy' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->fpan_y = (double*)PyArray_DATA(py_fy);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_fz))
    {
        std::cout << "ERROR: 'fz' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->fpan_z = (double*)PyArray_DATA(py_fz);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_fmag))
    {
        std::cout << "ERROR: 'fmag' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->fpan_mag = (double*)PyArray_DATA(py_fmag);

    // Get pointer to contiguous DATA block of NUMPY array
    if (!PyArray_ISFLOAT(py_cp))
    {
        std::cout << "ERROR: 'cp' in vlmresults.panelwise must have FLOAT results type" << std::endl;
        return ERR_TYPE;
    }
    results->cp = (double*)PyArray_DATA(py_cp);

    return ERR_SUCCESS;
}

static int set_resultstruct( PyObject* py_data, resultstruct* results )
{
    PyObject* py_attr = NULL;
    PyObject* py_dict = NULL;
    PyObject* py_key = NULL;

    // GET AERODYNAMIC FORCES DICT

    // Get dict attribute 'forces' of VLMData
    if (!PyObject_HasAttrString(py_data, "forces"))
    {
        std::cout << "ERROR: vlmdata does not have attribute 'forces'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    py_attr = PyObject_GetAttrString(py_data, "forces");
    py_dict = py_attr;

    // SET AERODYNAMIC FORCES

    // Get total x-directional force
    py_key = PyUnicode_FromString("x");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'x' in vlmdata.forces" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->ftot_x));

    // Get total y-directional force
    py_key = PyUnicode_FromString("y");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'y' in vlmdata.forces" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->ftot_y));

    // Get total z-directional force
    py_key = PyUnicode_FromString("z");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'z' in vlmdata.forces" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->ftot_z));

    // Get total drag force
    py_key = PyUnicode_FromString("D");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'D' in vlmdata.forces" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->ftot_D));

    // Get total side force
    py_key = PyUnicode_FromString("C");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'C' in vlmdata.forces" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->ftot_C));

    // Get total lift force
    py_key = PyUnicode_FromString("L");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'L' in vlmdata.forces" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->ftot_L));

    // Get total x-directional moment
    py_key = PyUnicode_FromString("l");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'l' in vlmdata.forces" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->mtot_x));

    // Get total y-directional moment
    py_key = PyUnicode_FromString("m");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'm' in vlmdata.forces" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->mtot_y));

    // Get total z-directional moment
    py_key = PyUnicode_FromString("n");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'n' in vlmdata.forces" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->mtot_z));

    // GET AERODYNAMIC COEFFICIENTS DICT

    // Get dict attribute 'coeffs' of VLMData
    if (!PyObject_HasAttrString(py_data, "coeffs"))
    {
        std::cout << "ERROR: vlmdata does not have attribute 'coeffs'" << std::endl;
        return ERR_ATTRIBUTE;
    }
    py_attr = PyObject_GetAttrString(py_data, "coeffs");
    py_dict = py_attr;

    // SET AERODYNAMIC COEFFICIENTS

    // Get total x-directional force coefficient
    py_key = PyUnicode_FromString("x");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'x' in vlmdata.coeffs" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->Cx));

    // Get total y-directional force coefficient
    py_key = PyUnicode_FromString("y");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'y' in vlmdata.coeffs" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->Cy));

    // Get total z-directional force coefficient
    py_key = PyUnicode_FromString("z");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'z' in vlmdata.coeffs" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->Cz));

    // Get total drag force coefficient
    py_key = PyUnicode_FromString("D");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'D' in vlmdata.coeffs" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->CD));

    // Get total side force coefficient
    py_key = PyUnicode_FromString("C");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'C' in vlmdata.coeffs" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->CC));

    // Get total lift force coefficient
    py_key = PyUnicode_FromString("L");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'L' in vlmdata.coeffs" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->CL));

    // Get total x-directional moment coefficient
    py_key = PyUnicode_FromString("l");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'l' in vlmdata.coeffs" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->Cl));

    // Get total y-directional moment coefficient
    py_key = PyUnicode_FromString("m");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'm' in vlmdata.coeffs" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->Cm));

    // Get total z-directional moment coefficient
    py_key = PyUnicode_FromString("n");
    if (!PyDict_Contains(py_dict, py_key))
    {
        std::cout << "ERROR: no key 'n' in vlmdata.coeffs" << std::endl;
        return ERR_KEY;
    }
    PyDict_SetItem(py_dict, py_key, PyFloat_FromDouble(results->Cn));

    return ERR_SUCCESS;
}

// ===== FUNCTIONS FOR INTEGRATION IN PYTHON =====
// ===============================================

/* DOCSTRING */
// define "docstring" (in-Python documentation) for entire C module
static char c_vlm_docstr[] =
    "Contains C/C++ routines for Vortex-Lattice Method calculations\n";

// define "docstring" (in-Python documentation) for current code
static char py2c_lattice_docstr[] =
    "Interfaces the C/C++ lattice generation with Python\n";
static char py2c_downwash_docstr[] =
    "Interfaces the C/C++ downwash factor calculation with Python\n";
static char py2c_boundary_docstr[] =
    "Interfaces the C/C++ right-hand-side term calculation with Python\n";
static char py2c_results_docstr[] =
    "Interfaces the C/C++ VLM results calculation with Python\n";

// ===== Method mapping table (see Python docs) =====
// map function name to corresponding code, expected arguments and docstring
// NULL entry serves as delimiter (end of information)
static PyMethodDef c_vlmMethods[] = {
    { "py2c_lattice", py2c_lattice, METH_VARARGS, py2c_lattice_docstr},
    { "py2c_downwash", py2c_downwash, METH_VARARGS, py2c_downwash_docstr},
    { "py2c_boundary", py2c_boundary, METH_VARARGS, py2c_boundary_docstr},
    { "py2c_results", py2c_results, METH_VARARGS, py2c_results_docstr},
    {NULL, NULL, 0, NULL}   /* sentinel */
};

// ===== Module definition =====
static struct PyModuleDef c_vlm = {
    PyModuleDef_HEAD_INIT,  /* m_base */
    "c_vlm",                /* m_name */
    c_vlm_docstr,           /* m_doc */
    -1,                     /* m_size */
    c_vlmMethods            /* m_methods */
};

// ===== Initialiser function =====
PyMODINIT_FUNC PyInit_c_vlm(void)
{
    import_array() ; // numpy routine
    return PyModule_Create(&c_vlm);
}
