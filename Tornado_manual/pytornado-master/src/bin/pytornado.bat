:: ==============================
:: Windows launcher for PyTornado
:: ==============================
::
:: ----------------------------------------------------------------------
:: Copyright 2017-2020 Airinnova AB and the PyTornado authors
::
:: Licensed under the Apache License, Version 2.0 (the "License");
:: you may not use this file except in compliance with the License.
:: You may obtain a copy of the License at
::
::     http://www.apache.org/licenses/LICENSE-2.0
::
:: Unless required by applicable law or agreed to in writing, software
:: distributed under the License is distributed on an "AS IS" BASIS,
:: WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
:: See the License for the specific language governing permissions and
:: limitations under the License.
:: ----------------------------------------------------------------------
::
:: Author: Aaron Dettmann
::
:: This BAT file allows PyTornado to be run from the Windows command
:: line as 'pytornado'. All command line arguments are forwarded to
:: the Python script.
::
:: | Reason for using this BAT file:
:: |
:: | --> Windows seems to require a file extension "*.py" in order to
:: | correctly start a Python script (without preceding 'python.exe').
:: | BUT, a BAT file can be called as 'bat_file_name' without explicit
:: | file extension.
:: |
:: | --> Windows is easily confused with the system paths for libraries
:: | and executables. Having a library called 'abc' and an executable
:: | file called 'abc' can lead to a 'ModuleNotFoundError'
:: |   - See also https://stackoverflow.com/a/44339624

:: Turn off command echoing feature
@echo off

_pytornado_exe.py %*
