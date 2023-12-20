#
from steelpy import Spreadsheet
from steelpy import Formulas
from steelpy import Units
from steelpy import Materials
from steelpy import Sections
from steelpy import CodeCheck
#


units = Units()
#
def run_lms():
    """ """
    #
    #wb_name = 'LMS_tool_COND.xlsm'
    wb_name = 'C:\Temp\python\steelpy\steelpy\GA_clamp.xlsm'
    #
    sp = Spreadsheet()
    wb = sp.read_book(wb_name)
    #sheets = wb.sheet_names
    #
    ws_main = wb.sheets["Main"]
    #
    #
    # -----------------------------------
    # Define sections
    sect = ws_main['I8:M9']
    section = Sections()
    #
    Dp = sect[1][0] * units.mm
    tp = sect[1][1] * units.mm
    section = Sections()
    section["TUB"] = ['Tubular', Dp, tp]
    #
    # -----------------------------------
    # define material
    mat = ws_main['I15:N16']
    SMYS = mat[1][0] * units.MPa
    SMTS = mat[1][1] * units.MPa
    #E = mat[1][2] * units.MPa
    #alpha = (1.170E-05 + 1/273.15 ) / units.K
    #
    material = Materials()
    material["X65"] = ['elastic', SMYS, SMTS]
    #
    #
    #
    # Clamp check section
    forces = clamp(wb, material["X65"], section["TUB"])
    #
    #
    # -----------------------------------
    #
    wbw = sp.write_book(wb_name)
    ws_res = wbw.sheets["Ring_Forces"]
    ws_res.dataframe['A1'] = forces
    #wbw.save()
    #
    # -----------------------------------
    #
    # Piping check section
    piping(wb, material["X65"], section["TUB"])
    #
    #data = ws_main.key(column="Geometry", row="Data")    
    #
    print('----')

#
def piping(wb, material, section):
    """ """
    #
    ws_main = wb.sheets["Main"]
    # -----------------------------------
    #
    # Piping check section
    #
    code = CodeCheck()
    pipe = code.pipeline("PD8010")
    #
    pipe.section = section
    pipe.material = material
    #    
    # -----------------------------------
    # Main section    
    #
    header = ws_main['I4:N5']
    design_cond = header[1][0]
    method = header[1][1]
    location = header[1][2]
    pipe_type = header[1][3]
    #
    pipe.pipe_type = pipe_type
    #
    # -----------------------------------
    # Pressure section
    #
    pressure = ws_main['I26:M28']
    # operational
    if pressure[2][0]:
        Pi = float(pressure[2][0]) * units.bar
        try:
            Po = float(pressure[2][1]) * units.bar
        except:
            Po = 0 * units.bar
    #
    # hydrostatic
    if pressure[2][2]:
        Hw = float(pressure[2][2]) * units.m
        Tw = float(pressure[2][3]) * units.sec
        d  = float(pressure[2][4]) * units.m
    #
    pipe.pressure = [Pi, Po]
    #
    # -----------------------------------
    # Temperature section
    temp = ws_main['I22:M24']
    T = (float(temp[2][0]) + 273.15)* units.K
    #
    pipe.temperature = T
    #
    #
    # -----------------------------------
    # Functional load section
    #
    lfunc = ws_main['I30:M32']
    Fa = float(lfunc[2][0]) * units.kN
    Fs = float(lfunc[2][1]) * units.kN
    Mb = float(lfunc[2][2]) * units.kN*units.m
    Mt = float(lfunc[2][3]) * units.kN*units.m
    #
    pipe.functional_load = [Fa, Fs, Mb, Mt] 
    #
    # -----------------------------------
    #
    #
    pipe.get_results()    
#
def clamp(wb, material, section):
    """ """
    #
    ws_main = wb.sheets["Main"]
    ring_data = ws_main['H47:K49']
    #
    ws_torque = wb.sheets["Torque"]
    ring_force =  ws_torque.dataframe()
    #col = ws_torque.column['C']
    #ring_force
    #    
    # -----------------------------------
    #
    formulas = Formulas()
    #
    ctop = ring_data[1][2]
    theta_top = ring_data[1][3] * units.deg
    #
    cbottom = ring_data[2][2]
    theta_bottom = ring_data[2][3] * units.deg
    #
    ring = formulas.ring(72)
    ring.material = material #["X65"]
    ring.geometry = section #["TUB"]    
    #
    resf = []
    for step, item in enumerate(ring_force['Fseal']):
        P = item * units.kN
        ring.load[f'top_{step}'] = [ctop, -1*P, theta_top, 180* units.deg]
        ring.load[f'bottom_{step}'] = [cbottom, P, theta_bottom, 0* units.deg]
    #
    forces = ring.radial_forces()
    for i in range(step+1):
        resf.append([forces[f'top_{i}']['M'] + forces[f'bottom_{i}']['M'],
                     forces[f'top_{i}']['N'] + forces[f'bottom_{i}']['N'],
                     forces[f'top_{i}']['V'] + forces[f'bottom_{i}']['V']])
    #
    columns = []
    for items in resf:
        columns.append([item.maxabs() for item in items])
    #
    title = ['M', 'N', 'V']
    columns = list(zip(*columns))
    return {title[x]: col for x, col in enumerate(columns)}
        
#
#
def print_lms():
    """ """
    #import os
    #import datetime
    #
    #lms = LMStool()
    wb_name = 'LMS_tool.xlsm'
    #wb_name = 'LMS_tool_test.xlsm'
    #
    #lms.input_excel(wb_name=wb_name, ws_name='STAB_P08')
    #
    #lms.print_report()
    #
#
if __name__ == "__main__":
    #
    run_lms()
    #print_lms()