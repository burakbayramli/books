#
# Copyright (c) 2009 steelpy
#
# Python stdlib imports
from __future__ import annotations

# package imports
import steelpy.utils.io_module.text as common
from steelpy.utils.units.buckingham import Number
#from steelpy.utils.units.main import Units

# --------------------------------------------------------------------
#                            Modify material
# --------------------------------------------------------------------
#
def material_set_mean(materials, fy_mod:float=1.10):
    """
    """
    print('    * Modifiying materials Fy by {:}'.format(fy_mod))
    # increase Fy to mean
    for mat in materials.values():
        mat.Fy = mat.Fy * fy_mod
    #
    #return materials
    #
#
#
# material section
#
def new_materialXX(materials, new_mat, 
                 material_id:str|int='name',
                 material_number:int|None=None):
    """
    Add new material to exisitng fe model materail
    """
    print('    * Adding new materials by {:}'.format(material_id))
    #
    #
    if not material_number:
        _mat_list = [_mat.number for _mat in materials.values()]
        material_number = max(_mat_list)
    
    for _mat in new_mat.values():
        if material_id != 'name':
            material_number = int(float(_mat.number))
            _mat_name = 'new_material_' + str(_mat.number)
            #material_number = _mat.number
        else:
            _mat_name = _mat['name']
            material_number += 1
        
        #materials[_mat_name] = material.Material(_mat_name, material_number)
        materials[_mat_name] = 'Plastic'
        materials[_mat_name].number = material_number
        materials[_mat_name].Fy = float(_mat['Fy'])
        materials[_mat_name].E = float(_mat['E'])
        materials[_mat_name].poisson = float(_mat['poisson'])
        materials[_mat_name].density = float(_mat['density'])
        #materials[_mat_name].type = 'Plastic'

        if materials[_mat_name].Fy == 0:
            materials[_mat_name].type = 'Elastic'
        
        try:
            materials[_mat_name].Fu = _mat['Fu']
        except KeyError:
            pass
    #
    #
    #return materials
    #


#
# Jelly section
def add_mod_material(material_type, materials, elements, sets,
                     _member_no, _group_no, _factor,
                     material_number:int|None=None):
    """
    Create new material with modified elastic modulus and assing it to
    defined elements\n
    
    Input :\n
    materials  : materials in fe model\n
    elements   : elements in fe model\n
    sets       : sets in fe model\n
    _member_no : list with member's fe number\n
    _group_no  : list with set's fe number\n
    _factor    : factor to modify Elastic modulus (E = E/factor)\n
    """
    # print('    * Adding new jelly materials')
    #
    #
    _new_mat = []
    _member_items = []

    # find member's material
    for _item in _member_no:
        try:
            _new_mat.append(elements[_item].material[0].name.lower())
            _member_items.append(_item)
        except KeyError:
            continue

    # find member's material in groups
    for _set_no in _group_no:
        # Find materials
        for _item in sets[int(_set_no)].items:
            try:
                _new_mat.append(elements[_item].material[0].name.lower())
                _member_items.append(_item)
            except KeyError:
                continue
    #
    mat_name = {}
    #mat_number = {}
    #material_number = 0
    for key, _mat in materials.items():
        #mat_number[_mat.number] = key
        mat_name[str(_mat.name.lower())] = key
    #
    if not material_number:
        _mat_list = [_mat.number for _mat in materials.values()]
        material_number = max(_mat_list)    

    # filter materials
    _new_mat = list(set(_new_mat))

    # create new modify materials
    _mat_residual = []
    for _mat in _new_mat:
        _mat_name = mat_name[_mat]
        material_number += 1
        
        if material_type == 'jelly':
            _new_name = materials[_mat_name].name + '_jelly_' + str(material_number)
            materials[_new_name] = material.Material(_new_name, material_number)
            #
            # jelly set Fy to zero
            materials[_new_name].Fy = 0  # materials[_mat].Fy
            # Jelly modify Emodulus
            materials[_new_name].E = materials[_mat_name].E / float(_factor)
            materials[_new_name].type = 'Elastic'
        #
        elif material_type == 'reinforced':
            _new_name = materials[_mat_name].name + '_reinforced_' + str(_mat_name)
            materials[_new_name] = material.Material(_new_name, material_number)
            #
            # reinforced set Fy value
            if materials[_mat_name].Fy == 0:
                materials[_new_name].Fy = materials[_mat_name].E
            else:
                materials[_new_name].Fy =  materials[_mat_name].Fy  * float(_factor)
            # reinforced mofify Emodulus
            materials[_new_name].E = materials[_mat_name].E * float(_factor)
            materials[_new_name].type = 'Plastic'
        #
        else:
            print('  *** error material type {:} not recognized'.format(material_type))
            print('      process terminated')
            sys.exit()
        #
        materials[_new_name].alpha = materials[_mat_name].alpha
        materials[_new_name].density = materials[_mat_name].density
        materials[_new_name].poisson = materials[_mat_name].poisson
        
        #
        _mat_residual.append([_mat, _new_name])

    # update materials
    for _item in _member_items:

        for _mat in _mat_residual:

            if _mat[0] == elements[_item].material[0].name.lower():
                elements[_item].material[0] = materials[_mat[1]]
    # 
    #
    #return materials, elements
    #print('-->')
    #
#
#
def update_material_name(materials, _units,
                         renumber:bool=True):
    """
    Note:
    N/m^2  without conversion result is: g/(m*s^2)
    """
    #
    #
    print('    * Updating Materials')
    #
    try:
        base_units = units.Number(1, dims=_units[5])
    except:
        _input = _units[4] + '/' + _units[0] + '^2'
        base_units = units.Number(1, dims=_input)
    #
    if renumber:
        i = 0
        for _mat in materials.values():
            i += 1
            _grade = round(_mat.Fy / (base_units.value * 1000.0))
            _mat.get_name(i, _grade)
    else:
        for _mat in materials.values():
            _grade = round(_mat.Fy / (base_units.value * 1000.0))
            _mat.get_name(_mat.number, _grade)        
#
#
# --------------------------------------------------------------------
#
def get_material_step(_member, _buckets):
    """
    """
    for x in range(1, len(_member.material)):
        _material_step = _member.material[x]
        # second loop of bucket sections in steps
        for _new_material in  _buckets[_material_step.Fy].values():
            if _new_material.equal(_material_step):
                _member.material[x] = _new_material
                break
#
def simplify_material(materials, _element):
    """
    """
    print('    * simplifying Materials')
    _buckets = get_buckets(materials)
    _duplicated = filter_bucket(_buckets)
    
    for _material in materials.values():
        for _new_material in _buckets[_material.Fy].values():
            if _material.number == _new_material.number:
                for _member_name in _material.concepts:
                    _member = _element[_member_name] 
                    get_material_step(_member, _buckets)
            
            elif _material.equal(_new_material):
                for _member_name in _material.concepts:
                    _member = _element[_member_name]
                    _new_material.elements.append(_member_name)
                    _member.material[0] = _new_material
                    get_material_step(_member, _buckets)
    #
    # remove duplicated sections
    for _member_name in _duplicated:
        del materials[_member_name]    
#
def get_buckets(materials):
    """
    """
    _bucket = {}
    for key, _material in materials.items():
        try:
            _bucket[_material.Fy].update({key: _material})
        except KeyError:
            _bucket[_material.Fy] = {}
            _bucket[_material.Fy].update({key: _material})
    return _bucket
#
def filter_bucket(_bucket):
    """
    """
    _duplicated = []
    for _Fy, _items in _bucket.items():
        _dummy = copy.copy(_items)
        _to_delete = []
        for _mat_id, _material in _items.items():
            # remove self material
            try:
                del _dummy[_mat_id]
            except KeyError:
                continue
            # find duplicated
            for _item_name, _item in _dummy.items():
                if _material.equal(_item):
                    _to_delete.append(_item_name)
        
        _duplicated.extend(_to_delete)
        for _item_no in _to_delete:
            try:
                del _items[_item_no]
            except KeyError:
                continue
    #
    _duplicated = list(set(_duplicated))
    return _duplicated
#
#
#
# --------------------------------------------------------------------
#                            Modify material
# --------------------------------------------------------------------
#
def set_mean_material(geometry, factor:int):
    """
    Set existing materail's yield sstrength to its mean value
    """
    material_set_mean(geometry.materials, factor)
    #return _geometry
#
def add_new_materialXX(structure, foundation, _mat_name, _mat_number):
    """
    Add new materail to the fe model\n
    
    Input:\n
    structure  : fe model _structure
    _mat_name : list with new material's name\n
    material_number   : list with new material's fe number\n
    """
    #
    
    # find material number
    _mat_list = [_mat.number for _mat in structure.materials.values()]
    # check if soil springs
    try:
    #if model['soil']:
        _mat_list.extend([_mat.number for _mat in foundation.materials.values()])
    except AttributeError:
        pass
    # find maximum load number
    material_number = max(_mat_list)
    #
    if _mat_name:
        _type = 'name'
        new_material(structure.materials,
                     _mat_name, _type, material_number)

    if _mat_number:
        _type = 'number'
        new_material(structure.materials,
                     _mat_number, _type)

    #return _structure
#
def update_materialXX(geometry, _mat_name, _mat_number):
    """
    Update existing material of the fe model
    """
    if _mat_name:
        _type = 'name'
        material_update(geometry.materials,
                        _mat_name, _type)
    #
    if _mat_number:
        _type = 'number'
        material_update(geometry.materials,
                        _mat_number, _type)
        #
    #return geometry
#
def add_modified_material(_type, geometry, foundation,
                          member_list, group_list):
    """
    Create new material with modified properties and assing it to
    defined elements\n
    
    Input :\n
    _type      : jelly, reinforced
    geometry   : fe model geometry
    member_list : list with elements's fe number\n
    group_list  : list with set's fe number\n
    _factor    : factor to modify Elastic modulus (E = E/factor)\n
    """

    print('    * Adding new {:} materials'.format(_type))
    
    # find material number
    _mat_list = [_mat.number for _mat in geometry.materials.values()]
    # check if soil springs
    try:
        #if foundation.materials:
        _matsoil = foundation.materials
        _mat_list.extend([_mat.number for _mat in _matsoil.values()])
    except AttributeError:
        pass
    # find maximum load number
    material_number = max(_mat_list)
    
    for _member_list in member_list:
        _factor = _member_list[1]
        _group_no = []

        add_mod_material(_type,
                         geometry.materials,
                         geometry.elements,
                         geometry.sets,
                         _member_list[0],
                         _group_no,
                         _factor, material_number)
    #
    # find material number
    _mat_list.extend([_mat.number for _mat in geometry.materials.values()])
    material_number = max(_mat_list)
    
    for _group_list in group_list:
        _factor = _group_list[1]
        _member_no = []

        add_mod_material(_type,
                         geometry.materials,
                         geometry.elements,
                         geometry.sets,
                         _member_no,
                         _group_list[0],
                         _factor, material_number)

    #return geometry
#
#
#
def find_mass_piramid_material(geometry):
    """
    """
    #
    material = geometry.materials
    _list_mat = []
    _members = []
    for key, _mat in material.items():
        if _mat.name == 'mass_pyramid':
            _mat.type = 'Elastic'
            _list_mat.append(key)
            _members.extend(_mat.elements)
    #
    check_out = []
    if _members:
        # create new group
        _group = femodel.Groups('mass_pyramid', 1)
        _group.items = _members
        _new_sets = {'mass_pyramid':_group}
        add_new_groups(geometry.sets, _new_sets,
                       name='mass_pyramid')
        #
        check_out.append('\n')
        check_out.append("'{:} Mass Piramid Material Modified\n".format(50 * " "))
        check_out.extend(formatting.print_column_num(_list_mat))        
    #
    return check_out
#
# --------------------------------------------------------------------
#                            Material
# --------------------------------------------------------------------
#
#
def find_isomat_item(word_in:str):
    """
    """
    key = {"Fy": r"\b((fy|yield|smys)(\s*strength|stress)?)\b",
            "Fu": r"\b((fu|u(ltimate)?\s*t(ensile)|smts)(\s*strength|stress)?)\b",
            "E": r"\b((E(lastic)?|young(\'s)?|tensile)(\s*modulus)?)\b",
            "poisson": r"\b(poisson(\s*ration)?|nu|\u03BD)\b",
            "alpha": r"\b(termal\s*expansion|alpha|\u03B1)\b",
            "stiffness": r"\b(stiffness|k)\b",
            "density": r"\b(density|rho|\u03C1)\b",
            "spring": r"\b(spring)\b",
            "type": r"\b(type)\b",
            "damping": r"\b(damping)\b",
            "G": r"\b(g|shear\s*modulus)\b",
            "name": r"\b(name[s]?|id|nom(bre)?[s]?|navn)\b",
            "number": r"\b(num(ber[s]?|mer|ero[s]?))\b",
            "factor": r"\b(factor)\b",
            "grade" :  r"\b(grade)\b",
            "group" : r"\b(set|group)(s)?\b",
            "type": r"\b((material)?(\_)?type)\b"}

    match = common.find_keyword(word_in, key)
    return match
#
# 
#
def find_mat_type(word_in:str):
    """
    """
    key = {"elastic": r"\b(elastic|iso(tropic)?|linear)\b",
            "plastic": r"\b(plastic)\b",
            "curve": r"\b(curve|spring|py|tz|qz)\b"}
    match = common.find_keyword(word_in, key)
    return match
#
def assign_material_item(mat_items):
    """
    Assign material's input data to a dictionary by name/number
    """
    _mat = {}
    _type = 'name'

    for key, value in mat_items.items():
        _item = find_isomat_item(key)
        _mat[_item] = value
    #
    if 'name' in _mat:
        _id = _mat['name']
    elif 'number' in _mat:
        _id = _mat['number']
        _type = 'number'
    else:
        print('   ** warning: material id not provided, default is used')
        _id = 'new_mat'
    #
    _mat_out = {_id: _mat}
    # print('ok')
    return _mat_out, _type
#
def get_material_name(_number, _units):
    """
    Note:
    N/m^2  without conversion result is: g/(m*s^2)
    """
    base_units = Number(1, dims=_units) / 1e+6 # from gram to kilogram
    _length_factor = base_units.convert('millimetre').value
    
    _name = 'MT' + str(_number).zfill(3) + '_' + str(_grade)
#
#
# -----------------------------
#
def update_Fu(geometry, foundation, Fu_update):
    """
    """
    new_mat = []
    #new_grade = []
    for key, _Fu in Fu_update.items():
        try:
            _material =  geometry.materials[str(key)]
            kwargs = {'Fy' : _material.Fy,
                      'Young' : _material.E,
                      'density' : _material.density,
                      'name' : _material.name + "_UTS_" + str(int(_Fu['Fu'])),
                      'Fu' : float(_Fu['Fu']) * 1e9,
                      'grade' : str(int(_Fu['grade'] * 10)) + "EM",
                      'poisson' : _material.poisson}
            #
            new_material_number = {}
            new_material_name, _type = assign_material_item(kwargs)
            # new material
            add_new_material(geometry,
                             foundation,
                             new_material_name,
                             new_material_number)
            
            new_mat.append(_Fu['number'])
            #new_grade.append(_Fu['grade'])
            _Fu['name'] = kwargs['name']
        except KeyError:
            print('   *** warning material : {:} not found'.format(key))
    #
    for key, memb in geometry.concepts.items():
        if memb.material[0].number in new_mat:
            for x in range(len(memb.material)):
                for key, _Fu in Fu_update.items():
                    if memb.material[x].grade == _Fu['grade']:
                        if( memb.section[x].properties.Tw >= _Fu['tmin'] 
                            and memb.section[x].properties.Tw <= _Fu['tmax']):
                            mat = geometry.materials[_Fu['name']]
                            memb.material[x] = mat
                            break
    #
#
#
# -----------------------------
#
def get_isomat_prop(properties):
    """
    Fy, Fu, E, G, poisson, density, alpha
    """
    set_prop = [280_000_000, 0, 205_000_000_000, 80_770_000_000,
                0.30, 7850, 1.2E-5]
    set_units = ['pascal', 'pascal', 'pascal', 'pascal', None, 'kilogram/metre^3', 'kelvin']
    for x, item in enumerate(set_prop):
        try:
            set_prop[x] = properties[x].convert(set_units[x]).value
        except AttributeError:
            set_prop[x] = properties[x]
            raise IOError("units required")
        except IndexError:
            pass
    # check if Fu
    if not set_prop[1]:
        set_prop[1] = set_prop[0]/0.75
    return set_prop
#
def get_isomat_prop_df(df):
    """ """
    #units = Units()

    try:
        df["Fy"] = df["Fy"].apply(lambda x: x.convert('megapascal').value)
    
    except AttributeError:
        pass
    
    except KeyError:
        df["Fy"] = 280_000_000 #* units.Pa

    try:
        df["Fu"] = df["Fu"].apply(lambda x: x.convert('pascal').value)
    
    except AttributeError:
        pass
    
    except KeyError:
        df["Fu"] = df["Fy"]/0.75

    try:
        df["E"] = df["E"].apply(lambda x: x.convert('pascal').value)
    
    except AttributeError:
        pass
    
    except KeyError:
        df["E"] = 205_000_000_000 #* units.Pa

    try:
        df["G"] = df["G"].apply(lambda x: x.convert('pascal').value)
    
    except AttributeError:
        pass
    
    except KeyError:
        df["G"] = 80_770_000_000 #* units.Pa

    try:
        df["poisson"]
    except KeyError:
        df["poisson"] = 0.30

    try:
        df["density"] = df["density"].apply(lambda x: x.convert('kilogram/metre^3').value)
    
    except AttributeError:
        pass
    
    except KeyError:
        df["density"] = 7850 #* units.kilogram/units.metre**3

    try:
        df["alpha"] = df["alpha"].apply(lambda x: x.convert('kelvin').value)
    
    except AttributeError:
        pass
    
    except KeyError:
        df["alpha"] = 1.2E-5 #* units.kelvin

    #try:
    #    df["name"]
    #except KeyError:
    #    df["name"] = df["Fy"].apply(lambda x: f"mat_{str(x.convert('megapascal').value)}")

    try:
        df["type"]
    except KeyError:
        df["type"] = "elastic"

    return df[["name", "type", "Fy", "Fu", "E", "G", "poisson", "density", "alpha"]]
#