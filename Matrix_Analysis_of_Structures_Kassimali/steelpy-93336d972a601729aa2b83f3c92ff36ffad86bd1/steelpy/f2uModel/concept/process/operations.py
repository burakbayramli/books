#
# Copyright (c) 2009-2020 fem2ufo
# 

# Python stdlib imports
import cmath
import copy
import logging

# package imports
import fem2ufo.process.operations.joint as joint
from fem2ufo.process.graphics.euclid import Point3, Plane

#
def member2node(memb_name, memb_number, elements, concept):
    """
    module to find a node based on a member name or number given
    a position [end 1 or 2]
    """
    number = []
    # Find node based on member name & position
    for _set in memb_name:
        try:
            _items = list(_key for _key in concept[_set[0]].element.keys())
            _end = int(_set[1]) - 1

            if len(_items) == 1:
                _no = _items[0]
                _node_name = elements[_no].node[_end].name
            else:
                _group, _node_out = geometry.split_sets(_items, elements, 
                                                        'tempGroup', 'both')
                # bottom element end = 0
                if _end == 0:
                    _no = _group['tempGroup_1'][-1]
                    _node_name = _node_out['tempGroup_1'][-1]
                # top element, end = 1
                else:
                    _no = _group['tempGroup_1'][0]
                    _node_name = _node_out['tempGroup_1'][0]
            #
            number.append(_node_name)
        except KeyError:
            print('    *** warning element name {:} not found'.format(_set[0]))
            continue
    #
    # add member number
    for _set in memb_number:
        _end = int(_set[1]) - 1
        try:
            _node_name = elements[str(_set[0])].node[_end].name
        except KeyError:
            print('    *** warning element number {:} not found'.format(_set[0]))
            continue
        number.append(_node_name)
    #
    _list = list(set(number))
    number = _list
    #
    return number
#
#
#
def find_memb_sharing_node(_nodes, _elements):
    """
    Find and group members sharing node
    """
    print('    * Finding elements sharing nodes')
    #
    for _node in _nodes.values():
        _node.elements = [k for k, v in _elements.items()
                          if v.node[0].number == _node.number
                          or v.node[1].number == _node.number]
    #
    return _nodes
#
#
# --------------------------------------------------------------------
#                 finding fe number by concept name
# --------------------------------------------------------------------
#
#
def find_node(_node_number, geometry):
    """
    Check if node number exist in the f2u model
    """
    _node = []
    for _number in _node_number:
        try:
            _test = geometry._mesh.nodes[_number]
            _node.append(_number)
        except KeyError:
            continue
    
    return _node
#
# FIXME : Node number/name to be defined
def find_node_in_element_end(geometry,
                         _memb_name, _memb_number,
                         _node_number):
    """
    """
    print('    * Finding node displacement control')

    _node_number.extend(member2node(_memb_name, _memb_number,
                                    geometry.elements,
                                    geometry.concepts))
    #
    _node = find_node(_node_number, geometry)
    return _node
#
#
def find_fe_number_by_concept_name(concept, name, number=False):
    """
    find element fe number based on element's concept name
    """
    _item_number = []
    for _item in name:
        _concept = concept[_item]
        for _element in _concept.elements:
            _item_number.append(_element.number)
    #
    if number:
        _item_number.extend(number)
    #
    return _item_number
#
# --------------------------------------------------------------------
#                finding fe number by concept name
# --------------------------------------------------------------------
#
def find_item_fe_number_by_name(subject, name, number=False):
    """
    find item fe number by item's name
    """
    _subject_name = {_subject.name.lower(): _subject.number for key, _subject in subject.items()}
    _item_number = []
    for _item in name:
        try:
            _item_number.append(_subject_name[_item.lower()])
        except KeyError:
            logging.warning(' ** group : {:} not found'.format(_item))
    #
    if number:
        _item_number.extend(number)
    
    _item_number = list(set(_item_number))
    return _item_number
#
#
# --------------------------------------------------------------------
#                finding overlaping beam
# --------------------------------------------------------------------
#
#
def find_overlapping_beam(concept, alignment=False, tol=0.1):
    """
    """
    #
    print('--- Finding overlapping beams.. can take a while..')
    #
    _group = []
    _atol = tol * cmath.pi / 180.0
    _atol = 0.01
    #
    _temp = [key for key in sorted(concept, key=lambda name: concept[name].number)
             if 'beam' in concept[key].type]
    _temp = tuple(_temp)
    #
    for key1 in _temp:
        memb1 = concept[key1]
        # nodes member 1
        A = Point3(memb1.node[0].x, 
                   memb1.node[0].y, 
                   memb1.node[0].z)
        
        B = Point3(memb1.node[1].x, 
                   memb1.node[1].y, 
                   memb1.node[1].z)
        #
        for key2 in _temp:
            # check if same member
            if key1 == key2:
                #_temp.remove(key1)
                continue
            # define member 2
            memb2 = concept[key2]
            if memb2.overlap:
                continue
            
            # nodes member 2
            C = Point3(memb2.node[0].x, 
                       memb2.node[0].y, 
                       memb2.node[0].z)
            
            D = Point3(memb2.node[1].x, 
                       memb2.node[1].y,
                       memb2.node[1].z)
            #
            _r = B - A
            _s = D - C
            _v = C - A 
            #
            _rXs = _r.cross(_s)
            _vXr = _v.cross(_r)            
            #
            #if key1 == '2_1' or key1 == '5_2': # 
            #    if key2 == '2_1' or key2 == '5_2': # 
            #        print('--->', abs(_r), abs(_s))
            #
            # vector not parallel
            if cmath.isclose(abs(_rXs), 0) and cmath.isclose(abs(_vXr), 0):
                if is_collinear(A, B, C, D, _v, _r, _s, key1, key2):
                    #memb1.overlap = True
                    memb2.overlap = True
                    #_group.append(key1)
                    _group.append(key2)
                    #_temp.remove(key2)
            else:
                try:
                    _angle = abs(_r.angle(_s) - cmath.pi)
                except ValueError:
                    _angle = cmath.acos(_r.dot(_s) / (abs(_r)*abs(_s))) - cmath.pi
                    _angle = _angle.real
                #
                # check if almost parallel
                if _angle <= _atol or cmath.isclose(_angle, cmath.pi, rel_tol=0.01):
                    if  is_almost_collinear(A, B, C, D, _v, _r, _s):
                        #
                        # what is this?
                        #_t = _r.cross(_vXr)
                        if abs(_vXr) > 0.015:
                            continue
                        #
                        memb2.overlap = True
                        _group.append(key2)
                        #
                        # make section paralell
                        if alignment:
                            align_beam(A, B, C, D, memb2, _r, _s)
    #
    print('--- End finding overlapping beams')
    #
    return _group
    #    
#
def is_collinear(A, B, C, D, _v, _r, _s, key1, key2):
    """
    :param A:
    :param B:
    :param C:
    :param D:
    :param _r: B - A
    :param _s: D - C
    :param _v: C - A
    :return:
    """
    # check if identical coordinates
    if (A.is_close(C) and B.is_close(D)) or (A.is_close(D) and B.is_close(C)):
        return True
    #
    vDotR = _v.dot(_r)
    #rr = _r.dot(_r)
    #vDotS = _v.dot(_s)
    #ss = _s.dot(_s)
    #
    # Express the end points of the second segment (C and D -> s)
    # in terms of the equation of the first segment (A and B -> r)
    _qs = D - A
    
    if _s.dot(_r) < 0:
        _t0 = vDotR / _r.magnitude_squared()
        _t1 = _qs.dot(_r) / _r.magnitude_squared()
    else:
        _t1 = vDotR / _r.magnitude_squared()
        _t0 = _qs.dot(_r) / _r.magnitude_squared()        
    
    if (_t0 > 1.0 and _t1 > 1.0) or (_t0 < -1.0 and _t1 < -1.0):
        return False
    
    elif (_t1 >= 0 and _t1 <= 1.0) and (_t0 >= 0 and _t0 <= 1.0):
        return True
    
    elif  (_t0 >= 0 and _t0 <= 1.0):
        if A.is_close(D) or A.is_close(C):
            return False
            #if _t1 < 0:
            #    print('ok', key1, key2)
            #   return 0
            #else:
            #    print('X', key1, key2)
            #    return 0
            #    #print('ok')  
        
        #print('---> 1 ends', key1, key2)
        return True
    
    elif (_t1 >= 0 and _t1 <= 1.0) :
        if (B.is_close(D) or B.is_close(C)):
            return False
            #if _t0 < 0:
            #    print('ok', key1, key2)
            #    return 0
            #else:
            #    print('X', key1, key2)
            #    return 0
            #    #print('ok')
         
        #print('---> 2 ends', key1, key2)
        return True
    
    #elif (0 <= vDotR  and vDotR <= rr) or (0 <= vDotS  and vDotS <= ss):
    #    print('---> ?')
    
    #else:
    #AC = A-C
    #BC = B-C
    #_t1s = (AC.dot(_s) / _s.magnitude_squared())
    #_t0s = BC.dot(_s) / _s.magnitude_squared()
    #
    #if (_t1s >= -1 and _t1s <= 0.0) and (_t0s >= -1 and _t0s <= 0.0):
    #    return 1    
    #
    # check if common point along the line
    #_t = _t0 + _s.dot(_r) / _r.magnitude_squared()
    
    #if cmath.isclose(_t1, _t, rel_tol=0.05):
    #    return 1
    
    return False
#
def is_almost_collinear(A, B, C, D, _v, _r, _s):
    """
    :param A:
    :param B:
    :param C:
    :param D:
    :param _r: B - A
    :param _s: D - C
    :param _v: C - A
    :return:
    """
    # check if identical coordinates
    if (A.is_close(C) and B.is_close(D)) or (A.is_close(D) and B.is_close(C)):
        return True
    #
    #
    # Express the end points of the second segment (C and D -> s)
    # in terms of the equation of the first segment (A and B -> r)
    _qs = D - A
    vDotR = _v.dot(_r)
    _t1 = vDotR / _r.magnitude_squared()
    _t0 = _qs.dot(_r) / _r.magnitude_squared()
    
    if (_t1 >= 0 and _t1 <= 1.0) and (_t0 >= 0 and _t0 <= 1.0):
        return True
    
    return False
#
def align_beam(A, B, C, D, _beam, _r, _s):
    """
    """
    #if abs(_r) > abs(_s):
    _normalized = _r.normalized()
    _L1 = A.distance(C)
    _L2 = A.distance(D)
    # swap nodes?
    if _L1 > _L2:
        _L1 = A.distance(D)
        _L2 = A.distance(C)
    
    coord = [0, 0, 0]
    for x in range(3):
        coord[x] = _beam.node[0].coordinates[x] + _L1 * _normalized[x]
    #
    # update coordinates of member 2 (shorter)
    _beam.node[0].set_coordinates(coord[0],
                                  coord[1],
                                  coord[2])
    #
    _L3 = abs(_s)
    if cmath.isclose(_L3, _L2 - _L1, rel_tol=0.01):
        _L3 = _L2 - _L1
    #
    coord = [0, 0, 0]
    for x in range(3):
        coord[x] = _beam.node[0].coordinates[x] + (_L1 + _L3) * _normalized[x]
    #
    # update coordinates of member 2 (shorter)
    _beam.node[1].set_coordinates(coord[0],
                                  coord[1],
                                  coord[2])                            
    #else:
    #    vector = A-C
    #    _w = vector.cross(_s)
    #    _t = _s.cross(_w)
    #    if abs(_t) > _atol:
    #        continue
    #    
    #    _beam.overlap = True
    #    _group.append(key2)                            
    #    #_member = memb1
    #    # make section paralell
    #    _normalized = _s.normalized()
    #    _L1 = C.distance(A)
    #    _L2 = C.distance(B)
    #    # swap nodes?
    #    if _L1 > _L2:
    #        _L1 = C.distance(B)
    #        _L2 = C.distance(A)
    #    #
    #    #
    #    coord = [0, 0, 0]
    #    for x in range(3):
    #        coord[x] = _beam.node[0].coordinates[x] + _L1 * _normalized[x]
    #    #
    #    # update coordinates of member 2 (shorter)
    #    memb1.node[0].set_coordinates(coord[0],
    #                                  coord[1],
    #                                  coord[2])
    #    #
    #    _L3 = abs(_s)
    #    if cmath.isclose(_L3, _L2 - _L1, rel_tol=0.01):
    #        _L3 = _L2 - _L1
    #    #
    #    coord = [0, 0, 0]
    #    for x in range(3):
    #        coord[x] = _beam.node[0].coordinates[x] + (_L1 + _L3) * _normalized[x]
    #    #
    #    # update coordinates of member 2 (shorter)
    #    memb1.node[1].set_coordinates(coord[0],
    #                                  coord[1],
    #                                  coord[2])
    #    #  
    #if abs(_t) > _atol:
    #    continue
    #
    #if key2 == '0256_0253':
    #    print('--->', abs(_r), abs(_s))                        
    #
    #memb1.overlap = True 
    #_member.overlap = True
    #_group.append(key1)
    #_group.append(key2)
    #_temp.remove(key2)                        
    #    
#
def find_step_section(_element, _section, _material, 
                      asas=False): # _tapered,
    """
    """
    _eccentricity = _element.beams._elements.eccentricities
    _geometry_items = []
    for key, _beam in _element.beams.items():
        #
        #if _beam.name == '1745_2651' :
        #    print('--->') 
        #
        #if _beam.type != 'shim':
        #    _geometry_items.append(key)
        #
        #if _beam.type != 'beam' and _beam.type != 'pile':
        #    continue
        # 
        # -----------------------
        # FIXME : 
        # apply offsets to element (ASAS only)
        if asas:
            try:
                _sec_no = _beam.section[0].name
                _beam.offsets.append(_eccentricity[_sec_no])
                # FIXME : offset flag too weak
                _sec_no = str(_sec_no) + '_1'
                _beam.offsets.append(_eccentricity[_sec_no])
                
                #for _offset in _eccentricity.values():
                #    if _sec_no == _offset.name:
                #        _beam.offsets.append(_offset)
                #        #print('')
                #        break
            except KeyError:
                pass
        # ----------------------  
        # get material's concept
        #mat = _beam.material
        #mat.concepts.append(_beam.name)
        #
        # check if segmented beam
        #try:
        if steps :=_beam.step:
            _missing = False
            i = 0
            _step = 0
            # check if section step length
            try:
                _length = _beam.section[0].length
                _beam.step.append(_beam.section[0].length)
            except AttributeError:
                _length = 0
                _missing = _beam.section[0].name
                _beam.step.append(None)
            #
            for _sec_name in _beam.section[0].step:
                _new_sect = _section[_sec_name]
                _beam.section.append(_new_sect)
                
                try:
                    _beam.material.append(_new_sect.material)
                except AttributeError:
                    _beam.material.append(_beam.material[0])
                
                i += 1
                try:
                    _length += _new_sect.length
                    _beam.step.append(_new_sect.length)
                except AttributeError:
                    _missing = _sec_name
                    _step = i
                    _beam.step.append(None)
            #
            # this option may be to correct input wrong data?
            if _missing and _length > 0:
                # TODO : flexible length o node to node?
                _new_length = (_beam.length_node2node - _length)
                # TODO : I think this is an input error in lenght
                #        but try to correct it (probably asas specific)
                if _new_length < 0:
                    if _step == 0:
                        _beam.section[0] = _section[_beam.section[1].name]
                        _beam.material[0] = _material[_beam.material[1].name]
                        
                        del _beam.section[1]
                        del _beam.material[1]
                        _beam.step = []
                    else:
                        for j in range(_step, 0, -1):
                            _beam.step[j-1] = _beam.step[j]
                            _beam.section[j-1] = _section[_beam.section[j].name]
                            _beam.material[j-1] = _material[_beam.material[j].name]
                        
                        del _beam.section[_step]
                        del _beam.material[_step]
                        del _beam.step[_step]
                        print('fix here: multiple beam step')
                # assing length to missing section
                else:
                    _beam.step[_step] = _new_length
            #
            # this option may be a tapered beam
            elif _missing and _length == 0:
                if not _beam.properties.tapered:
                    print(' --> no tapper now what??')
                    continue
                get_taper_beam(_beam, _section)
            #
            # this option may be sacs specific/multiple segments?
            else:
                #if _beam.name == '2624_2904':
                #    print('--->')
                try:
                    _sum_length = sum(_beam.step)
                    if _sum_length == 1.0:
                        print(' check segmented beam --> may be sacs option B')
                        for x in range(len(_beam.step)):
                            _beam.step[x] = _beam.step[x] * _beam.flexible_length
                except TypeError:
                    _sum_length = 0
                    _index = None
                    for x in range(len(_beam.step)):
                        try:
                            _sum_length += _beam.step[x]
                        except TypeError:
                            _index = x
                    #
                    _beam.step[_index] = _beam.length_node2node - _sum_length
        
        #except AttributeError:
        #    continue
    #
    return _geometry_items
#
def get_taper_beam(_beam, _section):
    """
    """
    _Dav = (_beam.section[0].properties.D 
            + _beam.section[1].properties.D) / 2.0
    _length = _beam.length_node2node
    
    if _Dav > _length/3.0:
        #print('---> average')
        _new_name = _beam.properties.tapered[0][0] + '_av'
        try:
            _section[_new_name]
        except KeyError:
            _new_number = len(_section)
            _section[_new_name] = copy.deepcopy(_beam.section[0])
            _section[_new_name].number = _new_number
            # update section
            _section[_new_name].properties.D = (_beam.section[0].properties.D 
                                                + _beam.section[1].properties.D) / 2.0
            _section[_new_name].properties.Tw = (_beam.section[0].properties.Tw 
                                                 + _beam.section[1].properties.Tw) / 2.0
            _section[_new_name].properties.Bft = (_beam.section[0].properties.Bft 
                                                  + _beam.section[1].properties.Bft) / 2.0
            _section[_new_name].properties.Tft = (_beam.section[0].properties.Tft 
                                                  + _beam.section[1].properties.Tft) / 2.0
            _section[_new_name].properties.Bfb = (_beam.section[0].properties.Bfb 
                                                  + _beam.section[1].properties.Bfb) / 2.0
            _section[_new_name].properties.Tfb = (_beam.section[0].properties.Tfb 
                                                  + _beam.section[1].properties.Tfb) / 2.0
        
        #_section[_new_name].get_name(length_unit='metre')
        
        _beam.step = [_length]
        _beam.section = [_section[_new_name]]
        _beam.material = [_beam.material[0]]
    else:
        if _beam.properties.tapered[0][1] == 'B':
            _ends = [_beam.properties.tapered[0][0],
                     _beam.properties.tapered[1][0]]
        else:
            _ends = [_beam.properties.tapered[1][0],
                     _beam.properties.tapered[0][0]]
        #
        1/0.0
        #
        if len(_beam.section) == 2:
            #
            #_beam.step = [_beam.length_node2node/2.0, 
            #              _beam.length_node2node/2.0]
            #_beam.section[0] = _section[_ends[0]]
            #_beam.section[1] = _section[_ends[1]]
            #return
            #
            # TODO : fix this section, not working
            _new_length = _beam.length_node2node / 3.0
            _beam.step = [_new_length, _new_length, _new_length]
            _beam.section[0] = _section[_ends[0]]
            _beam.section.append(_section[_ends[1]])
            _beam.material.append(_beam.material[1])
            # create new average section
            _new_name = _beam.properties.tapered[0][0] + '_step_3'
            _new_number = len(_section)
            _section[_new_name] = copy.copy(_beam.section[1])
            _section[_new_name].number = _new_number
            _section[_new_name].name = _section[_new_name].name + 's3'
            # update section
            _section[_new_name].properties.D = (_beam.section[0].properties.D 
                                                + _beam.section[2].properties.D) / 2.0
            _section[_new_name].properties.Tw = (_beam.section[0].properties.Tw 
                                                 + _beam.section[2].properties.Tw) / 2.0
            _section[_new_name].properties.Bft = (_beam.section[0].properties.Bft 
                                                  + _beam.section[2].properties.Bft) / 2.0
            _section[_new_name].properties.Tft = (_beam.section[0].properties.Tft 
                                                  + _beam.section[2].properties.Tft) / 2.0
            _section[_new_name].properties.Bfb = (_beam.section[0].properties.Bfb 
                                                  + _beam.section[2].properties.Bfb) / 2.0
            _section[_new_name].properties.Tfb = (_beam.section[0].properties.Tfb + 
                                                  _beam.section[2].properties.Tfb) / 2.0                    
            #
            _beam.section[1] = _section[_new_name]
            _beam.properties.tapered = True
        else:
            print(' fix taper section with mor than 2 sections')
            1/0
#
#
# --------------------------------------------------------------------
#           concept operations
# --------------------------------------------------------------------
#
#
#