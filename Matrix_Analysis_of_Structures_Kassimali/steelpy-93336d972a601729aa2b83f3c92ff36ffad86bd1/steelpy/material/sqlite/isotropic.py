# 
# Copyright (c) 2009 steelpy
# 
#
# Python stdlib imports
from __future__ import annotations
from array import array
from collections.abc import Mapping
from dataclasses import dataclass
from typing import NamedTuple
import re
#

# package imports
from steelpy.utils.units.main import Units
#from steelpy.process.units.buckingham import Number
#from ..process.operations import
from ..process.print_report import print_isomat
from steelpy.f2uModel.mesh.sqlite.process_sql import create_connection, create_table
#
from ..process.operations import get_isomat_prop, get_isomat_prop_df
from ..process.mechanical import MaterialItem 
#
from steelpy.utils.dataframe.main import DBframework
#
#
class MaterialSQL(Mapping):
    __slots__ = ['db_file', 'db_system', '_labels', '_default',
                 '_number', '_type', '_elastic', '_curve']

    def __init__(self, db_file: str,
                 db_system:str="sqlite") -> None:
        """
        db_system: sqlite
        """
        self.db_file = db_file
        self.db_system = db_system
        #
        self._default: str|None = None
        self._labels:list[str|int] = []
        self._number: list[int] = []
        self._type:list[str|int] = []
        #self._default:Union[str,None] = None
        self._elastic = MaterialElasticSQL(self.db_file)
        # create node table
        conn = create_connection(self.db_file)
        with conn:        
            self._create_table(conn)
            
    #
    def __setitem__(self, material_name: str|int,
                    properties: list[str|float]) -> None:
        """
        """
        try:
            self._labels.index(material_name)
            raise IOError('   error material {:} already exist'.format(material_name))
        except ValueError:
            material_type = properties[0]
            self._labels.append(material_name)
            self._type.append(material_type)
            #
            conn = create_connection(self.db_file)
            with conn:
                mat_number = push_material(conn, material_name, material_type)
                #conn.commit()
                self._number.append(mat_number)
            # set material
            if re.match(r"\b(curve)\b", material_type, re.IGNORECASE):
                #self._material[material_name]
                raise Exception('--> No ready')
            elif re.match(r"\b(elastic|linear|isotropic)\b", material_type, re.IGNORECASE):
                self._elastic[material_name] = [mat_number, *properties[1:]]
            else:
                raise IOError(' material type {:} not recognised'
                              .format(material_type))
        #
        self._default = material_name
    #
    def __getitem__(self, material_name: str|int):
        """
        """
        try:
            index = self._labels.index(material_name)
            #mat_number = self._number[index]
            material_type = self._type[index]
            self._default = material_name
        except (IndexError, ValueError):
            raise KeyError('Invalid material name : {:}'.format(material_name))
        #
        #
        if re.match(r"\b(curve)\b", material_type, re.IGNORECASE):
            return self._curve[material_name]
        
        elif re.match(r"\b(elastic|linear|isotropic)\b", material_type, re.IGNORECASE):
            return self._elastic[material_name]
        
        else:
            raise IOError(' material type {:} not recognised'
                          .format(material_type))        
    #
    #def _push_material(self, conn, material_name: Union[str, int],
    #                   material_type:str):
    #    """
    #    """
    #    project = (material_name, None, material_type)
    #    sql = 'INSERT INTO  tb_Materials(name, title, type) VALUES(?,?,?)'
    #    cur = conn.cursor ()
    #    cur.execute (sql, project)
    #    return cur.lastrowid
    #
    def _create_table(self, conn) -> None:
        """ """
        _table_materials = "CREATE TABLE IF NOT EXISTS tb_Materials (\
                            number INTEGER PRIMARY KEY,\
                            name NOT NULL,\
                            title TEXT,\
                            type TEXT NOT NULL);"
        #
        #conn = create_connection(self.db_file)
        create_table(conn, _table_materials)
    #
    def __len__(self):
        return len(self._labels)

    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __contains__(self, value):
        return value in self._labels
    #
    #
    #@property
    #def _default(self):
    #    """ """
    #    print('here')
    #
    def get_number(self, start:int=1)-> Iterable[int]:
        """
        """
        try:
            n = max(self._number) + 1
        except ValueError:
            n = start
        #
        while True:
            yield n
            n += 1
    #    
    #
    @property
    def default(self):
        """ """
        return self._default

    @default.setter
    def default(self, material_name):
        """ """
        if not material_name in self._labels:
            raise IOError ( f'material {material_name} missing' )
        self._default = material_name
    #
    #
    #@elastic.setter
    def elastic(self, values:None|list=None,
                df=None):
        """ """
        if values:
            if isinstance(values, list):
                for item in values:
                    material_name = item[0]
                    material_type = "elastic"
                    self._labels.append(material_name)
                    self._type.append(material_type)
                    mat_number = next(self.get_number())
                    self._number.append(mat_number)
                    #self.__setitem__(item[0], item[1:])
                    properties = get_isomat_prop(item[1:])
                    self._elastic[material_name] = [material_name, *properties]
            
            else:
                raise IOError('material input not valid')
        # dataframe input
        try:
            df.columns
            df = df.drop_duplicates(['name'], keep='first')
            #
            group = df.groupby("type")
            elastic = group.get_group("elastic")
            elastic = get_isomat_prop_df(elastic)
            elastic = elastic.drop_duplicates(['name'])
            #
            self._labels.extend(elastic.name)
            self._type.extend(elastic.type)
            material_number = [next(self.get_number()) for _ in elastic.name]
            self._number.extend(material_number)
            #
            self._elastic.df = elastic
        except AttributeError:
            pass
        #print('--')
        #return self._elastic
        return MaterialType(mat_type="elastic",
                            cls_type=self._elastic, cls=self)
#
#
@dataclass
class MaterialType:
    __slots__ = ['_type', '_labels', '_number',
                 '_cls_type', '_item_type', 'db_file']
    
    def __init__(self, mat_type: str, cls_type, cls):
        """
        """
        self._cls_type = cls_type
        self._item_type = mat_type
        self._labels = cls._labels
        self._type = cls._type
        self._number = cls._number
        self.db_file = cls.db_file
        
    def __setitem__(self, material_name:str|int,
                    properties:list[str|float]) -> None:
        """
        """
        self._labels.append(material_name)
        self._type.append(self._item_type)
        # set material master
        conn = create_connection(self.db_file)
        with conn:
            mat_number = push_material(conn, material_name, self._item_type)
        #
        self._number.append(mat_number)
        # set material type
        prop = get_isomat_prop(properties)
        self._cls_type[material_name] = [mat_number, *prop]
    
    def __getitem__(self, material_name:str|int):
        """
        """       
        return self._cls_type[material_name]
    #
    @property
    def df(self):
        """ """
        return self._cls_type.df
    #
    #def get_number(self, start:int=1)-> Iterable[int]:
    #    """
    #    """
    #    try:
    #        n = max(self._number) + 1
    #    except ValueError:
    #        n = start
    #    #
    #    while True:
    #        yield n
    #        n += 1
    #   
#
#
def push_material(conn, material_name: str|int,
                  material_type:str):
    """
    """
    project = (material_name, None, material_type)
    sql = 'INSERT INTO  tb_Materials(name, title, type) VALUES(?,?,?)'
    cur = conn.cursor ()
    cur.execute (sql, project)
    return cur.lastrowid
#
#
class GetMaterial(NamedTuple):
    """ Linear Material Data"""
    name:Union[int,str]
    type:str
    E:float
    Fy:float
    Fu:float
    G:float
    nu:float
    rho:float
    alpha:float
#
#
def get_materials(conn, component_name):
    """
    """
    materials = {}
    cur = conn.cursor()
    #cur.execute("SELECT * FROM tb_materials;")
    cur.execute("SELECT tb_Materials.name, tb_Materials.type,\
                tb_MatElastoPlastic.*\
                FROM tb_Materials, tb_MatElastoPlastic\
                WHERE  tb_materials.number = tb_MatElastoPlastic.number;")
    rows = cur.fetchall()
    for row in rows:
        #print(row)
        #if row[3] == "curve":
        #    pass
        #else:
        materials[row[0]] = GetMaterial(name=row[0], type=row[1],
                                        E=row[4], Fy=row[5], Fu=row[6], G=row[7],
                                        nu=row[8], rho=row[9], alpha=row[10])
        #    print("boundary : ",row[9])
    #conn.close()
    #print("--->")
    return materials
#
#
#
def get_materialSQL(conn, material_name:int|str):
    """
    """
    cur = conn.cursor()
    cur.execute("SELECT tb_Materials.name, tb_Materials.type,\
                tb_MatElastoPlastic.*\
                FROM tb_Materials, tb_MatElastoPlastic\
                WHERE  tb_Materials.number = tb_MatElastoPlastic.material_number\
                AND tb_Materials.name = ?",(material_name, ))
    row = cur.fetchone()
    #
    return MaterialItem(name=row[0], number=row[3],
                        Fy=row[4], Fu=row[5],
                        E=row[6], G=row[7],
                        poisson=row[8], density=row[9],
                        alpha=row[10])    
    #print("--->")
    #return materials
#
#
#
@dataclass
class GetMaterialSQL:
    """ Linear Material Data"""

    __slots__ = ['number', 'index', 'cls', 'type', 'db_file', 'units']

    def __init__(self, cls, material_number: int) -> None:
        """
        """
        self.index: int = cls._labels.index(material_number)
        self.cls = cls
        self.number: int = material_number
        self.db_file: str = cls.db_file
        # get material name
        self.type: str = "elastic"
        self.units = Units()
    #
    #
    @property
    def Fy(self):
        return self.get_item(item="Fy") * self.units.Pa

    @Fy.setter
    def Fy(self, value) -> None:
        self.update_item(item='Fy', value=value.convert("pascal").value)

    #
    @property
    def E(self):
        return self.get_item(item="E") * self.units.Pa

    @E.setter
    def E(self, value):
        self.update_item(item='E', value=value.convert("pascal").value)

    #
    @property
    def G(self):
        return self.get_item(item="G") * self.units.Pa

    @G.setter
    def G(self, value):
        self.update_item(item='G', value=value.convert("pascal").value)

    #
    @property
    def Fu(self):
        Fu = self.get_item(item="Fu")
        try:
            1 / Fu
            return Fu * self.units.Pa
        except ZeroDivisionError:
            return self.Fy / 0.75

    @Fu.setter
    def Fu(self, value):
        """
        Fu :
        """
        self.update_item(item='Fu', value=value.convert("pascal").value)

    #
    @property
    def density(self):
        return self.get_item(item="density") * self.units.kg / self.units.m ** 3

    @density.setter
    def density(self, value):
        self.update_item(item='density', value=value.value)

    #
    #
    @property
    def alpha(self):
        """"""
        return self.get_item(item="alpha") * self.units.K

    #
    @property
    def poisson(self):
        """"""
        return self.get_item(item="poisson")

    #
    @property
    def name(self):
        """ """
        return self.cls._title[self.index]

    @name.setter
    def name(self, name: Union[str, int]):
        """ """
        self.cls._title[self.index] = name
        #

    def update_item(self, item: str, value: float):
        """ """
        conn = create_connection(self.db_file)
        with conn:
            self._update_item(conn, self.number, item, value)
            conn.commit()

    #
    def _update_item(self, conn, name, item, value):
        """ """
        project = (value, name)
        sql = 'UPDATE tb_MatElastoPlastic SET {:} = ? WHERE material_number = ?'.format(item)
        cur = conn.cursor()
        cur.execute(sql, project)

    #
    #
    def get_item(self, item: str):
        """ """
        conn = create_connection(self.db_file)
        with conn:
            value = self._get_item(conn, self.number, item)
        return value

    #
    def _get_item(self, conn, name, item):
        """ """
        project = (name,)
        sql = 'SELECT {:} FROM tb_MatElastoPlastic WHERE material_number = ?'.format(item)
        cur = conn.cursor()
        cur.execute(sql, project)
        record = cur.fetchone()
        return record[0]

    #
    #
    # def set_default(self):
    #    """ """
    #    #name = self.cls._cls._labels[]
    #    index = self.cls._number.index(self.number)
    #    self.cls._default = self.cls._labels[index]
    #
    def __str__(self) -> str:
        return print_isomat(self)


#
#
class MaterialElasticSQL(Mapping):
    __slots__ = ['db_file', '_labels', '_default',
                 'f2u_units', '_title', '_number']

    def __init__(self, db_file: str):
        """
        """
        self.db_file = db_file
        #self._title: list[str | int] = []
        self._labels: list[str | int] = []
        self._number: array = array('I', [])
        # create node table
        self._create_table()

    #
    # @property
    # def type(self) -> str:
    #    """ Material type classification"""
    #    return  "elastic"
    #
    def __setitem__(self, material_name: int|str,
                    properties: list[float]) -> None:
        """
        """
        try:
            self._labels.index(material_name)
            raise Exception(f' *** warning material {material_name} already exist')
        except ValueError:
            self._labels.append(material_name)
            material_number = properties.pop(0)
            self._number.append(material_number)
            #self._title.append(properties.pop(0))
            #
            conn = create_connection(self.db_file)
            with conn:
                self._push_material(conn, material_number, properties)
                # conn.commit()
            

    #
    def __getitem__(self, material_name: int|str) -> tuple:
        """
        """
        try:
            index = self._labels.index(material_name)
            # index = self._cls._number.index(material_number)
            #material_name = self._cls._labels[index]
            #return GetMaterialSQL(self, material_number)
            conn = create_connection(self.db_file)
            return get_materialSQL(conn, material_name)
        except ValueError:
            raise IndexError('   *** material {:} does not exist'.format(material_name))

    #
    def _create_table(self) -> None:
        """ """
        _table_material = "CREATE TABLE IF NOT EXISTS tb_MatElastoPlastic(\
                            number INTEGER PRIMARY KEY,\
                            material_number INTEGER NOT NULL REFERENCES tb_Materials(number),\
                            Fy DECIMAL NOT NULL,\
                            Fu DECIMAL NOT NULL,\
                            E DECIMAL NOT NULL,\
                            G DECIMAL NOT NULL,\
                            poisson DECIMAL NOT NULL,\
                            density DECIMAL NOT NULL, \
                            alpha DECIMAL NOT NULL);"
        #
        conn = create_connection(self.db_file)
        create_table(conn, _table_material)

    #
    def _push_material(self, conn, material_number, properties):
        """
        """
        project = (material_number, *properties)
        sql = 'INSERT INTO  tb_MatElastoPlastic(material_number,\
                                                Fy, Fu, E, G, poisson, density , alpha)\
                                                VALUES(?,?,?,?,?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
        #return cur.lastrowid

    #
    def __len__(self) -> float:
        return len(self._labels)

    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels
    #
    #
    @property
    def df(self):
        """ raw data for dataframe"""
        conn = create_connection(self.db_file)
        with conn:
            cur = conn.cursor()
            cur.execute("SELECT tb_Materials.name, tb_Materials.type, tb_Materials.title, \
                        tb_MatElastoPlastic.*\
                        FROM tb_Materials, tb_MatElastoPlastic\
                        WHERE  tb_Materials.number = tb_MatElastoPlastic.material_number")
            rows = cur.fetchall()            
        
        #
        db = DBframework()
        header = ['name', 'type', 'title',
                  'number', 'material_number',
                  'Fy', 'Fu', 'E', 'G', 'poisson', 'density', 'alpha']
        matdf = db.DataFrame(data=rows, columns=header)        
        #
        header = ['name', 'type', 
                  'Fy', 'Fu', 'E', 'G', 'poisson', 'density', 'alpha', 'title']        
        return matdf[header]
    
    @df.setter
    def df(self, df):
        """ """
        #
        conn = create_connection(self.db_file)
        #
        # push material header
        dfmat = df[['name', 'type']].copy()
        dfmat['title'] = None        
        with conn:
            dfmat.to_sql('tb_Materials', conn,
                         index_label=['name', 'title', 'type'], 
                         if_exists='append', index=False)
        #
        self._labels.extend(dfmat['name'].tolist())
        #
        cur = conn.cursor()
        cur.execute("SELECT * from tb_Materials")
        rows = cur.fetchall()
        mat_name = {item[1]: item[0] for item in rows}
        #
        # push elastic material
        dfel = df[['Fy', 'Fu', 'E', 'G',
                   'poisson', 'density' , 'alpha']].copy()
        dfel['material_number'] = [int(mat_name[str(item)]) for item in df.name]
        with conn:
            dfel.to_sql('tb_MatElastoPlastic', conn,
                        index_label=['material_number',
                                     'Fy', 'Fu', 'E', 'G',
                                     'poisson', 'density' , 'alpha'], 
                        if_exists='append', index=False)
        #
        self._number.extend(dfel['material_number'].tolist())
        #print('--')    
#
#
#
#