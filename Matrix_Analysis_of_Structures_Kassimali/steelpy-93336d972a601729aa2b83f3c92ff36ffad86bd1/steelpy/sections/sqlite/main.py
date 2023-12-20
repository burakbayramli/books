# 
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
#from collections.abc import Mapping
import re
#

# package imports
from steelpy.f2uModel.mesh.sqlite.process_sql import create_connection, create_table
#
from .angle import AngleSQLite
from .tubular import TubularSQL
from .tee import TeeSQLite
from .channel import ChannelSQLite
from .box import BoxSQLite
from .ibeam import IbeamSQLite
from .solid import SolidSectionSQL
#from ..process.operations import get_sect_properties #, SectionBasic
#
from ..inmemory.tubular import TubularBasic
from ..inmemory.solid import RectangleBasic, CircleBasic, Trapeziod
from ..inmemory.ibeam import IbeamBasic
from ..inmemory.box import BoxBasic
from ..inmemory.channel import ChannelBasic
from ..inmemory.tee import TeeBasic
from ..inmemory.angle import AngleBasic
from ..inmemory.operations import SectionBasic
#
from steelpy.utils.dataframe.main import DBframework

#
class SectionSQL(SectionBasic):
    __slots__ = ['_labels', '_number', '_title', '_type',
                 '_tubular', '_solid', '_ibeam', '_box',
                 '_channel', '_tee', '_angle', '_default',
                 'db_file']    

    def __init__(self, db_file: str,
                 db_system: str = "sqlite"):
        """
        """
        super().__init__()
        #
        self.db_file = db_file
        self._create_table()
        #
        self._tubular = TubularSQL(db_file=db_file)
        self._solid = SolidSectionSQL(db_file=db_file)
        self._ibeam = IbeamSQLite(db_file=db_file)
        self._box = BoxSQLite(db_file=db_file)
        self._channel = ChannelSQLite(db_file=db_file)
        self._tee = TeeSQLite(db_file=db_file)
        self._angle = AngleSQLite(db_file=db_file)

    #
    # 
    #
    # def push_sections(self):
    #    """
    #    """
    #    conn = create_connection(self.bd_file)
    #    with conn:
    #        for key, section in self._sections.items():
    #            sect_number = self._push_section_table(conn, section)
    #            section.number = sect_number
    #            self._push_property_table(conn, section)
    #        conn.commit()
    #
    # def _push_property_table(self, conn, section):
    #    """ """
    #    project = (section.number, *section.properties)
    #    sql = 'INSERT INTO  tb_SecProperties(number, area, Zc, Yc,\
    #                                        Iy, Zey, Zpy, ry,\
    #                                        Iz, Zez, Zpz, rz,\
    #                                        J, Cw)\
    #                                        VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
    #    cur = conn.cursor()
    #    cur.execute(sql, project)
    #
    # def _push_section_table(self, conn, section) -> int:
    #    """
    #    """
    #    project = section._get_section_table()
    #    sql = 'INSERT INTO  tb_Sections(name, title, type, diameter, wall_thickness,\
    #                                    height, web_thickness,\
    #                                    top_flange_width, top_flange_thickness,\
    #                                    bottom_flange_width, bottom_flange_thickness)\
    #                                    VALUES(?,?,?,?,?,?,?,?,?,?,?)'
    #    cur = conn.cursor()
    #    cur.execute(sql, project)
    #    return cur.lastrowid
    #
    def _create_table(self) -> None:
        """ """
        table_sections = "CREATE TABLE IF NOT EXISTS tb_Sections (\
                            number INTEGER PRIMARY KEY NOT NULL,\
                            name NOT NULL,\
                            title TEXT,\
                            type TEXT NOT NULL,\
                            diameter DECIMAL,\
                            wall_thickness DECIMAL,\
                            height DECIMAL,\
                            web_thickness DECIMAL,\
                            top_flange_width DECIMAL,\
                            top_flange_thickness DECIMAL,\
                            bottom_flange_width DECIMAL,\
                            bottom_flange_thickness DECIMAL,\
                            fillet_radius DECIMAL, \
                            SA_inplane DECIMAL, \
                            SA_outplane DECIMAL,\
                            shear_stress TEXT, \
                            build TEXT, \
                            compactness TEXT);"
        #
        table_properties = "CREATE TABLE IF NOT EXISTS tb_SecProperties (\
                            number INTEGER PRIMARY KEY NOT NULL REFERENCES tb_Sections(number),\
                            area DECIMAL,\
                            Zc DECIMAL,\
                            Yc DECIMAL,\
                            Iy DECIMAL,\
                            Zey DECIMAL,\
                            Zpy DECIMAL,\
                            ry DECIMAL,\
                            Iz DECIMAL,\
                            Zez DECIMAL,\
                            Zpz DECIMAL,\
                            rz DECIMAL,\
                            J DECIMAL,\
                            Cw DECIMAL);"
        conn = create_connection(self.db_file)
        create_table(conn, table_sections)
        create_table(conn, table_properties)
    #
    #
    @property
    def df(self):
        """ """
        db = DBframework()
        #
        conn = create_connection(self.db_file)
        with conn:
            df = db.read_sql_query("SELECT * FROM tb_Sections", conn)
        #
        df.drop(columns=['number'], inplace=True)
        title = df.pop('title')
        df.insert(len(df.columns), 'title', title)
        return df
#
#
#
# def get_properties(self):
#    """
#    """
#    summary = {}
#    for key, item in self._sections.items():
#        summary[key] = item._get_properties()
#    return summary
#
#
def get_section2(section_name):
    """ """
    conn = create_connection(self.db_file)
    with conn:
        row = _get_section(conn, section_name)
    return row[1:]
#
def _get_section(conn, section_number:int):
    """
    """
    cur = conn.cursor()
    cur.execute("SELECT * from tb_Sections \
                WHERE tb_Sections.number = {:};".format(section_number))
    row = cur.fetchone()
    #print("--->")
    return row
#
# TODO: repeated code
def get_section(conn, section_name: int|str):
    """ """
    #
    cur = conn.cursor()
    try:
        cur.execute("SELECT * from tb_Sections \
                    WHERE tb_Sections.name = {:};".format(section_name))
        row = cur.fetchone()

    except :
        cur.execute("SELECT * from tb_Sections")
        rows = cur.fetchall()
        for item in rows:
            if item[1] ==  section_name:
                row = item
                break
        #1 / 0
    #
    row = row[1:]
    shape_type = row[2]    
    #
    if re.match(r"\b(tub(ular)?|pipe)\b", shape_type, re.IGNORECASE):
        return TubularBasic(name=row[0], 
                            diameter=row[3], thickness=row[4]) 

    #elif re.match(r"\b((solid|bar(\_)?)?rectangle|trapeziod|circular|round)\b", shape_type, re.IGNORECASE):
    #    return self._solid[shape_name]
    elif re.match(r"\b((solid|bar(\_)?)?circular|round)\b", shape_type, re.IGNORECASE):
        d = row[3]
        return CircleBasic(name=row[0], d=d, type=shape_type)

    elif re.match(r"\b((solid|bar(\_)?)?rectangle)\b", shape_type, re.IGNORECASE):
        d = row[3]
        wb = row[7]
        return RectangleBasic(name=row[0], depth=d, width=wb,
                              type=shape_type)

    elif re.match(r"\b((solid|bar(\_)?)?trapeziod)\b", shape_type, re.IGNORECASE):
        d = row[5]
        wb = row[7]
        wt = row[9]            
        c = abs(wt - wb) / 2.0
        return Trapeziod(name=row[0], depth=d, width=wb,
                         a=wt, c=c, type=shape_type)    
    
    elif re.match(r"\b(i((\_)?beam|section)?|w|m|s|hp|ub|uc|he|ipe|pg)\b", shape_type, re.IGNORECASE):
        return IbeamBasic(name=row[0], 
                          d=row[5], tw=row[6],
                          bft=row[7], tft=row[8],
                          bfb=row[9], tfb=row[10])
    
    elif re.match(r"\b(b(ox)?|rhs|shs)\b", shape_type, re.IGNORECASE):
        return BoxBasic(name=row[0], 
                d=row[5], tw=row[6],
                b=row[7], tb=row[8])
    
    elif re.match(r"\b(c(hannel)?)\b", shape_type, re.IGNORECASE):
        return ChannelBasic(name=row[0], 
                    d=row[5], tw=row[6],
                    b=row[7], tb=row[8])
    
    elif re.match(r"\b(t(ee)?)\b", shape_type, re.IGNORECASE):
        return TeeBasic(name=row[0], 
                        d=row[5], tw=row[6],
                        b=row[7], tb=row[8])
    
    elif re.match(r"\b(l|angle)\b", shape_type, re.IGNORECASE):
        return AngleBasic(name=row[0], 
                          d=row[5], tw=row[6],
                          b=row[7], r=0)
    
    else:
        raise IOError(f' Section type {shape_type} not recognised')    