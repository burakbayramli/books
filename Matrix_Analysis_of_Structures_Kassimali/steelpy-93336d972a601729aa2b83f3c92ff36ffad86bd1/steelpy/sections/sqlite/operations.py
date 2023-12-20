#
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
#

# package imports
#
from steelpy.f2uModel.mesh.sqlite.process_sql import create_connection #, create_table
from ..process.operations import ShapeProperty, get_sect_prop_df
from steelpy.utils.dataframe.main import DBframework

#
class SectionSQLite:
    """ """
    #__slots__ = ['db_file']

    def __init__(self, db_file :str):
        """
        Shear Stress: MAXIMUM / AVERAGE
        section = (name, title, type,
                   diameter, wall_thickess,
                   height, web_thickness,
                   top_flange_width, top_flange_thickness,
                   bottom_flange_width, bottom_flange_thickness,
                   SA_inplane, SA_outplane,
                   shear_stress, build, compactness,)
        """
        self.db_file = db_file
        self._properties = None
        self._labels:list = []
        self._number:list = []
        # create table
        #conn = create_connection(self.db_file)
        #with conn:
        #    self.number = self._push_section_table(conn, section)
    #
    #
    def update_item(self, item :str, value :float):
        """ """
        conn = create_connection(self.db_file)
        with conn:
            self._update_item(conn, self.number, item, value)
    #
    def _update_item(self, conn, name, item, value):
        """ """
        project = (value, name)
        sql = 'UPDATE tb_Sections SET {:} = ? WHERE number = ?'.format(item)
        cur = conn.cursor()
        cur.execute(sql, project)
    #
    def get_item(self, item :str):
        """ """
        conn = create_connection(self.db_file)
        with conn:
            value = self._get_item(conn, self.number, item)
        return value
    #
    def _get_item(self, conn, name, item):
        """ """
        project = (name,)
        sql = 'SELECT {:} FROM tb_Sections WHERE number = ?'.format(item)
        cur = conn.cursor()
        cur.execute(sql, project)
        record = cur.fetchone()
        return record[0]
    #
    #
    def push_property(self, name):
        """ """
        index = self._labels.index(name)
        number = self._number[index]
        #try:
        properties = self.properties
        self._push_property(number, properties)
        #except:
        #    pass
    #
    def _push_property(self, number, properties :tuple):
        """ """
        conn = create_connection(self.db_file)
        with conn:
            self._push_property_table(conn, number, properties)
    #
    def _push_property_table(self, conn, section_number :int, properties :list[float]):
        """ """
        project = (section_number, *properties)
        sql = 'INSERT INTO  tb_SecProperties(number, area, Zc, Yc,\
                                            Iy, Zey, Zpy, ry,\
                                            Iz, Zez, Zpz, rz,\
                                            J, Cw)\
                                            VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
    #
    #
    #
    def push_section(self, section):
        """ """
        # create table
        conn = create_connection(self.db_file)
        with conn:
            number = self._push_section_table(conn, section)
        return number
    #
    def _push_section_table(self, conn, section) -> int:
        """
        """
        project = section
        sql = 'INSERT INTO  tb_Sections(name, title, type,\
                                        diameter, wall_thickness,\
                                        height, web_thickness,\
                                        top_flange_width, top_flange_thickness,\
                                        bottom_flange_width, bottom_flange_thickness,\
                                        fillet_radius,\
                                        SA_inplane, SA_outplane,\
                                        shear_stress, build, compactness)\
                                        VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
        return cur.lastrowid
    #
    #
    #
    def get_section(self, section_name):
        """ """
        conn = create_connection(self.db_file)
        with conn:
            row = self._get_section(conn, section_name)
        return row[1:]
    #
    def _get_section(self, conn, section_name:int|str):
        """
        """
        cur = conn.cursor()
        #cur.execute("SELECT tb_Sections.name, tb_Sections.number, tb_SecProperties.*\
        #            from tb_Sections, tb_SecProperties \
        #            WHERE tb_Sections.number = {:} \
        #            AND tb_SecProperties.number = tb_Sections.number;".format(section_number))
        #
        if isinstance(section_name, str):
            cur.execute(f"SELECT * from tb_Sections \
                        WHERE tb_Sections.name = '{section_name}';")
        else:
            cur.execute(f"SELECT * from tb_Sections \
                        WHERE tb_Sections.name = {section_name};")
        row = cur.fetchone()
        #sections = PropertyOut(*row[1:])
        #conn.close()
        #print("--->")
        return row
    #
    #
    #
    @property
    def df(self):
        """ """
        db = DBframework()
        conn = create_connection(self.db_file)
        #with conn:
        df = db.read_sql_query("SELECT * FROM tb_Sections", conn)          
        return df 
    
    @df.setter
    def df(self, df):
        """ """
        # 
        df = get_sect_prop_df(df)
        conn = create_connection(self.db_file)
        with conn:
            df.to_sql('tb_Sections', conn,
                         index_label=['name', 'type', 'title',
                                      'diameter', 'wall_thickness',
                                      'height', 'web_thickness',
                                      'top_flange_width', 'top_flange_thickness',
                                      'bottom_flange_width', 'bottom_flange_thickness',
                                      'fillet_radius',
                                      'SA_inplane', 'SA_outplane',
                                      'shear_stress', 'build', 'compactness'], 
                         if_exists='append', index=False)
        #
        self._labels.extend(df['name'].tolist())
        # TODO : fix numbering
        nitems = len(self._number) + 1
        self._number.extend([item + nitems for item in df.index])
        #print('-->')
#
def get_sections(conn, component_name):
    """
    """
    sections = {}
    cur = conn.cursor()
    cur.execute("SELECT tb_Sections.name, tb_Sections.number, tb_SecProperties.*\
                from tb_Sections, tb_SecProperties\
                WHERE  tb_SecProperties.number = tb_Sections.number;")
    rows = cur.fetchall()
    for row in rows:
        # data = [row[0], *row[4:]]
        # print(row)
        sections[row[0]] = ShapeProperty._make(row[3:])
    # conn.close()
    # print("--->")
    return sections

