# 
# Copyright (c) 2018-2021 steelpy


# Python stdlib imports
import datetime
from typing import List
#

# package imports
#from steelpy.process.io_module.text import search_line

#
#
def print_header(code_detail:List[str], 
                 text_lenght:int=87, 
                 version:str="BETA Version"):
    """
    """
    _margin = text_lenght - 2
    today = datetime.date.today()
    output = []
    #
    output.append("{:}\n".format(text_lenght*"*"))
    output.append("*{:}CODE CHECK TOOL{:}*\n".format(35*" ", 35*" "))
    text, step, end = get_margins(_margin, code_detail[0])
    output.append("*{:}{:}{:}*\n".format(step*" ", text, end*" "))
    text, step, end = get_margins(_margin, code_detail[1])
    output.append("*{:}{:}{:}*\n".format(step*" ", text, end*" "))
    text, step, end = get_margins(_margin, version)
    output.append("*{:}{:}{:}*\n".format(step*" ", text, end*" "))
    output.append("{:}\n".format(87*"*"))
    output.append("DATE: {:8s} {:>59}".format(str(today), ""))
    # 
    return output
#
def get_margins(_margin, text):
    """ """
    text = text.strip()
    lenght_text = len(text)
    rem = _margin - lenght_text
    step = rem//2
    end = rem - step
    return text, step, end