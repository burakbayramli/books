"""
Improvement over PrmDictBase.
"""

class Data(object):
    def __init__(self, name=None, type_check=True, **parameters):
        self.name = name
        self.prm = {}   # or self.name2value
        self.type = {}  # or self.name2type
        self.register(type_check=type_check, **parameters)
        # what about extra data such as widget_type etc?
        # self.properties[name] can hold a dict for such things
        # self.properties[name]['type'] can replace self.type :-)

    def register(self, type_check=True, **parameters):
        self.prm.update(parameters)
        if type_check:
            for name in parameters:
                # numbers?
                if isinstance(parameters[name],
                              (int, float, complex, long)):
                    self.type[key] = (int, float, complex, long)
                else:
                    # some instance (list, str, tuple, ndarray, etc):
                    self.type[key] = type(parameters[name])

    def set(self, **parameters):
        for name in parameters:
            value = parameters[name]

            # is the name registered?
            if not name in self.prm:
                raise NameError, \
                      'Parameter "%s" is not registered' % name

            # do we need to check the type?
            if name in self.type:
                if callable(value):
                    # always accepted even if return value is wrong
                    pass 
                elif not isinstance(value, self.type[name]):
                    raise TypeError, \
                    'Parameter "%s" was registered with type %s '\
                    'while the value %s is of type %s' % \
                    (name, str(self.type[name]), value, type(value))

            self.prm[name] = value
        return True # if we come here, set was successful...

    def get(self, *names):
        for name in names:
            if not name in self.prm:
                raise ValueError, \
                      'Parameter "%s" is not registered' % name
        return [self.prm[name] for name in names]
    

# build a lsit of Data, then a tree of DataList?
class DataList(object):
    def __init__(self, type_check=True, **data_objects):
        self.list = {}
        self.register(type_check, **data_objects)

    def register(self, type_check=True, **data_objects):
        for name in data_objects:
            if isinstance(data_objects[name], dict):
                data = Data(type_check, **data_objects[name])
                self.list[name] = data
            elif isinstance(data_objects[name], Data):
                self.list[name] = data_objects[name]
            else:
                raise TypeError, '%s is not Data or dict' % name

    def set(self, data_object_name, paramter_name, value):


DataList(physics={'a': ...,...}, numerics={...}, type_check=True)


        

    
