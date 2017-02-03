import sys, argparse, os
# eval on input text does not work for string objects, use
# str2obj instead as a better eval:
from scitools.misc import str2obj

class ReadInput:
    """Simplified beginner's version."""
    def __init__(self, parameters):
        self.p = parameters

    def get(self, parameter_name):
        return self.p[parameter_name]

    def get_all(self):
        return [self.p[name] for name in sorted(self.p)]

    def __str__(self):
        import pprint
        return pprint.pformat(self.p)

# Here is a class with a more sophisticated user interface
    
class ReadInput:
    def __init__(self, parameters):
        self.p = parameters

    def get_all(self):
        """
        Return all parameter values in a list,
        sorted alphabetically according to the parameter names.

        Example:
        >>> inp = ReadInput(dict(filename='tmp.dat', n=3, a=0, b=1))
        >>> # extract data into variables listed alphabetically.
        >>> a, b, filename, n = inp.get_all()  
        """
        return [self.p[name] for name in sorted(self.p)]
        
    def get(self, *parameter_names):
        if len(parameter_names) == 1:
            return self.p[parameter_names[0]]
        else:
            return [self.p[name] for name in parameter_names]

    def set(self, **name_value_pairs):
        for name in name_value_pairs:
            self.p[name] = name_value_pairs[name]

    def __str__(self):
        import pprint
        return pprint.pformat(self.p)


class PromptUser(ReadInput):
    def __init__(self, parameters):
        ReadInput.__init__(self, parameters)
        self._prompt_user()

    def _prompt_user(self):
        for name in self.p:
            self.p[name] = str2obj(raw_input("Give " + name + ": "))


class ReadInputFile(ReadInput):
    def __init__(self, parameters):
        ReadInput.__init__(self, parameters)
        self._read_file()

    def _read_file(self, infile=sys.stdin):
        for line in infile:
            if "=" in line:
                name, value = line.split("=")
                self.p[name.strip()] = str2obj(value.strip())


class ReadCommandLine(ReadInput):
    def __init__(self, parameters):
        self.sys_argv = sys.argv[1:]  # copy
        ReadInput.__init__(self, parameters)
        self._read_command_line()

    def _read_command_line(self):
        parser = argparse.ArgumentParser()
        # Make argparse list of options
        for name in self.p:
            # Default type: str
            parser.add_argument('--'+name, default=self.p[name])

        args = parser.parse_args()
        for name in self.p:
            self.p[name] = str2obj(getattr(args, name))

import Pmw, Tkinter

class GUI(ReadInput):
    def __init__(self, parameters):
        ReadInput.__init__(self, parameters)
        self.root = Tkinter.Tk()
        Pmw.initialise(self.root)
        self._make_GUI(self.root)
        self.root.mainloop()

    def _make_GUI(self, master):
        self.widget_list = []
        self.widget_dict = {}
        for name in self.p:
            w = Pmw.EntryField(master,
                               labelpos='w',
                               label_text=name,
                               value=self.p[name],
                               entry_width=15)
            w.pack()
            self.widget_list.append(w)
            self.widget_dict[name] = w
        Pmw.alignlabels(self.widget_list)
        Tkinter.Button(master, text="Run program",
                       command=self._read_from_GUI).pack(pady=4)

    def _read_from_GUI(self, event=None):
        for name in self.p:
            self.p[name] = str2obj(self.widget_dict[name].get())
        self.root.destroy()


def _test1(p):
    outfile = open("tmp.inp", "w")
    for name in p:
        outfile.write("%-20s = %s\n" % (name, p[name]))
    outfile.close()

    # Construct command line
    default_options = []
    for name in p:
        default_options.append('--%s "%s"' % (name, p[name]))
    # Let the user's command-line args override the default ones (come last)
    sys.argv = [sys.argv[0]] + default_options + sys.argv[1:]
                        
    inp = ReadCommandLine(p)
    print "From the command line:\n", inp

    # Redirect sys.stdin to the tmp.inp file
    infile = open("tmp.inp")
    sys_stdin_orig = sys.stdin
    sys.stdin = infile
    inp = ReadInputFile(p)
    print "From file:\n", inp
    infile.close()
    
    sys.stdin = sys_stdin_orig
    inp = PromptUser(p)
    print "Prompt user:\n", inp

    inp = GUI(p)
    print "GUI:\n", inp
    
if __name__ == "__main__":
    _test1(dict(a=1, b=0))
    _test1(dict(formula='0', a=0, b=1, n=2, filename='tmp.dat'))

    
    
    
    
