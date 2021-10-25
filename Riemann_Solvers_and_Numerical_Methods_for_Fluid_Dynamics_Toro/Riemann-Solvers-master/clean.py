import os

def action():
    r = os.getcwd()
    for f in os.listdir(r):
        if os.path.isfile(f):
            _, ext = os.path.splitext(f)
            if ext in ['.txt', '.out', '.exe', '.png'] or f.endswith('.DS_Store'):
                os.remove(f)


def clean(root_dir):
    t = os.path.abspath(root_dir)
    t = os.path.split(t)[-1]
    if(t == '.git'):
        return
    
    action()
    for f in os.listdir(root_dir):
        if os.path.isdir(f):
            os.chdir(os.path.join('.', f))
            clean('.')
            os.chdir('..')

if __name__ == '__main__':
    clean(os.getcwd())