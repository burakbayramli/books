from time import sleep
def long_func():
    sleep(10)

def med_func():
    sleep(4)

def short_func():
    sleep(0.1)

def redundant_func():
    for it in range(0,25):
        short_func()


if __name__ == "__main__":
    long_func()
    redundant_func()
    med_func()


