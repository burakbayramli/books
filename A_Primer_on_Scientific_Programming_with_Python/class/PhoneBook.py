class Person:
    def __init__(self, name,
                 mobile_phone=None, office_phone=None, 
                 private_phone=None, email=None):
        self.name = name
        self.mobile = mobile_phone
        self.office = office_phone
        self.private = private_phone
        self.email = email

    def add_mobile_phone(self, number):
        self.mobile = number
        
    def add_office_phone(self, number):
        self.office = number
        
    def add_private_phone(self, number):
        self.private = number

    def add_email(self, address):
        self.email = address

    def __str__(self):
        s = self.name + '\n'
        if self.mobile is not None:
            s += 'mobile phone:   %s\n' % self.mobile
        if self.office is not None:
            s += 'office phone:   %s\n' % self.office
        if self.private is not None:
            s += 'private phone:  %s\n' % self.private
        if self.email is not None:
            s += 'email address:  %s\n' % self.email
        return s

class PhoneBook:
    def __init__(self):
        self.contacts = {}   # dict of Person instances

    def add(self, name, mobile=None, office=None,
            private=None, email=None):
        p = Person(name, mobile, office, private, email)
        self.contacts[name] = p

    def __call__(self, name):
        return self.contacts[name]
    
    def __str__(self):
        s = ''
        for p in sorted(self.contacts):
            s += str(self.contacts[p]) + '\n'
        return s

def _test():
    b = PhoneBook()
    b.add('Ole Olsen', office='767828292',
          email='olsen@somemail.net')
    b.add('Hans Hanson',
          office='767828283', mobile='995320221')
    b.add('Per Person', mobile='906849781')
    print b('Per Person')
    print b
    
if __name__ == '__main__':
    _test()

