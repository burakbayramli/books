Preliminary Material and Preparations
-------------------------------------

Many PDS students will come with no prior experience programming. This is fine- we
will work together to show you what it means to write code and develop software. The
journey from zero to versatile programmer will be long, with many difficulties along
the way. In particular, the first steps up the learning curve will be particularly
steep. With this in mind, I plan on having us taking these steps together. I ask
only that you come to class equipped with the programming tools to get started
on our journey towards becomming a practical data scientist. 

Below are several software systems that we will be using in class. I have given
notes for both Microsoft Windows and Apple OS-X. I will do what I can to help
users with other operating systems, but you'll probably need to do some exploring
on your own. I have installed all the relevant software on my Mac, the instructions
for Windows are slightly less well-tested. If you run into any hiccups or problems
during your install, I advise you to try the first lesson of a practical data
scientist- Google your error messages. Chances are, someone else encountered the 
same problem, and someone likely had a solution to this problem. [Stackoverflow](stackoverflow.com)
is another great resource, a compliation of programming-related questions and answers.
If you've spent 10 minutes looking online and are still stuck, please reach out to me.
I want you to learn how to solve problems, but I don't want you spinning your 
wheels. 

Unix
----

Unix is a powerful operating system, a crucial component of modern data science systems.

- Mac is already based on unix, (you can access unix shell through the "terminal" application). I prefer the [ITerm2](http://www.iterm2.com/#/section/home) shell application. I recommend Mac users install this.
- Windows users need some kind of unix emulator. Please [install cygwin](http://jattenberg.github.io/PDS-Fall-2013/assets/install/CygwinInstallationGuide.pdf). Some additional [cygwin notes](http://lifehacker.com/179514/geek-to-live--introduction-to-cygwin-part-i).


Python
------

Python is a powerful programming language, a central component of this course. It is broadly used in data science. However, despite python's power, it has a reputation as having a difficult to manage library system. Fortunately, the anaconda python distribution gives us all the libraries we'll be using in one convenient package. 

- Mac instructions for [installing anaconda python](http://docs.continuum.io/anaconda/install.html#mac-install)
- Windows instructions for [installing anaconda python](http://jattenberg.github.io/PDS-Fall-2013/assets/install/AnacondaPythonInstallationGuide.pdf) and getting it to work with cygwin. Note that you may wish to [check the anaconda site](http://continuum.io/downloads) for a more recent version of hte anaconda library.  [Official install documentation](http://docs.continuum.io/anaconda/install.html#windows-install).
 

MySQL
-----

MySQL is a popular relational database application. I'll be setting up a database that we'll be referencing in class, you'll be installing software to interface with this database server. 

- Mac: I've had luck with [Sequel Pro](http://www.sequelpro.com/). During class, I'll give instructions on how to connect this application to our database server.
- Windows: Previous students have had luck with sqlyog. [Software downloads here](https://code.google.com/p/sqlyog/downloads/list). 
 
Note that you can install the official MySQL client software using whatever package manager you like, or via the [MySQL website](http://www.mysql.com/).


Text Editor
-----------

At the most fundamental level, programs are text that you need to write. There are special text editors that give extra information to programmers, coloring certain key words in an informative way, etc. I don't have much of a preference as to which text editor you use. 

- [Sublime Text](http://www.sublimetext.com/) is a very powerful and popular text editor used by many programmers. I'd recommend this.
- Mac: [TextWrangler](http://www.barebones.com/products/textwrangler/) has been used with some success by students in the past
- Windows: [Notepad++](http://notepad-plus-plus.org/) has been used by previous students with Windows operating system.
 

Of course, at the end of the day, all you really need to do is write plain text documents, and there are many programs that can do this. Some students may be more familiar with vim or emacs or nano. These are also fine (in class, you'll see me using emacs). There are also more feature-rich programming environments, IDEs. These are also fine, but are a bit more than we'll be needing for this class. I also won't be able to help you with their setup and use. 

Git and Github
--------------

Git is a version control system, a great way for keeping track of changes in the code you write, and an convenient way to share software amongst multiple developers. Github is a website allowing you to host your software, and share it with the web. Once you start writing some code, you can put your github on your resume to show off! You can also look at the source code for systems or projects that interest you, see how they tick. 

The first step is to go to the [github site](https://github.com/) and create an account. [Instructions for setting up git](http://redmine.jamoma.org/projects/jamoma/wiki/Installing_and_setting_up_GIT) on Windows (using cygwin package management) or Mac are here, including how to configure your github account


Putting it Together
-------------------

In order to check that everything is working, try writing a very simple python program. Begin editing a new file, `hello.py`. You can put this file anywhere. In this file, type `print "hello world!"`. This is your first very simple program. To test it out, open a terminal, and type `python hello.py`. The program should print out the requested message. Note that you way need to provide the terminal with the complete path to the hello.py program if it can't find it. 

If you're looking to dig right into programming right away, I recommend checking out [Learn Python the Hard Way](http://learnpythonthehardway.org/book/). This is easy to read, and gives a primer on some python basics. Don't worry, we'll go over this stuff again in class. Remember that, we're using the anaconda distribution of python, and the text editor you installed from above, not what is suggested in Learn Python the Hard Way. 
