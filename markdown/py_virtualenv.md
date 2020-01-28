## Create and use a virtualenv project

install with pip
```
pip install virtualenv
```

go to the desired directory and create a virtualenv project
```
cd myproj
virtualenv myproj
```

to specify a python version use the -p argument
```
virtualenv -p /usr/bin/python2.7 myproj
```

start using the virtualenv
```
source myproj/bin/activate
```

install some packages within `myproj` virtualenv project and then export the packages list and tjeir versions to a text file
```
pip freeze --local > packages.txt
```

in order to leave the virtual envirnoment just enter:
```
deactivate
```

to install all dependencies of the virtualenv `myproj` on a new setup enter:
```
pip install -r packages.txt
```