# -*- coding: utf-8 -*-
"""
Created on Tue Oct 12 14:59:28 2021

@author: jaf54iq
"""

"""
#load modules necessary for file selection
"""

import tkinter as tk
# from tkinter.filedialog import askopenfilename
# from tkinter.filedialog import askdirectory
from tkinter import filedialog
import os

#hide the Tk window
root = tk.Tk()
root.withdraw()

"""
#ask the user to find their version of the InvertPy module
"""
inpy = filedialog.askdirectory(title = 'Please select your version of InvertPy',
                                 # filetypes = (  
                                 #                  ('python scripts', '*.py'), 
                                 #                  ('All files', '*.*') 
                                 #              )
                                 )
inpy_dir = os.path.dirname(inpy)
#os.chdir(inpy_dir)
#best practice to set up a context manager when changing directory
#https://stackoverflow.com/a/13197763/3745353
class cd:
    """Context manager for changing the current working directory"""
    def __init__(self, newPath):
        self.newPath = os.path.expanduser(newPath)

    def __enter__(self):
        self.savedPath = os.getcwd()
        os.chdir(self.newPath)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.savedPath)

cd(inpy_dir)

import sys
sys.path
"""
# Import InvertPy
"""
conda info --envs 
#if invertpy is not currently activated, activate it
        # conda activate invertpy
#make sure that invpy is installed in this environment
sys.path.append(inpy)
sys.path
pip install C:/Users/jaf54iq/src/InvertPy
#make sure requirements are met
pip install -r C:/Users/jaf54iq/src/InvertPy/requirements.txt
#this is missing from requirements!
conda install matplotlib

import invertpy
"""
# Try out some examples
"""
# runfile(inpy +'/examples/create_bee_eye.py', wdir = inpy)
# FileNotFoundError: [Errno 2] No such file or directory: 'C:\\Users\\jaf54iq\\Anaconda3\\envs\\invertpy\\Lib\\data\\eyes\\bee_right.csv'
# guess I'm not allowed to save there?
# mkdir 'C:\\Users\\jaf54iq\\Anaconda3\\envs\\invertpy\\Lib\\data\\eyes'
adr = os.path.dirname(inpy_dir) + '/Anaconda3/envs/invertpy/Lib/'
os.mkdir(adr + 'data')
os.mkdir(adr + 'data/eyes')
os.listdir(inpy +'/examples')
        # ['create_bee_eye.py',
        #  'run_compass.py',
        #  'run_compound_eye.py',
        #  'run_cx.py',
        #  'run_incentive_circuit.py',
        #  'test_zernike_moments.py']
runfile(inpy +'/examples/create_bee_eye.py', wdir = inpy)

#now load a sky
pip install C:/Users/jaf54iq/src/InvertSy
#make sure requirements are met
pip install -r C:/Users/jaf54iq/src/InvertSy/requirements.txt
# after a pip install of https://github.com/InsectRobotics/InvertSy.git
#InvertSy not working
sys.path.append('C:/Users/jaf54iq/src/InvertSy/')
#I edited invertsy.agent to get it working
        # runfile('C:/Users/jaf54iq/src/InvertSy/examples/test_sky.py', wdir = 'C:/Users/jaf54iq/src/InvertSy/')
        # from invertsy.env.sky import Sky
        # from invertsy.simplot._plots import plot_sky

runfile(inpy + '/examples/test_zernike_moments.py')

runfile(inpy + '/examples/run_compass.py')

runfile(inpy + '/examples/run_compound_eye.py')

# runfile(inpy + '/examples/run_cx.py') # cannot import name 'BeeCentralComplex' from 'invertpy.brain.centralcomplex'

runfile(inpy + '/examples/run_incentive_circuit.py')
