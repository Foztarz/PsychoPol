# -*- coding: utf-8 -*-
"""
Created on Mon May 10 10:12:52 2021

@author: jaf54iq
"""

conda create --name ComputerVision python=2.7 -y
conda activate ComputerVision
#needs Spyder 3.3.6 to run at all. Deprecated 2020.01.01
#conda create --name ComputerVision python=3.7 -y
#conda list --revisions

#in TERMINAL
#source activate /Users/jamesfoster/opt/anaconda3/envs/ComputerVision

#conda install -c menpo opencv -y#menpo opencv is not yet 3.8 compatible
#conda install python=3.7#THIS WAS A VERY BAD IDEA

# conda create --name ComputerVision python=3.7 -y
#Open the environment in Anaconda Navigator, don't try it here
#check which environment you're in
conda info --envs
conda install -c menpo opencv

pip install opencv-python
#or conda install -c conda-forge opencv
#conda install -c anaconda numpy -y
conda install pandas -y
conda install -c anaconda sqlalchemy -y
#conda install -c conda- forge matplotlib -y
conda install matplotlib
