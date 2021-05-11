# -*- coding: utf-8 -*-
"""
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster        DATE: 2021 05 05
#     MODIFIED:	James Foster        DATE: 2021 05 05
#
#  DESCRIPTION: Install dependencies for wdd2 from github.com/BioroboticsLab
#
#      OUTPUTS: None.
#
#	   CHANGES: -
#
#   REFERENCES: Wario, F., Wild, B., Rojas, R., Landgraf, T., 2017.
#               Automatic detection and decoding of honey bee waggle dances.
#               PLoS ONE 12, 1–16.
#               https://doi.org/10.1371/journal.pone.0188626

#
#TODO
#- Check packages
#- Test video
#- Test own video

"""
pip install click
#conda install sys # not found
#conda install time # not found
pip install imageio
#conda install distutils # not found
#pip install pyflycapture2
# downloaded https://github.com/GreenSlime96/PyCapture2_NumPy
pip install git+https://github.com/Foztarz/bb_wdd2.git@WIP

#before running check
python bb_wdd2/wdd/scripts/bb_wdd.py --help

#bare minimum would be something like
cd /Users/jamesfoster/Documents
python Github/bb_wdd2/wdd/scripts/bb_wdd.py --capture_type OpenCV --video_device DanceResults/WDD2019_sample.mp4  --output_path DanceResults --cam_identifier laptop  --background_path DanceResults
