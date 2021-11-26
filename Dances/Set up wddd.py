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
conda install git
# downloaded https://github.com/GreenSlime96/PyCapture2_NumPy
# pip install git+https://github.com/Foztarz/bb_wdd2.git@WIP
pip install git+https://github.com/Foztarz/bb_wdd2.git@master #DDormagen has updated this

pip install pytz
pip install opt_einsum
pip install numba
pip install sklearn
#before running check
# python bb_wdd2/wdd/scripts/bb_wdd.py --help
C:\Users\jaf54iq\Documents\GitHub\PsychoPol\Dances\WDD\venv\Scripts\bb_wdd.exe --help
# Options:
#   --capture_type TEXT             Whether to use OpenCV or PyCapture2 to
#                                   acquire images  [default: PyCapture2]
#   --video_device TEXT             OpenCV video device. Can be camera index or
#                                   video path  [required]
#   --height INTEGER                Video frame height in px (before
#                                   subsampling).  [default: 180]
#   --width INTEGER                 Video frame width in px (before
#                                   subsampling).  [default: 342]
#   --subsample INTEGER             Fast subsampling by using every Xth pixel of
#                                   the images.
#   --fps INTEGER                   Frames per second  [default: 60]
#   --bee_length INTEGER            Approximate length of a bee in px (before
#                                   subsampling).  [default: 7]
#   --binarization_threshold FLOAT  Binarization threshold for waggle detection
#                                   in log scale. Can be used to tune
#                                   sensitivity/specitivity  [default: 6.0]
#   --max_frame_distance FLOAT      Maximum time inbetween frequency detections
#                                   within one waggle in seconds  [default: 0.5]
#   --min_num_detections FLOAT      Minimum time of a waggle in seconds
#                                   [default: 0.1]
#   --output_path PATH              Output path for results.  [required]
#   --cam_identifier TEXT           Identifier of camera (used in output storage
#                                   path).  [required]
#   --debug                         Enable debug outputs/visualization
#                                   [default: False]
#   --debug_frames INTEGER          Only visualize every debug_frames frame in
#                                   debug mode (can be slow if low)  [default:
#                                   11]
#   --no_multiprocessing            Do not use a multiprocessing queue to fetch
#                                   the images.
#   --no_warmup                     Do not warm up the image retrieval..
#   --start_timestamp TEXT          Instead of using the wall-clock, generate
#                                   camera timestamps based on the FPS starting
#                                   at this iso-formatted timestamp (example:
#                                   '2019-08-30T12:30:05.000100+00:00').
#   --autoopt TEXT                  Automatically optimize hyperparameters given
#                                   a CSV file with annotations.
#   --eval TEXT                     Check the detected waggles against a given
#                                   CSV file and print evaluation results.
#   --roi INTEGER...                Specify a region of interest in pixels in
#                                   the original video. The four arguments are
#                                   'left, top, width, height'.
#   --ipc TEXT                      Socket address to send out detections to
#                                   (e.g. 'localhost:9901:password').
#   --record_video TEXT             Specify the OpenCV FourCC code to record a
#                                   video to instead of processing (e.g. 'HFYU'
#                                   or 'png ').
#   --help                          Show this message and exit.


#bare minimum would be something like
# cd /Users/jamesfoster/Documents
# python Github/bb_wdd2/wdd/scripts/bb_wdd.py --capture_type OpenCV --video_device DanceResults/WDD2019_sample.mp4  --output_path DanceResults --cam_identifier laptop  --background_path DanceResults
cd C:/Users/jaf54iq/Documents
C:\Users\jaf54iq\Documents\GitHub\PsychoPol\Dances\WDD\venv\Scripts\bb_wdd --capture_type OpenCV --video_device DanceResults/WDD2019_sample.mp4  --output_path DanceResults --cam_identifier laptop  --output_path DanceResults --no_multiprocessing --debug
