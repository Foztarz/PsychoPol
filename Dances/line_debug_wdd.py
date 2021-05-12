#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 11 18:02:33 2021

@author: jamesfoster
"""


# import click
import cv2
import os
import sys
import time
import numpy as np
from imageio import imsave

from multiprocessing_generator import ParallelGenerator
from skimage.transform import resize

from wdd.camera import OpenCVCapture, Flea3Capture, cam_generator
from wdd.processing import FrequencyDetector, WaggleDetector
from wdd.export import WaggleExporter

# def main(capture_type, video_device, height, width, fps, bee_length, binarization_threshold, max_frame_distance, 
         # min_num_detections, output_path, cam_identifier, background_path, debug, debug_frames):
cd '/Users/jamesfoster/Documents'

# FIXME: should be proportional to fps (how fast can a bee move in one frame while dancing)
capture_type = 'OpenCV'
video_device = 'DanceResults/WDD2019_sample.mp4'
height=180
width=342
fps=60
bee_length=7
binarization_threshold=6
max_frame_distance=0.2
min_num_detections=0.1
output_path= 'DanceResults'
cam_identifier=1
background_path='DanceResults'
debug=True
debug_frames=11

max_distance = bee_length
binarization_threshold = np.expm1(binarization_threshold)
max_frame_distance = max_frame_distance * fps
min_num_detections = min_num_detections * fps

if capture_type == 'OpenCV':
    cam_obj = OpenCVCapture
elif capture_type == 'PyCapture2':
    cam_obj = Flea3Capture
else:
    raise RuntimeError('capture_type must be either OpenCV or PyCapture2')

frame_generator = cam_generator(cam_obj, warmup=False, width=width, height=height, fps=fps, device=video_device,
                                background=None, fullframe_path=None)
_, _, frame_orig, _ = next(frame_generator)


full_frame_buffer_roi_size = bee_length * 10
pad_size = full_frame_buffer_roi_size // 2
full_frame_buffer_len = 100
full_frame_buffer = np.zeros((full_frame_buffer_len, frame_orig.shape[0] + 2 * pad_size, frame_orig.shape[1] + 2 * pad_size), dtype=np.uint8)

frame_scale = frame_orig.shape[0] / height, frame_orig.shape[1] / width

dd = FrequencyDetector(height=height, width=width, fps=fps)
exporter = WaggleExporter(cam_id=cam_identifier, output_path=output_path, full_frame_buffer=full_frame_buffer,
                          full_frame_buffer_len=full_frame_buffer_len, full_frame_buffer_roi_size=full_frame_buffer_roi_size)
wd = WaggleDetector(max_distance=max_distance, binarization_threshold=binarization_threshold, 
                    max_frame_distance=max_frame_distance, min_num_detections=min_num_detections,
                    dilation_selem_radius=7, exporter=exporter)

background_file = os.path.join(background_path, 'background_{}.npy'.format(cam_identifier))
if os.path.exists(background_file):
    print('Loading background image: {}'.format(background_file))
    background = np.load(background_file)
else:
    print('No background image found for {}, starting from scratch'.format(cam_identifier))
    background = None

fullframe_path = os.path.join(output_path, 'fullframes')
if not os.path.exists(fullframe_path):
    os.mkdir(fullframe_path)

frame_generator = cam_generator(cam_obj, warmup=True, width=width, height=height, fps=fps, device=video_device,
                                background=None, fullframe_path=None)

frame_idx = 0
start_time = time.time()

##this is where the problem is
## Traceback (most recent call last):
##     for ret, frame, frame_orig, background in gen:
##     return self.next()
##     "The generator died unexpectedly."
## GeneratorDied: The generator died unexpectedly.

#With warmup, produces lots of values which throws an error
# ret, frame, frame_orig, background = frame_generator
from matplotlib import pyplot as plt
plt.imshow(frame_orig, cmap = 'gray')
# for ii in frame_generator: plt.imshow(ii)
#Without warmup
# ret, frame, frame_orig, background = cam_generator(cam_obj,
#                                                    warmup=False, 
#                                                    width=width, 
#                                                    height=height, 
#                                                    fps=fps, 
#                                                    device=video_device,
#                                                    background=None, 
#                                                    fullframe_path=None)
# plt.imshow(frame_orig, cmap = 'gray')
#Would be nice to be able to check "ret"

full_frame_buffer[frame_idx % full_frame_buffer_len, pad_size:-pad_size, pad_size:-pad_size] = \
            (((frame_orig + 1) / 2) * 255).astype(np.uint8)
#read only the first frame            
plt.imshow(full_frame_buffer[frame_idx], cmap = 'gray')

# r = dd.process(frame, full_frame_buffer[1])


for ret, frame, frame_orig, background in frame_generator:
    if frame_idx % 10000 == 0:
        start_time = time.time()

    if not ret:
        print('Unable to retrieve frame from video device')
        break

    full_frame_buffer[frame_idx % full_frame_buffer_len, pad_size:-pad_size, pad_size:-pad_size] = \
        (((frame_orig + 1) / 2) * 255).astype(np.uint8)

    r = dd.process(frame, background)
    if r is not None:
            activity, frame_diff = r
            wd.process(frame_idx, activity)
    # print(frame_idx)
    if frame_idx % 10000 == 0:
            print('\nSaving background image: {}'.format(background_file))
            np.save(background_file, background)
        
    if debug and frame_idx % debug_frames == 0:
        current_waggle_num_detections = [len(w.xs) for w in wd.current_waggles]
        current_waggle_positions = [(w.ys[-1], w.xs[-1]) for w in wd.current_waggles]
        for blob_index, ((y, x), nd) in enumerate(zip(current_waggle_positions, current_waggle_num_detections)):
            cv2.circle(frame_orig, (int(x * frame_scale[0]), int(y * frame_scale[1])), 10, (0, 0, 255), 2)
                
        cv2.imshow('WDD', (((frame_orig + 1) / 2) * 255).astype(np.uint8))
        cv2.waitKey(1)
    if frame_idx % 60 == 0:
        end_time = time.time()
        processing_fps = ((frame_idx % 10000) + 1) / (end_time - start_time)
        #THIS COULD BE THE PROBLEM
        # sys.stdout.write('\rCurrently processing with FPS: {:.1f} | Max DD: {:.2f} | [{:16s} {}]'.format(processing_fps, np.log1p(activity.max()), cam_identifier, video_device))
        sys.stdout.flush()

    frame_idx = (frame_idx + 1)

# aa = ParallelGenerator(frame_generator, max_lookahead=fps)
# for ret, frame, frame_orig, background in gen:
#         if frame_idx % 10000 == 0:
#             start_time = time.time()
            
with ParallelGenerator(frame_generator, max_lookahead=fps) as gen:
    for ret, frame, frame_orig, background in gen:
        if frame_idx % 10000 == 0:
            start_time = time.time()

        if not ret:
            print('Unable to retrieve frame from video device')
            break

        full_frame_buffer[frame_idx % full_frame_buffer_len, pad_size:-pad_size, pad_size:-pad_size] = \
            (((frame_orig + 1) / 2) * 255).astype(np.uint8)

        r = dd.process(frame, background)
        if r is not None:
            activity, frame_diff = r
            wd.process(frame_idx, activity)

        if frame_idx % 10000 == 0:
            print('\nSaving background image: {}'.format(background_file))
            np.save(background_file, background)
        
        if debug and frame_idx % debug_frames == 0:
            current_waggle_num_detections = [len(w.xs) for w in wd.current_waggles]
            current_waggle_positions = [(w.ys[-1], w.xs[-1]) for w in wd.current_waggles]
            for blob_index, ((y, x), nd) in enumerate(zip(current_waggle_positions, current_waggle_num_detections)):
                cv2.circle(frame_orig, (int(x * frame_scale[0]), int(y * frame_scale[1])), 10, (0, 0, 255), 2)
                    
            cv2.imshow('WDD', (((frame_orig + 1) / 2) * 255).astype(np.uint8))
            cv2.waitKey(1)
        
        if frame_idx % 60 == 0:
            end_time = time.time()
            processing_fps = ((frame_idx % 10000) + 1) / (end_time - start_time)
            # sys.stdout.write('\rCurrently processing with FPS: {:.1f} | Max DD: {:.2f} | [{:16s} {}]'.format(processing_fps, np.log1p(activity.max()), cam_identifier, video_device))
            sys.stdout.flush()

        frame_idx = (frame_idx + 1)
if __name__ == '__main__':
    main()

