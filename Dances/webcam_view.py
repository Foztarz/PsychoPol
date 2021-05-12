# -*- coding: utf-8 -*-
"""
Created on Thu May  6 09:09:55 2021
https://stackoverflow.com/a/11449901/660921
@author: jaf54iq
"""
import cv2
cv2.namedWindow("preview")
vc = cv2.VideoCapture(1)#0)

if vc.isOpened(): # try to get the first frame
    rval, frame = vc.read()
else:
    rval = False

while rval:
    cv2.imshow("preview", frame)
    rval, frame = vc.read()
    key = cv2.waitKey(20)
    if key == 27: # exit on ESC
        break
cv2.destroyWindow("preview")