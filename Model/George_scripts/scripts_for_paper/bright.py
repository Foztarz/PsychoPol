import numpy as np
import argparse
import cv2
import math

# Construct the argument parser and parse the arguments
ap = argparse.ArgumentParser()
ap.add_argument("-i", "--image", required=True, help="Path to the image file")
ap.add_argument("-r", "--radius", type=int, required=True, help="Radius of Gaussian blur; must be a positive odd number")
ap.add_argument("-s", "--scale", type=float, default=0.5, help="Scale factor for resizing the output images")
args = vars(ap.parse_args())

# Ensure the radius is positive and odd
if args["radius"] <= 0:
    raise ValueError("The radius must be a positive number")
if args["radius"] % 2 == 0:
    raise ValueError("The radius must be an odd number")

# Load the image
image = cv2.imread(args["image"])
if image is None:
    raise ValueError("Could not load image. Please check the path.")

# Get image dimensions
(h, w) = image.shape[:2]
center = (w // 2, h // 2)

# Convert the image to grayscale
gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

# Perform a naive attempt to find the (x, y) coordinates of the area with the largest intensity value
(minVal, maxVal, minLoc, maxLoc) = cv2.minMaxLoc(gray)
cv2.circle(image, maxLoc, 5, (255, 0, 0), 2)

# Resize the image for display
image_resized = cv2.resize(image, None, fx=args["scale"], fy=args["scale"], interpolation=cv2.INTER_LINEAR)

# Display the results of the naive attempt
cv2.imshow("Naive", image_resized)

# Apply a Gaussian blur to the image then find the brightest region
gray_blurred = cv2.GaussianBlur(gray, (args["radius"], args["radius"]), 0)
(minVal, maxVal, minLoc, maxLoc) = cv2.minMaxLoc(gray_blurred)

# Copy the original image and mark the brightest spot found after blurring
image_blurred = image.copy()
cv2.circle(image_blurred, maxLoc, args["radius"], (255, 0, 0), 2)

# Resize the blurred image for display
image_blurred_resized = cv2.resize(image_blurred, None, fx=args["scale"], fy=args["scale"], interpolation=cv2.INTER_LINEAR)

# Display the results of the newly improved method
cv2.imshow("Robust", image_blurred_resized)
cv2.waitKey(0)
cv2.destroyAllWindows()

# Calculate the angle
# Top-center point
top_center = (w // 2, 0)

# Vector from center to top-center
vector_top_center = (top_center[0] - center[0], top_center[1] - center[1])

# Vector from center to maxLoc
vector_maxLoc = (maxLoc[0] - center[0], maxLoc[1] - center[1])

# Calculate the angle between the vectors using the dot product
dot_product = vector_top_center[0] * vector_maxLoc[0] + vector_top_center[1] * vector_maxLoc[1]
magnitude_top_center = math.sqrt(vector_top_center[0] ** 2 + vector_top_center[1] ** 2)
magnitude_maxLoc = math.sqrt(vector_maxLoc[0] ** 2 + vector_maxLoc[1] ** 2)

# Avoid division by zero
if magnitude_top_center == 0 or magnitude_maxLoc == 0:
    angle = 0
else:
    cos_angle = dot_product / (magnitude_top_center * magnitude_maxLoc)
    angle = math.acos(cos_angle) * (180.0 / math.pi)
angle = 360 - angle
print(f"Sun azimuth: {angle:.2f} degrees")
print(f"Brightest pixel location: {maxLoc}")
