# -*- coding: utf-8 -*-
"""
# Details ---------------------------------------------------------------
#       AUTHOR:	Lucid Vision Labs   DATE: 2021 04 07
#     MODIFIED:	James Foster        DATE: 2023 03 14
#
#  DESCRIPTION: Captures images from a Lucid PHX050S1-P camera. Adapted from 
#              py_image_buffer_save_Mono8_to_png_with_PIL.py  and
#              py_sequencer_HDR.py in Lucid's Arena SDK.
#               
#      OUTPUTS: Images as bitmap (png).
#
#	   CHANGES: -
#
#   REFERENCES: Sony Polarization Image Sensor range
#               https://www.sony-semicon.co.jp/products/common/pdf/IMX250_264_253MZR_MYR_Flyer_en.pdf
# 
#TODO   
#- Test run  +
#- Image acquisition + 
#- Image brackets +
#- Set acquisition rate
#- GUI
#- Interactive exposure choice
#- Neaten up          

# -----------------------------------------------------------------------------
# Copyright (c) 2021, Lucid Vision Labs, Inc.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# -----------------------------------------------------------------------------
"""
#user defined exposure time in us
user_exposure = 5004.408#0.02*1000000
#user exposure value steps
user_ev_step = 2
#user acquisition frame rate
user_acquisition_rate_Hz =0.5#0.1 minimum
#user save directory, not yet implemented
# user_savedir = 'C:/Users/jaf54iq/Documents/'

def exposure_HDR_function(user_exposure, count, steps):
    """
    This function will act like a generator. 
    every time it is triggered would return the exposure as str in the
    format shown
    """
    yield(str( np.round(user_exposure*steps**(count-1), 3) ))
    # while True:
    #     now = datetime.now()
    #     yield now.strftime('%H_%M_%S_%f')


#from HDR
import os
import time
from pathlib import Path

from arena_api import enums
from arena_api.__future__.save import Writer
from arena_api.system import system

#from save to PNG
import ctypes  # ctypes.cast(), ctypes.POINTER(), ctypes.c_ushort
import numpy as np  # pip install numpy
from PIL import Image as PIL_Image  # pip install Pillow


# Note: buffer contains an image with or without chunkdata

user_ev_step = np.float64(user_ev_step)

print(f'Exposures will be {(user_exposure*user_ev_step**np.float64([ii -1 for ii in range(0,3)]))/1000000}s :')
                #I would like to view the middle image before saving
                # from matplotlib import pyplot as plt

def create_devices_with_tries():
    """
    This function waits for the user to connect a device before raising
    an exception
    """

    tries = 0
    tries_max = 6
    sleep_time_secs = 10
    while tries < tries_max:  # Waits for devices
        devices = system.create_device()
        if not devices:
            print(
                f'Try {tries+1} of {tries_max}: waiting for {sleep_time_secs} '
                f'secs for a device to be connected!')
            for sec_count in range(sleep_time_secs):
                time.sleep(1)
                print(f'{sec_count + 1 } seconds passed ',
                      '.' * sec_count, end='\r')
            tries += 1
        else:
            print(f'Created {len(devices)} device(s)')
            return devices
    else:
        raise Exception(f'No device found! Please connect a device and run '
                        f'the example again.')


def set_sequencer_set(nodemap, set_number, exposure_time, path_next_set,
                      trigger_source):

    # Set Sequencer Set Selector to sequence number
    nodemap['SequencerSetSelector'].value = set_number
    print(f'Updating set {set_number} :')

    # Set Exposure Time to the desired value
    nodemap['SequencerFeatureSelector'].value = 'ExposureTime'
    nodemap['ExposureTime'].value = exposure_time
    print(f'\texposure time value = {exposure_time}')

    # Select the path we want it to follow from this set to the next set. There
    # can be multiple paths so the first path will always be set to 0
    nodemap['SequencerPathSelector'].value = 0

    # Set next state in the sequence, ensure it does not exceed the maximum
    nodemap['SequencerSetNext'].value = path_next_set
    print(f'\tset next            = {path_next_set}')

    # Set Sequencer Trigger Source to Frame Start
    nodemap['SequencerTriggerSource'].value = trigger_source
    print(f'\ttrigger source      = {trigger_source}')

    # Save current state
    # Once all appropriate settings have been configured, make sure to
    # save the state to the sequence. Notice that these settings will be
    # lost when the camera is power-cycled.
    print(f'\tSave sequence set {set_number}')
    nodemap['SequencerSetSave'].execute()


def acquire_and_save_buffers(device):

    # Get width, height, and pixel format nodes
    width_node = device.nodemap['Width']
    height_node = device.nodemap['Height']
    pixelformat_node = device.nodemap['PixelFormat']

    if not width_node.is_readable or \
            not height_node.is_readable or \
            not pixelformat_node.is_readable:
        raise Exception('Width, Height, or PixelFormat node is not readable')

    pixelformat_node.value = 'Mono8'

    # Starting the stream allocates buffers, which can be passed in as
    # an argument (default: 10), and begins filling them with data.
    print('\nStart streaming')
    with device.start_stream(3):

        # Get an image buffer in each set of sequencer
        print('Getting 3 image buffers')

        # Save images
        #   Create an image writer
        #   The writer, optionally, can take width, height, and bits per pixel
        #   of the image(s) it would save. if these arguments are not passed
        #   at run time, the first buffer passed to the Writer.save()
        #   function will configure the writer to the arguments buffer's width,
        #   height, and bits per pixel

        writer = Writer()

        # Run our 3 sets one time
        for count in range(3):
            print(f'\tConverting and saving image {count}')
            
            # FROM  py_save_writer_advanced.py
            writer.register_tag(name='middle_exposure',
                            generator=exposure_HDR_function(user_exposure = user_exposure,
                                                            count = np.float64(count),
                                                            steps = np.float64(user_ev_step)
                                                            )
                            )
            
            writer.pattern = 'HDRcapture_--<middle_exposure>us_<count>.tiff'
            # Get image
            buffer = device.get_buffer()

            # Default name for the image is 'image_<count>.jpg' where count
            # is a pre-defined tag that gets updated every time a buffer image
            # is saved. More custom tags can be added using
            # Writer.register_tag() function
            writer.save(buffer)
            print(f'Image saved {writer.saved_images[-1]}')
            
            # FROM py_image_buffer_save_Mono8_to_png_with_PIL.py
            
            # image_buffer = buffer
            # # Buffer.pdata is a (uint8, ctypes.c_ubyte) type
            # # Buffer.data is a list of elements each represents one byte.
            # # Since Mono8 uses 16Bits (2 bytes), It is easier to user Buffer.pdata
            # # over Buffer.data. Buffer.pdata must be cast to (uint16, c_ushort)
            # # so every element in the array would represent one pixel.
            # pdata_as16 = ctypes.cast(image_buffer.pdata,
            #                          ctypes.POINTER(ctypes.c_ushort))
            # nparray_reshaped = np.ctypeslib.as_array(
            #     pdata_as16,
            #     (image_buffer.height, image_buffer.width))
    
            # # Saving --------------------------------------------------------------
            # print('Saving image')
            
            # nodemap = device.nodemap
            # exposure_time_str = str(nodemap['ExposureTime'].value)
            # # png_name = f'from_{pixel_format_name}_to_png_with_pil.png'
            # tiff_name = f'--{exposure_time_str}us.tiff'
    
            # # ---------------------------------------------------------------------
            # # These steps are due to a bug in Pillow saving 16bits png images
            # # more : https://github.com/python-pillow/Pillow/issues/2970
    
            # nparray_reshaped_as_bytes = nparray_reshaped.tobytes()
            # tiff_array = PIL_Image.new('I', nparray_reshaped.T.shape)
            # tiff_array.frombytes(nparray_reshaped_as_bytes, 'raw', 'I;16')
            # # ---------------------------------------------------------------------
            # plt.imshow(tiff_array)
            
            # tiff_array.save(tiff_name)
            

            # Requeue image buffer
            device.requeue_buffer(buffer)
        print(f'Requeued {count + 1} buffers')

    # Stream stops automatically when the scope of the context manager ends
    print('Stream stopped')


def set_exposure_auto_to_off(nodemap):

    # If Sequencer Configuration Mode is 'On', it makes 'ExposureAuto'
    # a read-only
    if nodemap['SequencerConfigurationMode'].value == 'On':
        print('Turn \'SequencerConfigurationMode\' Off')
        nodemap['SequencerConfigurationMode'].value = 'Off'
        print(f'\t\'SequencerConfigurationMode\' is '
              f'''{nodemap['SequencerConfigurationMode'].value} now''')

    print('Turn \'ExposureAuto\' Off')
    nodemap['ExposureAuto'].value = 'Off'
    print(f'''\t\'ExposureAuto\' is {nodemap['ExposureAuto'].value} now''')

#INSPIRED BY THE ABOVE
def set_acqusition_frame_rate_enabled(nodemap, user_acquisition_rate_Hz):

    # # If Sequencer Configuration Mode is 'On', it makes 'ExposureAuto'
    # # a read-only
    # if nodemap['SequencerConfigurationMode'].value == 'On':
    #     print('Turn \'SequencerConfigurationMode\' Off')
    #     nodemap['SequencerConfigurationMode'].value = 'Off'
    #     print(f'\t\'SequencerConfigurationMode\' is '
    #           f'''{nodemap['SequencerConfigurationMode'].value} now''')

    print('Set \'AcquisitionFrameRateEnable\' true')
    nodemap['AcquisitionFrameRateEnable'].value = True
    print(f'''\t\'AcquisitionFrameRateEnable\' is {nodemap['AcquisitionFrameRateEnable'].value} now''')
    nodemap['AcquisitionFrameRate'].value = np.float64(user_acquisition_rate_Hz)


def set_sequencer_configuration_mode_on(nodemap):

    # If Sequencer Mode is 'On', it makes 'SequencerConfigurationMode'
    # a read-only
    if nodemap['SequencerMode'].value == 'On':
        print('Turn \'SequencerMode\' Off')
        nodemap['SequencerMode'].value = 'Off'
        print(
            f'''\t\'SequencerMode\' is {nodemap['SequencerMode'].value} now''')

    print('Turn \'SequencerConfigurationMode\' On')
    nodemap['SequencerConfigurationMode'].value = 'On'
    print(f'\t\'SequencerConfigurationMode\' is '
          f'''{nodemap['SequencerConfigurationMode'].value} now''')


def example_entry_point():

    # Create a device
    devices = create_devices_with_tries()
    device = devices[0]
    print(f'Device used in the example:\n\t{device}')

    nodemap = device.nodemap
    tl_stream_nodemap = device.tl_stream_nodemap

    #  Set up nodes -----------------------------------------------------------

    # Enable stream auto negotiate packet size
    tl_stream_nodemap['StreamAutoNegotiatePacketSize'].value = True

    # Enable stream packet resend
    tl_stream_nodemap['StreamPacketResendEnable'].value = True    

    # Disable automatic exposure and gain before setting an exposure time.
    # Automatic exposure and gain controls whether they are set manually or
    # automatically by the device. Setting automatic exposure and gain to
    # 'Off' stops the device from automatically updating the exposure time
    # while streaming.
    set_exposure_auto_to_off(nodemap)
    #inspired by the above
    set_acqusition_frame_rate_enabled(nodemap, user_acquisition_rate_Hz)
    # nodemap['AcquisitionFrameRate'] = np.float64(user_acquisition_rate_Hz)
    
    #check all settings are below maximum
    ex_max = nodemap['ExposureTime'].max
    print(f'maximum possible exposure for this frame rate now {np.round(ex_max/1000000,6)}s')
    # If 'SequencerMode' is on, turn it off so the sequencer becomes
    # configurable through 'SequencerConfigurationMode'.
    # Put sequencer in configuration mode.
    # Sequencer configuration mode must be on while making changes to
    # the sequencer sets.
    set_sequencer_configuration_mode_on(nodemap)

    # Set up sequencer sets ---------------------------------------------------

    # From device.nodemap['SequencerSetSelector'].max gives the maximum
    # of sequencer sets can be set on the device.

    # Make sure the example works with all devices.
    # Take the smaller value to set a long exposure time of some devices
    # exposure_time_long = min(nodemap['ExposureTime'].max, 100000.0)

    # print('Set up sequencer sets')
    # sets_settings = [
    #     {
    #         'set_number': 0,
    #         'exposure_time': exposure_time_long / 40,
    #         'path_next_set': 1,
    #         'trigger_source': 'FrameStart'
    #     },
    #     {
    #         'set_number': 1,
    #         'exposure_time': exposure_time_long / 20,
    #         'path_next_set': 2,
    #         'trigger_source': 'FrameStart'
    #     },
    #     {
    #         'set_number': 2,
    #         'exposure_time': exposure_time_long,
    #         'path_next_set': 0,  # Means it goes back to the set in index 0
    #         'trigger_source': 'FrameStart'
    #     }
    # ]
    exposure_time_medium = user_exposure
    
    print('Set up sequencer sets')
    sets_settings = [
        {
            'set_number': 0,
            'exposure_time': min(ex_max, np.round(exposure_time_medium / user_ev_step, 3) ),
            'path_next_set': 1,
            'trigger_source': 'FrameStart'
        },
        {
            'set_number': 1,
            'exposure_time': min(ex_max, np.round(exposure_time_medium, 3) ),
            'path_next_set': 2,
            'trigger_source': 'FrameStart'
        },
        {
            'set_number': 2,
            'exposure_time': min(ex_max, np.round(exposure_time_medium * user_ev_step,3) ),
            'path_next_set': 0,  # Means it goes back to the set in index 0
            'trigger_source': 'FrameStart'
        }
    ]

    for set_settings in sets_settings:
        set_sequencer_set(nodemap, **set_settings)

    # Sets the sequencer starting set to 0
    print('Set stream to start from sequencer set 0')
    nodemap['SequencerSetStart'].value = 0

    # Turn off configuration mode
    print('Turn \'SequencerConfigurationMode\' Off')
    nodemap['SequencerConfigurationMode'].value = 'Off'
    print(f'\t\'SequencerConfigurationMode\' is '
          f'''{nodemap['SequencerConfigurationMode'].value} now''')

    # Turn on sequencer
    #    When sequencer mode is on and the device is streaming it will
    #    follow the sequencer sets according to their saved settings.
    print('Turn \'SequencerMode\' On')
    nodemap['SequencerMode'].value = 'On'
    print(f'''\t\'SequencerMode\' is {nodemap['SequencerMode'].value} now''')

    # Acquire and Save image buffers ------------------------------------------

    # This function will start the stream, acquire a buffer in each set
    # of the sequencer using its corresponding settings, save each buffer
    # and then stop the stream.
    acquire_and_save_buffers(device)

    # Clean up ------------------------------------------------------------

    # Turn sequencer mode off so the device is set to the original settings
    print('Turn \'SequencerMode\' Off')
    nodemap['SequencerMode'].value = 'Off'
    print(f'''\t\'SequencerMode\' is {nodemap['SequencerMode'].value} now''')

    # Destroy all created devices. This call is optional and will
    # automatically be called for any remaining devices when the system module
    # is unloading.
    system.destroy_device()
    print('Destroyed all created devices')


if __name__ == '__main__':
    print('\nWARNING:\nCHANGING DEVICE SETTINGS!')
    print('\nCapture started\n')
    example_entry_point()
    print('\nCapture completed successfully')
