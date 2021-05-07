# -*- coding: utf-8 -*-
"""
Created on Fri May  7 15:48:11 2021

@author: jaf54iq
"""

    ##pip install git+https://github.com/genicam/harvesters.git && cd harvesters && python setup.py install
    #pip install arena_api-2.1.4-py3-none-any.whl
from harvesters.core import Harvester
#Then instantiate a Harvester object; we're going to use h that stands for Harvester as its identifier.
h = Harvester()
##And load a CTI file; loading a CTI file, you can communicate with the GenTL Producer:

# ATTENTION! Please use the CTI file in the original location!

# Why? Visit https://github.com/genicam/harvesters/wiki/FAQ and
# read "I pointed out a CTI file but Harvester says the image doesn't
# exist (Part 2)."

h.add_file('C:/Users/jaf54iq/Anaconda3/Lib/site-packages/genicam')
#Note that you can add one or more CTI files on a single Harvester Core object. To add another CTI file, just repeat calling add_file method passing another target CTI file:

    #h.add_file('path/to/another_gentl_producer.cti')
#And the following code will let you know the CTI files that have been loaded on the Harvester object:

h.files

#In a contrary sense, you can remove a specific CTI file that you have added with the following code:

    #h.remove_file('path/to/gentl_producer.cti')
    
#And now you have to update the list of remote devices; it fills up your device information list and you'll select a remote device to control from the list:

h.update()
#The following code will let you know the remote devices that you can control:

h.device_info_list
#Our friendly GenTL Producer, so called TLSimu, gives you the following information:

# [(unique_id='TLSimuMono', vendor='EMVA_D', model='TLSimuMono', tl_type='Custom', user_defined_name='Center', serial_number='SN_InterfaceA_0', version='1.2.3'),
#  (unique_id='TLSimuColor', vendor='EMVA_D', model='TLSimuColor', tl_type='Custom', user_defined_name='Center', serial_number='SN_InterfaceA_1', version='1.2.3'),
#  (unique_id='TLSimuMono', vendor='EMVA_D', model='TLSimuMono', tl_type='Custom', user_defined_name='Center', serial_number='SN_InterfaceB_0', version='1.2.3'),
#  (unique_id='TLSimuColor', vendor='EMVA_D', model='TLSimuColor', tl_type='Custom', user_defined_name='Center', serial_number='SN_InterfaceB_1', version='1.2.3')]
# And you create an image acquirer object specifying a target remote device. The image acquirer does the image acquisition task for you. In the following example it's trying to create an acquirer object of the first candidate remote device in the device information list:

ia = h.create_image_acquirer(0)
#Or equivalently:

ia = h.create_image_acquirer(list_index=0)
#You can connect the same remote device passing more unique information to the method. In the following case, we specify a serial number of the target remote device:

ia = h.create_image_acquirer(serial_number='SN_InterfaceA_0')
#You can specify a target remote device using properties that are provided through the device_info_list property of the Harvester class object. Note that it is invalid if the specifiers gives you two ore more remote devices. Please specify sufficient information so that the combination gives you a unique target remote device.

W#e named the image acquirer object ia in the above example but in a practical occasion, you may give it a purpose oriented name like ia_face_detection. Note that a camera itself does NOT acquirer/receive images but it just transmits them. In a machine vision application, there should be two roles at least: One transmits images and the other acquires them. The ImageAcquirer class objects play the latter role and it holds a camera as the remote_device object, the source of images.

#Anyway, then now we start image acquisition:

ia.start_acquisition()
#Once you started image acquisition, you should definitely want to get an image. Images are delivered to the acquirer allocated buffers. To fetch a buffer that has been filled up with an image, you can have 2 options; the first option is to use the with statement:

with ia.fetch_buffer() as buffer:
    # Work with the Buffer object. It consists of everything you need.
    print(buffer)
    # The buffer will automatically be queued.
#Having that code, the fetched buffer is automatically queued once the code step out from the scope of the with statement. It's prevents you to forget queueing it by accident. The other option is to manually queue the fetched buffer by yourself:

buffer = ia.fetch_buffer()
print(buffer)
# Don't forget to queue the buffer.
buffer.queue()
#In this option, again, please do not forget that you have to queue the buffer by yourself. If you forget queueing it, then you'll lose a buffer that could be used for image acquisition. Everything is up to your design, so please choose an appropriate way for you. In addition, once you queued the buffer, the Buffer object will be obsolete. There's nothing to do with it.

#Okay, then you would stop image acquisition with the following code:

ia.stop_acquisition()
#And the following code disconnects the connecting remote device from the image acquirer; you'll have to create an image acquirer object again when you have to work with a remote device:

ia.destroy()
#If you finished working with the Harvester object, then release the acquired resources calling the reset method:

h.reset()
#Now you can quit the program! Please not that Harvester and ImageAcquirer also support the with statement. So you may write program as follows:

with Harvester() as h:
    with h.create_image_acquirer(0) as ia:
        # Work, work, and work with the ia object.
        # The ia object will automatically call the destroy method
        # once it goes out of the block.

    # The h object will automatically call the reset method
    # once it goes out of the block.
#This way prevents you forget to release the acquired external resources. If this notation doesn't block your use case then you should rely on the with statement.


