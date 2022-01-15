# This file is an extension for the ACT-R Environment to actually 
# display .gif files as images in the visible-virtual-windows
# using the image-vdi objects created in the tools/image-feature.lisp
# file.
# 
# All of the .gif files to display must be available in the
# gui/AGI-images directory.
#
# Mouse clicks on the item will be handled in the virtual
# view on the Lisp side just like they would if the window
# were not shown even if a person clicks on it (the window
# sends all clicks back to Lisp where they get processed).


# The code below adds agi handlers that perform the necessary
# changes to cleanly provide the image items using the new
# visible-virtual extension mechanism with add_agi_handler.
#


# When the window closes delete the images to save mem.
 
proc agi_image_close_handler {cmd win coords params} {

  foreach i [$win.can find withtag image] {
    $win.can dtag $i image
    set name [lindex [$win.can gettags $i] 0]
    image delete $name
  }
}

add_agi_handler close agi_image_close_handler


# When the window is cleared remove the image items

proc agi_image_clear_handler {cmd win coords params} {
  $win.can delete image 
}

add_agi_handler clear agi_image_clear_handler


# When an item is removed delete it to save mem.

proc agi_image_remove_handler {cmd win coords params} {

  set tags [$win.can gettags [lindex $params 0]]
  set tindex [lsearch -exact $tags image]
  if {$tindex != -1} {
    image delete [lindex $params 0]
  }
}

add_agi_handler remove agi_image_remove_handler


# When an image cmd is sent read the file from the images
# directory in gui and add it to the window's canvas.
# The params sent are:  item_name x y file_name width height

proc agi_image_creation_handler {cmd win coords params} {

  global tcl_env_dir
  # Assume that it exists to keep the example simple.    

  image create photo [lindex $params 0] -file [file join $tcl_env_dir "AGI-images" [lindex $params 3]] -width [lindex $params 4] -height [lindex $params 5]
                                                                                                     
  # Place that image into the display.  Again, this is kept simple for an example, but it
  # should probably actually first create a 'dummy' image and subsample or zoom to fit 
  # the width and height specified instead of just clipping the image.

  $win.can create image [lindex $params 1] [lindex $params 2] -anchor nw -image [lindex $params 0] -tags [list [lindex $params 0] image] 
} 

add_agi_handler image agi_image_creation_handler
