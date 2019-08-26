import os


def create_bundle( path ):
    if not os.path.exists( path ):
        os.makedirs( path )
        # Mac call to set the xcode workspace / project as bundle, skip it on
        # Windows
        os.system( "setfile -a B " + path )
