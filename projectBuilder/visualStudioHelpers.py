import os

def is_header_file( file ):
    filename, extension = os.path.splitext( file )
    return extension == ".h"

def is_source_file( file ):
    filename, extension = os.path.splitext( file )
    return extension == ".cpp" or extension == ".mm"

def is_assembler_file( file ):
    filename, extension = os.path.splitext( file )
    return extension == ".asm"

def is_target_file( file ):
    filename, extension = os.path.splitext( file )
    return extension == ".json"