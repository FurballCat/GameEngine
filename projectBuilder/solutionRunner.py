import os


def open_solution( platform_name ):
    if platform_name == "Mac":
        print( "NOT IMPLEMENTED" )
    elif platform_name == "Windows":
        solution_file_path = os.path.join( os.getcwd(), "intermediate", "projects", "voodooEngine.sln" )
        exec( solution_file_path )
    else:
        raise Exception( "Not supported platform" )
