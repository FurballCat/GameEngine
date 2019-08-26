import os

from sys import platform as _platform


def resolve_platform():
    if _platform == "darwin":
        return "Mac"
    elif _platform == "win32":
        return "Windows"
    elif _platform == "linux2":
        return "Linux"

    return "unknown"


def open_projects( platform_name ):
    if platform_name == "Windows":
        solution_file_path = os.path.join( os.getcwd(), "voodooEngine.sln" )
        os.startfile( solution_file_path )
    elif platform_name == "Mac":
        return
    elif platform_name == "Linux":
        return
    else:
        raise Exception( "Unsupported platform" )


def get_excluded_directories_for_active_platform():
    active_platform_name = resolve_platform()
    
    if active_platform_name == "Windows":
        return ["mac", "linux"]
    elif active_platform_name == "Mac":
        return ["windows", "linux"]
    elif active_platform_name == "Linux":
        return ["mac", "windows"]
