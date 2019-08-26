import json
import os

from thirdPartyProject import ThirdPartyProjectDefinition

def load_data_from_config_file( third_party_definitions ):
    config_file = open( third_party_definitions.config_file, "r" )
    target_data = json.load( config_file )

    if "include_paths" in target_data:
        third_party_definitions.include_paths = target_data["include_paths"]
    if "static_libraries_paths_windows" in target_data:
        third_party_definitions.static_libraries_paths_windows = target_data["static_libraries_paths_windows"]
    if "dynamic_libraries_paths_windows" in target_data:
        third_party_definitions.dynamic_libraries_paths_windows = target_data["dynamic_libraries_paths_windows"]
    if "static_libs_to_link_windows" in target_data:
        third_party_definitions.static_libs_to_link_windows = target_data["static_libs_to_link_windows"]
    if "dynamic_libs_to_copy_windows" in target_data:
        third_party_definitions.dynamic_libs_to_copy_windows = target_data["dynamic_libs_to_copy_windows"]

    config_file.close()


def collect_all_data_for_every_third_party_project( third_party_definitions ):
    for definition in third_party_definitions:
        load_data_from_config_file( definition )


def gather_all_third_party_definitions():
    third_party_definitions = []

    print( "Project Builder is gathering targets description for third party libraries..." )

    source_directory_path = os.path.join( os.getcwd(), "thirdparty" )
    for (root, dirs, files) in os.walk( source_directory_path, topdown=True ):
        for file in files:
            if file == "Target.json":
                path, folder_name = os.path.split( root )
                file_path = os.path.join( root, file )
                third_party_definitions.append(ThirdPartyProjectDefinition( root, folder_name, file_path ) )

    print( "Project Builder found {} third party projects.".format( len( third_party_definitions ) ) )

    collect_all_data_for_every_third_party_project( third_party_definitions )

    return third_party_definitions
