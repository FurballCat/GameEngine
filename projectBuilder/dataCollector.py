import json
import os

from project import ProjectDefinition
import platformUtils
from platformUtils import get_excluded_directories_for_active_platform

def load_data_from_config_file( project_definition ):
    config_file = open( project_definition.config_file, "r" )
    target_data = json.load( config_file )
    project_definition.dependencies = target_data["dependecies"]
    project_definition.type = target_data["type"]
    if "dependencies_osx" in target_data:
        project_definition.dependencies_osx = target_data["dependencies_osx"]
    if "dependencies_windows" in target_data:
        project_definition.dependencies_windows = target_data["dependencies_windows"]
    if "dependencies_thirdparty" in target_data:
        project_definition.dependencies_thirdparty = target_data["dependencies_thirdparty"]
    if "excluded_dirs_windows" in target_data:
        project_definition.excluded_dirs_windows = target_data["excluded_dirs_windows"]
    if "excluded_dirs_osx" in target_data:
        project_definition.excluded_dirs_osx = target_data["excluded_dirs_osx"]
    config_file.close()


def collect_project_files( project_definition ):
    for root, dirs, files in os.walk(project_definition.directory, topdown=True):
        excluded_directories = project_definition.get_excluded_directories()
        for file in files:
            file_path = os.path.join( root, file )
            path_list = file_path.split(os.sep)
            should_be_exluded = any(x in path_list for x in excluded_directories)
            if should_be_exluded:
                project_definition.excluded_files.append( file_path )
            else:
                project_definition.files.append( file_path )

def collect_data_for_project( project_definition ):
    load_data_from_config_file( project_definition )
    collect_project_files( project_definition )


def collect_all_data_for_every_project( project_definitions ):
    for definition in project_definitions:
        collect_data_for_project( definition )


def gather_all_projects_definitions():
    projects_definitions = []

    print( "Project Builder is gathering targets description..." )

    excluded_directories = get_excluded_directories_for_active_platform()

    source_directory_path = os.path.join( os.getcwd(), "source" )
    for (root, dirs, files) in os.walk( source_directory_path, topdown=True ):
        for file in files:
            if file == "Target.json":
                path, folder_name = os.path.split( root )
                file_path = os.path.join( root, file )
                path_list = file_path.split(os.sep)
                should_be_exluded = any(x in path_list for x in excluded_directories)
                projects_definitions.append( ProjectDefinition( root, folder_name, file_path, should_be_exluded ) )

    print( "Project Builder found {} projects to create".format( len( projects_definitions ) ) )

    collect_all_data_for_every_project( projects_definitions )

    return projects_definitions
