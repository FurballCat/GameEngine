import os
import threading

from visualStudioProjectFiltersGenerator import generate_project_filters_file
from visualStudioProjectGenerator import generate_project_file
from visualStudioSolutionGenerator import generate_solution_file
from visualStudioPropertiesSheetGenerator import generate_all_shared_props

def generate_single_project( definition, projects_definitions, third_party_definitions ):
    print( "Project Builder is generating '{}' project file...".format( definition.project_name ) )

    project_file_path = os.path.join( os.getcwd(), "intermediate", "projects", definition.project_name + ".vcxproj" )
    project_filters_file_path = os.path.join( os.getcwd(), "intermediate", "projects", definition.project_name + ".vcxproj.filters" )

    generate_project_file( project_file_path, definition, projects_definitions, third_party_definitions )
    generate_project_filters_file( project_filters_file_path, definition )


def generate_all_projects( projects_definitions, third_party_definitions ):
    threads = []
    for definition in projects_definitions:
        t = threading.Thread(target=generate_single_project, args=(definition,projects_definitions, third_party_definitions))
        threads.append(t)
        t.start()

    for thread in threads:
        thread.join();




def generate_solution( projects_definitions ):
    solution_file_path = os.path.join( os.getcwd(), "voodooEngine.sln" )
    generate_solution_file( solution_file_path, projects_definitions )


def generate_environment_for_visual_studio( projects_definitions, third_party_definitions ):
    generate_all_shared_props()
    generate_all_projects( projects_definitions, third_party_definitions )
    generate_solution( projects_definitions )
