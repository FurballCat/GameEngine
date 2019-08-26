import os
import shutil

from visualStudioGenerator import generate_environment_for_visual_studio
from xcodeGenerator import generate_xcode_projects


def prepare_directory_for_projects():
    destination_directory = os.path.join( os.getcwd(), "intermediate", "projects" )

    if os.path.exists( destination_directory ):
        shutil.rmtree( destination_directory )

    os.makedirs( destination_directory )


def generate_projects( platform_name, projects_definitions, third_party_definitions ):
    prepare_directory_for_projects()

    if platform_name == "Mac":
        generate_xcode_projects( projects_definitions, third_party_definitions )
    elif platform_name == "Windows":
        generate_environment_for_visual_studio( projects_definitions, third_party_definitions )
    else:
        raise Exception( "Not supported platform" )
