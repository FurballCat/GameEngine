import sys
import time

from commandLineParser import gather_command_line_arguments
from dataCollector import gather_all_projects_definitions
from thirdPartyDataCollector import gather_all_third_party_definitions
from genericGenerator import generate_projects
from messages import show_welcome_message
from platformUtils import resolve_platform
from platformUtils import open_projects

start_time = time.time()

show_welcome_message()
settings = gather_command_line_arguments()
platform_name = resolve_platform()
projects_definitions = gather_all_projects_definitions()
third_party_definitions = gather_all_third_party_definitions()
generate_projects( platform_name, projects_definitions, third_party_definitions )

if not settings.only_update:
    open_projects( platform_name )

end_time = time.time()
duration = round( end_time - start_time, 2 )

print( "Build time: {} sec".format( duration ) )

if not settings.devmode:
    exit()
