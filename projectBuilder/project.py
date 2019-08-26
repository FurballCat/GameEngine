# Structure of project definition used to generate specific project formats
import uuid

from platformUtils import resolve_platform

class ProjectDefinition:
    def __init__( self, directory, project_name, config_file, disable_on_active_platform ):
        self.directory = directory
        self.project_name = project_name
        self.config_file = config_file
        self.files = []
        self.excluded_files = []
        self.dependencies = []
        self.dependencies_osx = []
        self.dependencies_windows = []
        self.dependencies_thirdparty = []
        self.excluded_dirs_windows = []
        self.excluded_dirs_osx = []
        self.type = "console_application"
        self.guid = str( uuid.uuid4() ).upper()
        self.solution_filter = None
        self.disable_on_active_platform = disable_on_active_platform

    def add_file( self, path ):
        self.files.append( path )

    def get_excluded_directories(self):
        platform_name = resolve_platform()
        
        if platform_name == "Windows":
            return self.excluded_dirs_windows
        elif platform_name == "Mac":
            return self.excluded_dirs_osx
        else:
            return []
