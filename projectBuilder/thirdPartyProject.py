class ThirdPartyProjectDefinition:
    def __init__( self, directory, project_name, config_file ):
        self.directory = directory
        self.project_name = project_name
        self.config_file = config_file
        self.include_paths = []
        self.static_libraries_paths_windows = []
        self.dynamic_libraries_paths_windows = []
        self.static_libs_to_link_windows = []
        self.dynamic_libs_to_copy_windows = []
