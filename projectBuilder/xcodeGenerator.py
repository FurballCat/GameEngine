import os
import xmlSerializer
import xcode
import mac
import getpass


class WorkspaceSettingsXMLSaver:
    def __init__( self ):
        self.settings = {}

    def add_setting( self, key, value ):
        self.settings[key] = value

    def save_to_xml( self ):
        content = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" \
            "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" " \
                  "\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n" \
            "<plist version=\"1.0\">\n" \
            "<dict>\n"

        for key in self.settings:
            value = self.settings[key]
            content += "\t<key>%(k)s</key>\n" % {'k': key}

            if isinstance( value, str ):
                content += "\t<string>%(v)s</string>\n" % {'v': value}
            elif isinstance( value, bool ):
                if value:
                    content += "\t<true/>\n"
                else:
                    content += "\t<false/>\n"
            else:
                content += "\t<string>%(v)s</string>\n" % {'v': value}

        content += "</dict>\n" \
            "</plist>\n"

        return content


def generate_xcode_projects( projects_definitions, third_party_definitions ):

    # create workspace bundle
    source_path = os.getcwd() + "/source/"
    intermediate_path = os.getcwd() + "/intermediate/projects/"
    workspace_path = intermediate_path + "FurballCatGameEngine.xcworkspace/"
    mac.create_bundle( workspace_path )

    # create workspace data
    workspace = xcode.Workspace()

    project_name_to_path = {}
    for item in projects_definitions:
        project_name_to_path[item.project_name] = item.directory

    for project in projects_definitions:
        project_path = os.path.relpath( project.directory, source_path ) + "/"
        project_name = project.project_name
        project_bundle_path = project_path + project_name + ".xcodeproj"
        fill_project_path = intermediate_path + project_path
        group_path = os.path.relpath( fill_project_path + "../", intermediate_path )
        if group_path == ".":
            group_path = ""

        workspace.add_file( group_path, xcode.FileRef( project_bundle_path ) )

        mac.create_bundle( intermediate_path + project_bundle_path )

        # create project data
        xcode_project = xcode.Project( project_name, project.directory )

        for file in project.files:
            filename, extension = os.path.splitext( os.path.basename( file ) )
            if len( extension ) > 0:    # filter out hidden folders
                xcode_project.add_file( file )

        xcode_project_data = xcode_project.save( fill_project_path, source_path, project, project_name_to_path )

        xcode_project_file = open( intermediate_path + project_bundle_path + "/project.pbxproj", "w" )
        xcode_project_file.write( xcode_project_data )
        xcode_project_file.close()
        print( "Project '%(n)s' generated successfully." % {'n': project_name} )

    # serialize workspace data to XML
    serializer = xmlSerializer.XmlSerializer()
    workspace_xml = serializer.serialize( workspace )

    # create workspace content file
    workspace_content_path = workspace_path + "contents.xcworkspacedata"
    workspace_file = open( workspace_content_path, "w" )
    workspace_file.write( workspace_xml )
    workspace_file.close()

    # create workspace settings to set build and binaries output paths
    workspace_settings = WorkspaceSettingsXMLSaver()
    workspace_settings.add_setting( "BuildLocationStyle", "CustomLocation" )
    workspace_settings.add_setting( "CustomBuildIntermediatesPath", "../build" )
    workspace_settings.add_setting( "CustomBuildLocationType", "RelativeToWorkspace" )
    workspace_settings.add_setting( "CustomBuildProductsPath", "../../binaries" )
    workspace_settings.add_setting( "DerivedDataLocationStyle", "Default" )
    workspace_settings.add_setting( "IssueFilterStyle", "ShowActiveSchemeOnly" )
    workspace_settings.add_setting( "LiveSourceIssuesEnabled", True )

    workspace_user_data_path = workspace_path + "xcuserdata/" + getpass.getuser() + \
                               ".xcuserdatad/"

    os.makedirs( workspace_user_data_path )

    workspace_settings_file = open( workspace_user_data_path + "WorkspaceSettings.xcsettings", "w" )
    workspace_settings_file.write( workspace_settings.save_to_xml() )
    workspace_settings_file.close()
