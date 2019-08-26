import os
import xml.etree.cElementTree as xml_doc

from xmlUtils import format_xml_tree


def generate_project_configurations( project_node, configurations_names, platforms_names ):
    item_group_node = xml_doc.SubElement( project_node, "ItemGroup", Label="ProjectConfigurations" )

    for configuration_name in configurations_names:
        for platform_name in platforms_names:
            configurations_node = xml_doc.SubElement( item_group_node, "ProjectConfiguration",
                                                     Include=(configuration_name + "|" + platform_name) )
            xml_doc.SubElement( configurations_node, "Configuration" ).text = configuration_name
            xml_doc.SubElement( configurations_node, "Platform" ).text = platform_name


def generate_project_globals( project_node, project_name, project_guid ):
    property_group_node = xml_doc.SubElement( project_node, "PropertyGroup", Label="Globals" )

    visual_studio_project_version = "15.0"
    windows_target_platform_version = "10.0.17134.0"

    xml_doc.SubElement( property_group_node, "VCProjectVersion" ).text = visual_studio_project_version
    xml_doc.SubElement( property_group_node, "ProjectGuid" ).text = "{" + project_guid + "}"
    xml_doc.SubElement( property_group_node, "RootNamespace" ).text = project_name
    xml_doc.SubElement( property_group_node, "WindowsTargetPlatformVersion" ).text = windows_target_platform_version

    xml_doc.SubElement( project_node, "Import", Project="$(VCTargetsPath)\Microsoft.Cpp.Default.props" )



def generate_project_conditions( project_node, configurations_names, platforms_names, project_type ):
    platform_toolset = "v141"
    character_set = "MultiByte"

    if project_type == "dynamic_library":
        configuration_type = "DynamicLibrary"
    elif project_type == "utility":
        configuration_type = "Utility"
    elif project_type == "application":
        configuration_type = "Application"
    elif project_type == "console_application":
        configuration_type = "Application"

    for configuration_name in configurations_names:
        for platform_name in platforms_names:
            name_mix = configuration_name + "|" + platform_name
            property_group_node = xml_doc.SubElement( project_node, "PropertyGroup",
                                                     Condition="'$(Configuration)|$(Platform)'=='" + name_mix + "'",
                                                     Label="Configuration" )
            xml_doc.SubElement( property_group_node, "ConfigurationType" ).text = configuration_type
            xml_doc.SubElement( property_group_node,
                               "UseDebugLibraries" ).text = "true" if configuration_name == "Debug" else "false"
            xml_doc.SubElement( property_group_node, "PlatformToolset" ).text = platform_toolset
            xml_doc.SubElement( property_group_node, "CharacterSet" ).text = character_set


def generate_project_property_sheet_imports( project_node, configurations_names, platforms_names ):
    debug_shared_props_name = "sharedPropertySheetDebug.props"
    release_shared_props_name = "sharedPropertySheetRelease.props"

    for configuration_name in configurations_names:
        for platform_name in platforms_names:
            name_mix = configuration_name + "|" + platform_name
            import_group_node = xml_doc.SubElement( project_node, "ImportGroup",
                                                   Condition="'$(Configuration)|$(Platform)'=='" + name_mix + "'",
                                                   Label="PropertySheets" )
            xml_doc.SubElement( import_group_node, "Import",
                               Project="$(UserRootDir)\Microsoft.Cpp.$(Platform).user.props",
                               Condition="exists('$(UserRootDir)\Microsoft.Cpp.$(Platform).user.props')",
                               Label="LocalAppDataPlatform" )
            xml_doc.SubElement( import_group_node, "Import",
                              Project=debug_shared_props_name if configuration_name == "Debug" else release_shared_props_name )


def expand_additional_include_paths_to_one_text_entry( additional_include_paths ):
    entry = ""

    if additional_include_paths:
        for path in additional_include_paths:
            entry += path
            entry += ";"

    entry += "%(AdditionalIncludeDirectories)"
    return entry


def find_third_party_project_definition(third_party_definitions, third_party_dependency_name):
    for definition in third_party_definitions:
        if definition.project_name.lower() == third_party_dependency_name.lower():
            return definition;
    return None


def generate_project_item_definition_groups( project_node, configurations_names, platforms_names, definition, all_definitions, third_party_definitions ):
    additional_include_paths = []
    additional_link_paths = []
    link_dependencies = []

    additional_include_paths.append( definition.directory )
    additional_include_paths.append( definition.directory + "\\internal" )

    if definition.dependencies_thirdparty:
        additional_include_paths.append( os.path.join( os.getcwd(), "thirdparty" ) )
        for third_party_dependency_name in definition.dependencies_thirdparty:
            third_party_definition = find_third_party_project_definition( third_party_definitions, third_party_dependency_name )
            if third_party_definition:
                for include_path in third_party_definition.include_paths:
                    path = os.path.normpath( os.path.join( third_party_definition.directory, include_path ) )
                    additional_include_paths.append( path )
                for link_path in third_party_definition.static_libraries_paths_windows:
                    path = os.path.normpath( os.path.join( third_party_definition.directory, link_path ) )
                    additional_link_paths.append( path )
                for lib_name in third_party_definition.static_libs_to_link_windows:
                    link_dependencies.append( lib_name )

    for project_name in (definition.dependencies + definition.dependencies_windows):
        for project_definition in all_definitions:
            if project_definition.project_name == project_name:
                dependencies_path = project_definition.directory
                dependencies_path += "\\.."
                additional_include_paths.append( dependencies_path )

    for configuration_name in configurations_names:
        for platform_name in platforms_names:
            name_mix = configuration_name + "|" + platform_name

            item_definition_group_node = xml_doc.SubElement( project_node, "ItemDefinitionGroup", Condition="'$(Configuration)|$(Platform)'=='" + name_mix + "'" )
            cl_compile_node = xml_doc.SubElement( item_definition_group_node, "ClCompile" )
            xml_doc.SubElement( cl_compile_node, "AdditionalIncludeDirectories" ).text = expand_additional_include_paths_to_one_text_entry( additional_include_paths )
            xml_doc.SubElement( cl_compile_node, "PreprocessorDefinitions" ).text = definition.project_name.upper() + "_EXPORT;DLL_ENABLED;PLATFORM_WINDOWS"

            link_node = xml_doc.SubElement( item_definition_group_node, "Link" )
            xml_doc.SubElement( link_node, "TreatLinkerWarningAsErrors" ).text = "true"

            if definition.type == "application":
                xml_doc.SubElement( link_node, "SubSystem" ).text = "Windows"
            elif  definition.type == "console_application":
                xml_doc.SubElement( link_node, "SubSystem" ).text = "Console"

            if additional_link_paths:
                xml_doc.SubElement( link_node, "AdditionalLibraryDirectories" ).text = expand_additional_include_paths_to_one_text_entry( additional_link_paths )

            if link_dependencies:
                xml_doc.SubElement( link_node, "AdditionalDependencies" ).text = expand_additional_include_paths_to_one_text_entry( link_dependencies )


def generate_project_header_files_list( project_node, project_file_path, configurations_names, platforms_names, definition ):
    item_group_node = xml_doc.SubElement( project_node, "ItemGroup" )

    for file in definition.files:
        file_name = os.path.basename( file )
        file_path, file_extension = os.path.splitext( file )
        dirname = os.path.dirname( project_file_path )
        relativepath = os.path.relpath( file, dirname )

        if file_extension == ".h":
            xml_doc.SubElement( item_group_node, "ClInclude", Include=relativepath )


def generate_project_source_files_list( project_node, project_file_path, configurations_names, platforms_names, definition ):
    item_group_node = xml_doc.SubElement( project_node, "ItemGroup" )

    for file in definition.files:
        file_name = os.path.basename( file )
        file_path, file_extension = os.path.splitext( file )
        dirname = os.path.dirname( project_file_path )
        relativepath = os.path.relpath( file, dirname )

        if file_extension == ".cpp" or file_extension == ".mm":
            cl_compile_node = xml_doc.SubElement( item_group_node, "ClCompile", Include=relativepath )

        if file_extension == ".asm":
            cl_compile_node = xml_doc.SubElement( item_group_node, "Masm", Include=relativepath )

        if file_name == "pch.cpp":
            xml_doc.SubElement( cl_compile_node, "PrecompiledHeader",
                                Condition="'$(Configuration)|$(Platform)'=='Debug|x64'" ).text = "Create"
            xml_doc.SubElement( cl_compile_node, "PrecompiledHeader",
                                Condition="'$(Configuration)|$(Platform)'=='Release|x64'" ).text = "Create"

def generate_project_target_json( project_node, project_file_path, configurations_names, platforms_names, definition ):
    item_group_node = xml_doc.SubElement( project_node, "ItemGroup" )
    
    file_name = os.path.basename( definition.config_file )
    file_path, file_extension = os.path.splitext( definition.config_file )
    dirname = os.path.dirname( project_file_path )
    relativepath = os.path.relpath( definition.config_file, dirname )
    none_group_node = xml_doc.SubElement( item_group_node, "None", Include=relativepath )

    xml_doc.SubElement( none_group_node, "ExcludedFromBuild", Condition="'$(Configuration)|$(Platform)'=='Debug|x64'" ).text = "true"
    xml_doc.SubElement( none_group_node, "ExcludedFromBuild", Condition="'$(Configuration)|$(Platform)'=='Release|x64'" ).text = "true"


def generate_project_excluded_files( project_node, project_file_path, configurations_names, platforms_names, definition ):
    item_group_node = xml_doc.SubElement( project_node, "ItemGroup" )

    for file in definition.excluded_files:
        file_name = os.path.basename( file )
        file_path, file_extension = os.path.splitext( file )
        dirname = os.path.dirname( project_file_path )
        relativepath = os.path.relpath( file, dirname )
        none_group_node = xml_doc.SubElement( item_group_node, "None", Include=relativepath )

        xml_doc.SubElement( none_group_node, "ExcludedFromBuild", Condition="'$(Configuration)|$(Platform)'=='Debug|x64'" ).text = "true"
        xml_doc.SubElement( none_group_node, "ExcludedFromBuild", Condition="'$(Configuration)|$(Platform)'=='Release|x64'" ).text = "true"


def generate_project_files_list( project_node, project_file_path, configurations_names, platforms_names, definition ):
    generate_project_source_files_list( project_node, project_file_path, configurations_names, platforms_names, definition )
    generate_project_header_files_list( project_node, project_file_path, configurations_names, platforms_names, definition )
    generate_project_target_json( project_node, project_file_path, configurations_names, platforms_names, definition )
    generate_project_excluded_files( project_node, project_file_path, configurations_names, platforms_names, definition )


def generate_project_dependencies_list( project_node, project_file_path, definition, all_definitions ):
    item_group_node = xml_doc.SubElement( project_node, "ItemGroup" )

    for project_name in (definition.dependencies + definition.dependencies_windows):
        project_reference_node = xml_doc.SubElement( item_group_node, "ProjectReference", Include = project_name + ".vcxproj" )

        for project_definition in all_definitions:
            if project_definition.project_name == project_name:
                xml_doc.SubElement( project_reference_node, "Project" ).text = "{" + project_definition.guid + "}"
                break


def generate_project_file( project_file_path, definition, all_definitions, third_party_definitions ):
    project_node = xml_doc.Element( "Project", DefaultTargets="Build", ToolsVersion="15.0",
                                   xmlns="http://schemas.microsoft.com/developer/msbuild/2003" )

    configurations_names = ["Debug", "Release"]
    platforms_names = ["x64"]

    contains_asm_files = False

    for file in definition.files:
        file_path, file_extension = os.path.splitext( file )
        if file_extension == ".asm":
            contains_asm_files = True
            break;

    generate_project_configurations( project_node, configurations_names, platforms_names )
    generate_project_globals( project_node, definition.project_name, definition.guid )
    generate_project_conditions( project_node, configurations_names, platforms_names, definition.type )
    xml_doc.SubElement( project_node, "Import", Project="$(VCTargetsPath)\Microsoft.Cpp.props" )

    # extension for assembler
    if contains_asm_files:
        import_group = xml_doc.SubElement( project_node, "ImportGroup", Label="ExtensionSettings" )
        xml_doc.SubElement( import_group, "Import", Project="$(VCTargetsPath)\BuildCustomizations\masm.props" )

    generate_project_property_sheet_imports( project_node, configurations_names, platforms_names )
    generate_project_item_definition_groups( project_node, configurations_names, platforms_names, definition, all_definitions, third_party_definitions )
    generate_project_files_list( project_node, project_file_path, configurations_names, platforms_names, definition )
    generate_project_dependencies_list( project_node, project_file_path, definition, all_definitions )
    
    xml_doc.SubElement( project_node, "Import", Project="$(VCTargetsPath)\Microsoft.Cpp.targets" )

    # extension for assembler
    if contains_asm_files:
        import_group = xml_doc.SubElement( project_node, "ImportGroup", Label="ExtensionTargets" )
        xml_doc.SubElement( import_group, "Import", Project="$(VCTargetsPath)\BuildCustomizations\masm.targets" )

    format_xml_tree( project_node )

    tree = xml_doc.ElementTree( project_node )
    tree.write( project_file_path, encoding='utf-8', xml_declaration=True, method="xml" )
