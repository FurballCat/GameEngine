import os
import xml.etree.cElementTree as xml_doc

from xmlUtils import format_xml_tree

def generate_shared_props_content( configuration_name, root_node ):
    xml_doc.SubElement( root_node, "ImportGroup", Label="PropertySheets" )
    xml_doc.SubElement( root_node, "PropertyGroup", Label="UserMacros" )

    paths_node = xml_doc.SubElement( root_node, "PropertyGroup" )
    xml_doc.SubElement( paths_node, "OutDir" ).text = "$(SolutionDir)binaries\\$(Platform)_$(Configuration)\\"
    xml_doc.SubElement( paths_node, "IntDir" ).text = "$(SolutionDir)intermediate\\build\\$(Platform)_$(Configuration)\\$(ProjectName)\\"
    xml_doc.SubElement( paths_node, "TargetName" ).text = "$(ProjectName)"
    xml_doc.SubElement( paths_node, "LibraryPath" ).text = "$(SolutionDir)binaries;$(LibraryPath)"

    item_definition_node = xml_doc.SubElement( root_node, "ItemDefinitionGroup" )

    cl_compile_node = xml_doc.SubElement( item_definition_node, "ClCompile" )
    xml_doc.SubElement( cl_compile_node, "TreatWarningAsError" ).text = "true"
    xml_doc.SubElement( cl_compile_node, "PrecompiledHeader" ).text = "Use"
    xml_doc.SubElement( cl_compile_node, "PrecompiledHeaderFile" ).text = "pch.h"
    xml_doc.SubElement( cl_compile_node, "RuntimeLibrary" ).text = "MultiThreadedDLL" if configuration_name == "Debug" else "MultiThreadedDebugDLL"
    xml_doc.SubElement( cl_compile_node, "PreprocessToFile" ).text = "false"
    xml_doc.SubElement( cl_compile_node, "PreprocessSuppressLineNumbers" ).text = "false"
    xml_doc.SubElement( cl_compile_node, "PreprocessorDefinitions" ).text = "_WINDLL;PLATFORM_WINDOWS;DLL_ENABLED;%(PreprocessorDefinitions)"
    xml_doc.SubElement( cl_compile_node, "AdditionalIncludeDirectories" ).text = "$(SolutionDir)intermediate\generated\$(Platform)_$(Configuration)\$(ProjectName)\;$(SolutionDir);%(AdditionalIncludeDirectories)"
    xml_doc.SubElement( cl_compile_node, "DisableSpecificWarnings" ).text = "4251;4503"

    lib_node = xml_doc.SubElement( item_definition_node, "Lib" )
    xml_doc.SubElement( lib_node, "AdditionalLibraryDirectories" ).text = "$(SolutionDir);%(AdditionalLibraryDirectories)"
    xml_doc.SubElement( lib_node, "TreatLibWarningAsErrors" ).text = "true"

    link_node = xml_doc.SubElement( item_definition_node, "Link" )
    xml_doc.SubElement( link_node, "AdditionalLibraryDirectories" ).text = "$(SolutionDir);%(AdditionalLibraryDirectories)"
    xml_doc.SubElement( link_node, "TreatLinkerWarningAsErrors" ).text = "true"

    pre_build_event_node = xml_doc.SubElement( item_definition_node, "PreBuildEvent" )
    xml_doc.SubElement( pre_build_event_node, "Command" ).text = "rmdir $(SolutionDir)intermediate\generated\$(Platform)_$(Configuration)\$(ProjectName) /S /Q mkdir $(SolutionDir)intermediate\generated\$(Platform)_$(Configuration)\$(ProjectName)"

    fx_compile_node = xml_doc.SubElement( item_definition_node, "FxCompile" )
    xml_doc.SubElement( fx_compile_node, "ShaderModel" ).text = "5.0"
    xml_doc.SubElement( fx_compile_node, "TreatWarningAsError" ).text = "true"
    xml_doc.SubElement( fx_compile_node, "ObjectFileOutput" ).text = "$(SolutionDir)content\shaders\%(Filename).cso"

    xml_doc.SubElement( root_node, "ItemGroup" )


def generate_shared_props_file( configuration_name, shared_props_file_path ):
    root_node = xml_doc.Element( "Project", ToolsVersion="4.0", xmlns="http://schemas.microsoft.com/developer/msbuild/2003" )

    generate_shared_props_content( configuration_name, root_node )

    format_xml_tree( root_node )

    tree = xml_doc.ElementTree( root_node )
    tree.write( shared_props_file_path, encoding='utf-8', xml_declaration=True, method="xml" )



def generate_all_shared_props():
    debug_shared_props_file_path = os.path.join( os.getcwd(), "intermediate", "projects", "sharedPropertySheetDebug.props" )
    release_shared_props_file_path = os.path.join( os.getcwd(), "intermediate", "projects", "sharedPropertySheetRelease.props" )

    generate_shared_props_file( "Debug", debug_shared_props_file_path )
    generate_shared_props_file( "Release", release_shared_props_file_path )
