import os
import uuid
import xml.etree.cElementTree as xml_doc

from xmlUtils import format_xml_tree
from visualStudioHelpers import is_header_file
from visualStudioHelpers import is_source_file
from visualStudioHelpers import is_assembler_file
from visualStudioHelpers import is_target_file

def add_unique_filter( filters_collection, current_filter ):
    if current_filter in filters_collection:
        return

    filters_collection.append(current_filter)
    
    subfilter, dir = os.path.split( current_filter ) 
    if subfilter != '':
        add_unique_filter( filters_collection, subfilter )

def generate_project_filters_file( project_filers_file_path, definition ):
    root = xml_doc.Element( "Project", ToolsVersion="4.0", xmlns="http://schemas.microsoft.com/developer/msbuild/2003" )
    headers = xml_doc.SubElement( root, "ItemGroup" )
    sources = xml_doc.SubElement( root, "ItemGroup" )
    other = xml_doc.SubElement( root, "ItemGroup" )
    masm = xml_doc.SubElement( root, "ItemGroup" )
    filters = xml_doc.SubElement( root, "ItemGroup" )

    filters_collection = []

    for file in ( definition.files + definition.excluded_files ):
        filename, extension = os.path.splitext( file )
        dirname = os.path.dirname( project_filers_file_path )
        relativepath = os.path.relpath( file, dirname )

        filter_relative_path = os.path.relpath( file, definition.directory )
        filter = os.path.dirname(filter_relative_path)

        if filter != '':
            add_unique_filter( filters_collection, filter )

        if is_header_file( file ):
            compile_group_node = xml_doc.SubElement( headers, "ClInclude", Include=relativepath )
            if filter != '':
                xml_doc.SubElement( compile_group_node, "Filter" ).text = filter
        elif is_source_file( file ):
            compile_group_node = xml_doc.SubElement( sources, "ClCompile", Include=relativepath )
            if filter != '':
                xml_doc.SubElement( compile_group_node, "Filter" ).text = filter
        elif is_target_file( file ):
            compile_group_node = xml_doc.SubElement( other, "None", Include=relativepath )
            if filter != '':
                xml_doc.SubElement( compile_group_node, "Filter" ).text = filter
        elif is_assembler_file( file ):
            compile_group_node = xml_doc.SubElement( masm, "Masm", Include=relativepath )
            if filter != '':
                xml_doc.SubElement( compile_group_node, "Filter" ).text = filter

        if file in definition.excluded_files:
            compile_group_node = xml_doc.SubElement( other, "None", Include=relativepath )
            if filter != '':
                xml_doc.SubElement( compile_group_node, "Filter" ).text = filter


    for filter in filters_collection:
        filter_group_node = xml_doc.SubElement( filters, "Filter", Include=filter )
        xml_doc.SubElement( filter_group_node, "UniqueIdentifier" ).text = "{" + str( uuid.uuid4() ).upper() + "}"

    format_xml_tree( root )

    tree = xml_doc.ElementTree( root )
    tree.write( project_filers_file_path, encoding='utf-8', xml_declaration=True, method="xml" )
