import uuid
import os

class FilterDefinition:
    def __init__( self, name, full_relative_path, parent_guid ):
        self.name = name
        self.full_relative_path = full_relative_path
        self.guid = str( uuid.uuid4() ).upper()
        self.parent_guid = parent_guid

def generate_general_solution_section( file ):
    file.write( "Microsoft Visual Studio Solution File, Format Version 12.00\n" )
    file.write( "# Visual Studio 15\n" )
    file.write( "VisualStudioVersion = 15.0.26430.12\n" )
    file.write( "MinimumVisualStudioVersion = 10.0.40219.1\n" )


def generate_list_of_projects( file, projects_definitions ):
    for definition in projects_definitions:
        file.write( "Project(\"{8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942}\") = \"" )
        file.write( definition.project_name )
        file.write( "\", \"intermediate\\projects\\" )
        file.write( definition.project_name )
        file.write( ".vcxproj\", \"{" )
        file.write( definition.guid )
        file.write( "}\"\n" )
        file.write( "EndProject\n" )

def collect_unique_filters( filters_collection, current_filter, full_relative_path, parent_guid ):

    subfilter, dir = os.path.split( current_filter )

    filter_guid = None
    internal_parent_guid = None

    if subfilter != '':
        for inner_filter in filters_collection:
            if inner_filter.full_relative_path == subfilter:
                internal_parent_guid = inner_filter.guid
                break

        if internal_parent_guid == None:
            internal_parent_guid = collect_unique_filters( filters_collection, subfilter, full_relative_path, parent_guid )

    if dir != '':
        new_filter = FilterDefinition( dir, current_filter, internal_parent_guid )
        filter_guid = new_filter.guid
        filters_collection.append( new_filter )

    return filter_guid


def generate_filters_for_projects( file, projects_definitions ):
    filters_collection = []

    source_directory_path = os.path.join( os.getcwd(), "source" )

    for definition in projects_definitions:
        filter = os.path.dirname(definition.directory)
        filter_relative_path = os.path.relpath( filter, source_directory_path )

        found_elm = None
        for filter in filters_collection:
            if filter.full_relative_path == filter_relative_path:
                found_elm = filter
                break

        filter_guid = None
        if filter != '':
            if found_elm == None:
                filter_guid = collect_unique_filters( filters_collection, filter_relative_path, filter_relative_path, None )
            else:
                filter_guid = found_elm.guid

        definition.solution_filter = filter_guid

    for filter in filters_collection:
        file.write( "Project(\"{2150E333-8FDC-42A3-9474-1A3956D46DE8}\") = \"" )
        file.write( filter.name )
        file.write( "\", \"" )
        file.write( filter.name )
        file.write( "\", \"{" )
        file.write( filter.guid )
        file.write( "}\"\n" )
        file.write( "EndProject\n" )

    return filters_collection


def generate_global_configurations( file ):
    file.write( "\tGlobalSection(SolutionConfigurationPlatforms) = preSolution\n" )
    file.write( "\t\tDebug|x64 = Debug|x64\n" )
    file.write( "\t\tRelease|x64 = Release|x64\n" )
    file.write( "\tEndGlobalSection\n" )


def generate_global_projects_configurations( file, projects_definitions ):
    file.write( "\tGlobalSection(ProjectConfigurationPlatforms) = postSolution\n" )

    for definition in projects_definitions:
        file.write( "\t\t{" )
        file.write( definition.guid )
        file.write( "}.Debug|x64.ActiveCfg = Debug|x64\n" )

        if not definition.disable_on_active_platform:
            file.write( "\t\t{" )
            file.write( definition.guid )
            file.write( "}.Debug|x64.Build.0 = Debug|x64\n" )

        file.write( "\t\t{" )
        file.write( definition.guid )
        file.write( "}.Release|x64.ActiveCfg = Release|x64\n" )

        if not definition.disable_on_active_platform:
            file.write( "\t\t{" )
            file.write( definition.guid )
            file.write( "}.Release|x64.Build.0 = Release|x64\n" )

    file.write( "\tEndGlobalSection\n" )


def generate_global_solution_properties( file ):
    file.write( "\tGlobalSection(SolutionProperties) = preSolution\n" )
    file.write( "\t\SolutionGuid = FALSE\n" )
    file.write( "\tEndGlobalSection\n" )


def generate_global_solution_nested_projects( file, projects_definitions, filters ):
    file.write( "\tGlobalSection(NestedProjects) = preSolution\n" )
    
    for definition in projects_definitions:
        if definition.solution_filter != None:
            file.write( "\t\t{" )
            file.write( definition.guid )
            file.write( "} = {" )
            file.write( definition.solution_filter )
            file.write( "}\n" )

    for filter in filters:
        if filter.parent_guid != None:
            file.write( "\t\t{" )
            file.write( filter.guid )
            file.write( "} = {" )
            file.write( filter.parent_guid )
            file.write( "}\n" )

    file.write( "\tEndGlobalSection\n" )


def generate_global_solution_extensibility_globals( file ):
    file.write( "\tGlobalSection(ExtensibilityGlobals) = postSolution\n" )
    file.write( "\t\tHideSolutionNode = {" )
    file.write( str( uuid.uuid4() ).upper() )
    file.write( "}\n" )
    file.write( "\tEndGlobalSection\n" )


def generate_global_sections( file, projects_definitions, filters ):
    file.write( "Global\n" )

    generate_global_configurations( file )
    generate_global_projects_configurations( file, projects_definitions )
    generate_global_solution_properties( file )
    generate_global_solution_nested_projects( file, projects_definitions, filters )
    generate_global_solution_extensibility_globals( file )

    file.write( "EndGlobal\n" )


def generate_solution_file( solution_file_path, projects_definitions ):

    print( "Project Builder is generating solution file..." )

    solution_file = open( solution_file_path, "w+" )

    generate_general_solution_section( solution_file )
    generate_list_of_projects( solution_file, projects_definitions )
    filters = generate_filters_for_projects( solution_file, projects_definitions )
    generate_global_sections( solution_file, projects_definitions, filters )

    solution_file.close()
