import os
import random
from enum import Enum


class FileRef:
    def __init__( self, relative_path, group="group" ):
        self.location = group + ":" + relative_path


class Group:
    def __init__( self, name, location="container:" ):
        self.location = location
        self.name = name
        self.groups = []
        self.files = []

    def add_file( self, group_path, relative_path ):
        if len( group_path ) > 0:
            group_partition = group_path.partition( "/" )
            group_name = group_partition[0]

            # find the group or create new one
            found = False
            first_sub_group = None
            for group in self.groups:
                if group.name == group_name:
                    first_sub_group = group
                    found = True

            if not found:
                first_sub_group = Group( group_name )
                self.groups.append( first_sub_group )

            if len( group_partition ) > 2:
                first_sub_group.add_file( group_partition[2], relative_path )
        else:
            self.files.append( relative_path )


class Workspace:
    def __init__( self ):
        self.version = 1.0
        self.groups = []
        self.files = []

    def add_file( self, group_path, file_ref ):
        if len( group_path ) > 0:
            group_partition = group_path.partition( "/" )
            group_name = group_partition[0]

            # find the group or create new one
            found = False
            first_sub_group = None
            for group in self.groups:
                if group.name == group_name:
                    first_sub_group = group
                    found = True

            if not found:
                first_sub_group = Group( group_name )
                self.groups.append( first_sub_group )

            if len( group_partition ) > 2:
                first_sub_group.add_file( group_partition[2], file_ref )
        else:
            self.files.append( file_ref )


def xcode_uuid():
    length = 24
    return hex( random.getrandbits( 4 * length ) ).upper()[2:-1].zfill( length )


class PBXFileReference:
    def __init__( self, path, project_path, is_executable=False ):
        self.path = path
        self.name = os.path.basename( path )
        self.uuid = xcode_uuid()
        self.project_path = project_path
        self.is_executable = is_executable

    def __str__( self ):
        return self.to_string()

    def to_string( self ):
        if self.is_executable:
            return self._to_string_as_executable_file()
        elif self.is_h():
            return self._to_string_as_header()
        elif self.is_cpp():
            return self._to_string_as_source()
        elif self.is_asm():
            return self._to_string_as_assembler()
        elif self.is_mm():
            return self._to_string_as_mm()
        else:
            return self._to_string_as_text_file()

    def get_relative_path( self ):
        return os.path.relpath( self.path, self.project_path )

    def _to_string_as_source( self ):
        relative_path = self.get_relative_path()
        c_or_cpp_weird_thing = "cpp.cpp"

        if self.is_h():
            c_or_cpp_weird_thing = "c.h"

        result = "%(u)s /* %(f)s */ = {isa = PBXFileReference; " \
                 "lastKnownFileType = sourcecode.%(c)s; name = %(f)s; path = %(p)s; " \
                 "sourceTree = \"<group>\"; };" \
                 % {'u': self.uuid, 'f': self.name, 'p': relative_path, 'c': c_or_cpp_weird_thing}
        return result

    def _to_string_as_mm( self ):
        relative_path = self.get_relative_path()

        result = "%(u)s /* %(f)s */ = {isa = PBXFileReference; " \
                 "lastKnownFileType = sourcecode.c.objcpp; name = %(f)s; path = %(p)s; " \
                 "sourceTree = \"<group>\"; };" \
                 % {'u': self.uuid, 'f': self.name, 'p': relative_path}
        return result

    def _to_string_as_header( self ):
        relative_path = self.get_relative_path()
        c_or_cpp_weird_thing = "cpp.cpp"

        if self.is_h():
            c_or_cpp_weird_thing = "c.h"

        result = "%(u)s /* %(f)s */ = {isa = PBXFileReference; " \
                 "fileEncoding = 4; lastKnownFileType = sourcecode.%(c)s; name = %(f)s; path = %(p)s; " \
                 "sourceTree = \"<group>\"; };" \
                 % {'u': self.uuid, 'f': self.name, 'p': relative_path, 'c': c_or_cpp_weird_thing}
        return result

    def _to_string_as_assembler( self ):
        relative_path = self.get_relative_path()

        result = "%(u)s /* %(f)s */ = {isa = PBXFileReference; " \
                 "fileEncoding = 4; lastKnownFileType = sourcecode.asm; name = %(f)s; path = %(p)s; " \
                 "sourceTree = \"<group>\"; };" \
                 % {'u': self.uuid, 'f': self.name, 'p': relative_path}
        return result

    def _to_string_as_text_file( self ):
        relative_path = self.get_relative_path()
        temp, extension = os.path.splitext( self.path )

        result = "%(u)s /* %(f)s */ = {isa = PBXFileReference; " \
                 "fileEncoding = 4; lastKnownFileType = text%(e)s; name = %(f)s; path = %(p)s; " \
                 "sourceTree = \"<group>\"; };" \
                 % {'u': self.uuid, 'f': self.name, 'p': relative_path, 'e': extension}
        return result

    def _to_string_as_executable_file( self ):
        relative_path = self.path
        temp, extension = os.path.splitext( self.path )

        result = "%(u)s /* %(f)s */ = {isa = PBXFileReference; " \
                 "explicitFileType = \"compiled.mach-o.executable\"; includeInIndex = 0; path = %(p)s; " \
                 "sourceTree = BUILT_PRODUCTS_DIR; };" \
                 % {'u': self.uuid, 'f': self.name, 'p': relative_path, 'e': extension}
        return result

    def is_cpp( self ):
        return self.path.endswith( ".cpp" )

    def is_c( self ):
        return self.path.endswith( ".c" )

    def is_h( self ):
        return self.path.endswith( ".h" )

    def is_asm( self ):
        return self.path.endswith( ".s" )

    def is_source_code( self ):
        return self.is_cpp() or self.is_h() or self.is_mm() or self.is_asm() or self.is_c() or self.is_m()

    def is_for_build_sources( self ):
        return self.is_cpp() or self.is_mm() or self.is_asm() or self.is_c() or self.is_m()

    def is_plist( self ):
        return self.path.endswith( ".plist" )

    def is_pch( self ):
        return self.path.endswith( "pch.h" )

    def is_m( self ):
        return self.path.endswith( ".m" )

    def is_mm( self ):
        return self.path.endswith( ".mm" )


class PBXFileReferenceLibrary:
    def __init__( self, name ):
        self.name = name
        self.uuid = xcode_uuid()
        self.libname = "lib%(n)s.dylib" % {'n': name}

    def __str__( self ):
        result = "%(u)s /* lib%(f)s.dylib */ = {isa = PBXFileReference; " \
                 "lastKnownFileType = \"compiled.mach-o.dylib\"; name = %(f)s; path = lib%(f)s.dylib; " \
                 "sourceTree = BUILT_PRODUCTS_DIR; };" \
                 % {'u': self.uuid, 'f': self.name}
        return result


class PBXFileReferenceSystemLibrary:
    def __init__( self, name, path ):
        self.name = name
        self.uuid = xcode_uuid()
        self.path = path

    def __str__( self ):
        result = "%(u)s /* %(n)s */ = {isa = PBXFileReference; " \
                 "lastKnownFileType = wrapper.framework; name = %(n)s; path = %(p)s; " \
                 "sourceTree = SDKROOT; };" \
                 % {'u': self.uuid, 'n': self.name, 'p': self.path}
        return result


class PBXFileReferenceThirdParty:
    def __init__( self, name, path ):
        self.name = name
        self.uuid = xcode_uuid()
        self.path = path

    def __str__( self ):
        result = "%(u)s /* %(n)s */ = {isa = PBXFileReference; " \
                 "lastKnownFileType = \"%(f)s\"; name = %(n)s; path = %(p)s; " \
                 "sourceTree = \"<group>\"; };" \
                 % {'u': self.uuid, 'n': self.name, 'p': self.path, 'f': self.get_last_known_file_type()}
        return result

    def is_framework(self):
        return self.path.endswith(".framework")

    def is_archive(self):
        return self.path.endswith(".a")

    def get_last_known_file_type(self):
        if self.is_framework():
            return "wrapper.framework"
        elif self.is_archive():
            return "archive.ar"
        else:
            return "compiled.mach-o.dylib"


class PBXBuildFile:
    def __init__( self, name, file_uuid, source="Sources", code_signed=False ):
        self.name = name
        self.file_uuid = file_uuid
        self.uuid = xcode_uuid()
        self.source = source
        self.code_signed = code_signed

    def __str__( self ):
        return self.to_string()

    def to_string( self ):
        if self.code_signed:
            additional_settings = "settings = {ATTRIBUTES = (CodeSignOnCopy,);};"
        else:
            additional_settings = ""

        result = "%(u)s /* %(f)s in %(s)s */ = {isa = PBXBuildFile; " \
                 "fileRef = %(r)s /* %(f)s */; %(a)s };" \
                 % {'u': self.uuid, 'f': self.name, 'r': self.file_uuid, 's': self.source, 'a': additional_settings}
        return result


class PBXFileEntry:
    def __init__( self, name, uuid ):
        self.name = name
        self.uuid = uuid


class PBXGroup:
    def __init__( self, name, folder_name="" ):
        self.uuid = xcode_uuid()
        self.files = []
        self.name = name
        self.folder_name = folder_name
        self.subgroups = dict()
        self.files_subgroups = []

    def add_file( self, group_path, pbx_file ):
        if len( group_path ) > 0:
            group_partition = group_path.partition( "/" )
            group_name = group_partition[0]

            if group_name not in self.subgroups:
                first_sub_group = PBXGroup( group_name )
                self.subgroups[group_name] = first_sub_group
                self.files_subgroups.append( first_sub_group )
            else:
                first_sub_group = self.subgroups[group_name]

            if len( group_partition ) > 2:
                first_sub_group.add_file( group_partition[2], pbx_file )
        else:
            self.files.append( pbx_file )

    def save( self, builder ):
        builder.save_group( self )
        for key in self.subgroups:
            subgroup = self.subgroups[key]
            subgroup.save( builder )


class PBXBuildPhase:
    def __init__( self, name, phase_type ):
        self.name = name
        self.uuid = xcode_uuid()
        self.files = []
        self.phase_type = phase_type

    def add_file( self, file ):
        self.files.append( file )


class PBXNativeTarget:
    def __init__( self, name, product_name, product_reference, product_type, build_configuration_list ):
        self.name = name
        self.uuid = xcode_uuid()
        self.product_name = product_name
        self.product_reference = product_reference
        self.product_type = product_type
        self.build_configuration_list = build_configuration_list
        self.build_phases = []
        self.build_rules = []
        self.dependencies = []

    def add_build_phase( self, build_phase ):
        self.build_phases.append( build_phase )

    def add_build_rule( self, build_rule ):
        self.build_phases.append( build_rule )

    def add_dependency( self, dependency ):
        self.build_phases.append( dependency )


class PBXProjectObject:
    def __init__( self, organization, native_target_uuid, main_group_uuid, product_ref_group, build_configuration_list ):
        self.uuid = xcode_uuid()
        self.organization = organization
        self.native_target_uuid = native_target_uuid
        self.main_group_uuid = main_group_uuid
        self.product_ref_group = product_ref_group
        self.build_configuration_list = build_configuration_list
        self.targets = []

    def add_target( self, target ):
        self.targets.append( target )


class PBXConfiguration:
    def __init__( self, name ):
        self.uuid = xcode_uuid()
        self.name = name
        self.debug_information_format = "dwarf"
        self.enable_testability = False
        self.no_optimizations = False
        self.use_debug_preprocessor_definitions = False
        self.gcc_dynamic_no_pic = False
        self.only_active_arch = False
        self.mtl_enable_debug_info = False
        self.enable_nl_assertions = False


class ProductType( Enum ):
    App = 1
    ConsoleApp = 2
    DynLib = 3
    Lib = 4


class PBXNativeTargetConfiguration:
    def __init__( self, name, product_type ):
        self.uuid = xcode_uuid()
        self.name = name
        self.product_type = product_type
        self.preprocessor_definitions = []
        self.header_search_paths = []
        self.plist_file_relative_path = ""
        self.organization_name = "unknown"
        self.app_name = "unknown"
        self.use_precompiled_header = False
        self.precompiled_header_path = ""

    def add_macro( self, macro ):
        self.preprocessor_definitions.append( macro )

    def add_header_search_path( self, path ):
        self.header_search_paths.append( path )


class PBXBuildConfigurationList:
    def __init__( self, name ):
        self.uuid = xcode_uuid()
        self.name = name
        self.configurations = []
        self.default_configuration = None
        self.is_for = "PBXProject"

    def add_configuration( self, configuration ):
        self.configurations.append( configuration )


class PBXProjectBuilder:
    def __init__( self ):
        self.depth = 0
        self.result = ""

    def clear( self ):
        self.depth = 0
        self.result = ""

    def utf_header( self ):
        self._write_line( "// !$*UTF8*$!" )

    def begin_root( self ):
        self._write_line( "{" )
        self._push_tab()
        self._write_line( "archiveVersion = 1;" )
        self._write_line( "classes = {};" )
        self._write_line( "objectVersion = 46;" )

    def end_root( self, root_object_uuid ):
        self._write_line( "rootObject = %(r)s /* Project object */;" % {'r': root_object_uuid} )
        self._pop_tab()
        self._write_line( "}" )

    def begin_section( self, name ):
        self._write_line( "" )
        self._write_line_no_indent( "/* Begin %(n)s section */" % {'n': name} )

    def end_section( self, name ):
        self._write_line_no_indent( "/* End %(n)s section */" % {'n': name} )

    def begin_objects( self ):
        self._write_line( "objects = {" )
        self._push_tab()

    def end_objects( self ):
        self._pop_tab()
        self._write_line( "};" )

    def save_object( self, obj ):
        self._write_line( obj )

    def save_build_phase( self, phase ):
        self._write_line( "%(u)s /* %(n)s */ = {" % {'u': phase.uuid, 'n': phase.name} )
        self._push_tab()
        self._write_line( "isa = %(n)s;" % {'n': phase.phase_type} )
        self._write_line( "buildActionMask = 2147483647;" )

        # little hack for copy files build phase
        if phase.name == "CopyFiles":
            self._write_line("dstPath = \"\";")
            self._write_line("dstSubfolderSpec = 10;")

        self._begin_round_list( "files" )
        for item in phase.files:
            self._write_line( "%(u)s /* %(n)s */," % {'u': item.uuid, 'n': item.name} )
        self._end_round_list()

        self._write_line( "runOnlyForDeploymentPostprocessing = 0;" )

        self._pop_tab()
        self._write_line( "};" )

    def save_group( self, group ):
        self._write_line( "%(u)s /* %(n)s */ = {" % {'u': group.uuid, 'n': group.name} )
        self._push_tab()

        self._write_line( "isa = PBXGroup;" )

        self._begin_round_list( "children" )
        for file in group.files_subgroups:
            self._write_line( "%(u)s /* %(n)s */," % {'u': file.uuid, 'n': file.name} )
        for file in group.files:
            self._write_line( "%(u)s /* %(n)s */," % {'u': file.uuid, 'n': file.name} )
        self._end_round_list()

        if len( group.name ) > 0:
            self._write_line( "name = %(n)s;" % {'n': group.name} )

        self._write_line( "sourceTree = \"<group>\";" )

        self._pop_tab()
        self._write_line( "};" )

    def save_native_target( self, target ):
        self._write_line( "%(u)s /* %(n)s */ = {" % {'u': target.uuid, 'n': target.name} )
        self._push_tab()
        self._write_line( "isa = PBXNativeTarget;" )
        self._write_line( "buildConfigurationList = %(u)s /* Build configuration list for PBXNativeTarget \"%(n)s\" */;" % {'u': target.build_configuration_list, 'n': target.name} )

        self._begin_round_list( "buildPhases" )
        for item in target.build_phases:
            self._write_line( "%(u)s /* %(n)s */," % {'u': item.uuid, 'n': item.name} )
        self._end_round_list()

        self._begin_round_list( "buildRules" )
        for item in target.build_rules:
            self._write_line( "%(u)s /* %(n)s */," % {'u': item.uuid, 'n': item.name} )
        self._end_round_list()

        self._begin_round_list( "dependencies" )
        for item in target.dependencies:
            self._write_line( "%(u)s /* %(n)s */," % {'u': item.uuid, 'n': item.name} )
        self._end_round_list()

        self._write_line( "name = %(n)s;" % {'n': target.name} )
        self._write_line( "productName = %(n)s;" % {'n': target.product_name} )
        self._write_line( "productReference = %(u)s /* %(n)s */;" % {'u': target.product_reference.uuid, 'n': target.product_reference.name} )
        self._write_line( "productType = \"%(n)s\";" % {'n': target.product_type} )

        self._pop_tab()
        self._write_line( "};" )

    def save_project_object( self, project ):
        self._write_line( "%(u)s /* Project object */ = {" % {'u': project.uuid} )
        self._push_tab()

        self._write_line( "isa = PBXProject;" )

        self._begin_list( "attributes" )
        self._write_line( "LastUpgradeCheck = 0830;" )
        self._write_line( "ORGANIZATIONNAME = \"%(n)s\";" % {'n': project.organization} )
        self._begin_list( "TargetAttributes" )
        self._begin_list( project.native_target_uuid )
        self._write_line( "CreatedOnToolsVersion = 8.3;" )
        self._write_line( "ProvisioningStyle = Automatic;" )
        self._end_list()
        self._end_list()
        self._end_list()

        self._write_line( "buildConfigurationList = %(u)s /* Build configuration list for PBXProject \"%(n)s\" */;" % {'u': project.build_configuration_list.uuid, 'n': project.build_configuration_list.name} )

        self._write_line( "compatibilityVersion = \"Xcode 3.2\";" )
        self._write_line( "developmentRegion = en;" )
        self._write_line( "hasScannedForEncodings = 0;" )

        self._write_line( "knownRegions = (" )
        self._push_tab()
        self._write_line( "en," )
        self._write_line( "Base," )
        self._pop_tab()
        self._write_line( ");" )

        self._write_line( "mainGroup = %(u)s;" % {'u': project.main_group_uuid} )
        self._write_line( "productRefGroup = %(u)s /* Products */;" % {'u': project.product_ref_group} )

        self._write_line( "projectDirPath = \"\";" )
        self._write_line( "projectRoot = \"\";" )
        self._write_line( "targets = (" )
        self._push_tab()
        for item in project.targets:
            self._write_line( "%(u)s /* %(n)s */," % {'u': item.uuid, 'n': item.name} )
        self._pop_tab()
        self._write_line( ");" )

        self._pop_tab()
        self._write_line( "};" )

    def save_configuration( self, configuration ):
        self._begin_list( "%(u)s /* %(n)s */" % {'u': configuration.uuid, 'n': configuration.name} )

        self._write_line( "isa = XCBuildConfiguration;" )

        self._begin_list( "buildSettings" )

        self._write_line( "ALWAYS_SEARCH_USER_PATHS = NO;" )
        self._write_line( "CLANG_ANALYZER_LOCALIZABILITY_NONLOCALIZED = YES;" )
        self._write_line( "CLANG_ANALYZER_NONNULL = YES;" )
        self._write_line( "CLANG_ANALYZER_NUMBER_OBJECT_CONVERSION = YES_AGGRESSIVE;" )
        self._write_line( "CLANG_CXX_LANGUAGE_STANDARD = \"gnu++14\";" )
        self._write_line( "CLANG_CXX_LIBRARY = \"libc++\";" )
        self._write_line( "CLANG_ENABLE_MODULES = YES;" )
        self._write_line( "CLANG_ENABLE_OBJC_ARC = NO;" ) # changed to NO because of glfw
        self._write_line( "CLANG_WARN_BLOCK_CAPTURE_AUTORELEASING = YES;" )
        self._write_line( "CLANG_WARN_BOOL_CONVERSION = YES;" )
        self._write_line( "CLANG_WARN_COMMA = NO;" ) # maybe should be YES, but then glfw spams
        self._write_line( "CLANG_WARN_CONSTANT_CONVERSION = YES;" )
        self._write_line( "CLANG_WARN_DEPRECATED_OBJC_IMPLEMENTATIONS = NO;" ) # maybe should be yes, but glfw
        self._write_line( "CLANG_WARN_DIRECT_OBJC_ISA_USAGE = YES_ERROR;" )
        self._write_line( "CLANG_WARN_DOCUMENTATION_COMMENTS = NO;" ) # changed to NO because of glfw
        self._write_line( "CLANG_WARN_EMPTY_BODY = YES;" )
        self._write_line( "CLANG_WARN_ENUM_CONVERSION = YES;" )
        self._write_line( "CLANG_WARN_INFINITE_RECURSION = YES;" )
        self._write_line( "CLANG_WARN_INT_CONVERSION = YES;" )
        self._write_line( "CLANG_WARN_NON_LITERAL_NULL_CONVERSION = YES;" )
        self._write_line( "CLANG_WARN_OBJC_IMPLICIT_RETAIN_SELF = YES;" )
        self._write_line( "CLANG_WARN_OBJC_LITERAL_CONVERSION = YES;" )
        self._write_line( "CLANG_WARN_OBJC_ROOT_CLASS = YES_ERROR;" )
        self._write_line( "CLANG_WARN_QUOTED_INCLUDE_IN_FRAMEWORK_HEADER = YES;" )
        self._write_line( "CLANG_WARN_RANGE_LOOP_ANALYSIS = YES;" )
        self._write_line( "CLANG_WARN_STRICT_PROTOTYPES = YES;" )
        self._write_line( "CLANG_WARN_SUSPICIOUS_MOVE = YES;" )
        self._write_line( "CLANG_WARN_UNREACHABLE_CODE = YES;" )
        self._write_line( "CLANG_WARN__DUPLICATE_METHOD_MATCH = YES;" )
        self._write_line( "CLANG_ENABLE_OBJC_WEAK = YES;" )
        self._write_line( "CODE_SIGN_IDENTITY = \"-\";" )
        self._write_line( "COPY_PHASE_STRIP = NO;" )
        self._write_line( "DEBUG_INFORMATION_FORMAT = %(n)s;" % {'n':configuration.debug_information_format} )

        if not configuration.enable_nl_assertions:
            self._write_line( "ENABLE_NS_ASSERTIONS = NO;" )

        self._write_line( "ENABLE_STRICT_OBJC_MSGSEND = YES;" )

        if configuration.enable_testability:
            self._write_line( "ENABLE_TESTABILITY = YES;" )

        self._write_line( "GCC_C_LANGUAGE_STANDARD = gnu11;" )

        if configuration.gcc_dynamic_no_pic:
            self._write_line( "GCC_DYNAMIC_NO_PIC = NO;" )

        self._write_line( "GCC_NO_COMMON_BLOCKS = YES;" )

        if configuration.no_optimizations:
            self._write_line( "GCC_OPTIMIZATION_LEVEL = 0;" )

        if configuration.use_debug_preprocessor_definitions:
            self._write_line( "GCC_PREPROCESSOR_DEFINITIONS = (" )
            self._push_tab()
            self._write_line( "\"DEBUG=1\"," )
            self._write_line( "\"$(inherited)\"," )
            self._pop_tab()
            self._write_line( ");" )

        self._write_line( "GCC_WARN_64_TO_32_BIT_CONVERSION = YES;" )
        self._write_line( "GCC_WARN_ABOUT_RETURN_TYPE = YES_ERROR;" )
        self._write_line( "GCC_WARN_UNDECLARED_SELECTOR = YES;" )
        self._write_line( "GCC_WARN_UNINITIALIZED_AUTOS = YES_AGGRESSIVE;" )
        self._write_line( "GCC_WARN_UNUSED_FUNCTION = YES;" )
        self._write_line( "GCC_WARN_UNUSED_VARIABLE = YES;" )
        self._write_line( "MACOSX_DEPLOYMENT_TARGET = 10.14;" )

        if configuration.mtl_enable_debug_info:
            self._write_line( "MTL_ENABLE_DEBUG_INFO = YES;" )
        else:
            self._write_line( "MTL_ENABLE_DEBUG_INFO = NO;" )

        if configuration.only_active_arch:
            self._write_line( "ONLY_ACTIVE_ARCH = YES;" )

        self._write_line( "SDKROOT = macosx;" )

        self._end_list()

        self._write_line( "name = %(n)s;" % {'n': configuration.name} )

        self._end_list()

    def save_native_target_configuration( self, configuration ):
        self._begin_list( "%(u)s /* %(n)s */" % {'u': configuration.uuid, 'n': configuration.name} )

        self._write_line( "isa = XCBuildConfiguration;" )

        self._begin_list( "buildSettings" )

        if configuration.product_type == ProductType.DynLib:
            self._write_line( "DYLIB_COMPATIBILITY_VERSION = 1;" )
            self._write_line( "DYLIB_CURRENT_VERSION = 1;" )
            self._write_line( "EXECUTABLE_PREFIX = lib;" )
        elif configuration.product_type == ProductType.App:
            self._write_line( "ASSETCATALOG_COMPILER_APPICON_NAME = AppIcon;" )
            self._write_line( "CODE_SIGN_IDENTITY = \"-\";" )
            self._write_line( "COMBINE_HIDPI_IMAGES = YES;" )
            self._write_line( "INFOPLIST_FILE = \"%(n)s\";" % {'n': configuration.plist_file_relative_path} )
            self._write_line( "LD_RUNPATH_SEARCH_PATHS = \"$(inherited) @executable_path\";" )
            self._write_line( "OTHER_CODE_SIGN_FLAGS = \"--deep\";" )     # required for codesign command for deep signing
            self._write_line( "PRODUCT_BUNDLE_IDENTIFIER = \"%(o)s.%(a)s\";" % {'o': configuration.organization_name, 'a': configuration.app_name} )

        if configuration.use_precompiled_header:
            self._write_line( "GCC_PRECOMPILE_PREFIX_HEADER = YES;" )
            self._write_line( "GCC_PREFIX_HEADER = %(p)s;" % {'p': configuration.precompiled_header_path} )

        self._begin_round_list( "GCC_PREPROCESSOR_DEFINITIONS" )
        for item in configuration.preprocessor_definitions:
            self._write_line( "\"%(n)s\"," % {'n': item} )

        self._write_line( "\"$(inherited)\"," )
        self._end_round_list()

        self._write_line( "GCC_WARN_ABOUT_DEPRECATED_FUNCTIONS = NO;" ) # because of glfw

        self._begin_round_list( "HEADER_SEARCH_PATHS" )
        for item in configuration.header_search_paths:
            self._write_line( "%(p)s," % {'p': item} )
        self._end_round_list()

        self._write_line( "PRODUCT_NAME = \"$(TARGET_NAME)\";" )

        self._end_list()

        self._write_line( "name = %(n)s;" % {'n': configuration.name} )

        self._end_list()

    def save_configuration_list( self, configuration_list ):
        self._begin_list( "%(u)s /* Build configuration list for %(f)s \"%(n)s\" */" % {'u': configuration_list.uuid, 'f': configuration_list.is_for, 'n': configuration_list.name} )

        self._write_line( "isa = XCConfigurationList;" )

        self._write_line( "buildConfigurations = (" )
        self._push_tab()
        for item in configuration_list.configurations:
            self._write_line( "%(u)s /* %(n)s */," % {'u': item.uuid, 'n': item.name} )
        self._pop_tab()
        self._write_line( ");" )

        self._write_line( "defaultConfigurationIsVisible = 0;" )
        self._write_line( "defaultConfigurationName = %(n)s;" % {'n': configuration_list.default_configuration.name} )

        self._end_list()

    def _begin_list( self, name ):
        self._write_line( "%(n)s = {" % {'n': name} )
        self._push_tab()

    def _end_list( self ):
        self._pop_tab()
        self._write_line( "};" )

    def _begin_round_list( self, name ):
        self._write_line( "%(n)s = (" % {'n': name} )
        self._push_tab()

    def _end_round_list( self ):
        self._pop_tab()
        self._write_line( ");" )

    def _push_tab( self ):
        self.depth += 1

    def _pop_tab( self ):
        self.depth -= 1

    def _indent( self ):
        return "\t" * self.depth

    def _write_line( self, text ):
        self._write( text.__str__() + "\n" )

    def _write( self, text ):
        self.result += self._indent() + text

    def _write_line_no_indent( self, text ):
        self.result += text + "\n"


class Project:
    def __init__( self, name, path ):
        self.files = []
        self.name = name
        self.path = path

    def add_file( self, path ):
        self.files.append( path )

    def save( self, path, source_path, project_description, project_name_to_path ):
        project_name = project_description.project_name
        project_type_string = project_description.type
        dependencies = project_description.dependencies
        dependencies_osx = project_description.dependencies_osx
        dependencies_thirdparty = project_description.dependencies_thirdparty
        project_to_source_relative_path = os.path.relpath( source_path, path )
        project_to_thirdparty_relative_path = os.path.relpath( source_path + "/../thirdparty", path )

        if project_type_string == "application":
            product_type = ProductType.App
            product_type_name = "com.apple.product-type.application"
            product_ref_name = "%(n)s.app" % {'n': project_name}
        elif project_type_string == "console_application":
            product_type = ProductType.ConsoleApp
            product_type_name = "com.apple.product-type.tool"
            product_ref_name = "%(n)s" % {'n': project_name}
        elif project_type_string == "dynamic_library" or project_type_string == "utility":
            product_type = ProductType.DynLib
            product_type_name = "com.apple.product-type.library.dynamic"
            product_ref_name = "lib%(n)s.dylib" % {'n': project_name}
        else:
            product_type = ProductType.Lib
            product_type_name = "com.apple.product-type.library.static"
            product_ref_name = "lib%(n)s.lib" % {'n': project_name}

        organization = "Furball Cat"
        organization_name_no_spaces = "FurballCat"

        pbx_file_refs = []
        pbx_build_files = []

        product = PBXFileReference( product_ref_name, product_ref_name, True )

        pbx_file_refs.append( product )

        group_main = PBXGroup( "" )

        build_phase_headers = PBXBuildPhase( "Headers", "PBXHeadersBuildPhase" )
        build_phase_sources = PBXBuildPhase( "Sources", "PBXSourcesBuildPhase" )
        copy_files_build_phase_sources = PBXBuildPhase("CopyFiles", "PBXCopyFilesBuildPhase")

        plist_file_ref = None
        precompiled_header_ref = None

        for file in self.files:
            # create group name (filter)
            file_path = os.path.dirname( file )
            group = os.path.relpath( file_path, self.path )
            if group == ".":
                group = ""

            # create file reference
            ref = PBXFileReference( file, path )
            pbx_file_refs.append( ref )

            file_entry = PBXFileEntry( ref.name, ref.uuid )

            group_main.add_file( group, file_entry )

            build_file_ref = PBXBuildFile( ref.name, ref.uuid )

            if ref.is_for_build_sources():
                build_phase_sources.add_file( build_file_ref )
            elif ref.is_h():
                build_phase_headers.add_file( build_file_ref )
                if ref.is_pch():
                    precompiled_header_ref = ref
            elif ref.is_plist():
                plist_file_ref = ref

            if not (file.endswith( "json" ) or file.endswith( ".DS_Store" )):
                pbx_build_files.append( build_file_ref )

        build_phase_framework = PBXBuildPhase( "Frameworks", "PBXFrameworksBuildPhase" )

        header_search_paths = set()

        # add library dependencies
        for item in dependencies:
            project_ref_file = PBXFileReferenceLibrary( item )
            project_build_file_ref = PBXBuildFile( project_ref_file.libname, project_ref_file.uuid, "Frameworks" )

            pbx_file_refs.append( project_ref_file )
            pbx_build_files.append( project_build_file_ref )

            build_phase_framework.add_file( project_build_file_ref )

            group_main.add_file( "Frameworks", project_ref_file )

            # construct header search path for given dependency
            header_path = project_to_source_relative_path + "/" + \
                os.path.relpath( project_name_to_path[item] + "/../", source_path )
            header_search_paths.add( header_path )

        system_library_path = {}
        system_library_path["metal"] = "System/Library/Frameworks/Metal.framework"
        system_library_path["metalkit"] = "System/Library/Frameworks/MetalKit.framework"
        system_library_path["appkit"] = "System/Library/Frameworks/AppKit.framework"
        system_library_path["quartzcore"] = "System/Library/Frameworks/QuartzCore.framework"
        system_library_path["gamecontroller"] = "System/Library/Frameworks/GameController.framework"
        system_library_path["iokit"] = "System/Library/Frameworks/IOKit.framework"

        # add OSX SDK library dependencies
        for item in dependencies_osx:
            if item in system_library_path:
                project_ref_file = PBXFileReferenceSystemLibrary( item, system_library_path[item] )
                project_build_file_ref = PBXBuildFile( project_ref_file.name, project_ref_file.uuid, "Frameworks" )

                pbx_file_refs.append( project_ref_file )
                pbx_build_files.append( project_build_file_ref )

                build_phase_framework.add_file( project_build_file_ref )

                group_main.add_file( "Frameworks", project_ref_file )
            else:
                project_ref_file = PBXFileReferenceLibrary( item )
                project_build_file_ref = PBXBuildFile( project_ref_file.libname, project_ref_file.uuid, "Frameworks" )

                pbx_file_refs.append( project_ref_file )
                pbx_build_files.append( project_build_file_ref )

                build_phase_framework.add_file( project_build_file_ref )

                group_main.add_file( "Frameworks", project_ref_file )

                # construct header search path for given dependency
                header_path = project_to_source_relative_path + "/" + \
                              os.path.relpath( project_name_to_path[item] + "/../", source_path )
                header_search_paths.add( header_path )

        third_party_library_paths = {}
        third_party_library_paths["fmod"] = "fmod/lowlevel/lib/libfmod.dylib"
        third_party_library_paths["vulkan"] = "vulkansdk/macOS/lib/libvulkan.1.dylib"
        third_party_library_paths["physx"] = "PhysX-4.1/physX/bin/mac.x86_64/release/libPhysX_static_64.a"
        third_party_library_paths["physx_common"] = "PhysX-4.1/physX/bin/mac.x86_64/release/libPhysXCommon_static_64.a"
        third_party_library_paths["physx_foundation"] = "PhysX-4.1/physX/bin/mac.x86_64/release/libPhysXFoundation_static_64.a"
        third_party_library_paths["physx_character_kinematic"] = "PhysX-4.1/physX/bin/mac.x86_64/release/libPhysXCharacterKinematic_static_64.a"
        third_party_library_paths["physx_extensions"] = "PhysX-4.1/physX/bin/mac.x86_64/release/libPhysXExtensions_static_64.a"


        # add third party library dependencies
        for item in dependencies_thirdparty:
            if item in third_party_library_paths:
                project_ref_file = PBXFileReferenceThirdParty(item, project_to_thirdparty_relative_path + "/" + third_party_library_paths[item])
                project_build_file_ref = PBXBuildFile(project_ref_file.name, project_ref_file.uuid, "Frameworks", True)

                pbx_file_refs.append(project_ref_file)
                pbx_build_files.append(project_build_file_ref)

                build_phase_framework.add_file(project_build_file_ref)

                group_main.add_file("Frameworks", project_ref_file)

                copy_files_build_phase_sources.add_file(project_build_file_ref)
                header_search_paths.add(project_to_thirdparty_relative_path)

                if item == "physx":
                    # construct header search path for PhysX
                    header_search_paths.add(project_to_thirdparty_relative_path + "/PhysX-4.1/physx/include")
                    header_search_paths.add(project_to_thirdparty_relative_path + "/PhysX-4.1/pxshared/include")

        # continue
        group_main.add_file( "Products", product )
        group_products = group_main.subgroups["Products"]

        project_build_configuration_list = PBXBuildConfigurationList( project_name )
        native_target_build_configuration_list = PBXBuildConfigurationList( project_name )

        native_target = PBXNativeTarget( project_name, project_name, product, product_type_name,
                                        native_target_build_configuration_list.uuid )
        native_target.add_build_phase( copy_files_build_phase_sources )
        native_target.add_build_phase( build_phase_sources )
        native_target.add_build_phase( build_phase_framework )
        native_target.add_build_phase( build_phase_headers )

        project_object = PBXProjectObject( organization, native_target.uuid, group_main.uuid, group_products.uuid,
                                          project_build_configuration_list )
        project_object.add_target( native_target )

        configuration_debug = PBXConfiguration( "Debug" )
        configuration_debug.debug_information_format = "dwarf"
        configuration_debug.enable_testability = True
        configuration_debug.gcc_dynamic_no_pic = True
        configuration_debug.no_optimizations = True
        configuration_debug.use_debug_preprocessor_definitions = True
        configuration_debug.gcc_dynamic_no_pic = True
        configuration_debug.only_active_arch = True
        configuration_debug.mtl_enable_debug_info = True
        configuration_debug.enable_nl_assertions = True

        configuration_release = PBXConfiguration( "Release" )
        configuration_release.debug_information_format = "\"dwarf-with-dsym\""
        configuration_release.enable_testability = False
        configuration_release.gcc_dynamic_no_pic = False
        configuration_release.no_optimizations = False
        configuration_release.use_debug_preprocessor_definitions = False
        configuration_release.gcc_dynamic_no_pic = False
        configuration_release.only_active_arch = False
        configuration_release.mtl_enable_debug_info = False
        configuration_release.enable_nl_assertions = False

        project_build_configuration_list.add_configuration( configuration_debug )
        project_build_configuration_list.add_configuration( configuration_release )
        project_build_configuration_list.default_configuration = configuration_release
        project_build_configuration_list.is_for = "PBXProject"

        native_target_configuration_debug = PBXNativeTargetConfiguration( "Debug", product_type )
        native_target_configuration_debug.add_macro( "DEBUG=1" )
        native_target_configuration_debug.add_macro( "DLL_ENABLED=1" )
        native_target_configuration_debug.add_macro( "PLATFORM_OSX=1" )

        native_target_configuration_release = PBXNativeTargetConfiguration( "Release", product_type )
        native_target_configuration_release.add_macro( "DLL_ENABLED=1" )
        native_target_configuration_release.add_macro( "PLATFORM_OSX=1" )

        native_target_configuration_debug.organization_name = organization_name_no_spaces
        native_target_configuration_release.organization_name = organization_name_no_spaces

        if product_type == ProductType.App:
            if plist_file_ref is None:
                print( "Error: missing Info.plist file for \"%(p)s\" application project." % {'p': project_name} )
            native_target_configuration_debug.plist_file_relative_path = plist_file_ref.get_relative_path()
            native_target_configuration_release.plist_file_relative_path = plist_file_ref.get_relative_path()
            native_target_configuration_debug.app_name = project_name
            native_target_configuration_release.app_name = project_name
        elif product_type == ProductType.DynLib:
            dll_api_name = "%(n)s_EXPORT" % {'n': project_name.upper()}
            native_target_configuration_debug.add_macro( "%(n)s=1" % {'n': dll_api_name} )
            native_target_configuration_release.add_macro( "%(n)s=1" % {'n': dll_api_name} )

        if precompiled_header_ref is not None:
            native_target_configuration_debug.use_precompiled_header = True
            native_target_configuration_debug.precompiled_header_path = precompiled_header_ref.get_relative_path()
            native_target_configuration_release.use_precompiled_header = True
            native_target_configuration_release.precompiled_header_path = precompiled_header_ref.get_relative_path()

        for item in header_search_paths:
            native_target_configuration_debug.add_header_search_path( item )
            native_target_configuration_release.add_header_search_path( item )

        native_target_build_configuration_list.add_configuration( native_target_configuration_debug )
        native_target_build_configuration_list.add_configuration( native_target_configuration_release )
        native_target_build_configuration_list.default_configuration = native_target_configuration_release
        native_target_build_configuration_list.is_for = "PBXNativeTarget"

        builder = PBXProjectBuilder()
        builder.utf_header()
        builder.begin_root()

        builder.begin_objects()

        builder.begin_section( "PBXBuildFile" )
        for item in pbx_build_files:
            builder.save_object( item )
        builder.end_section( "PBXBuildFile" )

        builder.begin_section( "PBXFileReference" )
        for item in pbx_file_refs:
            builder.save_object( item )
        builder.end_section( "PBXFileReference" )

        builder.begin_section("PBXCopyFilesBuildPhase")
        builder.save_build_phase(copy_files_build_phase_sources)
        builder.end_section("PBXCopyFilesBuildPhase")

        builder.begin_section( "PBXFrameworksBuildPhase" )
        builder.save_build_phase( build_phase_framework )
        builder.end_section( "PBXFrameworksBuildPhase" )

        builder.begin_section( "PBXGroup" )
        group_main.save( builder )
        builder.end_section( "PBXGroup" )

        builder.begin_section( "PBXHeadersBuildPhase" )
        builder.save_build_phase( build_phase_headers )
        builder.end_section( "PBXHeadersBuildPhase" )

        builder.begin_section( "PBXNativeTarget" )
        builder.save_native_target( native_target )
        builder.end_section( "PBXNativeTarget" )

        builder.begin_section( "PBXProject" )
        builder.save_project_object( project_object )
        builder.end_section( "PBXProject" )

        builder.begin_section( "PBXSourcesBuildPhase" )
        builder.save_build_phase( build_phase_sources )
        builder.end_section( "PBXSourcesBuildPhase" )

        builder.begin_section( "XCBuildConfiguration" )
        builder.save_configuration( configuration_debug )
        builder.save_configuration( configuration_release )
        builder.save_native_target_configuration( native_target_configuration_debug )
        builder.save_native_target_configuration( native_target_configuration_release )
        builder.end_section( "XCBuildConfiguration" )

        builder.begin_section( "XCConfigurationList" )
        builder.save_configuration_list( project_build_configuration_list )
        builder.save_configuration_list( native_target_build_configuration_list )
        builder.end_section( "XCConfigurationList" )

        builder.end_objects()

        builder.end_root( project_object.uuid )

        return builder.result
