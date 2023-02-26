cmake_minimum_required(VERSION 3.10)
include(${CMAKE_SOURCE_DIR}/config/CMakeLists.txt)

target_link_libraries(${ProjectName}
	ccore
	cmath)

# third party: "physx", "physx_common", "physx_foundation", "physx_extensions", "physx_character_kinematic"
include_directories(${FurThirdPartyPath}/PhysX/physx/include)
include_directories(${FurThirdPartyPath}/PhysX/pxshared/include)

if(WIN32)
set(PHYSX_RELEASE_FOLDER ${FurThirdPartyPath}/PhysX/physX/bin/win.x86_64.vc142.mt/release)
set(PHYSX_DEBUG_FOLDER ${FurThirdPartyPath}/PhysX/physX/bin/win.x86_64.vc142.mt/debug)
target_link_libraries(${ProjectName}
	$<$<NOT:$<CONFIG:Debug>>:${PHYSX_RELEASE_FOLDER}/PhysX_64.lib>
	$<$<NOT:$<CONFIG:Debug>>:${PHYSX_RELEASE_FOLDER}/PhysXCommon_64.lib>
	$<$<NOT:$<CONFIG:Debug>>:${PHYSX_RELEASE_FOLDER}/PhysXFoundation_64.lib>
	$<$<NOT:$<CONFIG:Debug>>:${PHYSX_RELEASE_FOLDER}/PhysXCharacterKinematic_static_64.lib>
	$<$<NOT:$<CONFIG:Debug>>:${PHYSX_RELEASE_FOLDER}/PhysXExtensions_static_64.lib>
	
	$<$<CONFIG:Debug>:${PHYSX_DEBUG_FOLDER}/PhysX_64.lib>
	$<$<CONFIG:Debug>:${PHYSX_DEBUG_FOLDER}/PhysXCommon_64.lib>
	$<$<CONFIG:Debug>:${PHYSX_DEBUG_FOLDER}/PhysXFoundation_64.lib>
	$<$<CONFIG:Debug>:${PHYSX_DEBUG_FOLDER}/PhysXCharacterKinematic_static_64.lib>
	$<$<CONFIG:Debug>:${PHYSX_DEBUG_FOLDER}/PhysXExtensions_static_64.lib>)
	
# copy .dll to bin folder
add_custom_command(TARGET ${ProjectName} POST_BUILD
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/release PhysX_64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/release PhysXFoundation_64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/release PhysXCommon_64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/release glut32.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/release PhysXDevice64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/release PhysXCooking_64.dll)
	
add_custom_command(TARGET ${ProjectName} POST_BUILD
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/debug PhysX_64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/debug PhysXFoundation_64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/debug PhysXCommon_64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/debug glut32.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/debug PhysXDevice64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/debug PhysXCooking_64.dll)
	
add_custom_command(TARGET ${ProjectName} POST_BUILD
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/RelWithDebInfo PhysX_64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/RelWithDebInfo PhysXFoundation_64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/RelWithDebInfo PhysXCommon_64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/RelWithDebInfo glut32.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/RelWithDebInfo PhysXDevice64.dll
	COMMAND robocopy ${PHYSX_RELEASE_FOLDER} ${FurBinPath}/RelWithDebInfo PhysXCooking_64.dll)
	
elseif(APPLE)
target_link_libraries(${ProjectName}
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysX_static_64.a
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysXCommon_static_64.a
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysXFoundation_static_64.a
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysXCharacterKinematic_static_64.a
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysXExtensions_static_64.a)

endif()
