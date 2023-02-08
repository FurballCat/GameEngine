cmake_minimum_required(VERSION 3.10)
include(${CMAKE_SOURCE_DIR}/config/CMakeLists.txt)

target_link_libraries(${ProjectName}
	ccore
	cmath)

# third party: "physx", "physx_common", "physx_foundation", "physx_extensions", "physx_character_kinematic"
include_directories(${FurThirdPartyPath}/PhysX/physx/include)
include_directories(${FurThirdPartyPath}/PhysX/pxshared/include)

if(WIN32)
target_link_libraries(${ProjectName}
	${FurThirdPartyPath}/PhysX/physX/bin/win.x86_64.vc142.mt/release/PhysX_64.lib
	${FurThirdPartyPath}/PhysX/physX/bin/win.x86_64.vc142.mt/release/PhysXCommon_64.lib
	${FurThirdPartyPath}/PhysX/physX/bin/win.x86_64.vc142.mt/release/PhysXFoundation_64.lib
	${FurThirdPartyPath}/PhysX/physX/bin/win.x86_64.vc142.mt/release/PhysXCharacterKinematic_static_64.lib
	${FurThirdPartyPath}/PhysX/physX/bin/win.x86_64.vc142.mt/release/PhysXExtensions_static_64.lib)
	
elseif(APPLE)
target_link_libraries(${ProjectName}
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysX_static_64.a
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysXCommon_static_64.a
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysXFoundation_static_64.a
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysXCharacterKinematic_static_64.a
	${FurThirdPartyPath}/PhysX/physX/bin/mac.x86_64/release/libPhysXExtensions_static_64.a)

endif()
