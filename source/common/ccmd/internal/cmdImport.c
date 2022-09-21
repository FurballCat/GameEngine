/* Copyright (c) Furball Cat */
#include "cmd.h"
#include <stdio.h>
#include "ccore/public.h"
#include "ccore/serialize.h"
#include "canim/public.h"
#include "cimport/public.h"

int fc_cmd_import(int argc, char* argv[], fc_cmd_execute_ctx_t* ctx, fc_alloc_callbacks_t* pAllocCallbacks)
{
	if(CMD_FLAG("-help"))
	{
		CMD_LOG("Import asset from source file (like .fbx) to engine file (like .anim).\n"
				"ccmd import [asset-type] [asset-unique-name] [...]\n"
				"Example usage: ccmd import -anim \"zelda-idle-01\"\n"
				"List of arguments:\n"
				"  [asset-type]:\n"
				"    -anim : animation clip\n"
				"    -rig : rig, skeleton for animations\n"
				"    -mesh : 3D mesh, model\n");
		return 0;
	}
	
	//$(LD_DYLIB_INSTALL_NAME_$(LLVM_TARGET_TRIPLE_VENDOR):default=$(EXECUTABLE_PATH))
	
	const char* fileName = argv[2];
	
	fa_anim_clip_t* animClip = NULL;
	
	{
		fi_depot_t depot = {};
		depot.path = "../../../../../";
		
		char pathSource[256] = {};
		fc_path_concat(pathSource, "", "assets/characters/zelda/animations/", fileName, ".fbx");
		
		char pathEngine[256] = {};
		fc_path_concat(pathEngine, ctx->depotPath, "depot/animations/", fileName, ".anim");
		
		// import animation from FBX
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = pathSource;
			ctx.extractRootMotion = true;
			//ctx.rig = rig;
			
			fi_import_anim_clip(&depot, &ctx, &animClip, pAllocCallbacks);
		}
		
		// save engine file
		{
			fc_serializer_t serializer = {};
			serializer.file = fopen(pathEngine, "wb");
			serializer.isWriting = true;
			
			fa_anim_clip_serialize(&serializer, animClip, pAllocCallbacks);
			
			fclose(serializer.file);
		}
	}
	
	return 0;
}
