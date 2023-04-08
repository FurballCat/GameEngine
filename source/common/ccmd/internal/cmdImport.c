/* Copyright (c) Furball Cat */
#include "cmd.h"
#include <stdio.h>
#include "ccore/public.h"
#include "ccore/serialize.h"
#include "canim/public.h"
#include "cimport/public.h"
#include "cmath/public.h"

void fcCmdImportRigApplyProperties(FcRig* rig, const FcAllocator* allocator)
{
	// apply rig properties
	{
		// set locomotion joint to track and apply root motion
		{
			rig->idxLocoJoint = fcRigFindBoneIdx(rig, SID("motion"));
		}
		
		// left leg IK setup
		{
			FcAnimIKSetup* ik = &rig->ikLeftLeg;
			ik->idxBeginParent = fcRigFindBoneIdx(rig, SID("Bip001_Pelvis"));
			ik->idxBegin = fcRigFindBoneIdx(rig, SID("Bip001_Thigh_L"));
			ik->idxMid = fcRigFindBoneIdx(rig, SID("Bip001_Calf_L"));
			ik->idxEnd = fcRigFindBoneIdx(rig, SID("Bip001_Foot_L"));
			ik->hingeAxisMid = FM_AXIS_Z;
			ik->minAngle = 0.02f;
			ik->maxAngle = 2.8f;
		}
		
		// right leg IK setup
		{
			FcAnimIKSetup* ik = &rig->ikRightLeg;
			ik->idxBeginParent = fcRigFindBoneIdx(rig, SID("Bip001_Pelvis"));
			ik->idxBegin = fcRigFindBoneIdx(rig, SID("Bip001_Thigh_R"));
			ik->idxMid = fcRigFindBoneIdx(rig, SID("Bip001_Calf_R"));
			ik->idxEnd = fcRigFindBoneIdx(rig, SID("Bip001_Foot_R"));
			ik->hingeAxisMid = FM_AXIS_NEG_Z;
			ik->minAngle = 0.02f;
			ik->maxAngle = 2.8f;
		}
		
		// head look-at setup
		{
			FcAnimLookAtSetup* lookAt = &rig->headLookAt;
			lookAt->idxHead = fcRigFindBoneIdx(rig, SID("Bip001_Head"));
			lookAt->idxNeck = fcRigFindBoneIdx(rig, SID("Bip001_Neck"));
			lookAt->idxSpine3 = fcRigFindBoneIdx(rig, SID("Bip001_Spine3"));
			lookAt->limitYaw = FM_DEG_TO_RAD(60.0f);
			lookAt->limitPitchDown = FM_DEG_TO_RAD(25.0f);
			lookAt->limitPitchUp = FM_DEG_TO_RAD(45.0f);
		}
		
		// masks
		{
			rig->maskUpperBody = FUR_ALLOC_ARRAY_AND_ZERO(u8, rig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, allocator);
			const int16_t idxSpine = fcRigFindBoneIdx(rig, SID("Bip001_Spine"));
			const FcStringId hashes[9] = {
				SID("Bip001_Pelvis"),
				SID("Bip001_Thigh_L"),
				SID("Bip001_Calf_L"),
				SID("Bip001_Foot_L"),
				SID("Bip001_Thigh_R"),
				SID("Bip001_Calf_R"),
				SID("Bip001_Foot_R"),
				SID("Bip001_Spine"),
				SID("Bip001_Spine1")
			};
			
			const FcStringId hashesPartial[18] = {
				SID("Bip001_UpperArm_L"),
				SID("Bip001_UpperArm3_L"),
				SID("Bip001_Hand_L"),
				SID("Bip001_Thumb1_L"),
				SID("Bip001_Thumb2_L"),
				SID("Bip001_Thumb3_L"),
				SID("Bip001_Index1_L"),
				SID("Bip001_Index2_L"),
				SID("Bip001_Index3_L"),
				SID("Bip001_Middle1_L"),
				SID("Bip001_Middle2_L"),
				SID("Bip001_Middle3_L"),
				SID("Bip001_Ring1_L"),
				SID("Bip001_Ring2_L"),
				SID("Bip001_Ring3_L"),
				SID("Bip001_Pinky1_L"),
				SID("Bip001_Pinky2_L"),
				SID("Bip001_Pinky3_L")
			};
			
			const FcStringId noHashes[2] = {
				SID("rootTransform"),
				SID("motion")
			};
			
			if(idxSpine != -1)
			{
				for(u32 i=0; i<rig->numBones; ++i)
				{
					u8 w = 220;
					for(u32 j=0; j<9; ++j)
					{
						if(rig->boneNameHashes[i] == hashes[j])
						{
							w = 0;
							break;
						}
					}
					for(u32 j=0; j<18; ++j)
					{
						if(rig->boneNameHashes[i] == hashesPartial[j])
						{
							w = 100;
						}
					}
					for(u32 j=0; j<2; ++j)
					{
						if(rig->boneNameHashes[i] == noHashes[j])
						{
							w = 0;
						}
					}
					rig->maskUpperBody[i] = w;
				}
			}
		}
		
		// face mask
		{
			rig->maskFace = FUR_ALLOC_ARRAY_AND_ZERO(u8, rig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, allocator);
			const int16_t idxSpine = fcRigFindBoneIdx(rig, SID("Bip001_Spine"));
			const FcStringId hashes[] = {
				//SID("Bip001_Neck"),
				//SID("Bip001_Head"),
				SID("Bip001_Jaw"),
				SID("Bip001_Chin"),
				SID("Bip001_LipLower_C"),
				SID("Bip001_LipLower1_R"),
				SID("Bip001_LipLower1_L"),
				SID("Bip001_LipLower2_R"),
				SID("Bip001_LipLower2_L"),
				SID("Bip001_TeethLower"),
				SID("Bip001_Tongue"),
				SID("Bip001_Tongue1"),
				SID("Bip001_Tongue2"),
				SID("Bip001_Tongue3"),
				SID("Bip001_LipUpper_C"),
				SID("Bip001_LipUpper1_R"),
				SID("Bip001_LipUpper1_L"),
				SID("Bip001_LipUpper2_R"),
				SID("Bip001_LipUpper2_L"),
				SID("Bip001_LipCorner_R"),
				SID("Bip001_LipCorner_L"),
				SID("Bip001_Cheek_R"),
				SID("Bip001_Cheek_L"),
				SID("Bip001_Cheek1_R"),
				SID("Bip001_Cheek1_L"),
				SID("Bip001_Cheek2_R"),
				SID("Bip001_Cheek2_L"),
				SID("Bip001_Smile_R"),
				SID("Bip001_Smile_L"),
				SID("Bip001_Frown_R"),
				SID("Bip001_Frown_L"),
				SID("Bip001_TeethUpper"),
				SID("Bip001_BrowInner_L"),
				SID("Bip001_BrowInner_R"),
				SID("Bip001_BrowMid_L"),
				SID("Bip001_BrowMid_R"),
				SID("Bip001_BrowOuter_L"),
				SID("Bip001_BrowOuter_R"),
				SID("Bip001_Nostril_L"),
				SID("Bip001_Nostril_R"),
				SID("Bip001_Nose"),
				SID("Bip001_EyelidUp_L"),
				SID("Bip001_EyelidUp_R"),
				SID("Bip001_EyelidDown_L"),
				SID("Bip001_EyelidDown_R"),
				SID("Bip001_Eye_L"),
				SID("Bip001_EyeSpec_L"),
				SID("Bip001_EyeSpec1_L"),
				SID("Bip001_EyeSpec2_L"),
				SID("Bip001_EyeSpec3_L"),
				SID("Bip001_Eye_R"),
				SID("Bip001_EyeSpec_R"),
				SID("Bip001_EyeSpec1_R"),
				SID("Bip001_EyeSpec2_R"),
				SID("Bip001_Ear_L"),
				SID("Bip001_Ear_R"),
				SID("Bip001_BrowFlesh_L"),
				SID("Bip001_BrowFlesh_R"),
				SID("Bip001_EyelidCrevace_L"),
				SID("Bip001_EyelidCrevace_R"),
			};
			
			if(idxSpine != -1)
			{
				for(u32 i=0; i<rig->numBones; ++i)
				{
					u8 w = 0;
					const u32 numHashes = FUR_ARRAY_SIZE(hashes);
					for(u32 j=0; j<numHashes; ++j)
					{
						if(rig->boneNameHashes[i] == hashes[j])
						{
							w = 255;
							break;
						}
					}
					rig->maskFace[i] = w;
				}
			}
		}
		
		// hands mask
		{
			rig->maskHands = FUR_ALLOC_ARRAY_AND_ZERO(u8, rig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, allocator);
			const int16_t idxSpine = fcRigFindBoneIdx(rig, SID("Bip001_Spine"));
			const FcStringId hashes[] = {
				SID("Bip001_Index1_L"),
				SID("Bip001_Index2_L"),
				SID("Bip001_Index3_L"),
				SID("Bip001_Middle1_L"),
				SID("Bip001_Middle2_L"),
				SID("Bip001_Middle3_L"),
				SID("Bip001_Ring1_L"),
				SID("Bip001_Ring2_L"),
				SID("Bip001_Ring3_L"),
				SID("Bip001_Pinky1_L"),
				SID("Bip001_Pinky2_L"),
				SID("Bip001_Pinky3_L"),
				SID("Bip001_Thumb_L"),
				SID("Bip001_Thumb1_L"),
				SID("Bip001_Thumb2_L"),
				SID("Bip001_Thumb3_L"),
				SID("Bip001_Index1_R"),
				SID("Bip001_Index2_R"),
				SID("Bip001_Index3_R"),
				SID("Bip001_Middle1_R"),
				SID("Bip001_Middle2_R"),
				SID("Bip001_Middle3_R"),
				SID("Bip001_Ring1_R"),
				SID("Bip001_Ring2_R"),
				SID("Bip001_Ring3_R"),
				SID("Bip001_Pinky1_R"),
				SID("Bip001_Pinky2_R"),
				SID("Bip001_Pinky3_R"),
				SID("Bip001_Thumb_R"),
				SID("Bip001_Thumb1_R"),
				SID("Bip001_Thumb2_R"),
				SID("Bip001_Thumb3_R"),
			};
			
			if(idxSpine != -1)
			{
				for(u32 i=0; i<rig->numBones; ++i)
				{
					u8 w = 0;
					const u32 numHashes = FUR_ARRAY_SIZE(hashes);
					for(u32 j=0; j<numHashes; ++j)
					{
						if(rig->boneNameHashes[i] == hashes[j])
						{
							w = 255;
							break;
						}
					}
					rig->maskHands[i] = w;
				}
			}
		}
	}
}

int fcCmdImportRig(int argc, char* argv[], FcCmdExecuteCtx* ctx, const FcAllocator* allocator)
{
	const char* srcPath = CMD_ARG("-p");
	if(!srcPath)
	{
		CMD_LOG_ERROR("No source path provided, please use -p path argument");
		return 1;
	}
	
	const char* dstName = CMD_ARG("-o");
	if(!dstName)
	{
		CMD_LOG_ERROR("No output name provided, please use -o name argument");
		return 1;
	}
	
	char pathSource[256] = {0};
	fcPathConcat(pathSource, ctx->assetsPath, srcPath, "", "");
	
	char pathEngine[256];
	fcPathConcat(pathEngine, ctx->enginePath, "rig/", dstName, ".rig");
	
	FcRig* rig = NULL;
	
	// import rig
	{
		fi_depot_t depot = {0};
		depot.path = ""; // todo: remove
		
		fi_import_rig_ctx_t rigCtx = {0};
		rigCtx.path = pathSource;
		
		CMD_LOG("Importing %s", pathSource);
		fcImportRig(&depot, &rigCtx, &rig, allocator);
		
		if(!rig)
		{
			CMD_LOG_ERROR("Failed to import rig");
			return 1;
		}
		
		// apply properties
		fcCmdImportRigApplyProperties(rig, allocator);
	}
	
	// save engine file
	{
		FcSerializer serializer = {0};
		serializer.file = fopen(pathEngine, "wb");
		serializer.isWriting = true;
		
		CMD_LOG("Saving %s", pathEngine);
		fcRigSerialize(&serializer, rig, allocator);
		
		fclose(serializer.file);
	}
	
	FUR_FREE(rig, allocator);
	
	return 0;
}

int fcCmdImportAnim(int argc, char* argv[], FcCmdExecuteCtx* ctx, const FcAllocator* allocator)
{
	// requires rig name to be provided
	const char* rigName = CMD_ARG("-r");
	if(!rigName)
	{
		CMD_LOG_ERROR("No rig provided, please use -r name argument");
		return 1;
	}
	
	const char* srcPath = CMD_ARG("-p");
	if(!srcPath)
	{
		CMD_LOG_ERROR("No source path provided, please use -p path argument");
		return 1;
	}
	
	const char* dstName = CMD_ARG("-o");
	if(!dstName)
	{
		CMD_LOG_ERROR("No output name provided, please use -o name argument");
		return 1;
	}
	
	FcRig rig = {0};
	
	// load rig from engine file
	{
		// create rig engine file path
		char rigFilePath[256] = {0};
		fcPathConcat(rigFilePath, ctx->enginePath, "rig/", rigName, ".rig");
		FILE* rigFile = fopen(rigFilePath, "rb");
		if(!rigFile)
		{
			CMD_LOG_ERROR("Cannot find rig \'%s\'", rigFilePath);
			return 1;
		}
		
		FcSerializer ser = {0};
		ser.file = rigFile;
		ser.isWriting = false;
		
		fcRigSerialize(&ser, &rig, allocator);
		
		fclose(rigFile);
	}
	
	FcAnimClip* animClip = NULL;
	
	{
		fi_depot_t depot = {0};
		depot.path = "";	// todo: remove that
		
		char pathSource[256] = {0};
		fcPathConcat(pathSource, ctx->assetsPath, srcPath, "", "");
		
		char pathEngine[256] = {0};
		fcPathConcat(pathEngine, ctx->enginePath, "anim/", dstName, ".anim");
		
		// import animation
		{
			FcImportAnimClipCtx ctx = {0};
			ctx.path = pathSource;
			ctx.extractRootMotion = true;
			ctx.rig = &rig;
			
			CMD_LOG("Importing %s", pathSource);
			fcImportAnimClip(&depot, &ctx, &animClip, allocator);
		}
		
		// save engine file
		{
			FcSerializer serializer = {0};
			serializer.file = fopen(pathEngine, "wb");
			serializer.isWriting = true;
			
			CMD_LOG("Saving %s", pathEngine);
			fcAnimClipSerialize(&serializer, animClip, allocator);
			
			fclose(serializer.file);
		}
	}
	
	return 0;
}

int fcCmdImportMesh(int argc, char* argv[], FcCmdExecuteCtx* ctx, const FcAllocator* allocator)
{
	const char* srcPath = CMD_ARG("-p");
	if(!srcPath)
	{
		CMD_LOG_ERROR("No source path provided, please use -p path argument");
		return 1;
	}
	
	const char* dstName = CMD_ARG("-o");
	if(!dstName)
	{
		CMD_LOG_ERROR("No output name provided, please use -o name argument");
		return 1;
	}
	
	FcMeshResource* meshResource = NULL;
	
	// import mesh
	{
		fi_depot_t depot = {0};
		depot.path = "";	// todo: remove that
		
		char pathSource[256] = {0};
		fcPathConcat(pathSource, ctx->assetsPath, srcPath, "", "");
		
		FcImportMeshCtx loadCtx;
		loadCtx.path = pathSource;
		
		CMD_LOG("Importing %s", pathSource);
		fcImportMeshResource(&depot, &loadCtx, &meshResource, allocator);
	}
	
	// load or save serialized file
	{
		char pathEngine[256] = {0};
		fcPathConcat(pathEngine, ctx->enginePath, "mesh/", dstName, ".mesh");
		
		FcSerializer ser = {0};
		ser.file = fopen(pathEngine, "wb");
		ser.isWriting = true;
		
		CMD_LOG("Saving %s", pathEngine);
		fcMeshResourceSerialize(&ser, meshResource, allocator);
		
		fclose(ser.file);
	}
	
	FUR_FREE(meshResource, allocator);
	
	return 0;
}

int fcCmdImport(int argc, char* argv[], FcCmdExecuteCtx* ctx, const FcAllocator* allocator)
{
	if(CMD_FLAG("-help"))
	{
		CMD_LOG("Import asset from source file (like .fbx) to engine file (like .anim).\n"
				"ccmd import [asset-type] [asset-unique-name] [...]\n"
				"Example usage: ccmd import -anim -p \"zelda-idle-01\" -r \"zelda-rig\"\n"
				"List of arguments:\n"
				"  [asset-type]:\n"
				"    -anim : animation clip\n"
				"    -rig : rig, skeleton for animations\n"
				"    -mesh : 3D mesh, model\n");
		return 0;
	}
	
	if(CMD_FLAG("-rig"))
	{
		return fcCmdImportRig(argc, argv, ctx, allocator);
	}
	
	if(CMD_FLAG("-anim"))
	{
		return fcCmdImportAnim(argc, argv, ctx, allocator);
	}
	
	if(CMD_FLAG("-mesh"))
	{
		return fcCmdImportMesh(argc, argv, ctx, allocator);
	}
	
	CMD_LOG_ERROR("Please provide resource flag type for import (example: -anim)");
	return 1;
}
