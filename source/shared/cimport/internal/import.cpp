/* Copyright (c) 2016-2019 Furball Cat */

#include "import.h"
#include <string.h>
#include <fstream>
#include <vector>
#include <map>
#include "ccore/public.h"
#include "cmath/public.h"
#include "canim/public.h"
#include "ofbx.h"
#include "ccore/serialize.h"

#define MIN(a, b) a < b ? a : b;

bool IsFileExtensionEqualTo(const char* path, const char* ext)
{
	const size_t pathLen = strlen(path);
	const size_t extLen = strlen(ext);
	
	if(strcmp(path + pathLen - extLen, ext) == 0)
	{
		return true;
	}
	
	return false;
}

ofbx::IScene* OpenScene_FBX(const char* path)
{
	std::ifstream file(path, std::ifstream::in | std::ifstream::binary | std::ifstream::ate);
	FUR_ASSERT(file);
	
	uint32_t fileSize = (uint32_t)file.tellg();
	file.seekg(0, std::ifstream::beg);
	
	std::vector<char> fileData;
	fileData.resize(fileSize);
	
	file.read(&fileData[0], fileSize);
	file.close();
	
	ofbx::IScene* scene = ofbx::load((const uint8_t*)fileData.data(), (int)fileData.size());
	FUR_ASSERT(scene);
	
	return scene;
}

struct FBXAnimCurve
{
	std::vector<float> m_times;
	std::vector<float> m_values;
};

struct FBXBoneInfo
{
	std::string m_name;
	std::string m_parentName;
	
	FBXAnimCurve m_translation[3];
	FBXAnimCurve m_rotation[3];
	FBXAnimCurve m_scale[3];
	
	uint32_t m_originalIndex;	// to fix skinning data
};

struct FBXRig
{
	std::vector<std::string> m_jointNames;
	std::vector<int16_t> m_parents;
	std::vector<fm_xform> m_referencePose;
};

void FillFBXAnimCurve(FBXAnimCurve& curve, const ofbx::AnimationCurve* fbxCurve)
{
	const int32_t numKeys = fbxCurve->getKeyCount();
	const int64_t* keyTimes = fbxCurve->getKeyTime();
	const float* keyValues = fbxCurve->getKeyValue();
	
	curve.m_times.resize(numKeys);
	curve.m_values.resize(numKeys);
	
	for(int32_t ik=0; ik<numKeys; ++ik)
	{
		curve.m_times[ik] = ofbx::fbxTimeToSeconds(keyTimes[ik]);
		curve.m_values[ik] = keyValues[ik];
	}
}

void fi_gather_anim_curves(ofbx::IScene* scene, std::map<std::string, FBXBoneInfo*>& bones)
{
	const int32_t numAnimStacks = scene->getAnimationStackCount();
	for(int32_t i=0; i<numAnimStacks; ++i)
	{
		const ofbx::AnimationStack* animStack = scene->getAnimationStack(i);
		const ofbx::AnimationLayer* layer = animStack->getLayer(0);	// usually there's only one layer, unless two animations are blended together
		const int32_t numCurveNodes = layer->getCurveNodeCount();
		
		for(int32_t icn=0; icn<numCurveNodes; ++icn)
		{
			const ofbx::AnimationCurveNode* curveNode = layer->getCurveNode(icn);
			const ofbx::Object* bone = curveNode->getBone();
			if(bone)
			{
				if(!(strcmp(bone->name, "Armature") == 0))		// Blender adds 'Armature' as root bone
				{
					if(bones.count(bone->name) == 0)
					{
						FBXBoneInfo* info = new FBXBoneInfo();
						bones[bone->name] = info;
						info->m_name = std::string(bone->name);
						info->m_originalIndex = (uint32_t)icn;
						
						const ofbx::Object* parentBone = bone->getParent();
						if(parentBone)
						{
							info->m_parentName = std::string(parentBone->name);
						}
					}
					
					FBXBoneInfo* info = bones[bone->name];
					
					std::string propertyName;
					const int32_t propNameSize = curveNode->getBoneLinkPropertySize();
					if(propNameSize < 128)
					{
						char buffer[128];
						curveNode->getBoneLinkProperty(buffer, propNameSize);
						buffer[propNameSize] = '\0';
						propertyName = std::string(buffer);
					}
					
					const int32_t numCurves = std::min(curveNode->getCurveCount(), 3);
					
					if(propertyName == "Lcl Translation")
					{
						for(int32_t ic=0; ic<numCurves; ++ic)
						{
							FillFBXAnimCurve(info->m_translation[ic], curveNode->getCurve(ic));
						}
					}
					else if(propertyName == "Lcl Rotation")
					{
						for(int32_t ic=0; ic<numCurves; ++ic)
						{
							FillFBXAnimCurve(info->m_rotation[ic], curveNode->getCurve(ic));
						}
					}
					else if(propertyName == "Lcl Scale")
					{
						for(int32_t ic=0; ic<numCurves; ++ic)
						{
							FillFBXAnimCurve(info->m_scale[ic], curveNode->getCurve(ic));
						}
					}
				}
			}
		}
	}
}

void fi_import_sort_bones(std::map<std::string, FBXBoneInfo*>& bones, std::vector<const FBXBoneInfo*>& sortedBones)
{
	if(!bones.empty())
	{
		// preparing stuff
		std::vector<const FBXBoneInfo*> rigBones;
		rigBones.reserve(bones.size());
		for(const auto& it : bones)
		{
			const FBXBoneInfo* info = it.second;
			rigBones.push_back(info);
		}
		
		sortedBones.reserve(rigBones.size());
		
		std::vector<std::string> parents;
		parents.reserve(bones.size());
		
		std::string parentName = "Armature";
		
		for(uint32_t i=0; i<rigBones.size(); ++i)
		{
			if(rigBones[i]->m_name == "rootTransform")
			{
				parentName = "rootTransform";
				sortedBones.push_back(rigBones[i]);
				break;
			}
		}
		
		parents.push_back(parentName);
		while(!parents.empty())
		{
			const std::string& parent = parents.front();
			
			for(uint32_t i=0; i<rigBones.size(); ++i)
			{
				if(rigBones[i]->m_parentName == parent)
				{
					sortedBones.push_back(rigBones[i]);
					parents.push_back(rigBones[i]->m_name);
				}
			}
			
			parents.erase(parents.begin());
		}
	}
}

fi_result_t fi_import_rig(const fi_depot_t* depot, const fi_import_rig_ctx_t* ctx,
									  fa_rig_t** ppRig, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FBXRig rig;
	
	std::string absolutePath = depot->path;
	absolutePath += ctx->path;
	
	// todo: validate absolute path
	
	if(IsFileExtensionEqualTo(absolutePath.c_str(), ".fbx"))
	{
		ofbx::IScene* scene = OpenScene_FBX(absolutePath.c_str());
		FUR_ASSERT(scene);
		
		std::map<std::string, FBXBoneInfo*> bones;
		std::vector<const FBXBoneInfo*> sortedRigBones;
		
		fi_gather_anim_curves(scene, bones);
		fi_import_sort_bones(bones, sortedRigBones);
		
		const uint32_t numBones = (uint32_t)sortedRigBones.size();
		
		if(numBones > 0)
		{
			// rig
			rig.m_parents.resize(numBones);
			rig.m_jointNames.resize(numBones);
			rig.m_referencePose.resize(numBones);
			
			if(!sortedRigBones.empty())
			{
				for(uint32_t i=0; i<sortedRigBones.size(); ++i)
				{
					rig.m_jointNames[i] = sortedRigBones[i]->m_name;
					
					fm_vec4 position = {0.0f, 0.0f, 0.0f, 0.0f};
					const float scale = 1.0f;
					
					const FBXAnimCurve* translationCurves = sortedRigBones[i]->m_translation;
					if(!translationCurves[0].m_values.empty())
					{
						position.x = translationCurves[0].m_values[0] * scale;
					}
					if(!translationCurves[1].m_values.empty())
					{
						position.y = translationCurves[1].m_values[0] * scale;
					}
					if(!translationCurves[2].m_values.empty())
					{
						position.z = translationCurves[2].m_values[0] * scale;
					}
					
					fm_euler_angles rotation = {0.0f, 0.0f, 0.0f};
					
					float x = 0.0f;
					float y = 0.0f;
					float z = 0.0f;
					
					const FBXAnimCurve* rotationCurves = sortedRigBones[i]->m_rotation;
					/*if(!rotationCurves[0].m_values.empty())
					{
						yaw = -rotationCurves[1].m_values[0];
					}
					if(!rotationCurves[1].m_values.empty())
					{
						pitch = -rotationCurves[0].m_values[0];
					}
					if(!rotationCurves[2].m_values.empty())
					{
						roll = -rotationCurves[2].m_values[0];
					}*/
					
					if(!rotationCurves[0].m_values.empty())
					{
						x = rotationCurves[0].m_values[0];
					}
					if(!rotationCurves[1].m_values.empty())
					{
						y = rotationCurves[1].m_values[0];
					}
					if(!rotationCurves[2].m_values.empty())
					{
						z = rotationCurves[2].m_values[0];
					}
					
					rotation.yaw = FM_DEG_TO_RAD(-y);
					rotation.pitch = FM_DEG_TO_RAD(-x);
					rotation.roll = FM_DEG_TO_RAD(-z);
					
					rig.m_referencePose[i].pos = position;
					fm_quat_make_from_euler_angles_xyz(&rotation, &rig.m_referencePose[i].rot);
				}
				
				for(int32_t i=0; i<sortedRigBones.size(); ++i)
				{
					const std::string& parentName = sortedRigBones[i]->m_parentName;
					int16_t parentIndex = -1;
					
					for(int32_t p=i-1; p>=0; --p)
					{
						if(sortedRigBones[p]->m_name == parentName)
						{
							parentIndex = (int16_t)p;
							break;
						}
					}
					
					rig.m_parents[i] = parentIndex;
					
					// rotate root bones by -90 degrees around X axis (joints have different orientation than world, so we fix this by rotating root)
					/*
					if(parentIndex == -1)
					{
						fm_quat rootFix90;
						fm_quat_make_from_axis_angle(1.0f, 0.0f, 0.0f, -M_PI, &rootFix90);
						fm_quat rootResult;
						
						fm_quat_mul(&rig.m_referencePose[i].rot,
									&rootFix90, &rootResult);
						
						rig.m_referencePose[i].rot = rootResult;
					}
					 */
				}
			}
			
			// copy to output rig
			{
				fa_rig_t* pRig = (fa_rig_t*)FUR_ALLOC_AND_ZERO(sizeof(fa_rig_t), 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				
				*ppRig = pRig;
				
				pRig->refPose = (fm_xform*)FUR_ALLOC(sizeof(fm_xform) * rig.m_referencePose.size(), 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				pRig->parents = (int16_t*)FUR_ALLOC(sizeof(int16_t) * rig.m_referencePose.size(), 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				pRig->numBones = (uint32_t)rig.m_referencePose.size();
				pRig->boneNameHashes = FUR_ALLOC_ARRAY_AND_ZERO(fc_string_hash_t, numBones, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				
				for(uint32_t i=0; i<pRig->numBones; ++i)
				{
					pRig->refPose[i] = rig.m_referencePose[i];
					pRig->parents[i] = rig.m_parents[i];
					pRig->boneNameHashes[i] = SID_REG(rig.m_jointNames[i].c_str());
				}
			}
			
			for(uint32_t i=0; i<sortedRigBones.size(); ++i)
			{
				delete sortedRigBones[i];
			}
		}
		
		scene->destroy();
		
		return FI_RESULT_OK;
	}
	
	return FI_RESULT_UNKNOWN_FILE_FORMAT_IMPORT_ERROR;
}

struct fi_temp_anim_curve_key_t
{
	uint16_t keyTime;
	bool isRotation;
	bool isLastCompMinus;	// for rotation
	uint16_t keyValues[3];
};

struct fi_temp_anim_curve_t
{
	uint16_t index;
	std::vector<fi_temp_anim_curve_key_t> keys;
	std::vector<fi_temp_anim_curve_key_t> posKeys;
};

struct fi_temp_anim_clip_t
{
	float duration;
	std::vector<fi_temp_anim_curve_t> curves;
};

void fi_sample_fbx_anim_curve(const FBXAnimCurve* curve, uint32_t size, float* result, float time)
{
	for(uint32_t i=0; i<size; ++i)
	{
		const uint32_t timesSize = (uint32_t)curve[i].m_times.size();
		
		uint32_t idx = 0;
		while(idx < timesSize-1 && curve[i].m_times[idx] < time)
		{
			++idx;
		}
		
		const uint32_t upperIdx = idx;
		const uint32_t lowerIdx = idx == 0 ? idx : idx - 1;
		
		if(lowerIdx == upperIdx)
		{
			const float value = curve[i].m_values[idx];
			result[i] = value;
		}
		else
		{
			const float lowerTime = curve[i].m_times[lowerIdx];
			const float upperTime = curve[i].m_times[upperIdx];
			
			const float alpha = (time - lowerTime) / (upperTime - lowerTime);
			
			const float lowerValue = curve[i].m_values[lowerIdx];
			const float upperValue = curve[i].m_values[upperIdx];
			const float value = lowerValue * (1.0f - alpha) + upperValue * alpha;
			result[i] = value;
		}
	}
}

const float Km  = 4.0*(0.4142135679721832275390625); // 4(sqrt(2)-1)
const float Khf = 2.414213657379150390625;           // sqrt(2)+1 = 1/(sqrt(2)-1)
const float Khi = 0.17157287895679473876953125;      // 3-2sqrt(2)

float fa_decompress_float_minus_one_plus_one(uint16_t value)
{
	return (((float)value) / 65535.0f) * 2.0f - 1.0f;
}

uint16_t fa_compress_float_minus_one_plus_on(float value)
{
	return (uint16_t)(((value + 1.0f) / 2.0f) * 65535.0f);
}

void fm_vec3_to_16bit(const fm_vec3* v, uint16_t* b)
{
	b[0] = fa_compress_float_minus_one_plus_on(v->x);
	b[1] = fa_compress_float_minus_one_plus_on(v->y);
	b[2] = fa_compress_float_minus_one_plus_on(v->z);
}

void fm_16bit_to_vec3(const uint16_t* b, fm_vec3* v)
{
	v->x = fa_decompress_float_minus_one_plus_one(b[0]);
	v->y = fa_decompress_float_minus_one_plus_one(b[1]);
	v->z = fa_decompress_float_minus_one_plus_one(b[2]);
}

void quat_fhm(fm_quat q, fm_vec3* v)
{
	float s = Khf / (1.0 + q.r + sqrt(2.0 + 2.0 * q.r));
	
	v->x = q.i * s;
	v->y = q.j * s;
	v->z = q.k * s;
}

fm_quat quat_ihm(const fm_vec3* v)
{
	float d = Khi * fm_vec3_dot(v, v);
	float a = (1.0+d);
	float b = (1.0-d)*Km;
	float c = 1.0/(a*a);
	fm_quat q;
	
	float bc = b * c;
	
	q.i = v->x * bc;
	q.j = v->y * bc;
	q.k = v->z * bc;
	q.r = (1.0 + d * (d - 6.0)) * c;
	
	return q;
}

void quat_fhm_16bit(fm_quat q, uint16_t* v)
{
	fm_vec3 vec;
	quat_fhm(q, &vec);
	fm_vec3_to_16bit(&vec, v);
}

fm_quat quat_ihm_16bit(const uint16_t* b)
{
	fm_vec3 vec;
	fm_16bit_to_vec3(b, &vec);
	return quat_ihm(&vec);
}

const float c_positionCompressionRange = 20.0f;

void vec4_com_16bit(fm_vec4 v, uint16_t* b)
{
	fm_vec3 vec = {v.x / c_positionCompressionRange, v.y / c_positionCompressionRange, v.z / c_positionCompressionRange};
	fm_vec3_to_16bit(&vec, b);
}

fm_vec4 vec4_decom_16bit(const uint16_t* v)
{
	fm_vec3 vec;
	fm_16bit_to_vec3(v, &vec);
	
	fm_vec4 res;
	res.x = vec.x * c_positionCompressionRange;
	res.y = vec.y * c_positionCompressionRange;
	res.z = vec.z * c_positionCompressionRange;
	res.w = 0.0f;
	
	return res;
}

float fa_decompress_key_time(const uint16_t time)
{
	return ((float)time) / 24.0f;
}

bool fi_is_same_key(const fi_temp_anim_curve_key_t* a, const fi_temp_anim_curve_key_t* b)
{
	return a->isLastCompMinus == b->isLastCompMinus
	&& a->isRotation == b->isRotation
	&& a->keyValues[0] == b->keyValues[0]
	&& a->keyValues[1] == b->keyValues[1]
	&& a->keyValues[2] == b->keyValues[2];
}

fi_result_t fi_import_anim_clip(const fi_depot_t* depot, const fi_import_anim_clip_ctx_t* ctx, fa_anim_clip_t** ppAnimClip, fc_alloc_callbacks_t* pAllocCallbacks)
{
	std::string absolutePath = depot->path;
	absolutePath += ctx->path;
	
	// todo: validate absolute path
	
	if(IsFileExtensionEqualTo(absolutePath.c_str(), ".fbx"))
	{
		std::string animName = absolutePath.substr(absolutePath.find_last_of("\\/") + 1);
		animName = animName.substr(0, animName.size() - strlen(".fbx"));
		
		ofbx::IScene* scene = OpenScene_FBX(absolutePath.c_str());
		FUR_ASSERT(scene);
		
		std::map<std::string, FBXBoneInfo*> bones;
		std::vector<const FBXBoneInfo*> sortedBones;
		
		fi_gather_anim_curves(scene, bones);
		fi_import_sort_bones(bones, sortedBones);
		
		const uint32_t numBones = (uint32_t)sortedBones.size();
		
		fi_temp_anim_clip_t tempClip;
		
		if(numBones > 0)
		{
			const uint16_t numCurves = numBones * 2;	// two channels: position & rotation
			tempClip.curves.reserve(numCurves);
			uint32_t numAllKeys = 0;
			
			float duration = 0.0f;
			
			// rotations
			for(uint32_t i_b=0; i_b<numBones; ++i_b)
			{
				const FBXBoneInfo* bone = sortedBones[i_b];
				
				tempClip.curves.push_back(fi_temp_anim_curve_t());
				fi_temp_anim_curve_t& tempCurve = tempClip.curves[tempClip.curves.size()-1];
				
				// gather all times
				std::vector<float> uniqueTimesSorted;
				for(uint32_t i=0; i<3; ++i)
				{
					const FBXAnimCurve& curve = bone->m_rotation[i];
					for(float t : curve.m_times)
					{
						uniqueTimesSorted.push_back(t);
					}
				}
				
				// sort times
				std::sort(uniqueTimesSorted.begin(), uniqueTimesSorted.end());
				
				// remove duplicates
				for(int32_t i=(int32_t)uniqueTimesSorted.size()-1; i>0; --i)
				{
					if(uniqueTimesSorted[i-1] == uniqueTimesSorted[i])
					{
						uniqueTimesSorted.erase(uniqueTimesSorted.begin()+i);
					}
				}
				
				const float curveDuration = uniqueTimesSorted.back();
				if(curveDuration > duration)
				{
					duration = curveDuration;
				}
				
				// todo: finding bone is linear search, so perhaps can be optimised
				const int16_t boneIdx = fa_rig_find_bone_idx(ctx->rig, SID(bone->m_name.c_str()));
				FUR_ASSERT(boneIdx != -1);
				
				const uint32_t numKeys = (uint32_t)uniqueTimesSorted.size();
				tempCurve.keys.resize(numKeys);
				tempCurve.index = boneIdx;
				
				for(uint32_t i=0; i<numKeys; ++i)
				{
					const float time = uniqueTimesSorted[i];
					
					float value[3] = {0.0f};
					fi_sample_fbx_anim_curve(bone->m_rotation, 3, value, time);
					
					tempCurve.keys[i].keyTime = (uint16_t)(time * 24.0f);
					
					fm_euler_angles angles;
					
					angles.yaw = FM_DEG_TO_RAD(-value[1]);
					angles.pitch = FM_DEG_TO_RAD(-value[0]);
					angles.roll = FM_DEG_TO_RAD(-value[2]);
					
					fm_quat quat;
					fm_quat_make_from_euler_angles_xyz(&angles, &quat);
					
					uint16_t* key = tempCurve.keys[i].keyValues;
					quat_fhm_16bit(quat, key);
					
					tempCurve.keys[i].isLastCompMinus = quat.r < 0.0f;
				}
				
				// if constant, then remove one key
				if(numKeys == 2 && fi_is_same_key(&tempCurve.keys[0], &tempCurve.keys[1]))
				{
					tempCurve.keys.pop_back();
				}
				
				numAllKeys += tempCurve.keys.size();
			}
			
			// positions
			for(uint32_t i_b=0; i_b<numBones; ++i_b)
			{
				const FBXBoneInfo* bone = sortedBones[i_b];
				
				fi_temp_anim_curve_t& tempCurve = tempClip.curves[i_b];
				
				// gather all times
				std::vector<float> uniqueTimesSorted;
				for(uint32_t i=0; i<3; ++i)
				{
					const FBXAnimCurve& curve = bone->m_translation[i];
					for(float t : curve.m_times)
					{
						uniqueTimesSorted.push_back(t);
					}
				}
				
				// sort times
				std::sort(uniqueTimesSorted.begin(), uniqueTimesSorted.end());
				
				// remove duplicates
				for(int32_t i=(int32_t)uniqueTimesSorted.size()-1; i>0; --i)
				{
					if(uniqueTimesSorted[i-1] == uniqueTimesSorted[i])
					{
						uniqueTimesSorted.erase(uniqueTimesSorted.begin()+i);
					}
				}
				
				const float curveDuration = uniqueTimesSorted.back();
				if(curveDuration > duration)
				{
					duration = curveDuration;
				}
				
				const uint32_t numKeys = (uint32_t)uniqueTimesSorted.size();
				tempCurve.posKeys.resize(numKeys);
				
				for(uint32_t i=0; i<numKeys; ++i)
				{
					const float time = uniqueTimesSorted[i];
					
					float value[3] = {0.0f};
					fi_sample_fbx_anim_curve(bone->m_translation, 3, value, time);
					
					tempCurve.posKeys[i].keyTime = (uint16_t)(time * 24.0f);
					
					fm_vec4 pos;
					
					pos.x = value[0];
					pos.y = value[1];
					pos.z = value[2];
					pos.w = 0.0f;
					
					//if(bone->m_name == "Bip001_Pelvis")
					//{
					//	printf("t=%1.2f   p={%1.3f, %1.3f, %1.3f}\n", time, pos.x, pos.y, pos.z);
					//}
					
					uint16_t* key = tempCurve.posKeys[i].keyValues;
					vec4_com_16bit(pos, key);
					
					tempCurve.posKeys[i].isLastCompMinus = false;
				}
				
				// if constant, then remove one key
				if(numKeys == 2 && fi_is_same_key(&tempCurve.posKeys[0], &tempCurve.posKeys[1]))
				{
					tempCurve.posKeys.pop_back();
				}
				
				numAllKeys += tempCurve.posKeys.size();
			}
			
			fa_anim_clip_t* animClip = (fa_anim_clip_t*)FUR_ALLOC_AND_ZERO(sizeof(fa_anim_clip_t), 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			animClip->curves = (fa_anim_curve_t*)FUR_ALLOC_AND_ZERO(sizeof(fa_anim_curve_t) * tempClip.curves.size(), 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			animClip->dataKeys = (fa_anim_curve_key_t*)FUR_ALLOC(sizeof(fa_anim_curve_key_t) * numAllKeys, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			
			animClip->numDataKeys = numAllKeys;
			animClip->numCurves = (uint16_t)tempClip.curves.size();
			animClip->duration = duration;
			animClip->name = SID_REG(animName.c_str());
			
			fa_anim_curve_key_t* curKey = animClip->dataKeys;
			
			for(uint32_t i_c=0; i_c<tempClip.curves.size(); ++i_c)
			{
				const fi_temp_anim_curve_t& tempCurve = tempClip.curves[i_c];
				fa_anim_curve_t* curve = &animClip->curves[i_c];
				
				curve->index = tempCurve.index;
				curve->numRotKeys = tempCurve.keys.size();
				curve->numPosKeys = tempCurve.posKeys.size();
				
				curve->rotKeys = curKey;
				for(uint32_t i=0; i<curve->numRotKeys; ++i)
				{
					const fi_temp_anim_curve_key_t& key = tempCurve.keys[i];
					
					FUR_ASSERT(curKey < animClip->dataKeys + animClip->numDataKeys);
					
					curKey->keyTime = key.keyTime;
					curKey->keyData[0] = key.keyValues[0];
					curKey->keyData[1] = key.keyValues[1];
					curKey->keyData[2] = key.keyValues[2];
					
					curKey += 1;
				}
				
				curve->posKeys = curKey;
				for(uint32_t i=0; i<curve->numPosKeys; ++i)
				{
					const fi_temp_anim_curve_key_t& key = tempCurve.posKeys[i];
					
					FUR_ASSERT(curKey < animClip->dataKeys + animClip->numDataKeys);
					
					curKey->keyTime = key.keyTime;
					curKey->keyData[0] = key.keyValues[0];
					curKey->keyData[1] = key.keyValues[1];
					curKey->keyData[2] = key.keyValues[2];
					
					curKey += 1;
				}
			}
			
			FUR_ASSERT(animClip->dataKeys + animClip->numDataKeys == curKey);
			
			*ppAnimClip = animClip;
		}
		
		scene->destroy();
	}
	
	if(ctx->extractRootMotion)
	{
		fa_anim_clip_t* animClip = *ppAnimClip;
		FUR_ASSERT(animClip);	// can't extract root motion on null anim
		
		const int16_t idxHips = 1;
		const int16_t idxLocoJoint = ctx->rig->idxLocoJoint;
		
		fa_anim_curve_t* curveHips = &animClip->curves[idxHips];
		const fa_anim_curve_t* curveLoco = &animClip->curves[idxLocoJoint];
		
		const uint32_t numPosKeys = curveHips->numPosKeys;
		const uint32_t numRotKeys = curveHips->numRotKeys;
		
		const uint32_t locoNumPosKeys = curveLoco->numPosKeys;
		const uint32_t locoNumRotKeys = curveLoco->numRotKeys;
		
		fm_vec4 posBegin = vec4_decom_16bit(curveLoco->posKeys[0].keyData);
		fm_vec4 posEnd = vec4_decom_16bit(curveLoco->posKeys[locoNumPosKeys-1].keyData);
		fm_quat rotBegin = quat_ihm_16bit(curveLoco->rotKeys[0].keyData);
		fm_quat rotEnd = quat_ihm_16bit(curveLoco->rotKeys[locoNumRotKeys-1].keyData);
		
		fm_vec4 posLoop = {};
		fm_vec4_sub(&posEnd, &posBegin, &posLoop);
		
		fm_quat_conj(&rotBegin);
		fm_quat rotLoop = {};
		fm_quat_mul(&rotBegin, &rotEnd, &rotLoop);
		
		animClip->motionDelta[0] = posLoop.x;
		animClip->motionDelta[1] = posLoop.y;
		animClip->motionDelta[2] = posLoop.z;
		animClip->motionDelta[3] = posLoop.w;
		
		animClip->motionDelta[4] = rotLoop.i;
		animClip->motionDelta[5] = rotLoop.j;
		animClip->motionDelta[6] = rotLoop.k;
		animClip->motionDelta[7] = rotLoop.r;
		
		for(uint32_t k=0; k<numPosKeys; ++k)
		{
			// zero out horizontal movement, leave only vertical movement
			fm_vec4 pos = vec4_decom_16bit(curveHips->posKeys[k].keyData);
			const float keyTime = fa_decompress_key_time(curveHips->posKeys[k].keyTime);
			
			fm_xform locoXform = {};
			fa_anim_curve_sample(curveLoco, keyTime, false, &locoXform);
			fm_vec4_sub(&pos, &locoXform.pos, &pos);
			
			vec4_com_16bit(pos, curveHips->posKeys[k].keyData);
		}
		
		for(uint32_t k=0; k<numRotKeys; ++k)
		{
			// zero out movement along vertical axis
			fm_quat rot = quat_ihm_16bit(curveHips->rotKeys[k].keyData);
			const float keyTime = fa_decompress_key_time(curveHips->posKeys[k].keyTime);
			
			fm_xform locoXform = {};
			fa_anim_curve_sample(curveLoco, keyTime, false, &locoXform);
			
			fm_quat_conj(&locoXform.rot);
			fm_quat_mul(&rot, &locoXform.rot, &rot);
			
			quat_fhm_16bit(rot, curveHips->rotKeys[k].keyData);
		}
	}
	
	return FI_RESULT_OK;
}

void fi_ofbx_matrix_to_fm_mat4(const ofbx::Matrix& src, fm_mat4& dst)
{
	dst.x.x = src.m[0];
	dst.x.y = src.m[1];
	dst.x.z = src.m[2];
	dst.x.w = src.m[3];
	
	dst.y.x = src.m[4];
	dst.y.y = src.m[5];
	dst.y.z = src.m[6];
	dst.y.w = src.m[7];
	
	dst.z.x = src.m[8];
	dst.z.y = src.m[9];
	dst.z.z = src.m[10];
	dst.z.w = src.m[11];
	
	dst.w.x = src.m[12];
	dst.w.y = src.m[13];
	dst.w.z = src.m[14];
	dst.w.w = src.m[15];
}

fi_result_t fi_import_mesh(const fi_depot_t* depot, const fi_import_mesh_ctx_t* ctx, fr_resource_mesh_t** ppMesh, fc_alloc_callbacks_t* pAllocCallbacks)
{
	std::string absolutePath = depot->path;
	absolutePath += ctx->path;
	
	// todo: validate absolute path
	
	if(IsFileExtensionEqualTo(absolutePath.c_str(), ".fbx"))
	{
		ofbx::IScene* scene = OpenScene_FBX(absolutePath.c_str());
		
		FUR_ASSERT(scene);
		
		const int32_t numMeshes = scene->getMeshCount();
		
		fr_resource_mesh_t* mesh = (fr_resource_mesh_t*)FUR_ALLOC_AND_ZERO(sizeof(fr_resource_mesh_t), 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		mesh->chunks = (fr_resource_mesh_chunk_t*)FUR_ALLOC_AND_ZERO(sizeof(fr_resource_mesh_chunk_t) * numMeshes, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		mesh->numChunks = numMeshes;
		
		for(int32_t i=0; i<numMeshes; ++i)
		{
			fr_resource_mesh_chunk_t* chunk = &mesh->chunks[i];
			
			const ofbx::Mesh* mesh = scene->getMesh(i);
			const ofbx::Geometry* geometry = mesh->getGeometry();
			
			const int32_t numVertices = geometry->getVertexCount();
			const ofbx::Vec3* vertices = geometry->getVertices();
			const ofbx::Vec3* normals = geometry->getNormals();
			const ofbx::Vec2* uvs = geometry->getUVs();
			
			const uint32_t strideFloats = 3 + 3 + 2;
			const uint32_t strideBytes = sizeof(float) * strideFloats;
			
			chunk->dataVertices = (float*)FUR_ALLOC_AND_ZERO(strideBytes * numVertices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			chunk->numVertices = numVertices;
			chunk->vertexStride = strideFloats;
			
			chunk->numIndices = numVertices;
			chunk->dataIndices = FUR_ALLOC_ARRAY(uint32_t, numVertices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			
			// create vertex stream
			float* itVertex = chunk->dataVertices;
			uint32_t* itIndex = chunk->dataIndices;
			
			for(int32_t iv=0; iv<numVertices; ++iv)
			{
				float* position = itVertex;
				float* normal = itVertex + 3;
				float* uv = itVertex + 3 + 3;
				
				position[0] = vertices[iv].x;
				position[1] = vertices[iv].y;
				position[2] = vertices[iv].z;
				
				//printf("vertex[%u] = {%1.2f, %1.2f, %1.2f}\n", iv, position[0], position[1], position[2]);
				
				normal[0] = normals[iv].x;
				normal[1] = normals[iv].y;
				normal[2] = normals[iv].z;
				
				if(uvs)
				{
					uv[0] = uvs[iv].x;
					uv[1] = 1.0f - uvs[iv].y;
				}
				else
				{
					uv[0] = 0.0f;
					uv[1] = 0.0f;
				}
				
				//printf("uv[%u] = {%1.2f, %1.2f}\n", iv, uv[0], uv[1]);
				
				*itIndex = (uint32_t)iv;
				
				itVertex += strideFloats;
				itIndex += 1;
			}
			
			FUR_ASSERT(itVertex == chunk->dataVertices + strideFloats * numVertices);
			FUR_ASSERT(itIndex == chunk->dataIndices + numVertices);
			
			// create skinning stream
			const ofbx::Skin* skin = geometry->getSkin();
			
			if(skin)
			{
				chunk->numBones = skin->getClusterCount();
				chunk->bindPose = FUR_ALLOC_ARRAY_AND_ZERO(fm_mat4, chunk->numBones, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				chunk->boneNameHashes = FUR_ALLOC_ARRAY_AND_ZERO(fc_string_hash_t, chunk->numBones, 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				chunk->dataSkinning = FUR_ALLOC_ARRAY_AND_ZERO(fr_resource_mesh_chunk_skin_t, numVertices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				
				// init all skin indices to -1
				for(uint32_t iv=0; iv<chunk->numVertices; ++iv)
				{
					for(uint32_t idxSkin=0; idxSkin<FUR_MAX_SKIN_INDICES_PER_VERTEX; ++idxSkin)
					{
						chunk->dataSkinning[iv].indices[idxSkin] = -1;
						chunk->dataSkinning[iv].weights[idxSkin] = 0.0f;
					}
				}
				
				// go through skin clusters (one cluster = one bone)
				const uint32_t numClusters = skin->getClusterCount();
				for(uint32_t ic=0; ic<numClusters; ++ic)
				{
					const uint32_t idxBone = ic;
					
					const ofbx::Cluster* cluster = skin->getCluster(ic);
					
					// fill in bone info
					ofbx::Matrix bindMatrix = cluster->getTransformMatrix();
					fm_mat4 bindMat4;
					fi_ofbx_matrix_to_fm_mat4(bindMatrix, bindMat4);
					const ofbx::Object* link = cluster->getLink();
					fc_string_hash_t boneName = fc_make_string_hash(link->name);
					chunk->boneNameHashes[ic] = boneName;
					chunk->bindPose[ic] = bindMat4;
					
					// fill in skin indices and weights for vertices coming from this cluster
					const uint32_t numSkinIndices = cluster->getIndicesCount();
					const int32_t* vertexIndices =  cluster->getIndices();
					const double* skinWeights =  cluster->getWeights();
					
					for(uint32_t iv=0; iv<numSkinIndices; ++iv)
					{
						const int32_t idxVertex = vertexIndices[iv];
						const float skinWeight = skinWeights[iv];
						
						fr_resource_mesh_chunk_skin_t* skin = &chunk->dataSkinning[idxVertex];
						
						uint32_t slotSkinIndices = 0;
						
						// find first free slot for index (in case this vertex is skinned to many bones)
						while(skin->indices[slotSkinIndices] != -1 && slotSkinIndices <= FUR_MAX_SKIN_INDICES_PER_VERTEX)
						{
							slotSkinIndices += 1;
						}
						
						FUR_ASSERT(slotSkinIndices <= FUR_MAX_SKIN_INDICES_PER_VERTEX);	// too many bones per vertex, max is FUR_MAX_SKIN_INDICES_PER_VERTEX
						
						skin->indices[slotSkinIndices] = (int16_t)idxBone;
						skin->weights[slotSkinIndices] = skinWeight;
					}
				}
				
				// normalize weights (sum of weights should be 1.0f)
				for(uint32_t is=0; is<chunk->numVertices; ++is)
				{
					fr_resource_mesh_chunk_skin_t* skin = &chunk->dataSkinning[is];
					const float totalWeight = skin->weights[0] + skin->weights[1] + skin->weights[2] + skin->weights[3];
					if(totalWeight > 0.0f)
					{
						const float factor = 1.0f / totalWeight;
						skin->weights[0] *= factor;
						skin->weights[1] *= factor;
						skin->weights[2] *= factor;
						skin->weights[3] *= factor;
					}
					
					const float totalWeightFinal = skin->weights[0] + skin->weights[1] + skin->weights[2] + skin->weights[3];
					FUR_ASSERT(0.999f < totalWeightFinal && totalWeightFinal < 1.001f);
				}
			}
		}
		
		*ppMesh = mesh;
		
		scene->destroy();
		
		return FI_RESULT_OK;
	}
	
	return FI_RESULT_UNKNOWN_FILE_FORMAT_IMPORT_ERROR;
}

void fr_mesh_release(fr_resource_mesh_t* pMesh, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fr_resource_mesh_chunk_t* chunks = pMesh->chunks;
	uint32_t numChunks = pMesh->numChunks;
	
	for(uint32_t i=0; i<numChunks; ++i)
	{
		FUR_FREE(chunks[i].dataIndices, pAllocCallbacks);
		FUR_FREE(chunks[i].dataVertices, pAllocCallbacks);
		
		if(chunks[i].bindPose)
			FUR_FREE(chunks[i].bindPose, pAllocCallbacks);
		
		if(chunks[i].dataSkinning)
			FUR_FREE(chunks[i].dataSkinning, pAllocCallbacks);
		
		if(chunks[i].boneNameHashes)
			FUR_FREE(chunks[i].boneNameHashes, pAllocCallbacks);
	}
	
	FUR_FREE(pMesh->chunks, pAllocCallbacks);
	FUR_FREE(pMesh, pAllocCallbacks);
}

void fc_path_concat(char* output, const char* folderAbsolute, const char* directoryRelative, const char* fileName, const char* fileExtension)
{
	const size_t folderLen = strlen(folderAbsolute);
	const size_t dirLen = strlen(directoryRelative);
	const size_t nameLen = strlen(fileName);
	const size_t extLen = strlen(fileExtension);
	
	memcpy(output, folderAbsolute, folderLen);
	memcpy(output + folderLen, directoryRelative, dirLen);
	memcpy(output + folderLen + dirLen, fileName, nameLen);
	memcpy(output + folderLen + dirLen + nameLen, fileExtension, extLen);
	memcpy(output + folderLen + dirLen + nameLen + extLen, "\0", 1);
}

typedef enum fr_mesh_version_t
{
	FR_MESH_VER_BASE = 0,
	FR_MESH_VER_LAST,
} fa_anim_clip_version_t;

void fr_resource_mesh_serialize(fc_serializer_t* pSerializer, fr_resource_mesh_t* mesh, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_SER_VERSION(FR_MESH_VER_LAST-1);
	
	FUR_SER_ADD(FR_MESH_VER_BASE, mesh->version);
	FUR_SER_ADD(FR_MESH_VER_BASE, mesh->numChunks);
	
	if(!pSerializer->isWriting)
	{
		FUR_ASSERT(mesh->chunks == NULL);
		mesh->chunks = FUR_ALLOC_ARRAY_AND_ZERO(fr_resource_mesh_chunk_t, mesh->numChunks, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
	}
	
	for(int32_t i=0; i<mesh->numChunks; ++i)
	{
		fr_resource_mesh_chunk_t* chunk = &mesh->chunks[i];
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->numBones);
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->numIndices);
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->numVertices);
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->vertexStride);
		
		if(!pSerializer->isWriting)
		{
			chunk->dataVertices = (float*)FUR_ALLOC_AND_ZERO(chunk->vertexStride * sizeof(float) * chunk->numVertices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			chunk->dataIndices = FUR_ALLOC_ARRAY_AND_ZERO(uint32_t, chunk->numIndices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		}
		
		// geometry data
		FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->dataVertices, chunk->vertexStride * sizeof(float) * chunk->numVertices);
		FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->dataIndices, sizeof(uint32_t) * chunk->numIndices);
		
		// skinning data
		if(chunk->numBones > 0)
		{
			if(!pSerializer->isWriting)
			{
				chunk->bindPose = FUR_ALLOC_ARRAY_AND_ZERO(fm_mat4, chunk->numBones, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				chunk->boneNameHashes = FUR_ALLOC_ARRAY_AND_ZERO(fc_string_hash_t, chunk->numBones, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				chunk->dataSkinning = FUR_ALLOC_ARRAY_AND_ZERO(fr_resource_mesh_chunk_skin_t, chunk->numVertices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			}
			
			FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->bindPose, sizeof(fm_mat4) * chunk->numBones);
			FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->boneNameHashes, sizeof(fc_string_hash_t) * chunk->numBones);
			FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->dataSkinning, sizeof(fr_resource_mesh_chunk_skin_t) * chunk->numVertices);
		}
	}
}
