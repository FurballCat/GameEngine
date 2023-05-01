/* Copyright (c) Furball Cat */

#include "cgame/script.h"
#include "ccore/public.h"
#include "ccore/buffer.h"
#include "cmath/math3dimpl.h"
#include "world.h"

// ***** scripts core ***** //

typedef enum FcScriptSegmentId
{
	FS_SEG_ID_UNKNOWN = 0,
	FS_SEG_ID_C_FUNC_CALL = 1,	// indicates function call, read FcScriptOpHeader next
	FS_SEG_ID_C_FUNC_ARG = 2,	// indicates function argument, read FcVariant next
	FS_SEG_ID_LAMBDA = 3,	// indicates whole lambda (sequence of function calls), read FcScriptSegmentHeader next
	FS_SEG_ID_STATE = 4,
	FS_SEG_ID_STATE_ON_EVENT = 5,
	FS_SEG_ID_STATE_TRACK = 6,
	FS_SEG_ID_STATE_SCRIPT = 7,
} FcScriptSegmentId;

typedef struct FcGameObject FcGameObject;
typedef struct FcWorld FcWorld;
typedef struct FcCameraSystem FcCameraSystem;

// todo: move it somewhere else
FcVariant fcScriptNative_Animate(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_WaitAnimate(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_EquipItem(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_WaitSeconds(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_GetVariable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_SetVariable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_Go(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_GoWhen(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_CmpGt(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_CmpEq(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);

// camera script functions
FcVariant fcScriptNative_CameraEnable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);

typedef FcVariant(*FcScriptNativeFn)(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);

typedef struct FcScriptNativeFnEntry
{
	FcStringId name;
	FcScriptNativeFn func;
	u32 numArgs;
} FcScriptNativeFnEntry;

// increase when out of slots
#define FUR_MAX_SCRIPT_NATIVE_FUNCTIONS 256

FcScriptNativeFnEntry g_nativeFuncLookUp[FUR_MAX_SCRIPT_NATIVE_FUNCTIONS];

#define FUR_DEFINE_SCRIPT_NATIVE_FUNCTION(_syntax_in_script, _func_in_code, _num_of_args) \
{	\
	FUR_ASSERT(entryCount < FUR_MAX_SCRIPT_NATIVE_FUNCTIONS);	\
	FcScriptNativeFnEntry entry = { SID(_syntax_in_script), _func_in_code, _num_of_args };	\
	g_nativeFuncLookUp[entryCount] = entry;	\
	entryCount++;	\
}

FcResult fcScriptInit(const FcAllocator* allocator)
{
	i32 entryCount = 0;

	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("animate", fcScriptNative_Animate, 2);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("wait-animate", fcScriptNative_WaitAnimate, 2);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("equip-item", fcScriptNative_EquipItem, 2);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("wait-seconds", fcScriptNative_WaitSeconds, 1);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("get-variable", fcScriptNative_GetVariable, 2);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("set-variable", fcScriptNative_SetVariable, 3);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("go", fcScriptNative_Go, 1);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("go-when", fcScriptNative_GoWhen, 2);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("cmp-gt", fcScriptNative_CmpGt, 2);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("cmp-eq", fcScriptNative_CmpEq, 2);
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("camera-enable", fcScriptNative_CameraEnable, 3);

	// keep this as the last entry
	FUR_DEFINE_SCRIPT_NATIVE_FUNCTION("__null", NULL, 0);

	return FC_SUCCESS;
}

void fcScriptRelease(const FcAllocator* allocator)
{
	// empty for now
}

FcGameObject* fcScriptLookUpGameObject(FcScriptCtx* ctx, FcStringId name)
{
	if (name == SID("self"))
	{
		return ctx->self;
	}
	// 	else if(name == SID("player"))
	// 	{
	// 		return ctx->gameObjectRegister->pPlayer;
	// 	}
	else
	{
		FcGameObjectStorage* storage = &ctx->world->gameObjects;
		for (i32 i = 0; i < storage->num; ++i)
		{
			if (storage->ptr[i]->name == name)
			{
				return storage->ptr[i];
			}
		}
	}

	return NULL;
}

typedef struct FcScriptOp
{
	FcScriptNativeFn func;
	FcVariant* args;	// not owning this memory, it's just a pointer
	u32 numArgs;
} FcScriptOp;

typedef struct FcScriptData
{
	FcScriptOp* ops;	// sequence of operations
	FcVariant* allArgs; // owning memory to all args for all calls

	u32 numOps;
	u32 numAllArgs;
} FcScriptData;

typedef struct FcScriptState
{
	u32 idxOp;
} FcScriptState;

typedef enum FcScriptParsingStage
{
	SPS_NONE = 0,
	SPS_READING,
	SPS_END,
} FcScriptParsingStage;

/*

 Header
 Arg1
 Arg2
 Header
 Arg1
 ...

 **/

typedef struct FcScriptOpHeader
{
	FcStringId opCode;
	u32 flags;
	u32 numArgs;
} FcScriptOpHeader;

typedef struct FcScriptSegmentHeader
{
	u8 segmentId;		// type of segment data (what to expect next)
	u8 padding;
	u16 dataSize; 		// segment size in bytes
	FcStringId name;	// unique name in segment scope (to know what to look for)
} FcScriptSegmentHeader;

typedef struct FcScriptExecutionCtx
{
	FcBinaryBufferStream scriptBufferStream;
	FcScriptCtx* scriptCtx;
	u32 numOpsExecuted;
	bool endOflambda;
} FcScriptExecutionCtx;

FcVariant fcScriptExecuteStep(FcScriptExecutionCtx* ctx)
{
	FcBinaryBufferStream* stream = &ctx->scriptBufferStream;

	// peek what's next in buffer, but do not advance the stream
	u8 op_pre_flag = FS_SEG_ID_UNKNOWN;
	fcBinaryBufferStreamPeek(stream, sizeof(u8), &op_pre_flag);

	FcVariant result = { 0 };

	if (op_pre_flag == FS_SEG_ID_C_FUNC_CALL)
	{
		// advance the stream to skip the peeked part
		u32 bytesRead = fcBinaryBufferStreamRead(stream, sizeof(u8), &op_pre_flag);
		FUR_ASSERT(bytesRead);

		ctx->numOpsExecuted += 1;

		// read operation header
		FcScriptOpHeader opHeader = { 0 };
		bytesRead = fcBinaryBufferStreamRead(stream, sizeof(FcScriptOpHeader), &opHeader);
		FUR_ASSERT(bytesRead);

		// read arguments
		FUR_ASSERT(opHeader.numArgs < 20);
		FcVariant args[20];

		for (u32 i = 0; i < opHeader.numArgs; ++i)
		{
			args[i] = fcScriptExecuteStep(ctx);
		}

		// execute operation
		if (ctx->scriptCtx->numSkipOps == 0)
		{
			// find operation/function pointer - todo: implement simple cache
			const u32 numFuncs = FUR_ARRAY_SIZE(g_nativeFuncLookUp);
			u32 idxFunc = 0;
			for (idxFunc = 0; idxFunc < numFuncs; ++idxFunc)
			{
				if (opHeader.opCode == g_nativeFuncLookUp[idxFunc].name)
				{
					break;
				}
			}

			FUR_ASSERT(idxFunc < numFuncs);	// op code not found

			// call function
			result = g_nativeFuncLookUp[idxFunc].func(ctx->scriptCtx, opHeader.numArgs, args);
		}
		else if (ctx->scriptCtx->numSkipOps > 0)
		{
			ctx->scriptCtx->numSkipOps -= 1;
		}
	}
	else if (op_pre_flag == FS_SEG_ID_C_FUNC_ARG)
	{
		// advance the stream to skip the peeked part
		u32 bytesRead = fcBinaryBufferStreamRead(stream, sizeof(u8), &op_pre_flag);
		FUR_ASSERT(bytesRead);

		bytesRead = fcBinaryBufferStreamRead(stream, sizeof(FcVariant), &result);
		FUR_ASSERT(bytesRead);
	}
	else
	{
		ctx->endOflambda = true;
	}

	return result;
}

FcScriptSegmentId fcScriptReadSegmentHeader(FcScriptExecutionCtx* ctx, FcScriptSegmentHeader* header)
{
	// read segment header to know what's in
	u32 bytesRead = fcBinaryBufferStreamRead(&ctx->scriptBufferStream, sizeof(FcScriptSegmentHeader), header);
	FUR_ASSERT(bytesRead);

	return (FcScriptSegmentId)header->segmentId;
}

FcStringId fcScriptSkipToNextSegment(FcScriptExecutionCtx* ctx, FcScriptSegmentId segmentType)
{
	FcScriptSegmentHeader header = {0};
	bool found = false;

	while (ctx->scriptBufferStream.pos <= ctx->scriptBufferStream.endPos)
	{
		// read segment header to know what's in
		u32 bytesRead = fcBinaryBufferStreamRead(&ctx->scriptBufferStream, sizeof(FcScriptSegmentHeader), &header);
		FUR_ASSERT(bytesRead);

		// is it the segment we are looking for?
		FcScriptSegmentId thisSegmentType = (FcScriptSegmentId)header.segmentId;
		if (thisSegmentType == segmentType)
		{
			found = true;
			return header.name;
		}

		// no? ok, let's skip to the next segment
		bytesRead = fcBinaryBufferStreamRead(&ctx->scriptBufferStream, header.dataSize, NULL);
		FUR_ASSERT(bytesRead);
	}

	return 0;
}

bool fcScriptSkipUntilSegment(FcScriptExecutionCtx* ctx, FcScriptSegmentId segmentType, FcStringId segmentName)
{
	FcStringId name = 0;

	do
	{
		name = fcScriptSkipToNextSegment(ctx, segmentType);
		if (name == segmentName)
			return true;
	} while (name != 0);

	return false;
}

void fcScriptExecuteLambda(FcScriptExecutionCtx* ctx)
{
	do
	{
		fcScriptExecuteStep(ctx);

		if (ctx->scriptCtx->waitSeconds > 0.0f)
		{
			ctx->scriptCtx->numSkipOps = ctx->numOpsExecuted;
			break;
		}
		else if (ctx->scriptCtx->nextState != 0)
		{
			break;
		}
	} while (!ctx->endOflambda);
}

void fcScriptExecute(const FcBinaryBuffer* scriptBuffer, FcScriptCtx* scriptCtx)
{
	FcScriptExecutionCtx ctx = {0};
	ctx.scriptCtx = scriptCtx;
	fcBinaryBufferStreamInit(scriptBuffer, &ctx.scriptBufferStream);

	FcStringId segName = fcScriptSkipToNextSegment(&ctx, FS_SEG_ID_STATE_SCRIPT);
	FUR_ASSERT(segName);

	bool found = fcScriptSkipUntilSegment(&ctx, FS_SEG_ID_STATE, scriptCtx->state);
	FUR_ASSERT(found);

	found = fcScriptSkipUntilSegment(&ctx, FS_SEG_ID_STATE_ON_EVENT, scriptCtx->stateEventToCall);
	FUR_ASSERT(found);

	FcScriptSegmentHeader segmentHeader = {0};
	fcScriptReadSegmentHeader(&ctx, &segmentHeader);

	if (segmentHeader.segmentId == FS_SEG_ID_STATE_TRACK)
	{
		// todo
		fcScriptReadSegmentHeader(&ctx, &segmentHeader);
		FUR_ASSERT(segmentHeader.segmentId == FS_SEG_ID_LAMBDA);
	}

	// run lambda
	fcScriptExecuteLambda(&ctx);
}

void fcScriptUpdateLambda(FcScriptLambda* lambda, FcWorld* world, f32 dt)
{
	if (lambda->isActive == false)
		return;

	if (lambda->waitSeconds > 0.0f)
	{
		lambda->waitSeconds = FM_MAX(lambda->waitSeconds - dt, 0.0f);

		if (lambda->waitSeconds > 0.0f)
		{
			return;
		}
	}

	FcScriptCtx scriptCtx = {0};
	scriptCtx.world = world;
	scriptCtx.self = lambda->selfGameObject;
	scriptCtx.state = lambda->lambdaName;
	scriptCtx.stateEventToCall = lambda->eventName;
	scriptCtx.numSkipOps = lambda->numSkipOps;

	fcScriptExecute(lambda->scriptBlob, &scriptCtx);

	if (scriptCtx.waitSeconds > 0.0f)
	{
		lambda->waitSeconds = scriptCtx.waitSeconds;
		lambda->numSkipOps = scriptCtx.numSkipOps;
	}
	else if (scriptCtx.nextState != 0)
	{
		lambda->state = scriptCtx.nextState;
		lambda->lambdaName = scriptCtx.nextState;
		lambda->eventName = SID("start");
		lambda->numSkipOps = 0;
		lambda->waitSeconds = 0.0f;
	}
	else if (lambda->eventName == SID("start"))
	{
		lambda->eventName = SID("update");
		lambda->numSkipOps = 0;
		lambda->waitSeconds = 0.0f;
	}
}
