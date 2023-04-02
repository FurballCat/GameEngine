/* Copyright (c) Furball Cat */

#pragma once

#include <inttypes.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

typedef struct FcAllocator FcAllocator;

// context of the command (depot, etc.)
typedef struct FcCmdExecuteCtx
{
	// path to FBX and other assets (input)
	const char* assetsPath;
	
	// path to generated engine files path (output)
	const char* enginePath;
} FcCmdExecuteCtx;

typedef int (*FcCmdExecuteFn)(int argc, char* argv[], FcCmdExecuteCtx* ctx, FcAllocator* pAllocCallbacks);

// command structure, include this as first property in every custom command structure (like derivative)
typedef struct FcCmd
{
	// unique command name (like import)
	const char* name;
	
	// execute function implementation
	FcCmdExecuteFn execute;
	
} FcCmd;

const FcCmd* fcFindCmd(const char* name);
void fcCmdPrintHelp(void);

const char* fcFindCmdArg(int argc, char* argv[], const char* name);
bool fcFindCmdFlag(int argc, char* argv[], const char* name);

// example: -f "path" returns "path" once -f is found
#define CMD_ARG(_name) fcFindCmdArg(argc, argv, _name)
#define CMD_FLAG(_name) fcFindCmdFlag(argc, argv, _name)

#define CMD_LOG_ERROR(...) fcCmdLogError(__VA_ARGS__)
#define CMD_LOG_WARNING(...) fcCmdLogWarning(__VA_ARGS__)
#define CMD_LOG(...) fcCmdLog(__VA_ARGS__)

void fcCmdLogError(const char *fmt, ...);
void fcCmdLogWarning(const char *fmt, ...);
void fcCmdLog(const char *fmt, ...);

// all available commands
int fcCmdImport(int argc, char* argv[], FcCmdExecuteCtx* ctx, FcAllocator* pAllocCallbacks);

#ifdef __cplusplus
}
#endif // __cplusplus
