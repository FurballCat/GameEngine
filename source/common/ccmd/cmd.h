/* Copyright (c) Furball Cat */

#pragma once

#include <inttypes.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

// context of the command (depot, etc.)
typedef struct fc_cmd_execute_ctx_t
{
	// path to FBX and other assets (input)
	const char* assetsPath;
	
	// path to generated engine files path (output)
	const char* enginePath;
} fc_cmd_execute_ctx_t;

typedef int (*fc_cmd_execute_fn_t)(int argc, char* argv[], fc_cmd_execute_ctx_t* ctx, fc_alloc_callbacks_t* pAllocCallbacks);

// command structure, include this as first property in every custom command structure (like derivative)
typedef struct fc_cmd_t
{
	// unique command name (like import)
	const char* name;
	
	// execute function implementation
	fc_cmd_execute_fn_t execute;
	
} fc_cmd_t;

const fc_cmd_t* fc_find_cmd(const char* name);
void fc_cmd_print_help(void);

const char* fc_find_cmd_arg(int argc, char* argv[], const char* name);
bool fc_find_cmd_flag(int argc, char* argv[], const char* name);

// example: -f "path" returns "path" once -f is found
#define CMD_ARG(_name) fc_find_cmd_arg(argc, argv, _name)
#define CMD_FLAG(_name) fc_find_cmd_flag(argc, argv, _name)

#define CMD_LOG_ERROR(...) fc_cmd_log_error(__VA_ARGS__)
#define CMD_LOG_WARNING(...) fc_cmd_log_warning(__VA_ARGS__)
#define CMD_LOG(...) fc_cmd_log(__VA_ARGS__)

void fc_cmd_log_error(const char *fmt, ...);
void fc_cmd_log_warning(const char *fmt, ...);
void fc_cmd_log(const char *fmt, ...);

// all available commands
int fc_cmd_import(int argc, char* argv[], fc_cmd_execute_ctx_t* ctx, fc_alloc_callbacks_t* pAllocCallbacks);

#ifdef __cplusplus
}
#endif // __cplusplus
