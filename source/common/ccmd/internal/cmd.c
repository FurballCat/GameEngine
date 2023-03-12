/* Copyright (c) Furball Cat */
#include "cmd.h"
#include "ccore/memory.h"
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

fc_cmd_t g_commands[] = {
	{"import", fc_cmd_import}
};

const fc_cmd_t* fc_find_cmd(const char* name)
{
	const i32 numCommands = FUR_ARRAY_SIZE(g_commands);
	for(i32 i=0; i<numCommands; ++i)
	{
		if(strcmp(g_commands[i].name, name) == 0)
			return &g_commands[i];
	}
	
	return NULL;
}

void fc_cmd_print_help(void)
{
	CMD_LOG("Cat's Commands (ccmd) is a Furball Cat's commandline tool for pipelines.\n"
			"Use it for importing, cooking, packaging, scanning, pretty much anything.\n"
			"Use ccmd [command-name] -help for info about particular command.\n"
			"Available commands:");
	
	const i32 numCommands = FUR_ARRAY_SIZE(g_commands);
	for(i32 i=0; i<numCommands; ++i)
	{
		CMD_LOG("  %s", g_commands[i].name);
	}
	
	CMD_LOG("");
}

const char* fc_find_cmd_arg(int argc, char* argv[], const char* name)
{
	for(i32 i=0; i<argc-1; ++i)
	{
		if(strcmp(name, argv[i]) == 0)
			return argv[i+1];
	}
	
	return NULL;
}

bool fc_find_cmd_flag(int argc, char* argv[], const char* name)
{
	for(i32 i=0; i<argc; ++i)
	{
		if(strcmp(name, argv[i]) == 0)
			return true;
	}
	
	return false;
}

void fc_cmd_log_error(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char txt[512] = {0};
	vsprintf(txt, fmt, args);
	va_end(args);
	
	printf("error: %s\n", txt);
}

void fc_cmd_log_warning(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char txt[512] = {0};
	vsprintf(txt, fmt, args);
	va_end(args);
	
	printf("warning: %s\n", txt);
}

void fc_cmd_log(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char txt[512] = {0};
	vsprintf(txt, fmt, args);
	va_end(args);
	
	printf("%s\n", txt);
}

