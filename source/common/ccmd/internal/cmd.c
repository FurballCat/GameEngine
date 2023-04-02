/* Copyright (c) Furball Cat */
#include "cmd.h"
#include "ccore/memory.h"
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

FcCmd g_commands[] = {
	{"import", fcCmdImport}
};

const FcCmd* fcFindCmd(const char* name)
{
	const i32 numCommands = FUR_ARRAY_SIZE(g_commands);
	for(i32 i=0; i<numCommands; ++i)
	{
		if(strcmp(g_commands[i].name, name) == 0)
			return &g_commands[i];
	}
	
	return NULL;
}

void fcCmdPrintHelp(void)
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

const char* fcFindCmdArg(int argc, char* argv[], const char* name)
{
	for(i32 i=0; i<argc-1; ++i)
	{
		if(strcmp(name, argv[i]) == 0)
			return argv[i+1];
	}
	
	return NULL;
}

bool fcFindCmdFlag(int argc, char* argv[], const char* name)
{
	for(i32 i=0; i<argc; ++i)
	{
		if(strcmp(name, argv[i]) == 0)
			return true;
	}
	
	return false;
}

void fcCmdLogError(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char txt[512] = {0};
	vsprintf(txt, fmt, args);
	va_end(args);
	
	printf("error: %s\n", txt);
}

void fcCmdLogWarning(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char txt[512] = {0};
	vsprintf(txt, fmt, args);
	va_end(args);
	
	printf("warning: %s\n", txt);
}

void fcCmdLog(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char txt[512] = {0};
	vsprintf(txt, fmt, args);
	va_end(args);
	
	printf("%s\n", txt);
}

