/* Copyright (c) Furball Cat */
#include "cmd.h"
#include "stdio.h"

int main(int argc, char* argv[])
{
	if(argc == 2 && CMD_FLAG("-help"))
	{
		fcCmdPrintHelp();
		return 0;
	}
	
	if(argc < 2)
	{
		CMD_LOG_ERROR("missing command name argument at argv[1]");
		return 1;
	}
	
	const char* cmdName = argv[1];
	
	const FcCmd* cmd = fcFindCmd(cmdName);
	if(cmd == NULL)
	{
		CMD_LOG_ERROR("can't find command: %s", cmdName);
		return 2;
	}
	
	FcCmdExecuteCtx ctx = {0};
	ctx.assetsPath = "../../assets/";
	ctx.enginePath = "../../data/";
	
	const int err = cmd->execute(argc, argv, &ctx, NULL);
	if(err != 0)
	{
		CMD_LOG_ERROR("failed %s", cmdName);
	}
	
	return err;
}
