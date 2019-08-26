import sys, getopt

class Settings:
    def __init__( self ):
        self.devmode = False
        self.only_update = False

def gather_command_line_arguments():
    settings = Settings()

    for opt in sys.argv:
        if opt == '-devmode':
            settings.devmode = True
        elif opt == '-update':
            settings.only_update = True

    return settings
