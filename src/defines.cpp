#include "defines.h"

const char* APPNAME = "serieviewer";
const char* DBUSNAME = "org.serieviewer";
const char* DEFAULTPLAYER = "/usr/bin/mplayer";
#ifdef Q_OS_WIN32
    const char* FILENAME = "serieviewer.xml";
    const char* BFILENAME = "serieviewer.xml.bak";
#else
    const char* FILENAME = ".serieviewer.xml";
    const char* BFILENAME = ".serieviewer.xml.bak";
#endif

const char* PROGRAMMVERSION = "1.0";
const char* DBUSARGS = "-f";
const float OPACITY= 0.85;
const char* FIRMNAME = "NoOrg";
const int NAMESIZE = 300;
const int ACTSIZE = 90;
const int EPISODESIZE = 90;
const int WATCHBSIZE = 80;
const int ONGOINGSIZE = 80;
const int DURATIONSIZE = 80;
const int LASTPLAYEDLABELSIZE = 30;
const char* DEFAULTSETTINGSFILE = "~/.serieviewer.xml";

const bool SAVESIZE = true;
