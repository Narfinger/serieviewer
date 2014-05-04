#ifndef DEFINES
#define DEFINES

/*
  standard values
*/


extern const char* APPNAME;
extern const char* DBUSNAME;
extern const char* DEFAULTPLAYER;
extern const char* PROGRAMMVERSION;
extern const char* DBUSARGS;

#ifndef ALLFILTERS
#define ALLFILTERS "*.avi" << "*.mkv" << "*.ogm" << "*.mpg" << "*.mpeg" << "*.rmvb" << "*.rm" << "*.mp4" << "*.flv" << "*.m4v" << "*.mov" << "*.xvid"
#endif

#ifndef REPLACELIST
#define REPLACELIST QPair<QString, QString>("_", " ") << QPair<QString, QString>(".", " ")
#endif


extern const int DEFAULTOPACITY;
extern const char* FIRMNAME;
extern const int NAMESIZE;
extern const int ACTSIZE;
extern const int EPISODESIZE;
extern const int ONGOINGSIZE;
extern const int WATCHBSIZE;
extern const int DURATIONSIZE;
extern const int LASTPLAYEDLABELSIZE;
extern const char* DEFAULTSETTINGSFILE;

extern const bool SAVESIZE;

#endif

