Serieviewer
==========================
Gives order into the current series you watch. Just add the directories to the serieviewer and double click on them
or use the various other functions. Remembers where you stopped watching.

Needs FFMPEG/libav to parse the duration of the next episode.

Compile
==========================
You need to have Qt 4.2 for this to work.
To Compile (on Linux. Windows user have to work it out by themselves):
qmake
make

Optional Features
==========================
- dbus (see FEATURE in serieviewer.pro)
- ffmpeg (to show duration of the next episode)

Todo
==========================
- Refactpr the serie index mess
- Add whole directory with subdirectories should add all the subdirs as series with links (toggleable)
- cleanup privat and public of mwindowimpl.cpp
- remove duration row if disabled
- port to cmake
- fix icons
- reload on switch getduration?