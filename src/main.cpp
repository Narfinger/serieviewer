#include <QApplication>
#include <QtGlobal>             //for qrand
#include <QMessageBox>          //for QT_REQUIRE_VERSION
#include <QString>
#include <time.h>

#ifdef FFMPEG_LIB
    extern "C"
    {
    #include <libavformat/avformat.h>
    #include <libavutil/dict.h>
    #include <pthread.h>
    }
#endif

#include "defines.h"
#include "serieviewerdbus.h"
#include "mwindowimpl.h"




#ifdef FFMPEG_LIB
        //ffmpeg needs locks, this does all the mutex stuff
        int lock_call_back(void ** mutex, enum AVLockOp op) {
            switch(op) {
            case AV_LOCK_CREATE:
            *mutex = (pthread_mutex_t *) malloc(sizeof(pthread_mutex_t));
                pthread_mutex_init((pthread_mutex_t *)(*mutex), NULL);
                break;
            case AV_LOCK_OBTAIN:
                pthread_mutex_lock((pthread_mutex_t *)(*mutex));
                break;
            case AV_LOCK_RELEASE:
                pthread_mutex_unlock((pthread_mutex_t *)(*mutex));
                break;
            case AV_LOCK_DESTROY:
                pthread_mutex_destroy((pthread_mutex_t *)(*mutex));
                free(*mutex);
                break;
            }
        return 0;
        }
#endif

int main(int argc, char *argv[])
{
    QT_REQUIRE_VERSION(argc, argv, "4.2");
    
    #ifdef FFMPEG_LIB
        //initialize ffmpeg
        av_register_all();
        av_log_set_level(AV_LOG_ERROR); // only show errors
        av_lockmgr_register(&lock_call_back);
    #endif
    
    const uint seed = static_cast<uint>(time(NULL));
    qsrand(seed);  //quuid could need random stuff
    QApplication app(argc, argv);
    MWindowImpl mv;

#ifdef SERIEVIEWER_DBUS
    SerieviewerAdaptor* sva = new SerieviewerAdaptor(&mv);
    Q_UNUSED(sva);
    QDBusConnection connection = QDBusConnection::sessionBus();
    connection.registerObject("/Serieviewer", &mv);
    connection.registerService(DBUSNAME);
#endif

    mv.show();
    if(argc==2)
    {
        //add the serie
        mv.addGuiSerie(argv[1]);
    }
    return app.exec();
}
