#ifndef SERIEVIEWERDBUSINTERFACE_H
#define SERIEVIEWERDBUSINTERFACE_H

#include <QObject>
#include <QtDBus/QtDBus>
#include <QDBusAbstractAdaptor>
#include <QCoreApplication>

#include "mwindowimpl.h"
#include "defines.h"


/**
   @class DBus Class which just reroutes calls
*/
class SerieviewerAdaptor: public QDBusAbstractAdaptor
{
    Q_OBJECT
    Q_CLASSINFO("D-Bus Interface", "org.serieviewer")
        //qt now automatically knows what stuff it needs to export
        private:
    MWindowImpl* mwindow;
  public:
    SerieviewerAdaptor(QObject *parent);
    SerieviewerAdaptor(MWindowImpl* newmwindow);
    ~SerieviewerAdaptor();
    
    
  public Q_SLOTS:
        //this are just the methods calling the methods in MWindowImpl, check there for documentation
        void playNextInSerie();
        void playNext();
        void playIndex(int index);
        void reload();
        void random();
        void newRandom();
        QString getCurrentName();
        QStringList getSerieNameList();
};

#endif //class
