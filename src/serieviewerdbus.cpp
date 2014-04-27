#include "serieviewerdbus.h"

#include "defines.h"

SerieviewerAdaptor::SerieviewerAdaptor(QObject *parent)
    : QDBusAbstractAdaptor(parent)
{}

SerieviewerAdaptor::SerieviewerAdaptor(MWindowImpl* newmwindow) 
    : QDBusAbstractAdaptor(newmwindow), mwindow(newmwindow)
{}

SerieviewerAdaptor::~SerieviewerAdaptor()
{}

void SerieviewerAdaptor::playNextInSerie()
{
    qDebug() << "clicked";
    mwindow->on_playNextInSerieButton_clicked(true);
}

void SerieviewerAdaptor::playNext()
{
    mwindow->on_playNextButton_clicked();
}

void SerieviewerAdaptor::playIndex(int index)
{
    mwindow->playIndex(index);
}

void SerieviewerAdaptor::reload()
{
    qDebug() << "reloaded";
    mwindow->reload();
}

void SerieviewerAdaptor::random()
{
    mwindow->random();
}

void SerieviewerAdaptor::newRandom()
{
    mwindow->newRandom();
}

QString SerieviewerAdaptor::getCurrentName()
{
    return mwindow->getCurrentName();
}

QStringList SerieviewerAdaptor::getSerieNameList()
{
    QStringList* names = mwindow->getSerieListNames();
    return *names;
}
