#include <QtDebug>
#include <QMessageBox>
#include <QStringList>
#include <QTime>
extern "C"
{
#include <libavformat/avformat.h>
#include <libavutil/dict.h>
}

#include "serie.h"
#include "settings.h"
#include "defines.h"


Serie::Serie(QString nametoset, QDir dirtoset, bool ongoingtoset, int maxtoset, QUuid uuidtoset, QObject *parent)
    : QObject(parent),
      m_uuid(uuidtoset),
      m_name(nametoset), 
      m_dir(dirtoset),
      m_finished(false),
      m_ongoing(ongoingtoset),
      m_disabled(false),
      m_episode(1),
      m_index(-1),
      m_player(DEFAULTPLAYER)
{
    // these filter holds for every operation on m_dir, even later, so we need only to set it once
    QStringList filters;
    filters << ALLFILTERS;
    m_dir.setNameFilters(filters);
    
    if(m_ongoing && m_dir.exists())
        m_max = m_dir.entryList(QDir::Files,QDir::Name).size();
    else
        m_max = maxtoset;
    
    if(!m_dir.exists() || ( m_ongoing && m_max<m_episode))
        m_disabled = true;
    
    m_link = QUuid();
}

Serie::~Serie()
{}

QString Serie::getPlayer()
{
    return m_player;
}

void Serie::setPlayer(QString player)
{
    m_player = player;
}

QString Serie::getArguments()
{
    return m_arguments;
}

void Serie::setArguments(QString arg)
{
    m_arguments = arg;
}


QString Serie::getName()
{
    return m_name;
}

void Serie::setName(QString name)
{
    m_name = name;
}

int Serie::getEpisodeNum()
{
    return m_episode;
}

int Serie::getMax()
{
    //if ongoing we need to get an episode past the episode now
    if(m_ongoing)
        return m_max + 1;
    return m_max;
}

QString Serie::getDir()
{
    return m_dir.absolutePath();
}

bool Serie::isFinished()
{
    return m_finished;
}

bool Serie::isOngoing()
{
    return m_ongoing;
}

QPair<QString, int> Serie::getDuration()
{
    if(this->isDisabled() || this->isFinished())
        return qMakePair(QString(""), m_index);
    
    //this is multiple times and stupid
    QString nextepisodetoplay = m_dir.absoluteFilePath( m_dir.entryList(QDir::Files, QDir::Name).at(m_episode-1) );
    QByteArray ba = nextepisodetoplay.toLocal8Bit();
    const char *c_str = ba.data();
    
    AVFormatContext* pFormatCtx = avformat_alloc_context();
    int ret = avformat_open_input(&pFormatCtx, c_str, NULL, NULL);
    
    if(ret != 0)
        return qMakePair(QString("?"), m_index);
    if(avformat_find_stream_info(pFormatCtx, 0) < 0) {
        qDebug() << "problem with stream info";
	return qMakePair(QString("?"), m_index);
    }
    
    const qint64 duration = static_cast<qint64>(pFormatCtx->duration / AV_TIME_BASE);
    qint64 seconds = duration;
    qint64 minutes = duration / 60;
    seconds %= 60;
    qint64 hours = minutes / 60;
    minutes %= 60;
    
    avformat_free_context(pFormatCtx);
    
    
    QTime time(hours,minutes,seconds);
    if(hours==0)
        return qMakePair(time.toString("mm:ss"), m_index);
    else
        return qMakePair(time.toString("hh:mm:ss"), m_index);
    
}

void Serie::setOngoing(bool valuetoset)
{
    m_ongoing = valuetoset;
    
    m_max = m_dir.entryList(QDir::Files,QDir::Name).size();
    if(m_ongoing)
    {
        m_finished = false; // if we are ongoing, we are never finished (toggle ongoing could screw this up)
        if(m_max < m_episode)
            m_disabled = true;
    }
    else
    {
        if(m_max < m_episode)
            m_finished = true;
    }
}

bool Serie::isDisabled()
{
    return m_disabled;
}

bool Serie::isDisabledNoDir()
{
    return m_disabled && !m_dir.exists();
}

void Serie::setEpisode(int episodetoset)
{
    Q_ASSERT(episodetoset > 0);
    Q_ASSERT(episodetoset <= getMax());
    m_episode = episodetoset;
    if( m_ongoing && m_max < m_episode)
    {
        m_disabled = true;
    }
}

QString Serie::getNextEpisodeName()
{
    QStringList entries = m_dir.entryList(QDir::Files, QDir::Name);
    if(entries.count() > m_episode)
        return entries.at(m_episode-1);
    else return QString();
}


QString Serie::getDirectoryListing()
{
    QStringList list = m_dir.entryList(QDir::Files, QDir::Name);
    QString returnstring = "";
    foreach(QString string, list)
        returnstring += string +"\n";
    return returnstring;
}

bool Serie::operator<(Serie& s1)
{
    Settings* instance = Settings::Instance();
    Q_ASSERT(instance != 0);
    if(! instance->getOngoingSort() )		//sort ongoing normal
    {
        if(this->isOngoing() )
            return false;
        if(s1.isOngoing() )
            return true;
    }
    if( instance->getPriorSort() )			//priortised sorting
    {
        if( this->getEpisodeNum() >1  && s1.getEpisodeNum() == 1)
        {
            return true;				
        }
        else
            if( this->getEpisodeNum() == 1 && s1.getEpisodeNum() >1 )
                return false;
        //if both >1 compare and if both ==1 compare
    }
    
    double dthis = static_cast<double>(this->getEpisodeNum()) / static_cast<double>(this->getMax());
    double dm = static_cast<double>(s1.getEpisodeNum()) / static_cast<double>(s1.getMax());
    return (dthis > dm );
}

void Serie::execActFile(QString additional_args)
{
    if(m_finished==false)
    {
        Settings* instance = Settings::Instance();
        Q_ASSERT(instance != 0);
	
        if( m_disabled )
        {
            QMessageBox::critical(qobject_cast<QWidget*> (this->parent()), "Not here at the moment", 
                                  "You didn't have this episode at the moment. Please wait till you got it.");
            return;
        }
	
        emit started();
                
        QProcess* process = new QProcess(this);
        connect(process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(afterFinished(int,QProcess::ExitStatus)));
#ifdef Q_OS_UNIX
        //stdout & stderr of process are forwarded to serieviewer channel
        //on windows we don't need this (and we want to not popup useless cmds
        process->setProcessChannelMode(QProcess::ForwardedChannels);	
#endif		
        QStringList arguments;
	
        arguments << (instance->getPlayerArguments(m_player)).split(" ", QString::SkipEmptyParts);
        arguments << m_arguments.split(" ",QString::SkipEmptyParts);
        
        Q_ASSERT(m_episode>=1);
        if( !m_dir.exists() )
        {
            QString errorstring;
            errorstring.append("The directory doesn't seem to exist, perhaps I made an error somewhere?\n Check directory: ");
            errorstring.append(m_dir.absolutePath());
            QMessageBox::critical(qobject_cast<QWidget*> (this->parent()), "Directoy doesn't exist", errorstring);
            return;
        }
        
        
        QString episodetoplay = m_dir.absoluteFilePath( m_dir.entryList(QDir::Files, QDir::Name).at(m_episode-1) );
        if(additional_args != "")
            arguments << episodetoplay << " " << additional_args;
        else
            arguments << episodetoplay;
        qDebug() << "playing:" << episodetoplay;
        
        qDebug() << arguments;
		process->start(m_player, arguments);
    }
    else
        qDebug() << "This Serie is already finished!";
}

void Serie::afterFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
    Q_UNUSED(exitCode);
    
    if(exitStatus==QProcess::NormalExit)
    {
        m_episode++;
        if(m_episode > m_max)
        {
            if(m_ongoing)
                m_disabled = true;
            else
                m_finished = true;
        }
        emit changed(m_index);		
    }
    else
        qDebug() << "Wrong exit code, the player crashed?";
    emit stopped(m_index);        
}

int Serie::getIndex()
{
    Q_ASSERT(m_index >=0);
    return m_index;
}

void Serie::setIndex(int indextoset)
{
    m_index = indextoset;
}

QString Serie::getReason()
{
    Q_ASSERT(m_disabled==true || m_finished==true);
    if(m_dir.exists() && !m_finished)
        return QString("None Left");
    if(m_finished)
        return QString("Finished");
    return QString("No Dir");
}

void Serie::rewind()
{
    Q_ASSERT(m_dir.exists());

    m_finished=false;
    m_disabled=false;

    Q_ASSERT(m_episode!=1);
    Q_ASSERT(m_episode!=0);

    setEpisode(m_episode - 1);
    emit changed(m_index);
}

QDebug operator<<(QDebug dbg, Serie &c)
{
    if(&c == NULL)
    {
        dbg << "Serie is null";
        return dbg;
    }

    dbg.space() << "dir:" << c.getDir() << "name:" << c.getName() << "episode:" 
                << c.getEpisodeNum() << "of" << c.getMax() << "finished:" << c.isFinished();
    return dbg.space();
}
