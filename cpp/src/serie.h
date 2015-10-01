#ifndef SERIE_H
#define SERIE_H

#include <QObject>
#include <QString>
#include <QDir>
#include <QStringList>
#include <QProcess>
#include <QUuid>
#include <QtDebug>
#include <QMutex>
#include <QPair>
#include <QSharedPointer>

/** @class Serie
Serie Class representing a serie
*/
class Serie : public QObject
{
    Q_OBJECT
  private:
    QUuid m_uuid;
    QString m_name;	//!< name of the serie
    QDir m_dir;		//!< directory of the serie
    bool m_finished;	//!< is the serie finished?
    bool m_ongoing;		//!< is the serie ongoing (new episodes get drop into directory)
    bool m_disabled;	//!< is this serie disabled (no episodes left or directory not avaible)
    int m_episode;		//!< actual episode we are in
    int m_max;		//!< number of episodes we see
    int m_index;		//!< which index we are in the list of MWindowImpl
    QUuid m_link;           //!< the link to the next serie
    QString m_player;		//!< which player do we want
    QString m_arguments;	//!< arguments for the players
    
    static QMutex mutex;
  public:
    /** Constructor
        @param nametoset The name we set
        @param dirtoset  The dir we set
        @param ongoingset Are we ongoing?
        @param maxtoset The number of episodes saved in the xml
    */
    Serie(QString nametoset, QDir dirtoset, bool ongoingtoset, int maxtoset, QUuid uuidtoset, QObject *parent = 0);
    
    ~Serie();
    
    QUuid getUuid()  { return m_uuid; }
    
    /**
       @return Returns the player
    */
    QString getPlayer();
    
    /**
       sets the playerid
    */
    void setPlayer(QString id);
    
    /**
       @return Returns the arguments for the player
    */
    QString getArguments();
		
    /**
       sets the arguments for the player
    */
    void setArguments(QString arg);
    
    /** 
        @return Returns the name of the serie
    */
    QString getName();
    
    void setName(QString name);
    
    /**
       @return Actual episode number
    */
    int getEpisodeNum();
    
    /**
       @param episodetoset Sets the episode number
    */
    void setEpisode(int episodetoset);
    
    /**
       @return returns the episodename of the episode which we play next
    */
    QString getNextEpisodeName();
    
    /**
       @return returns the all files we see in the directory sorted in the order as a string
    */
    QString getDirectoryListing();
    
    /**
       @return maximum value if we are ongoing, this is set to max+1 !
    */
    int getMax();
    
    /**
       @return returns the directory
    */
    QString getDir();
    
    /**
       @return is this serie finished?
    */
    bool isFinished();
    
    /**
       @return is this serie ongoing?
    */
    bool isOngoing();
    
    /**
       @return returns the duration of next episode with the index of the serie
    */
    QPair<QString, int> getDuration();
    
    /**
       set ongoing
    */
    void setOngoing(bool valuetoset);
    
    /**
       @return is this serie disabled? (this does not include isFinished, only directory not there and not enough files)
    */
    bool isDisabled();
    
    /**
       @return is the serie disabled because we have no dir
    */
    bool isDisabledNoDir();

    /**
       @return returns the index of the serie
    */
    int getIndex();
		
    /**
       @param indextoset sets the index
    */
    void setIndex(int indextoset);
		
    /* which serie should we link/play after this one */
    QUuid getLink()
    {
        return m_link;
    }

    void setLink(QUuid valuetoset)
    {
        m_link = valuetoset;
    }
                
    bool validLink()
    {
        return !m_link.isNull();
    }
                
    /**
       Values are "No Left" or "No Dir"
       @return returns the reason why this is disabled.
    */
    QString getReason();

    /**
       Rewinds a serie by one episode. If we were disabled we enable us again
    */
    void rewind();
		
    /**
       To compare serie
    */
    bool operator<(Serie& s1);
    		
  private slots:
    /**
       called after finished playing
    */
    void afterFinished( int exitCode, QProcess::ExitStatus exitStatus );
      
    
  public slots:
    /**
       play actual episode
    */
    void execActFile(QString additional_args = "");
		
  signals:
    /**
       we changed something with index int
    */
    void changed(int);
    
    /**
       playing started
    */
    void started();
    
    /**
       playing stopped
    */
    void stopped(int);
};

/**
   debug operator
*/
QDebug operator<<(QDebug dbg, Serie &c);

typedef QSharedPointer<Serie> SeriePtr;

#endif
