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
    QUuid m_link;           //!< the link to the next serie
    QString m_player;		//!< which player do we want
    QString m_arguments;	//!< arguments for the players
    
    static QMutex mutex;

    enum duration_state { NOT_STARTED, RUNNING, FINISHED };
    
    duration_state m_workerstate = NOT_STARTED;
    QString m_duration;
    
  private slots:
    /**
       called after finished playing
    */
    void afterFinished( int exitCode, QProcess::ExitStatus exitStatus );
    
    void getDurationWorker();
    
    
  public:
    /** Constructor
        @param nametoset The name we set
        @param dirtoset  The dir we set
        @param ongoingset Are we ongoing?
        @param maxtoset The number of episodes saved in the xml
    */
    Serie(QString nametoset, QDir dirtoset, bool ongoingtoset, int maxtoset, QUuid uuidtoset, QObject *parent = 0);
    
    ~Serie();
    
    QUuid getUuid() const { return m_uuid; }

    QString getPlayer() const { return m_player; };
    void setPlayer(const QString& player) { m_player = player; };

    QString getArguments() { return m_arguments; };
    void setArguments(const QString& arg) { m_arguments = arg; };
    
    QString getName() const { return m_name; };
    void setName(const QString& name) { m_name = name; };
    
    int getEpisodeNum() const { return m_episode; };
    void setEpisode(int episodetoset);
    
    /**
       @return returns the episodename of the episode which we play next
    */
    QString getNextEpisodeName();
    
    /**
       @return returns the all files we see in the directory sorted in the order as a string
    */
    QString getDirectoryListing();
    
    int getMax() const { if(m_ongoing) return m_max + 1; return m_max;};
    
    /**
       @return returns the directory
    */
    QString getDir() const { return m_dir.absolutePath(); };
    
    bool isFinished() const { return m_finished; };
    
    bool isOngoing() const { return m_ongoing; };
    
    /**
       @return returns the duration of next episode with the index of the serie
    */
    QString getDuration();
    
    /**
       set ongoing
    */
    void setOngoing(bool valuetoset);
    
    bool isDisabled() const { return m_disabled; };
    
    /**
       @return is the serie disabled because we have no dir
    */
    bool isDisabledNoDir() const { return m_disabled && !m_dir.exists(); };
    
    bool isReadyToPlay() { return !isDisabled() && !isFinished(); };
		
    /* which serie should we link/play after this one */
    QUuid getLink() const { return m_link; };
    void setLink(const QUuid& valuetoset) { m_link = valuetoset; };
                
    bool validLink() { return !m_link.isNull(); };
                
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
    
  public slots:
    /**
       play actual episode
    */
    void execActFile(QString additional_args = "");
		
  signals:
    /**
       we changed something with index int
    */
    void changed();
    
    /**
       playing started
    */
    void started();
    
    /**
       playing stopped
    */
    void stopped();    
};

/**
   debug operator
*/
QDebug operator<<(QDebug dbg, Serie &c);

typedef QSharedPointer<Serie> SeriePtr;

#endif
