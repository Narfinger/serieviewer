#ifndef SETTINGS_H
#define SETTINGS_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QList>
#include <QPair>
#include <QColor>
#include <QUuid>

/** 	@class Settings
	Singleton class for serving settings
*/
class Settings : public QObject
{
    Q_OBJECT		
  private:
    Settings();
    static Settings* pinstance;
    
    QList<QPair<QString, QString> > m_players; //!< players
    QString m_lastpath;		//!< lastpath we opened a directory in
    QString m_player;		//!< player we play files with
    bool m_sort;			//!< do the user wants to sort it
    bool m_ongoingsort;		//!< wants the user also sort ongoing or ignore (make them smaller)
    bool m_priorsort;
    bool m_reloadsort;              //!< do we want to sort on reload
    bool m_convertnames;
    bool m_scanmedia;              //!< if we should use ffmpeg to scan media
    QUuid m_lastplayed;            //!< name of the serie last played
    QUuid m_lastadded;             //!< uuid of last serie we added
    
    QString m_settingsfile; /* this is only for saving while the application is running
                               this gets not exported to xml but to QSettings */
    
    
    int find(QString searchstring);	//!< finds one element of the first string in the pair
    
  public:
    static Settings* Instance(); /* get an instance */
    
    bool noPlayer(); /* is the player empty? */	
    bool playerListContains(QString player);
    void clearPlayers();
    QString getPlayerArguments(QString player);
    QString getPlayer();		
    void addPlayer(QString player, QString arguments);
    QList<QPair<QString,QString> > getPlayerList();
    
    /* 
       getter and setters follow
    */
    QString getLastPath()          { return m_lastplayed; }
    void setLastPath(QString path) { m_lastpath = path; }
    
    bool getSort()                 { return m_sort; }
    void setSort(bool sort)        { m_sort = sort; }
		
    bool getOngoingSort()          { return m_ongoingsort; }
    void setOngoingSort(bool sort) { m_ongoingsort = sort; }
    
    bool getReloadSort()           { return m_reloadsort; } 
    void setReloadSort(bool sort)  { m_reloadsort = sort; }
    
    bool getPriorSort()            { return m_priorsort;  }
    void setPriorSort(bool sort)   { m_priorsort = sort;  }
    
    bool getConvertNames()         { return m_convertnames; }
    void setConvertNames(bool conv) { m_convertnames = conv; }
    
    bool getScanMedia()            { return m_scanmedia; }
    void setScanMedia(bool scan)   { m_scanmedia = scan; }
    
    QUuid getLastPlayed()          { return m_lastplayed;}
    void setLastPlayed(QUuid last) { m_lastplayed = last;}
    
    QUuid getLastAdded()           { return m_lastadded; }
    void setLastAdded(QUuid last)  { m_lastadded = last; }
    
    QString getSettingsFilename()  { return m_settingsfile; }
    void setSettingsFilename(QString file) { m_settingsfile = file; }
};	
#endif
