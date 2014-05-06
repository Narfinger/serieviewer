#ifndef SETTINGS_H
#define SETTINGS_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QList>
#include <QPair>
#include <QColor>
#include <QUuid>
#include <QVariant>
#include <QHash>
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
    QString m_player;		//!< player we play files with
    QString m_settingsfile; /* this is only for saving while the application is running
                               this gets not exported to xml but to QSettings */
    
    
    int find(QString searchstring);	//!< finds one element of the first string in the pair
    
  public:
    static Settings* Instance(); /* get an instance */

    QHash<QString, QVariant> m_settings;
    
    bool noPlayer(); /* is the player empty? */	
    bool playerListContains(QString player);
    void clearPlayers();
    QString getPlayerArguments(QString player);
    QString getPlayer();		
    void addPlayer(QString player, QString arguments);
    QList<QPair<QString,QString> > getPlayerList();
    
    QString getSettingsFilename()  { return m_settingsfile; }
    void setSettingsFilename(QString file) { m_settingsfile = file; }

    /* getter and setters for convenience */
    QString getLastPath()          { return m_settings.value("lastpath").toString(); }   
    void setLastPath(QString path) { m_settings.insert("lastpath", QVariant(path)); }
    
    bool getSort()                 { return m_settings.value("sort").toBool(); }
    void setSort(bool sort)        { m_settings.insert("sort", QVariant(sort)); }
               
    bool getOngoingSort()          { return m_settings.value("ongoingsort").toBool(); }
    void setOngoingSort(bool sort) { m_settings.insert("ongoingsort", QVariant(sort)); }
    
    bool getReloadSort()           { return m_settings.value("reloadsort").toBool(); }
    void setReloadSort(bool sort)  { m_settings.insert("reloadsort", QVariant(sort)); }
    
    bool getPriorSort()            { return m_settings.value("priorsort").toBool(); }
    void setPriorSort(bool sort)   { m_settings.insert("priorsort", QVariant(sort)); }
    
    bool getConvertNames()         { return m_settings.value("convertnames").toBool(); }
    void setConvertNames(bool conv){ m_settings.insert("convertnames", QVariant(conv)); }
    
    bool getScanMedia()            { return m_settings.value("scanmedia").toBool(); }
    void setScanMedia(bool scan)   { m_settings.insert("scanmedia", QVariant(scan)); }
    
    QUuid getLastPlayed()          { return m_settings.value("lastplayed").toUuid(); }
    void setLastPlayed(QUuid last) { m_settings.insert("lastplayed", QVariant(last)); }
    
    QUuid getLastAdded()           { return m_settings.value("lastadded").toUuid(); }
    void setLastAdded(QUuid last)  { m_settings.insert("lastadded", QVariant(last)); }
    
    int getOpacity()               { return m_settings.value("opacity").toInt(); }
    void setOpacity(int opa)     { m_settings.insert("opacity", QVariant(opa)); }


};	
#endif
