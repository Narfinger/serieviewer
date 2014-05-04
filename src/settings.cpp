#include "settings.h"

#include <QtDebug>
#include <QtAlgorithms>

#include "defines.h"

Settings* Settings::pinstance = 0;// initialize pointer


Settings* Settings::Instance () 
{
    if (pinstance == 0)  // is it the first call?
    {  
        pinstance = new Settings; // create sole instance
    }
    return pinstance; // address of sole instance
}

Settings::Settings()
   : m_lastpath(),m_player(DEFAULTPLAYER), m_sort(false), m_ongoingsort(true), 
     m_priorsort(false), m_reloadsort(false), m_convertnames(false),m_scanmedia(false), 
     m_opacity(DEFAULTOPACITY), m_settingsfile(DEFAULTSETTINGSFILE)
{ 
}


int Settings::find(QString searchstring)
{
    if(m_players.isEmpty())
        return -1;
    int i = 0;
    for(; i<m_players.size(); i++)
    {
        if(m_players.at(i).first == searchstring)
            return i;
    }
    if(i == m_players.size())
        return -1;
    Q_ASSERT_X(false, "QList<QPair<QString, QString> > m_players find", "We got at the end of the list and there is an error");
    return -2;
}

bool Settings::noPlayer()
{
    return m_players.isEmpty();
}

bool Settings::playerListContains(QString player)
{
    int i = find(player);
    if(0<=i && i< m_players.size())
        return true;
    else
        return false;
}

void Settings::clearPlayers()
{
    m_players.clear();
}

QString Settings::getPlayerArguments(QString player)
{
    int index = find(player);
    Q_ASSERT_X(index>=0, "settings", "You don't have any arguments about this player");
    QString arguments;
        
    // bugfix list for players
    if(player.contains("kaffeine"))
        arguments.append("--nofork ");
        
    arguments.append( m_players.at(index).second );
    return arguments;	
}

QString Settings::getPlayer()
{
    return m_players.at(0).first;
}

void Settings::addPlayer(QString player, QString arguments)
{
    int index = find(player);
    if( index == -1 )
        m_players.append( qMakePair(player, arguments) );
    else
    {
		
        if( m_players.at(index).first != arguments )
        {
            m_players[index] = qMakePair( player, arguments );
        }
    }
}
		
QList<QPair<QString,QString> > Settings::getPlayerList()
{
    return m_players;
}



