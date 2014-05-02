#include <QtDebug>
#include <QMessageBox>
#include <QString>
#include <QDir>
#include <QFile>
#include <QHash>
#include <QUuid>

#include "defines.h"
#include "xmlhandler.h"
#include "serie.h"
#include "settings.h"


XMLHandler::XMLHandler(QObject *parent)
    : QObject(parent)
{
}


XMLHandler::~XMLHandler()
{
}


bool XMLHandler::readSetup(QDomDocument &doc, QFile& file)
{		
    QString error;
    int errorline;
    int errorcolumn;
    //QDomDocument doc;
    if(file.size()==0)	//sometimes we write empty files
        return false;
    if(doc.setContent(&file, false, &error, &errorline, &errorcolumn)==false)
    {
        qDebug() << "Error while Parsing:\n" << error << "at Line:" << errorline << "at Char:" << errorcolumn;
        return false;
    }

    QDomNodeList versionlist = doc.elementsByTagName("version");
    if(versionlist.size()==0)
        qDebug() << "no version info found";
    if(versionlist.at(0).toElement().text() != PROGRAMMVERSION )
    {
        QString errorstring = "We found a config file version missmatch. This programm was compiled for version: ";
        errorstring.append(PROGRAMMVERSION);
        errorstring.append(" and your config file version is ");
        errorstring.append(versionlist.at(0).toElement().text());
        errorstring.append("\n Try to parse the files anyway.");
        qDebug() << errorstring;
    }
    return true;
}

void XMLHandler::readSettings(QDomDocument &doc)
{
    //parse for lastpath
    Settings* instance = Settings::Instance();		
    Q_ASSERT(instance != 0);
    QDomNodeList pathlist = doc.elementsByTagName("lastpath");
    if(pathlist.size()==0)
        qDebug() << "no lastpath settings found";
    instance->setLastPath( pathlist.at(0).toElement().text() );
		
    //parse for player
    QDomNodeList playerlist = doc.elementsByTagName("player");
    if(playerlist.size()==0)
        qDebug() << "no player settings found";
    for(int i=0; i<playerlist.size(); i++)
    {
        QDomElement elem = playerlist.at(i).toElement();
        instance->addPlayer( elem.text(), elem.attribute("arguments") );
    }

    //sort
    QDomNodeList sortlist = doc.elementsByTagName("sort");
    QString sorttext = sortlist.at(0).toElement().text();
    bool sortit = (sorttext=="true");
    instance->setSort(sortit);
	
    //sort ongoing
    QDomNodeList sortonglist = doc.elementsByTagName("sortongoing");
    QString sortongtext = sortonglist.at(0).toElement().text();		
    bool sortong = (sortongtext=="true");
    instance->setOngoingSort(sortong);
	
    //sort priority
    QDomNodeList sortpriolist = doc.elementsByTagName("sortprior");
    QString sortpriotext = sortpriolist.at(0).toElement().text();
    bool sortprio = (sortpriotext == "true");
    instance->setPriorSort(sortprio);

    //sort reloaded list
    QDomNodeList sortreloadlist = doc.elementsByTagName("sortreload");
    QString sortreloadtext = sortreloadlist.at(0).toElement().text();
    bool sortreload = (sortreloadtext == "true");
    instance->setReloadSort(sortreload);

    //convert names
    QDomNodeList convertnameslist = doc.elementsByTagName("convertnames");
    QString convertnamestext = convertnameslist.at(0).toElement().text();
    bool convert = (convertnamestext == "true");
    instance->setConvertNames(convert);

    //scan media
    QDomNodeList scanmedialist = doc.elementsByTagName("scanmedia");
    QString scanmediatext = scanmedialist.at(0).toElement().text();
    bool scanmedia = (scanmediatext == "true");
    instance->setScanMedia(scanmedia);

    //lastplayed
    QDomNodeList lastplayedlist = doc.elementsByTagName("lastplayed");
    QUuid lastplayedtext(lastplayedlist.at(0).toElement().text());
    instance->setLastPlayed(lastplayedtext);

    //last added
    QDomNodeList lastaddedlist = doc.elementsByTagName("lastadded");
    QUuid lastaddedtext(lastaddedlist.at(0).toElement().text());
    instance->setLastAdded(lastaddedtext);
}

bool XMLHandler::readSerie(QDomDocument &doc)
{
    QDomNodeList domlist = doc.elementsByTagName("serie");
    if(domlist.size()==0)
    {
        qDebug() << "no Nodes found";
        return false;
    }
		
    for(int i=0; i<domlist.size(); i++)
    {
        QDir dir;
			
        QDomElement elem = domlist.at(i).toElement();
        dir.setPath(elem.attribute("dir"));
			
        int episode = elem.attribute("episode").toInt();
                
        QString name = elem.text();
			
        QString player = elem.attribute("player");
			
        QString arguments = elem.attribute("arguments");
			
        int max = elem.attribute("max").toInt();

        bool ongoing = ( elem.attribute("ongoing") == "true" );

        QUuid uuid(elem.attribute("uuid"));

        Serie *tmp = new Serie(name, dir, ongoing, max, uuid);
        tmp->setEpisode(episode);
        tmp->setArguments(arguments);
        if(!player.isEmpty())
            tmp->setPlayer(player);

        QUuid link(elem.attribute("link"));
        tmp->setLink(link);
		                
        emit serieParsed(tmp);
    }
    return true;
}

bool XMLHandler::read()
{
    const QString filename = this->getFileStringFromSettings();
    QFile file(filename);
    if(!file.exists())
    {
        qDebug() << "We don't see a settings file at the path " <<  file.fileName();
        emit askForPlayer();
        return false;
    }
    
    file.open(QIODevice::ReadOnly);
    QTextStream out;
    out.setDevice(&file);
    
    QDomDocument doc;
    bool ready = readSetup(doc,file);
    if(ready)
    {
        readSettings(doc);
        ready = readSerie(doc);
    }
    out.flush();
    file.close();
    return ready;
}

//---------------------------------------------------
//--------------------WRITING------------------------
//---------------------------------------------------

void XMLHandler::writeSettings(QDomDocument &doc, QDomElement &root)
{
    Settings* instance = Settings::Instance();
    Q_ASSERT(instance != 0);
	
    QDomElement settings = doc.createElement("settings");
    root.appendChild(settings);

    QDomElement version = doc.createElement("version");
    settings.appendChild(version);
    QDomText versionnumber = doc.createTextNode(PROGRAMMVERSION);
    version.appendChild(versionnumber);
	
    QDomElement lastpathelem = doc.createElement("lastpath");
    settings.appendChild(lastpathelem);
    QDomText path = doc.createTextNode( instance->getLastPath() );
    lastpathelem.appendChild(path);
	
    QList<QPair<QString, QString> > players = instance->getPlayerList();
	
    for(int i=0; i<players.size(); i++)
    {
        QDomElement playerelem = doc.createElement("player");
        settings.appendChild(playerelem);
        QDomText playertext = doc.createTextNode( players.at(i).first );
        playerelem.setAttribute("arguments", players.at(i).second  );
        playerelem.appendChild(playertext);
    }
	
    {
        QDomElement sortelem = doc.createElement("sort");
        settings.appendChild(sortelem);	
        QDomText sorttext;
        if( instance->getSort())
            sorttext = doc.createTextNode("true");
        else
            sorttext = doc.createTextNode("false");
        sortelem.appendChild(sorttext);
    }
	
    {
        QDomElement sortongelem = doc.createElement("sortongoing");
        settings.appendChild(sortongelem);	
        QDomText sortongtext;
        if( instance->getOngoingSort())
            sortongtext = doc.createTextNode("true");
        else
            sortongtext = doc.createTextNode("false");
        sortongelem.appendChild(sortongtext);
    }
	
    {
        QDomElement sortprioelem = doc.createElement("sortprior");
        settings.appendChild(sortprioelem);
        QDomText sortpriotext;
        if( instance->getPriorSort() )
            sortpriotext = doc.createTextNode("true");
        else
            sortpriotext = doc.createTextNode("false");
        sortprioelem.appendChild(sortpriotext);
    }

    {
        QDomElement sortreloadelem = doc.createElement("sortreload");
        settings.appendChild(sortreloadelem);
        QDomText sortreloadtext;
        if( instance->getReloadSort() )
            sortreloadtext = doc.createTextNode("true");
        else
            sortreloadtext = doc.createTextNode("false");
        sortreloadelem.appendChild(sortreloadtext);
    }

    {
        QDomElement convertnameselem = doc.createElement("convertnames");
        settings.appendChild(convertnameselem);
        QDomText convertnamestext;
        if( instance->getConvertNames() )
            convertnamestext = doc.createTextNode("true");
        else
            convertnamestext = doc.createTextNode("false");
        convertnameselem.appendChild(convertnamestext);
    }

    {
        QDomElement scanmediaelem = doc.createElement("scanmedia");
        settings.appendChild(scanmediaelem);
        QDomText scanmediatext;
        if( instance->getScanMedia() )
            scanmediatext = doc.createTextNode("true");
        else
            scanmediatext = doc.createTextNode("false");
        scanmediaelem.appendChild(scanmediatext);
    }

    {
        QDomElement lastplayed_elem = doc.createElement("lastplayed");
        settings.appendChild(lastplayed_elem);
        QDomText lastplayed_text;
        lastplayed_text = doc.createTextNode(instance->getLastPlayed().toString());
        lastplayed_elem.appendChild(lastplayed_text);
    }

    {
        QDomElement lastadded_elem = doc.createElement("lastadded");
        settings.appendChild(lastadded_elem);
        QDomText lastadded_text;
        lastadded_text = doc.createTextNode(instance->getLastAdded().toString());
        lastadded_elem.appendChild(lastadded_text);
    }
}

void XMLHandler::writeSerie(QDomDocument &doc, QDomElement &series, QList<Serie*> &list)
{
    foreach(Serie* serie, list)
    {
        Q_ASSERT(serie!=0);
        if(serie->isFinished()==false)
        {
            QDomElement serieelem = doc.createElement("serie");
            serieelem.setAttribute("dir", serie->getDir());
            serieelem.setAttribute("episode", serie->getEpisodeNum());
            serieelem.setAttribute("player", serie->getPlayer());
            serieelem.setAttribute("arguments", serie->getArguments());
            serieelem.setAttribute("uuid", serie->getUuid());
            serieelem.setAttribute("link", serie->getLink());
			
            int max = serie->getMax();
            if(serie->isOngoing())
                max--;
            serieelem.setAttribute("max", max);
            QDomText name = doc.createTextNode(serie->getName());
			
            if(serie->isOngoing())
                serieelem.setAttribute("ongoing", "true");
            else
                serieelem.setAttribute("ongoing", "false");
			
			
            serieelem.appendChild(name);
            series.appendChild(serieelem);
        }
    }
}


bool XMLHandler::write(QList<Serie*>  list)
{
    const QString filename = this->getFileStringFromSettings();
    QFile file(filename);
    file.open(QIODevice::WriteOnly | QIODevice::Truncate);
    QTextStream out;
    out.setDevice(&file);
    
    QDomDocument doc("serieviewer");
    
    QDomElement root = doc.createElement("serieviewer");
    doc.appendChild(root);
    
    writeSettings(doc,root);
    
    //series
    QDomElement series = doc.createElement("series");
    root.appendChild(series);
    
    writeSerie(doc,series, list);
    
    out << doc.toString();
    
    out.flush();
    file.close();
    
    return true;
}


const QString XMLHandler::getFileStringFromSettings()
{
    Settings* settings = Settings::Instance();
    QString filename = settings->getSettingsFilename();
    if(filename.startsWith("~"))
       filename.replace(0,1, QDir::homePath());
    return filename;
}
