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

void XMLHandler::setVariantByType(QVariant* value, QString text)
{
   QVariant::Type type = value->type();
   switch(type)
   {
      case QMetaType::Bool:
      {
	 bool b = (text == "true");
	 value->setValue(b);
	 break;
      }
      case QMetaType::QUuid:
      {
	 QUuid id = QUuid(text);
	 value->setValue(id);
	 break;
      }
      case QMetaType::Int:
      {
	 int integer = text.toInt();
	 value->setValue(integer);
	 break;
      }
      case QMetaType::QString:
      {
	 value->setValue(text);
	 break;
      }
      default:
	 qDebug() << "Did not find the correct type.";

   }
}

void XMLHandler::readSettings(QDomDocument &doc)
{
    //parse for lastpath
    Settings* instance = Settings::Instance();		
    Q_ASSERT(instance != 0);
	
    //parse for player
    QDomNodeList playerlist = doc.elementsByTagName("player");
    if(playerlist.size()==0)
        qDebug() << "no player settings found";
    for(int i=0; i<playerlist.size(); i++)
    {
        QDomElement elem = playerlist.at(i).toElement();
        instance->addPlayer( elem.text(), elem.attribute("arguments") );
    }


    // do the various settings
    QMutableHashIterator<QString, QVariant> i(instance->m_settings);
    while(i.hasNext())
    {
       i.next();
       QString key = i.key();
       QDomNodeList elementlist = doc.elementsByTagName(key);
       QString elementtext = elementlist.at(0).toElement().text();
       setVariantByType(&(i.value()), elementtext);
    }

    //sort
    QDomNodeList sortlist = doc.elementsByTagName("sort");
    QString sorttext = sortlist.at(0).toElement().text();
    bool sortit = (sorttext=="true");
    instance->setSort(sortit);
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
	
   
    // write various settings
    QHashIterator<QString, QVariant> i(instance->m_settings);
    while(i.hasNext())
    {
       i.next();
       QString key = i.key();
       QVariant value = i.value();
       QDomElement element = doc.createElement(key);
       settings.appendChild(element);
       QDomText text = doc.createTextNode( value.toString());
       element.appendChild(text);
    }

    QList<QPair<QString, QString> > players = instance->getPlayerList();
	
    for(int i=0; i<players.size(); i++)
    {
        QDomElement playerelem = doc.createElement("player");
        settings.appendChild(playerelem);
        QDomText playertext = doc.createTextNode( players.at(i).first );
        playerelem.setAttribute("arguments", players.at(i).second  );
        playerelem.appendChild(playertext);
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
            serieelem.setAttribute("uuid", serie->getUuid().toString());
            serieelem.setAttribute("link", serie->getLink().toString());
			
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
