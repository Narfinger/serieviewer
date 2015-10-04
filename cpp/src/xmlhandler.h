#ifndef XMLHANDLER_H
#define XMLHANDLER_H

#include <QObject>
#include <QTextStream>
#include <QList>
#include <QString>
#include <QtXml/QtXml>
#include <QFile>

#include "serie.h"

class Serie;

/** 	@class XMLHandler
	Does the reading and writing of the xml file
*/
class XMLHandler : public QObject
{
    Q_OBJECT
  private:
    bool readSetup(QDomDocument &doc, QFile &file);
    void setVariantByType(QVariant* value, QString text);
    void readSettings(QDomDocument &doc);
    bool readSerie(QDomDocument &doc);
    
    void writeSettings(QDomDocument & doc, QDomElement &root);
    void writeSerie( QDomDocument &doc, QDomElement &series, QList<SeriePtr> &list);
    
    const QString getFileStringFromSettings(); /* produces the correct string from settings, still does need to be opened */
    
    bool filedidnotexist = false;
  public:
    XMLHandler( QObject *parent = 0);
    
    ~XMLHandler();
		
    /**
       read the xml file specified in Settings module
    */
    bool read();
    
    /**
       write this list to the xml file specified by Settings Module
    */
    bool write(QList<SeriePtr>  list);
    
  signals:
    /**
       we parsed and constructed this serie
    */
    void serieParsed(Serie*);
    
    /**
       no file there, we need to ask for a player
    */
    void askForPlayer();
    
};

#endif
