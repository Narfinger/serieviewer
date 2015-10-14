#include "adddialogimpl.h"

#include <QtDebug>
#include <QMessageBox>
#include <QComboBox>
#include <QFileDialog>
#include <QList>
#include <QPair>
#include <QSharedPointer>

#include "defines.h"
#include "settings.h"

AddDialogImpl::AddDialogImpl(QWidget *parent): QDialog(parent) {
    ui.setupUi(this);
}

AddDialogImpl::AddDialogImpl(const QString& path, QWidget* parent) : QDialog(parent) {
  ui.setupUi(this);
  m_list = nullptr;
  construct(path);
}


AddDialogImpl::AddDialogImpl(QList<Serie*> *list, const QString& path, QWidget *parent) : QDialog(parent), m_list(list) {
  ui.setupUi(this);
  construct(path);
}
  
void AddDialogImpl::construct(const QString& path) {

    //fill replacelist
    replacelist << REPLACELIST;

    if(path == "")
        dirname = QFileDialog::getExistingDirectory(this, "Open Directory", Settings::Instance()->getLastPath() );
    else
        dirname = path;
	
    QList<QPair<QString,QString> > players = Settings::Instance()->getPlayerList();
	
    for(int i=0; i< players.size(); i++)
        ui.playerComboBox->addItem(players.at(i).first);
	
    if(dirname.isEmpty()==false)
    {
        bool media=false;
        QDir dir(dirname);	
        QStringList filters;
        //only support this file types at the moment
        filters << ALLFILTERS;
        dir.setNameFilters ( filters );
        QStringList entrys = dir.entryList(QDir::Files, QDir::Name);
		
        //get the whole error cases
        if(entrys.isEmpty())
        {
            dontshow = true;
            if(!media)
                QMessageBox::information(this, "No Media Files Found", "We found no Media Files in the Directory "+dirname);
        }
        else
        {
            ui.pathlabel->setText(dirname);

            Settings* instance = Settings::Instance();		
            Q_ASSERT(instance != 0);

            //fill replacelist with deleting media files
            QString extension;
            foreach(extension, filters)
            {
                QPair<QString, QString> pair(" "+ extension.replace("*.", "") , "");
                replacelist.push_back(pair);
            }

            //examplename for lineedit
            if(instance->getConvertNames())
            {
                QString name = entrys.at(0);
                QPair<QString, QString> pair;
                foreach(pair, replacelist)
                {
                    name.replace(pair.first, pair.second);
                }
                ui.nameline->setText(name);
            }
            else
                ui.nameline->setText(entrys.at(0));
        }


        // setup the linkbox
        /*ui.linkBox->addItem("No Link", -1);
        for(int i=0; i<m_list->size(); i++) {
	  qDebug() << i;
          Serie* serie = m_list->at(i);
          ui.linkBox->addItem(serie->getName(), i);
        }
        ui.linkBox->setCurrentIndex(0);*/
    }
    else
        dontshow = true;
}


AddDialogImpl::~AddDialogImpl()
{
}

bool AddDialogImpl::getShow()
{
    return !dontshow;
}

QString AddDialogImpl::getLastPath()
{
    QDir dir(ui.pathlabel->text());
    dir.cdUp();
    return dir.absolutePath();
	
}

SeriePtr AddDialogImpl::getResult(QObject *parent)
{
    QDir dir(ui.pathlabel->text());
    QStringList filters;
    filters << ALLFILTERS;
    dir.setNameFilters(filters);
    int max =  dir.entryList(QDir::Files,QDir::Name).size();
    SeriePtr tmp = QSharedPointer<Serie>(new Serie(ui.nameline->text(), dir, ui.ongoingCheckBox->isChecked(), max, QUuid::createUuid(), parent));
	
    Q_ASSERT(tmp!=0);
    tmp->setPlayer(ui.playerComboBox->currentText());
    tmp->setArguments(ui.argline->text());
    //if(ui.linkBox->currentIndex()==0)
        tmp->setLink(QUuid());
    //else {
    //    SeriePtr serie = QSharedPointer<Serie>(m_list->at(ui.linkBox->currentIndex() -1));
    //    tmp->setLink( serie->getUuid() );
    //}
    return tmp;
}
