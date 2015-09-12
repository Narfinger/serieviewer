#include "mwindowimpl.h"

#include <cstdlib>
#include <ctime>
#include <QtDebug>
#include <QString>
#include <QFileDialog>
#include <QMessageBox>
#include <QPushButton>
#include <QCheckBox>
#include <QSpinBox>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QSignalMapper>
#include <QObject>
#include <QInputDialog>
#include <QtAlgorithms>
#include <QSettings>
#include <QVariant>
#include <QFile>
#include <QIcon>
#include <QAction>
#include <QShortcut>
#include <Qt>
#include <QtConcurrent/QtConcurrent>

#include "defines.h"
#include "adddialogimpl.h"
#include "linkdialogimpl.h"
#include "settingsdialogimpl.h"
#include "setplayerdialogimpl.h"
#include "infodialogimpl.h"
#include "serie.h"
#include "xmlhandler.h"
#include "settings.h"
#include "seriemodel.h"

MWindowImpl::MWindowImpl(QWidget *parent)
    : QMainWindow(parent)
{
    ui.setupUi(this);
    
    {
      SerieModel* s = new SerieModel();
      ui.tableView->setModel(s);
    }
	
    //signalmapper = new QSignalMapper(this);
    spinmapper = new QSignalMapper(this);
    connect(spinmapper, SIGNAL(mapped(int)), this, SLOT(episodeChangedInGui(int))); //we need this here, else we will emit more and more signals

    xmlhandler = new XMLHandler( this);
	

    //load settings
    QSettings settings(FIRMNAME, APPNAME);
    {
        settings.beginGroup("mainwindow");
        ui.tableWidget->setColumnWidth(0, settings.value("col0", NAMESIZE).toInt());
        ui.tableWidget->setColumnWidth(1, settings.value("col1", ACTSIZE).toInt());
        ui.tableWidget->setColumnWidth(2, settings.value("col2", EPISODESIZE).toInt());
        ui.tableWidget->setColumnWidth(3, settings.value("col3", ONGOINGSIZE).toInt());
        ui.tableWidget->setColumnWidth(4, settings.value("col4", WATCHBSIZE).toInt());
        ui.tableWidget->setColumnWidth(5, settings.value("col5", DURATIONSIZE).toInt());
			
        QPoint pos = settings.value("pos", QPoint(200, 200)).toPoint();
        QSize size = settings.value("size", QSize(650, 480)).toSize();
        resize(size);
        move(pos);
        settings.endGroup();
    }
    QString settingsfile = settings.value("settingsfile").toString();
    if(!settingsfile.isNull())
       Settings::Instance()->setSettingsFilename(settingsfile);


    QTableWidgetItem* name = new QTableWidgetItem("Series Name");
    QTableWidgetItem* episode = new QTableWidgetItem("next Episode");
    QTableWidgetItem* max = new QTableWidgetItem("episodes");
    QTableWidgetItem* ongoing = new QTableWidgetItem("Ongoing");
    QTableWidgetItem* watch = new QTableWidgetItem("Watch it");
    QTableWidgetItem* duration = new QTableWidgetItem("Duration");


    ui.tableWidget->setHorizontalHeaderItem(0,name);
    ui.tableWidget->setHorizontalHeaderItem(1,episode);
    ui.tableWidget->setHorizontalHeaderItem(2,max);
    ui.tableWidget->setHorizontalHeaderItem(3,ongoing);
    ui.tableWidget->setHorizontalHeaderItem(4,watch);
    ui.tableWidget->setHorizontalHeaderItem(5,duration);

    connect(xmlhandler,SIGNAL(serieParsed(Serie*)), this,SLOT(addToList(Serie*)));
    connect(xmlhandler,SIGNAL(askForPlayer()), this, SLOT(askForSettings()));
    connect(ui.tableWidget, SIGNAL(currentCellChanged(int,int,int,int)), this, SLOT(cellFocusChanged(int,int,int,int)));
    connect(ui.tableWidget, SIGNAL(cellDoubleClicked(int,int)), this, SLOT(cellDoubleClicked(int,int)));
	  
    changed = false;
	
    buildmenus();
	
    if(!xmlhandler->read())
    {
        qDebug() << "Couldn't read any data. If you don't change anything you might recover the file.";
        // QFile backup(BFILENAME);
        // QFile file(FILENAME);
        // if(file.exists())
        // {
        //     qDebug() << "try to copy";
        //     if(backup.exists())
        //         backup.remove();
        //     file.copy(BFILENAME);
        // }
        //player = DEFAULTPLAYER;
    }

    ui.numberLabel->setText(QString::number(list.size()));

    //search ui
    connect(ui.searchEdit, SIGNAL(textChanged(QString)), this, SLOT(on_searchEdit_textChanged(QString)));
    ui.clearButton->setEnabled(true);
    ui.searchEdit->setEnabled(true);
    ui.filterLabel->setEnabled(true);
    //setup the unmarked brush, this is kind of ugly but i don't know how to do better
            
    // setup the context menu for items in tablewidget
    ui.tableWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(ui.tableWidget, SIGNAL(customContextMenuRequested(QPoint)), this, SLOT(rightClickPopup(QPoint)));


    {
        currentlyplaying = 0;
	   
        lastplayed = 0;
        QUuid uuidplayed = Settings::Instance()->getLastPlayed();
        lastplayed = getSerieForUuid(uuidplayed);
        if(lastplayed!=0)
            setLastPlayedName();
	      
        lastadded = 0;
        QUuid uuidadded = Settings::Instance()->getLastAdded();
        lastadded = getSerieForUuid(uuidadded);
    }
}


MWindowImpl::~MWindowImpl()
{
    Q_ASSERT(xmlhandler!=0);

    QSettings settings(FIRMNAME, APPNAME);
    if(changed)
    {
        saveXML(true);
        settings.setValue("settingsfile", Settings::Instance()->getSettingsFilename());
    }

    //save settings
    if(SAVESIZE)
    {
        //at the moment we save this into registry on windows, should we do that?
        QSettings settings(FIRMNAME, APPNAME);
        settings.beginGroup("mainwindow");
        settings.setValue("pos", pos());
        settings.setValue("size", size());
        settings.setValue("col0", ui.tableWidget->columnWidth(0));
        settings.setValue("col1", ui.tableWidget->columnWidth(1));
        settings.setValue("col2", ui.tableWidget->columnWidth(2));
        settings.setValue("col3", ui.tableWidget->columnWidth(3));
        settings.setValue("col4", ui.tableWidget->columnWidth(4));
        settings.setValue("col5", ui.tableWidget->columnWidth(5));
        settings.endGroup();

    }
}

void MWindowImpl::buildmenus()
{
    connect(ui.actionAdd_recursive, SIGNAL(triggered()), this, SLOT(addSerieRecursive()));
    connect(ui.actionSettings, SIGNAL(triggered()), this, SLOT(askForSettings()));
    connect(ui.actionRefresh, SIGNAL(triggered()), this, SLOT(reload()));
    connect(ui.actionClean_up_Series, SIGNAL(triggered()), this, SLOT(cleanupSeries()));
    connect(ui.actionQuit_without_saving, SIGNAL(triggered()), this, SLOT(quitWithoutSaving()));
    connect(ui.actionQuit, SIGNAL(triggered()), qApp, SLOT(quit()));
    
    connect(ui.actionShow_Series_Info, SIGNAL(triggered()), this, SLOT(showSerieInfo()));
    connect(ui.actionEnable_Ongoing, SIGNAL(triggered()), this, SLOT(setOngoing()));
    connect(ui.actionDisable_Ongoing, SIGNAL(triggered()), this, SLOT(setNotOngoing()));
    connect(ui.actionRewind_by_one, SIGNAL(triggered()), this, SLOT(rewind()));
    connect(ui.actionSet_the_link, SIGNAL(triggered()), this, SLOT(setLink()));
    connect(ui.actionSet_player, SIGNAL(triggered()), this, SLOT(setPlayer()));
    
    connect(ui.actionRandom, SIGNAL(triggered()), this, SLOT(random()));
    connect(ui.actionNewRandom, SIGNAL(triggered()), this, SLOT(newRandom()));
    connect(ui.actionPlay_next_in_Series, SIGNAL(triggered()), this, SLOT(on_playNextInSerieButton_clicked()));
    connect(ui.actionPlay_last_added, SIGNAL(triggered()), this, SLOT(on_playLastAddedButton_clicked()));
    connect(ui.actionPlay_next, SIGNAL(triggered()), this, SLOT(on_playNextButton_clicked()));
    connect(ui.actionUndo_last_play, SIGNAL(triggered()), this, SLOT(on_undoButton_clicked()));
    
    connect(ui.actionAbout_Qt, SIGNAL(triggered()), qApp, SLOT(aboutQt()));
    connect(ui.actionAbout, SIGNAL(triggered()), this, SLOT(about()));
}

void MWindowImpl::saveXML(bool sort)
{
    Q_ASSERT(xmlhandler!=0);
    Settings* instance = Settings::Instance();
    if(lastplayed!=0)
    {
        if(lastplayed->isFinished() && lastplayed->validLink())
        {
            instance->setLastPlayed( lastplayed->getLink() );
        }
        else
        {
            instance->setLastPlayed(lastplayed->getUuid());
        }
    }

    if(lastadded!=0)
    {
        if(lastadded->validLink())
            instance->setLastAdded (lastadded->getLink() );
    }
    
    if( sort && instance->getSort() )
    {
        qSort(list.begin(), list.end(), compareSerieP);
    }
    if(!xmlhandler->write(list))
    {
        QMessageBox::critical(this,"Error in save", "Can't save data!\n All changes will were not saved");
    }

}

QString MWindowImpl::getCurrentName()
{
    if(currentlyplaying==0)
        return QString();
    else
        return currentlyplaying->getName();
}

QStringList* MWindowImpl::getSerieListNames()
{
    QStringList* names = new QStringList();
    foreach(Serie* s, list)
    {
        names->append(s->getName());
    }
    return names;

}

void MWindowImpl::playIndex(int index)
{
    if(list.count() < index)
        return;
   
    Serie* serie = list.at(index);
    qDebug() << "Playing " + serie->getName() + " from dbus";
    if(serie->isDisabled() || serie->isFinished())
        return;
    else
        serie->execActFile(DBUSARGS);
}

void MWindowImpl::reload()
{
    ui.tableWidget->blockSignals(true); //need this because cellFocusChange

    // save lastplayed
    QUuid lastplayeduuid = QUuid();
    if(lastplayed!=0)
        lastplayeduuid = lastplayed->getUuid();

    saveXML(Settings::Instance()->getReloadSort());
    foreach(Serie* tmp, list)
        tmp->deleteLater();
    list.clear();
    ui.tableWidget->clearContents();
    while(ui.tableWidget->rowCount() !=0 )
        ui.tableWidget->removeRow(ui.tableWidget->rowCount() -1 );
    
    if(!xmlhandler->read())
    {
        qDebug() << "Couldn't read any data.";
    }
    ui.numberLabel->setText(QString::number(list.size()));

    lastplayed = getSerieForUuid(lastplayeduuid);
    setLastPlayedName();
    
    qDebug() << "reloaded";

    ui.tableWidget->blockSignals(false);

    ui.searchEdit->clear();
}


bool MWindowImpl::compareSerieP(Serie* s1, Serie* s2)
{
    return ((*s1) < (*s2) );
}

Serie* MWindowImpl::getSerieForUuid(QUuid uuid)
{
    if(!uuid.isNull())
    {
        foreach(Serie* serie, list)
        {
            if(uuid==serie->getUuid())
            {
                return serie;
            }
        }
    }
    return 0;
}

void MWindowImpl::about()
{
    QString message;
    message.append("This programm comes with no warranty.\n\n You are using Version: ");
    message.append(PROGRAMMVERSION);
    QMessageBox::about ( this, QString("Versioninfo"), message );
}

void MWindowImpl::addSerieRecursive()
{
    const int oldsize = list.size();
    QString dirstring = QFileDialog::getExistingDirectory(this, "Open Dir for Recursive", Settings::Instance()->getLastPath(), 
							  QFileDialog::ShowDirsOnly);
    if(!dirstring.isEmpty())
    {
        QDir dir(dirstring);
        for(const QFileInfo& d : dir.entryInfoList(QDir::Dirs | QDir::NoDotAndDotDot))
	{
	    if(d.isDir())
		addGuiSerie(d.absoluteFilePath());
	}
	
	//set lastpath we need to check if somebody added series, this is not nice but i don't know any other way really
	//I know this is a hack and ideally addGuiSerie should just return a bool
	if(oldsize != list.size())
	{
	    //setup links
	    for(int i = oldsize; i<list.size()-1; i++)
	    {
		Serie* s = list.at(i);
		Serie* next = list.at(i+1);
		s->setLink(next->getUuid());
		qDebug() << "linking:" << s->getName() << "to" << next->getName();
	    }
	    dir.cdUp();
	    Settings::Instance()->setLastPath(dir.absolutePath());
	}
    }
}

void MWindowImpl::addGuiSerie(QString path)
{
    AddDialogImpl* dialog=new AddDialogImpl(&list, path, this);
    if(dialog->getShow())
    {
        dialog->exec();
        if(dialog->result()==QDialog::Accepted)
        {
            Settings::Instance()->setLastPath( dialog->getLastPath() );
            Serie* result = dialog->getResult(this);
            addToList(result);
            lastadded = result;
                        
        }
        changed=true;
    }
    delete dialog;
    ui.numberLabel->setText(QString::number(list.size()));
}

void MWindowImpl::on_deleteButton_clicked()
{	
    QTableWidgetItem* item = ui.tableWidget->currentItem();
    if(item!=0)
    {
        int row = item->row();
        ui.tableWidget->removeRow(row);
        //delete item; //it seems we should not delete this, don't know why at the moment
        //Serie* serie = list.at(row);
        //delete serie;
        list.removeAt(row);
        reload();
        changed = true;
        ui.numberLabel->setText(QString::number(list.size()));
    }
    else 
        QMessageBox::information(this, "Delete", "You have to select an item to delete it.");

	
}

void MWindowImpl::on_playNextInSerieButton_clicked(bool from_dbus)
{
    if(lastplayed==0)
        QMessageBox::critical(this, "No serie to play", "There isn't any serie we can play at the moment.");
    else
    {
        if(lastplayed->isFinished() && lastplayed->validLink())
        {
            Serie* serie = hashmap[lastplayed->getLink()];
            if(from_dbus)
                serie->execActFile(DBUSARGS);
            else
                serie->execActFile();
        }
        else
        {
            if(lastplayed->isDisabled() || lastplayed->isFinished())
            {
                QString message = "This serie is disabled or finished, we cannot play this.\n Lastplayed was: " + lastplayed->getName();
                QMessageBox::critical(this, "Serie is disabled", message);
            }
            else
            {
                if(from_dbus)
                    lastplayed->execActFile(DBUSARGS);
                else
                    lastplayed->execActFile();
            }
        }
    }
}


void MWindowImpl::on_playNextButton_clicked()
{
    bool done = false;
    if(list.size() >0)
    {
        QListIterator<Serie*> it(list);
		
        while(it.hasNext() && !done)
        {
            Serie* elem = it.next();
            Q_ASSERT(elem != 0);
            if(elem->isDisabled() == false && elem->isFinished() == false)
            {
                elem->execActFile();
                it.toBack();
                done = true;
            }
        }
    }
    if(done == false)
        QMessageBox::critical(this, "No serie to play", "There isn't any serie we can play at the moment");
}

void MWindowImpl::on_playLastAddedButton_clicked()
{
    if(lastadded==0)
        QMessageBox::critical(this, "No serie to play", "There isn't any serie we can play at the moment (no last added).");
    else
    {
        if(!lastadded->isDisabled())
            lastadded->execActFile();
    }
}

void MWindowImpl::on_undoButton_clicked()
{
    if(lastplayed!=0 && lastplayed->getEpisodeNum()!=1)
    {
        lastplayed->rewind();
	setLastPlayedName();
    }
}

void MWindowImpl::on_clearButton_clicked()
{
    ui.searchEdit->clear();
    ui.tableWidget->clearSelection();
}

void MWindowImpl::cellFocusChanged(int currentrow, int currentcolumn, int previousrow, int previouscolumn)
{
    Q_UNUSED(currentcolumn);
    Q_UNUSED(previousrow);
    Q_UNUSED(previouscolumn);
    
//    qDebug() << "currentrow:" << currentrow << "listsize:" << list.size();

/*
  if currentrow==-1 we most likely have no cells hence we don't do anything
  if we call this programm in the gui, then we use the 0 everywhere, but 0 is a valid row
*/
    if( (currentrow!=previousrow || currentrow==0 ) && currentrow!=-1) 
    {
        Serie* serie = list.at(currentrow);
        Q_ASSERT(serie!=0);
        QAction* showinfoaction = ui.actionShow_Series_Info;
        QAction* ongoingaction = ui.actionEnable_Ongoing;
        QAction* notongoingaction = ui.actionDisable_Ongoing;
        QAction* rewindaction = ui.actionRewind_by_one;
        QAction* playeraction = ui.actionSet_player;
        Q_UNUSED(playeraction);
    
        if(serie->isOngoing())
        {
            ongoingaction->setEnabled(false);
            notongoingaction->setEnabled(true);
        }
        else
        {
            ongoingaction->setEnabled(true);
            notongoingaction->setEnabled(false);
        }

        if(serie->getEpisodeNum()==1)
            rewindaction->setEnabled(false);
        else
            rewindaction->setEnabled(true);

        if(serie->isDisabled())
            showinfoaction->setEnabled(false);
        else
            showinfoaction->setEnabled(true);
    }
}

void MWindowImpl::cellDoubleClicked(int row, int column)
{
    Q_UNUSED(column);
    Serie* serie = list.at(row);
    if(!serie->isDisabled() && !serie->isFinished())
        serie->execActFile();
}

void MWindowImpl::addToList(Serie* serie)
{
    Q_ASSERT(serie!=0);

    hashmap.insert(serie->getUuid(), serie);
    Q_ASSERT(serie->getUuid() == hashmap[serie->getUuid()]->getUuid() );

    serie->setIndex(list.size());	//the old size is the exact position where the new element comes in
    list.append(serie);
    
    //disable after serie started
    connect(serie, SIGNAL(started()), this, SLOT(serieStarted()));
    connect(serie, SIGNAL(stopped(int)), this, SLOT(serieStopped(int)));
	
    connect(serie, SIGNAL(changed(int)), this, SLOT(episodeChangedInSerie(int)));
    QTableWidgetItem* name = new QTableWidgetItem(serie->getName());
    name->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
    name->setToolTip(serie->getDir());

    QSpinBox* sbox = new QSpinBox();
    sbox->setMinimum(1);
    sbox->setMaximum(serie->getMax());
    sbox->setValue(serie->getEpisodeNum());
    sbox->setButtonSymbols(QAbstractSpinBox::PlusMinus);

    //if ongoing we show the actual max but the getmax is one up to have a nice spinbox
    int maximum;
    if(serie->isOngoing())
        maximum = serie->getMax() - 1;
    else
        maximum = serie->getMax();
  
    QTableWidgetItem* max = new QTableWidgetItem( QString::number( maximum ) );
    max->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
    
    QTableWidgetItem* ongoing = new QTableWidgetItem();
    if(serie->isOngoing())
    {
        QIcon icon(":/icons/button_ok.png");
        ongoing->setIcon(icon);
    }

    QPushButton* button = new QPushButton("Play");
    button->setToolTip("Play serie: \"" + serie->getName() + "\"");
    connect(button,SIGNAL(clicked()), serie, SLOT(execActFile()));

    if( Settings::Instance()->getScanMedia())
    {
        QFuture<QPair<QString, int> > futureduration = QtConcurrent::run(serie,
                                                                         &Serie::getDuration);
        QFutureWatcher<QPair<QString, int> >* m_futureWatcher = new QFutureWatcher<QPair<QString, int> >();
        m_futureWatcher->setFuture(futureduration);
        connect(m_futureWatcher, SIGNAL(finished()), this, SLOT(setDuration()));
    }

    int row = ui.tableWidget->rowCount();
    ui.tableWidget->insertRow(row);
    ui.tableWidget->setItem(row, 0, name);
    ui.tableWidget->setCellWidget(row, 1, sbox);
    ui.tableWidget->setItem(row, 2, max);
    ui.tableWidget->setItem(row, 3, ongoing);
    ui.tableWidget->setCellWidget(row, 4, button);

    //do something if is ongoing and no episodes left
    if(serie->isDisabled() )
        setGui( list.size() -1);
    sbox->setValue(serie->getEpisodeNum());
    
    //connect the spinmapper after setup, otherwise this gets weird
    spinmapper->removeMappings(sbox);
    connect(sbox,SIGNAL(valueChanged(int)), spinmapper, SLOT(map()));
    spinmapper->setMapping(sbox, serie->getIndex());
}

void MWindowImpl::episodeChangedInSerie(int snumber)
{
    Q_ASSERT(snumber>=0);
    cellFocusChanged(snumber,0,0,0);
    changed=true;
    setGui(snumber);
}

void MWindowImpl::episodeChangedInGui(int snumber)
{
    Q_ASSERT(snumber>=0);
    QSpinBox* sbox = qobject_cast<QSpinBox*>(ui.tableWidget->cellWidget(snumber,1));
    Q_ASSERT(sbox != 0);
    Serie* serie = list.at(snumber);

    //don't know if we still need everything but it can't hurt
    sbox->blockSignals(true);
    serie->blockSignals(true);
    spinmapper->blockSignals(true);
    this->blockSignals(true);
    if(sbox->value()!=serie->getEpisodeNum())
    {
        changed=true;
        serie->setEpisode(sbox->value());
    }
    cellFocusChanged(snumber,0,0,0);
    sbox->blockSignals(false);
    serie->blockSignals(false);
    spinmapper->blockSignals(false);
    this->blockSignals(false);
}

void MWindowImpl::askForSettings()
{
    //ask if player exists mit QValidator ableitung davon
    SettingsDialogImpl* dialog = new SettingsDialogImpl(this);
    dialog->exec();
	
    QString errorstring = "You have not configured a player software, defaulting to ";
    errorstring.append(DEFAULTPLAYER);
    errorstring.append("\nThis will probably not work on your system.\n Change the settings in the config.");

    if(Settings::Instance()->noPlayer())
    {
        QMessageBox::critical(this, "Player choosen", errorstring);
        Settings::Instance()->addPlayer( DEFAULTPLAYER ,"");
        changed = true;
    }
    else
        if(dialog->result() == QDialog::Accepted)
            changed = true;
    delete dialog;
}

void MWindowImpl::random()
{
    int first_available_index = 0;
    for(;first_available_index < list.size() && ( list.at(first_available_index)->isDisabled()
                                                  || list.at(first_available_index)->isFinished());first_available_index++)
    {}


    if(first_available_index >=list.size() || list.at(first_available_index)->isDisabled() || list.at(first_available_index)->isFinished())
    {
        QMessageBox::critical(this, "All series are disabled", "All series are disabled or no series available");
    }
    else
    {
        int available_series = list.size() - first_available_index;  //is this really correct?
        int index = (rand() % available_series) + first_available_index;
        qDebug() << "RANDOM: first_available_index:" << first_available_index
                 << "available_series:" << available_series
                 << "index:" << index;
        list.at( index )->execActFile();
    }
        
}

void MWindowImpl::newRandom()
{
    int first_available_index = 0;
    int not_played = 0;
    bool done = false;
    Serie* serie;
    foreach(serie, list)
    {
        qDebug() << "in:" << serie->isDisabled() << serie->getEpisodeNum() << "done:" << done;
        Q_ASSERT(serie!=0);
        if(!done && (serie->isDisabled() || serie->isFinished() || serie->getEpisodeNum()!=1))
            first_available_index++;
        else
            done = true;
        if(serie->getEpisodeNum()==1)
            not_played++;
    }

    const bool a = first_available_index >= list.size();
    const bool b =list.at(first_available_index)->isDisabled();
    const bool c = list.at(first_available_index)->getEpisodeNum()!=1;
    qDebug() << a << b << c;
    
    if(first_available_index >=list.size() || list.at(first_available_index)->isDisabled() || list.at(first_available_index)->isFinished() 
       || list.at(first_available_index)->getEpisodeNum()!=1)
    {
        QMessageBox::critical(this, "All series are disabled", "All series are disabled or no series available");
    }
    else
    {
        int index = (rand() % not_played) + first_available_index;
        for(;list.at(index)->getEpisodeNum()!=1 && index < list.size(); index++)
        {}
        
        qDebug() << "RANDOM: first_available_index:" << first_available_index
                 << "not_played:" << not_played
                 << "index:" << index;
        Q_ASSERT(index!=list.size()); //i think this should not happen but if it could we should use an if
        list.at( index )->execActFile();
    }
}

void MWindowImpl::cleanupSeries()
{
    QMessageBox::StandardButton answer = QMessageBox::question (this, "Cleanup Series?", 
                                                                "Do you want to cleanup series, which will delete series with no directory?", 
                                                                QMessageBox::Ok | QMessageBox::Cancel);

    if(answer==QMessageBox::Ok)               // 0-th button was pressed
    {
        bool needtoreload = false;
        for(int i=0;i < list.size(); i++)
        {
            Serie* serie = list.at(i);
            if(serie->isDisabledNoDir())
            {
                needtoreload = true;
                list.removeAt(i);
            }
        }
        if(needtoreload)
            reload();
    }
}

void MWindowImpl::quitWithoutSaving()
{
    changed = false;
    qApp->quit();
}

void MWindowImpl::showSerieInfo()
{
    QTableWidgetItem* item = ui.tableWidget->currentItem();
    if(item!=0)
    {
        int snumber = item->row();
        Serie* serie = list.at(snumber);
        Q_ASSERT(serie!=0);
        Q_ASSERT(!serie->isDisabled());

        InfoDialogImpl* infodialog = new InfoDialogImpl(this,serie);
	Serie* linked = getSerieForUuid(serie->getLink());
	if(linked!= nullptr)
	  infodialog->setLinkName(linked->getName());
        infodialog->exec();
        if(infodialog->result()==QDialog::Accepted)
        {
            QString newname = infodialog->getResult();
            serie->setName(newname);
            item->setText(newname);
            changed = true;
        }
        delete infodialog;
        
    }
}


void MWindowImpl::setOngoing()
{
    QTableWidgetItem* item = ui.tableWidget->currentItem();
    if(item!=0)
    {
        int snumber = item->row();
        Serie* serie = list.at(snumber);
        Q_ASSERT(serie!=0);
        Q_ASSERT(!serie->isOngoing());
        if(!serie->isOngoing())
        {
            serie->setOngoing(true);
        
            disconnect(serie,0,0,0);
            ui.tableWidget->removeCellWidget(snumber,1);
            
            QPushButton* button = qobject_cast<QPushButton*>(ui.tableWidget->cellWidget(snumber,4));
            if(serie->isDisabled()==false) // need to check this because ongoing series are never finished
            {
                connect(serie, SIGNAL(changed(int)), this, SLOT(episodeChangedInSerie(int)));	
                QSpinBox* sbox = new QSpinBox();
                sbox->setMinimum(1);
                sbox->setMaximum(serie->getMax());
                sbox->setButtonSymbols(QAbstractSpinBox::PlusMinus);
                
                //if ongoing we show the actual max but the getmax is one up to have a nice spinbox
                int maximum = serie->getMax();
                
                QTableWidgetItem* max = new QTableWidgetItem( QString::number( maximum ) );
                max->setFlags(Qt::ItemIsEnabled);
            

                button->setEnabled(true);
                
                ui.tableWidget->setCellWidget(snumber, 1, sbox);
            
                sbox->setValue(serie->getEpisodeNum());

                spinmapper->removeMappings(sbox);
                connect(sbox,SIGNAL(valueChanged(int)), spinmapper, SLOT(map()));
                spinmapper->setMapping(sbox, serie->getIndex());
            }
            else
            {
                QLineEdit* lineedit = new QLineEdit();
                lineedit->setReadOnly(true);

                ui.tableWidget->setCellWidget(snumber,1,lineedit);
                lineedit->insert("None Left");
                
                button->setEnabled(false);
            }

            QTableWidgetItem* ongoing = new QTableWidgetItem();
            QIcon icon(":/icons/button_ok.png");
            ongoing->setIcon(icon);
            ui.tableWidget->setItem(snumber, 3, ongoing);
                
            cellFocusChanged(snumber,0,0,0);
            changed = true;
        }
    }
}

void MWindowImpl::setNotOngoing()
{
    QTableWidgetItem* item = ui.tableWidget->currentItem();
    if(item!=0)
    {
        int snumber = item->row();
        Q_ASSERT(snumber!=-1);
        Serie* serie = list.at(snumber);
        Q_ASSERT(serie!=0);
        if(serie->isOngoing())
        {
            serie->setOngoing(false);
            
            // This is copy and pasted, i should better just use a function but on the first try it didn't work
            
            
            //remove the old one, this seems to be important, otherwise there are somehow 2 widgets
            disconnect(serie,0,0,0);
            ui.tableWidget->removeCellWidget(snumber,1);
            
            
            QPushButton* button = qobject_cast<QPushButton*>(ui.tableWidget->cellWidget(snumber,4));
            if(serie->isFinished()==false)
            {
                connect(serie, SIGNAL(changed(int)), this, SLOT(episodeChangedInSerie(int)));	
                QSpinBox* sbox = new QSpinBox();
                sbox->setMinimum(1);
                sbox->setMaximum(serie->getMax());
                sbox->setButtonSymbols(QAbstractSpinBox::PlusMinus);
                
                //if ongoing we show the actual max but the getmax is one up to have a nice spinbox
                int maximum = serie->getMax();
                
                QTableWidgetItem* max = new QTableWidgetItem( QString::number( maximum ) );
                max->setFlags(Qt::ItemIsEnabled);
                
                button->setEnabled(true);
                ui.tableWidget->setCellWidget(snumber, 1, sbox);
                sbox->setValue(serie->getEpisodeNum());
            }
            else
            {
                QLineEdit* lineedit = new QLineEdit();
                lineedit->setReadOnly(true);
                ui.tableWidget->setCellWidget(snumber,1,lineedit);
                lineedit->insert("Finished");
                
                button->setEnabled(false);
            }
            
            QTableWidgetItem* ongoing = new QTableWidgetItem();
            ui.tableWidget->setItem(snumber, 3, ongoing);
            
    
            cellFocusChanged(snumber,0,0,0);
            changed = true;
        }
    }
}

void MWindowImpl::rewind()
{
    QTableWidgetItem* item = ui.tableWidget->currentItem();
    if(item!=0)
    {
        int row = item->row();
        Serie* serie = list.at(row);
        if(serie->getEpisodeNum()==1)
            QMessageBox::information(this, "Rewind", "You can't rewind a serie which is at the beginning");
        else
        {
            serie->rewind();
                    
            cellFocusChanged(row,0,0,0);
            changed = true;
	    setLastPlayedName();
        }
    }
    else 
        QMessageBox::information(this, "Rewind", "You have to select an item to rewind it");

}

void MWindowImpl::setLink()
{
    QTableWidgetItem* item = ui.tableWidget->currentItem();
    if(item!=0)
    {
        int row = item->row();
        Serie* serie = list.at(row);
        LinkDialogImpl* dialog=new LinkDialogImpl(serie, &list, this);
        dialog->exec();
        if(dialog->result()==QDialog::Accepted)
        {
            QUuid result = dialog->getResult();
            serie->setLink(result);
            if(serie->validLink())
            {
                QString newlink = hashmap[result]->getName();
                ui.tableWidget->itemAt(row,0)->setToolTip(newlink);
            }
            changed=true;
        }
        delete dialog;
        changed = true;
    }
    else 
        QMessageBox::information(this, "Rewind", "You have to select an item to set a link for it");

}

void MWindowImpl::setPlayer()
{
    QTableWidgetItem* item = ui.tableWidget->currentItem();
    if(item!=0)
    {
        int row = item->row();
        Serie* serie = list.at(row);
        SetPlayerDialogImpl* dialog = new SetPlayerDialogImpl(serie, this);
        dialog->exec();
        if(dialog->result()==QDialog::Accepted)
        {
            changed = true;
        }
        else
            changed = false;
            
        delete dialog;
    }
}

void MWindowImpl::setGui(int snumber)
{
    Q_ASSERT(snumber>=0);
    Serie* serie = list.at(snumber);
    QPushButton* button = qobject_cast<QPushButton*>(ui.tableWidget->cellWidget(snumber,4));
    Q_ASSERT(button!=0);
        
    serie->blockSignals(true);

    QString durationstring;
    if(Settings::Instance()->getScanMedia())
        durationstring = serie->getDuration().first;
	   
    QTableWidgetItem* duration = new QTableWidgetItem(durationstring);
    ui.tableWidget->setItem(snumber,5,duration);
    if(serie->isFinished())
    {
        QSpinBox* sbox = qobject_cast<QSpinBox*>(ui.tableWidget->cellWidget(snumber,1));
        Q_ASSERT(sbox!=0);
        disconnect(sbox,0,0,0);
        spinmapper->removeMappings(sbox);
		
        QLineEdit* lineedit = new QLineEdit();
        lineedit->setReadOnly(true);
        ui.tableWidget->setCellWidget(snumber,1,lineedit);
        lineedit->insert("Finished");
                
        button->setEnabled(false);
    }
    else
    {	//FIXME see SerieChangedInGui fixme

        bool not_done_already = button->isEnabled();
        if(serie->isDisabled() && not_done_already )
        {
            QSpinBox* sbox = qobject_cast<QSpinBox*>(ui.tableWidget->cellWidget(snumber,1));
            Q_ASSERT(sbox != 0);
            sbox->blockSignals(true);

            disconnect(sbox,0,0,0);
            spinmapper->removeMappings(sbox);
            QLineEdit* lineedit = new QLineEdit();
            lineedit->setReadOnly(true);
            ui.tableWidget->setCellWidget(snumber,1,lineedit);
            lineedit->insert(serie->getReason());
            button->setEnabled(false);
                        
            sbox->blockSignals(false);
        }
        else
        {
            QSpinBox* sbox = qobject_cast<QSpinBox*>(ui.tableWidget->cellWidget(snumber,1));
            if(sbox!=0)
            {
                sbox->blockSignals(true);
                            
                sbox->setValue(list.at(snumber)->getEpisodeNum());
                sbox->blockSignals(false);
            }
            else
            {
                //we are in a case where there exists no spinbox, we likely deleted it somewhere
                serie->blockSignals(true);
                            
                QSpinBox* sbox = new QSpinBox();
                sbox->setMinimum(1);
                sbox->setMaximum(serie->getMax());
                sbox->setValue(serie->getEpisodeNum());
                sbox->setButtonSymbols(QAbstractSpinBox::PlusMinus);

                spinmapper->removeMappings(sbox);
                connect(sbox,SIGNAL(valueChanged(int)), spinmapper, SLOT(map()));
                spinmapper->setMapping(sbox, serie->getIndex());
                            
                QTableWidgetItem* ongoing = new QTableWidgetItem();
                ui.tableWidget->setCellWidget(snumber, 1, sbox);
                ui.tableWidget->setItem(snumber, 3, ongoing);
                ui.tableWidget->cellWidget(snumber,4)->setEnabled(true);

                serie->blockSignals(false);
            }
        }
    }
    serie->blockSignals(false);
}

void MWindowImpl::serieStarted()
{
   int opacity = Settings::Instance()->getOpacity();
   Serie* callee = qobject_cast<Serie*>(QObject::sender());
   if(callee==0)
      qDebug() << "The callee is null (in started), do we really want that?";
   currentlyplaying = callee;
   ui.clearButton->click();
   
   ui.tableWidget->setEnabled(false);
   
   ui.deleteButton->setEnabled(false);
   ui.menubar->setEnabled(false);
   ui.playNextButton->setEnabled(false);
   ui.playNextInSerieButton->setEnabled(false);
   ui.playLastAddedButton->setEnabled(false);
   ui.undoButton->setEnabled(false);
   
   ui.searchEdit->setEnabled(false);
   ui.clearButton->setEnabled(false);
   
   this->setWindowOpacity( (100 - opacity)/100.0 );
}

void MWindowImpl::serieStopped(int snumber)
{
    Serie* callee = qobject_cast<Serie*>(QObject::sender());
    if(callee==0)
        qDebug() << "The callee is null (in stopped), do we really want that?";
        
    currentlyplaying = 0;

    ui.tableWidget->setEnabled(true);
    ui.deleteButton->setEnabled(true);
    ui.menubar->setEnabled(true);
    ui.playNextButton->setEnabled(true);
    ui.playNextInSerieButton->setEnabled(true);
    ui.playLastAddedButton->setEnabled(true);
    ui.undoButton->setEnabled(true);

    ui.searchEdit->setEnabled(true);
    ui.clearButton->setEnabled(true);

    this->setWindowOpacity ( 1.0);

    Q_ASSERT(snumber!=-1);
    if(callee->isDisabled() || callee->isFinished())
    {
        QLineEdit* lineedit = new QLineEdit();
        lineedit->setReadOnly(true);
        ui.tableWidget->setCellWidget(snumber,1,lineedit);
        lineedit->insert(callee->getReason());
    }

    lastplayed = callee;
        
    setLastPlayedName();
}

void MWindowImpl::on_searchEdit_textChanged(const QString & text)
{
    Q_ASSERT(ui.tableWidget->rowCount()>=1);
    for(int i=0;i<list.size();i++)
    {
        Serie* serie = list.at(i);
        if(text.isEmpty())
            ui.tableWidget->showRow(i);
        else
        {
            if(!serie->getName().contains(text, Qt::CaseInsensitive))
                ui.tableWidget->hideRow(i);
            else
                ui.tableWidget->showRow(i);
        }
    }    
}

void MWindowImpl::rightClickPopup(QPoint point)
{
    const QModelIndex index = ui.tableWidget->indexAt(point);

    // this gets cast to NULL if it is not a QPushButton
    const QPushButton* button = qobject_cast<QPushButton*>(ui.tableWidget->indexWidget(index)); 

    if(!index.isValid() || button!=0)
        return;
    else
    {
      ui.menuSerie->popup(ui.tableWidget->mapToGlobal(point));
    }
}


void MWindowImpl::setLastPlayedName()
{
    if(lastplayed!=0)
    {
        if(lastplayed->isFinished())
        {
            ui.nextLabel->setText("");
            ui.nextNameLabel->setToolTip("");
            ui.nextNameLabel->setText("");
        }
        else
        {
            QString label = lastplayed->getName();
            if(label.size() > LASTPLAYEDLABELSIZE)
            {
                label.truncate(LASTPLAYEDLABELSIZE);
                label.append("[..]");
            }
            ui.nextLabel->setText(label);
      
            QString nextepisodename = lastplayed->getNextEpisodeName();
            ui.nextNameLabel->setToolTip(nextepisodename);
            //trim it if to long
            if(nextepisodename.size() > LASTPLAYEDLABELSIZE)
            {
                nextepisodename.truncate(LASTPLAYEDLABELSIZE);
                nextepisodename.append("[..]");
            }
            ui.nextNameLabel->setText(nextepisodename);	 
        }
    }
}

void MWindowImpl::setDuration()
{
    QObject* sender = QObject::sender();
    QFutureWatcher<QPair<QString, int> >* futurewatcher = static_cast<QFutureWatcher<QPair<QString, int> >* >(sender);
    QFuture<QPair<QString, int> > future =  futurewatcher->future();
  
    QTableWidgetItem* duration = new QTableWidgetItem(future.result().first);
    ui.tableWidget->setItem(future.result().second, 5, duration);  
  
    delete futurewatcher;
}
