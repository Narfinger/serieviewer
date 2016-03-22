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


#include "defines.h"
#include "adddialogimpl.h"
#include "linkdialogimpl.h"
#include "settingsdialogimpl.h"
#include "setplayerdialogimpl.h"
#include "infodialogimpl.h"
#include "serie.h"
#include "xmlhandler.h"
#include "settings.h"
#include "seriesortfilterproxymodel.h"
#include "seriemodel.h"
#include "seriemodeliterator.h"

MWindowImpl::MWindowImpl(QWidget *parent)
    : QMainWindow(parent)
{
  ui.setupUi(this);
    
  sm = new SerieModel();
  pm = new SerieSortFilterProxyModel(this);
  pm->setSourceModel(sm);
  pm->setFilterKeyColumn(0);
  ui.tableView->setModel(pm);
  ui.tableView->setSortingEnabled(true);
  connect(sm, &SerieModel::serieStarted, this, &MWindowImpl::serieStarted);
  connect(sm, &SerieModel::serieStopped, this, &MWindowImpl::serieStopped);

  xmlhandler = new XMLHandler( this);
	

  //load settings
  QSettings settings(FIRMNAME, APPNAME);
  {
    settings.beginGroup("mainwindow");
    ui.tableView->setColumnWidth(0, settings.value("col0", NAMESIZE).toInt());
    ui.tableView->setColumnWidth(1, settings.value("col1", ACTSIZE).toInt());
    ui.tableView->setColumnWidth(2, settings.value("col2", EPISODESIZE).toInt());
    ui.tableView->setColumnWidth(3, settings.value("col3", ONGOINGSIZE).toInt());
    ui.tableView->setColumnWidth(4, settings.value("col4", WATCHBSIZE).toInt());
    ui.tableView->setColumnWidth(5, settings.value("col5", DURATIONSIZE).toInt());

    QPoint pos = settings.value("pos", QPoint(200, 200)).toPoint();
    QSize size = settings.value("size", QSize(650, 480)).toSize();
    resize(size);
    move(pos);
    settings.endGroup();
  }
  QString settingsfile = settings.value("settingsfile").toString();
  if(!settingsfile.isNull())
    Settings::Instance()->setSettingsFilename(settingsfile);

  connect(xmlhandler,SIGNAL(serieParsed(SeriePtr)), this,SLOT(addToList(SeriePtr)));
  connect(xmlhandler,SIGNAL(askForPlayer()), this, SLOT(askForSettings()));
  //connect(ui.tableWidget, SIGNAL(cellDoubleClicked(int,int)), this, SLOT(cellDoubleClicked(int,int)));

  buildmenus();

  if(!xmlhandler->read()) {
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
  ui.numberLabel->setText(QString::number(sm->rowCount()));

  //search ui
  connect(ui.searchEdit, &QLineEdit::textChanged, pm, &QSortFilterProxyModel::setFilterFixedString);
  ui.clearButton->setEnabled(true);
  ui.searchEdit->setEnabled(true);
  ui.filterLabel->setEnabled(true);
  
  // setup the context menu for items in tablewidget
  ui.tableView->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(ui.tableView, SIGNAL(customContextMenuRequested(QPoint)), this, SLOT(rightClickPopup(QPoint)));

  QUuid uuidplayed = Settings::Instance()->getLastPlayed();
  sm->lastplayed = sm->getSerieFromUuid(uuidplayed);
  setLastPlayedName();
   
/*
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
    }*/
}


MWindowImpl::~MWindowImpl() {
    Q_ASSERT(xmlhandler!=0);
    ui.clearButton->click();

    QSettings settings(FIRMNAME, APPNAME);
    if(sm->changed)
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
        settings.setValue("col0", ui.tableView->columnWidth(0));
        settings.setValue("col1", ui.tableView->columnWidth(1));
        settings.setValue("col2", ui.tableView->columnWidth(2));
        settings.setValue("col3", ui.tableView->columnWidth(3));
        settings.setValue("col4", ui.tableView->columnWidth(4));
        settings.setValue("col5", ui.tableView->columnWidth(5));
        settings.endGroup();

    }
}

void MWindowImpl::buildmenus() {
  connect(ui.actionAdd_recursive, SIGNAL(triggered()), this, SLOT(addSerieRecursive()));
  connect(ui.actionSettings, SIGNAL(triggered()), this, SLOT(askForSettings()));
  connect(ui.actionRefresh, SIGNAL(triggered()), this, SLOT(reload()));
  connect(ui.actionClean_up_Series, SIGNAL(triggered()), this, SLOT(cleanupSeries()));
  connect(ui.actionQuit_without_saving, SIGNAL(triggered()), this, SLOT(quitWithoutSaving()));
  connect(ui.actionQuit, SIGNAL(triggered()), qApp, SLOT(quit()));

  connect(ui.actionShow_Series_Info, SIGNAL(triggered()), this, SLOT(showSerieInfo()));
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

void MWindowImpl::saveXML(bool sort) {
  Q_ASSERT(xmlhandler!=0);
  Settings* instance = Settings::Instance();

  if (!sm->lastplayed.isNull())
    instance->setLastPlayed(sm->lastplayed->getUuid());
  QList<SeriePtr> list;
  SerieModelIterator i(pm);
  while (i.hasNext())
    list.append(i.next());
  
  if (!xmlhandler->write(list))
    QMessageBox::critical(this,"Error in save", "Can't save data!\n All changes will were not saved");
}

const QModelIndex MWindowImpl::selectedTranslatedIndex() const {
  const QModelIndexList il = ui.tableView->selectedIndexes();
  if (!il.isEmpty()) {
    const QModelIndex i = il.at(0);
    if (i.isValid()) {
      return pm->mapToSource(i);
    }
  }
  return QModelIndex();
}

QString MWindowImpl::getCurrentName() {
    if(currentlyplaying==0)
        return QString();
    else
        return currentlyplaying->getName();
}

void MWindowImpl::reload() {
  saveXML();
  sm->removeRows(0,sm->rowCount());
  xmlhandler->read();
  ui.searchEdit->clear();
}

void MWindowImpl::about() {
    QString message;
    message.append("This programm comes with no warranty.\n\n You are using Version: ");
    message.append(PROGRAMMVERSION);
    QMessageBox::about ( this, QString("Versioninfo"), message );
}

void MWindowImpl::addSerieRecursive() {
  const QString dirstring = QFileDialog::getExistingDirectory(this, "Open Dir for Recursive", Settings::Instance()->getLastPath(), 
							      QFileDialog::ShowDirsOnly);
  if(!dirstring.isEmpty()) {
    QDir dir(dirstring);
    for(const QFileInfo& d : dir.entryInfoList(QDir::Dirs | QDir::NoDotAndDotDot)) {
      if(d.isDir())
	addGuiSerie(d.absoluteFilePath());
    }
    dir.cdUp();
    Settings::Instance()->setLastPath(dir.absolutePath());
  }
}

void MWindowImpl::addGuiSerie(QString path) {
  AddDialogImpl* dialog=new AddDialogImpl(path, this);
  if(dialog->getShow()) {
    dialog->exec();
    if(dialog->result()==QDialog::Accepted) {
      Settings::Instance()->setLastPath( dialog->getLastPath() );
      SeriePtr result = dialog->getResult(this);
      addToList(result);
      sm->changed = true;
    }
  }
  delete dialog;
  ui.numberLabel->setText(QString::number(sm->rowCount()));
}

void MWindowImpl::on_deleteButton_clicked() {
  const QModelIndexList mil = ui.tableView->selectedIndexes();
  if (mil.isEmpty()) { QMessageBox::information(this, "Delete", "You have to select an item to delete it"); return; }
  
  const QModelIndex i = mil.at(0);
  sm->removeRows(i.row(),1);
}

void MWindowImpl::on_playNextInSerieButton_clicked(bool from_dbus) {
  if (sm->lastplayed.isNull())
    QMessageBox::critical(this, "No serie to play", "There isn't any serie we can play at the moment.");
  else {
    SeriePtr s;
    if (!sm->lastplayed->isFinished()) {
      s = sm->lastplayed;
    } else {
      const QUuid link = sm->lastplayed->getLink();
      if (!link.isNull()) {
	QMessageBox::critical(this, "No serie to play", "Serie is finished and we do not have a link");
	return;
      } else
	s = sm->getSerieFromUuid(link);
    }
    if (!s->isReadyToPlay()) {
      QMessageBox::critical(this, "No serie to play", "Something is wrong with the serie we would like to play");
      return;
    } else {
      if (from_dbus)
	sm->lastplayed->execActFile(DBUSARGS);
      else
	sm->lastplayed->execActFile();
    }
  }
}

void MWindowImpl::on_playNextButton_clicked() {
  SerieModelIterator i(pm);
  while (i.hasNext()) {
    const SeriePtr s = i.next();
    if (s->isReadyToPlay()) {
      s->execActFile();
      return;
    }
  }
  QMessageBox::critical(this, "No serie to play", "There isn't any serie we can play at the moment");
}

void MWindowImpl::on_playLastAddedButton_clicked() {
  if(lastadded==0)
    QMessageBox::critical(this, "No serie to play", "There isn't any serie we can play at the moment (no last added).");
  else {
    if(!lastadded->isDisabled())
      lastadded->execActFile();
  }
}

void MWindowImpl::on_undoButton_clicked() {
  sm->rewind();
  setLastPlayedName();
}

void MWindowImpl::on_clearButton_clicked() {
  ui.searchEdit->clear();
}

void MWindowImpl::addToList(SeriePtr s) {
  sm->addSerie(s);
}

void MWindowImpl::askForSettings() {
  //ask if player exists mit QValidator ableitung davon
  SettingsDialogImpl* dialog = new SettingsDialogImpl(this);
  dialog->exec();

  QString errorstring = "You have not configured a player software, defaulting to ";
  errorstring.append(DEFAULTPLAYER);
  errorstring.append("\nThis will probably not work on your system.\n Change the settings in the config.");

  if(Settings::Instance()->noPlayer()) {
    QMessageBox::critical(this, "Player choosen", errorstring);
    Settings::Instance()->addPlayer( DEFAULTPLAYER ,"");
    //changed = true;
  } else {
        if(dialog->result() == QDialog::Accepted)
	    delete dialog;
            //changed = true;
  }
}

void MWindowImpl::random() {
  if (!sm->playRandom())
    QMessageBox::critical(this, "Error", "All messages done or empty.");
}

void MWindowImpl::newRandom() {
  if (!sm->playNewRandom())
    QMessageBox::critical(this, "Error", "All messages done or empty.");
}

void MWindowImpl::cleanupSeries() {
  QMessageBox::StandardButton answer = QMessageBox::question (this, "Cleanup Series?", 
                                                                "Do you want to cleanup series, which will delete series with no directory?", 
                                                                QMessageBox::Ok | QMessageBox::Cancel);
  if (answer==QMessageBox::Ok)
    sm->cleanupSeries();
}

void MWindowImpl::quitWithoutSaving() {
  sm->changed = false;
  qApp->quit();
}

void MWindowImpl::showSerieInfo() {
  const QModelIndex i = selectedTranslatedIndex();
  SeriePtr s = sm->serieAtIndex(i);
  InfoDialogImpl infodialog(this, s);
  infodialog.exec();
  /*const QModelIndexList il = ui.tableView->selectedIndexes();
  if (!il.isEmpty()) {
    const QModelIndex i = ui.tableView->selectedIndexes().at(0);
    if (i.isValid()) {
      SeriePtr s = sm->serieAtIndex(i);
      InfoDialogImpl infodialog(this, s);
      infodialog.exec();
   
      if (infodialog.result() == QDialog::Accepted) {
	const QString newname = infodialog.getResult();
	s->setName(newname);
	changed = true;
      }
    }
  }*/
}

void MWindowImpl::rewind() {
  /*
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
*/
}

void MWindowImpl::setLink() {
  /*
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
*/
}

void MWindowImpl::setPlayer() {
  /*
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
    }*/
}

void MWindowImpl::serieStarted() {
   int opacity = Settings::Instance()->getOpacity();
   //SeriePtr callee = qobject_cast<Serie*>(QObject::sender());
   //if(callee.isNull())
   //   qDebug() << "The callee is null (in started), do we really want that?";
   //currentlyplaying = callee;
   ui.clearButton->click();
   
   ui.tableView->setEnabled(false);
   
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

void MWindowImpl::serieStopped() {
  ui.tableView->setEnabled(true);
  ui.deleteButton->setEnabled(true);
  ui.menubar->setEnabled(true);
  ui.playNextButton->setEnabled(true);
  ui.playNextInSerieButton->setEnabled(true);
  ui.playLastAddedButton->setEnabled(true);
  ui.undoButton->setEnabled(true);

  ui.searchEdit->setEnabled(true);
  ui.clearButton->setEnabled(true);

  this->setWindowOpacity ( 1.0);
  setLastPlayedName();
}

void MWindowImpl::rightClickPopup(QPoint point) {
  const QModelIndex index = ui.tableView->indexAt(point);

  // this gets cast to NULL if it is not a QPushButton
  const QPushButton* button = qobject_cast<QPushButton*>(ui.tableView->indexWidget(index)); 

  if(!index.isValid() || button!=0)
    return;
  else {
    ui.menuSerie->popup(ui.tableView->mapToGlobal(point));
  }
}


void MWindowImpl::setLastPlayedName() {
  if(!sm->lastplayed.isNull()) {
    if(sm->lastplayed->isFinished()){
            ui.nextLabel->setText("");
            ui.nextNameLabel->setToolTip("");
            ui.nextNameLabel->setText("");
    } else {
      QString label = sm->lastplayed->getName();
      if(label.size() > LASTPLAYEDLABELSIZE) {
	label.truncate(LASTPLAYEDLABELSIZE);
        label.append("[..]");
    }
    ui.nextLabel->setText(label);
      
    QString nextepisodename = sm->lastplayed->getNextEpisodeName();
    ui.nextNameLabel->setToolTip(nextepisodename);
    //trim it if to long
    if(nextepisodename.size() > LASTPLAYEDLABELSIZE) {
      nextepisodename.truncate(LASTPLAYEDLABELSIZE);
      nextepisodename.append("[..]");
    }
    ui.nextNameLabel->setText(nextepisodename);
    }
  }
}
