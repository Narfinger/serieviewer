#include "settingsdialogimpl.h"

#include <QtDebug>
#include <QFileDialog>
#include <QMessageBox>
#include <QCheckBox>
#include <QDir>
#include <QInputDialog>
#include <QStringList>

#include "settings.h"

SettingsDialogImpl::SettingsDialogImpl(QWidget *parent)
 : QDialog(parent)
{
    ui.setupUi(this);
    
    //setup the whole ui from settings
    Settings* settings = Settings::Instance();
    
    connect(ui.sortCheckBox, SIGNAL(stateChanged(int)), this, SLOT(sortStateChanged(int)));
    if(settings->getSort() == true)
        ui.sortCheckBox->setCheckState(Qt::Checked);
    else
    {
        ui.sortCheckBox->setCheckState(Qt::Unchecked);
        ui.sortOngoingCheckBox->setEnabled(false);
        ui.sortPriorCheckBox->setEnabled(false);
        ui.sortPriorLabel->setEnabled(false);
    }
    connect(ui.buttonBox, SIGNAL(accepted()), this, SLOT(acceptClicked()));
    
    if(settings->getOngoingSort() == true)
        ui.sortOngoingCheckBox->setCheckState(Qt::Checked);
    else
		ui.sortOngoingCheckBox->setCheckState(Qt::Unchecked);
    
    if(settings->getPriorSort() == true)
        ui.sortPriorCheckBox->setCheckState(Qt::Checked);
    else
        ui.sortPriorCheckBox->setCheckState(Qt::Unchecked);
    if(settings->getReloadSort() == true)
        ui.sortReloadCheckBox->setCheckState(Qt::Checked);
    else
        ui.sortReloadCheckBox->setCheckState(Qt::Unchecked);
    
    if(settings->getConvertNames() == true)
        ui.convertnamescheckbox->setCheckState(Qt::Checked);
    else
        ui.convertnamescheckbox->setCheckState(Qt::Unchecked);
    
    if(settings->getScanMedia() == true)
        ui.scanmediabox->setCheckState(Qt::Checked);
    else
        ui.scanmediabox->setCheckState(Qt::Unchecked);
    
    // qDebug() << settings->getSort() << settings->getOngoingSort() << settings->getPriorSort();
    ui.tableWidget->setColumnWidth(0,300);
    ui.tableWidget->setColumnWidth(0,200);
    
    
    QTableWidgetItem* name = new QTableWidgetItem("Player");
    
    ui.tableWidget->setHorizontalHeaderItem(0,name);
    
    
    QList<QPair<QString, QString> > players = Settings::Instance()->getPlayerList();
    for(int i=0; i<players.size(); i++)
    {
        QTableWidgetItem* playeritem = new QTableWidgetItem( players.at(i).first );
        playeritem->setFlags(Qt::ItemIsEnabled);
	
        QTableWidgetItem* argitem = new QTableWidgetItem( players.at(i).second );
        //argitem->setFlags(Qt::ItemIsEnabled);
	
        int row = ui.tableWidget->rowCount();
        ui.tableWidget->insertRow(row);
        ui.tableWidget->setItem(row, 0, playeritem);
        ui.tableWidget->setItem(row, 1, argitem);
    }
    ui.settingsfilelabel->setText(Settings::Instance()->getSettingsFilename());
}


SettingsDialogImpl::~SettingsDialogImpl()
{
}

void SettingsDialogImpl::addToWidget(QString player, QString argument)
{
    QFile playerfile(player);
    if(!playerfile.exists())
    {
        QMessageBox::critical ( this, QString("Error in Player Choose"), QString("Your Playerpath doesn't point to any file.") );
        return;
    }
    
    if( Settings::Instance()->playerListContains(player) )
    {
        QMessageBox::information ( this, QString("Error in Player Choose"), QString("You have this player already in the list.") );
			return;
    }
    QTableWidgetItem* playeritem = new QTableWidgetItem(player);
    QTableWidgetItem* argitem = new QTableWidgetItem(argument);
		
    int row = ui.tableWidget->rowCount();
    ui.tableWidget->insertRow(row);
    ui.tableWidget->setItem(row, 0, playeritem);
    ui.tableWidget->setItem(row, 1, argitem);
}

bool SettingsDialogImpl::getSort()
{
    return (ui.sortCheckBox->checkState()==Qt::Checked);
}

bool SettingsDialogImpl::getOngoingSort()
{
    return (ui.sortOngoingCheckBox->checkState()==Qt::Checked);	
}

bool SettingsDialogImpl::getPriorSort()
{
    return (ui.sortPriorCheckBox->checkState()==Qt::Checked);
}

bool SettingsDialogImpl::getReloadSort()
{
    return (ui.sortReloadCheckBox->checkState()==Qt::Checked);
}

bool SettingsDialogImpl::getConvertNames()
{
    return (ui.convertnamescheckbox->checkState()==Qt::Checked);
}


bool SettingsDialogImpl::getScanMedia()
{
    return (ui.scanmediabox->checkState()==Qt::Checked);

}

QString SettingsDialogImpl::getSettingsFile()
{
    return ui.settingsfilelabel->text();
}

void SettingsDialogImpl::on_chooseButton_clicked()
{
    bool ok;
    QString player = QFileDialog::getOpenFileName(this, "Open Player", QDir::homePath());
    
    QString arguments =  QInputDialog::getText(this, QString("Choose Arguments"), QString("Choose Arguments for the player" + player),
                                               QLineEdit::Normal, QString(), &ok);
    
    if(!player.isEmpty() && ok)
    {
        addToWidget(player,arguments);
    }
}

void SettingsDialogImpl::on_addButton_clicked()
{
    bool bplay, barg;
    QString player =  QInputDialog::getText(this, QString("Add Player"), QString("Player path"),QLineEdit::Normal, 
                                            QString(), &bplay);
    
    QString arguments =  QInputDialog::getText(this, QString("Choose Arguments"), 
                                               QString("Choose Arguments for the player " + player),QLineEdit::Normal, QString(), &barg);
    
    if(bplay && barg)
    {
        addToWidget(player,arguments);
    }
}


void SettingsDialogImpl::on_upButton_clicked()
{
    //doesn't work, qhash sorts somehow
    QTableWidgetItem* item = ui.tableWidget->currentItem();
    if(item != 0)
    {
        int row = item->row();
        if(row != 0)
        {
            QTableWidgetItem* above0=ui.tableWidget->takeItem(row-1, 0);
            QTableWidgetItem* above1=ui.tableWidget->takeItem(row-1, 1);
            QTableWidgetItem* act0=ui.tableWidget->takeItem(row, 0);
            QTableWidgetItem* act1=ui.tableWidget->takeItem(row, 1);
            
            ui.tableWidget->setItem(row  , 0, above0 );
            ui.tableWidget->setItem(row  , 1, above1 );
            
            ui.tableWidget->setItem(row-1, 0, act0   );
            ui.tableWidget->setItem(row-1, 1, act1   );
        }
    }
}

void SettingsDialogImpl::on_downButton_clicked()
{
    QTableWidgetItem* item = ui.tableWidget->currentItem();
    if(item != 0)
    {
        int row = item->row();
        if(row != ui.tableWidget->rowCount() -1 )
        {
            QTableWidgetItem* next0=ui.tableWidget->takeItem(row+1, 0);
            QTableWidgetItem* next1=ui.tableWidget->takeItem(row+1, 1);
            QTableWidgetItem* act0=ui.tableWidget->takeItem(row, 0);
            QTableWidgetItem* act1=ui.tableWidget->takeItem(row, 1);
				
            
            ui.tableWidget->setItem(row  , 0, next0 );
            ui.tableWidget->setItem(row  , 1, next1 );
            
            ui.tableWidget->setItem(row+1, 0, act0   );
            ui.tableWidget->setItem(row+1, 1, act1   );
        }
    }    
}

void SettingsDialogImpl::on_settingsfileButton_clicked()
{
    QString oldfile = getSettingsFile();
    QDir dir = QDir(oldfile);
    QString filename = QFileDialog::getSaveFileName(this, "Chose new Settings File", dir.path());
    ui.settingsfilelabel->setText(filename);
}


void SettingsDialogImpl::acceptClicked()
{
    if(ui.tableWidget->rowCount() == 0)
    {
        QString errorstring = "You have to choose a player.";
        QMessageBox::critical( this, QString("Error in Player choose"), errorstring);
        return;
    }
    
    
    Settings* instance = Settings::Instance();
    instance->clearPlayers();
    
    instance->setSort( getSort() );
    instance->setOngoingSort( getOngoingSort() );
    instance->setPriorSort( getPriorSort() ); 
    instance->setReloadSort( getReloadSort() );
    
    for(int i=0; i<ui.tableWidget->rowCount(); i++)
    {
        QString player = ui.tableWidget->item(i,0)->text();
        QString arg = ui.tableWidget->item(i,1)->text();
        instance->addPlayer( player, arg );
	
    }
    
    instance->setConvertNames(getConvertNames());
    instance->setScanMedia(getScanMedia());
    
    instance->setSettingsFilename(getSettingsFile());
    
    accept();
}


void SettingsDialogImpl::sortStateChanged(int sort)
{
    //disable parts of the ui if we selected sorting or not sorting
    if(sort == Qt::Unchecked)
    {
        ui.sortOngoingCheckBox->setCheckState(Qt::Unchecked);
        ui.sortPriorCheckBox->setCheckState(Qt::Unchecked);
        ui.sortReloadCheckBox->setCheckState(Qt::Unchecked);
	
        ui.sortOngoingCheckBox->setEnabled(false);
        ui.sortPriorCheckBox->setEnabled(false);
        ui.sortReloadCheckBox->setEnabled(false);
        
        ui.sortPriorLabel->setEnabled(false);
    }
    else
    {
        if(sort == Qt::Checked)
        {	
            ui.sortOngoingCheckBox->setEnabled(true);
            ui.sortPriorCheckBox->setEnabled(true);
            ui.sortReloadCheckBox->setEnabled(true);
            
            ui.sortPriorLabel->setEnabled(true);
        }
    }
}
