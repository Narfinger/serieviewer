#include "setplayerdialogimpl.h"

#include <QList>
#include <QListIterator>
#include <QPair>

#include "serie.h"
#include "settings.h"

SetPlayerDialogImpl::SetPlayerDialogImpl(QWidget *parent)
    : QDialog(parent)
{
    ui.setupUi(this);
}

SetPlayerDialogImpl::SetPlayerDialogImpl(Serie* current, QWidget *parent)
    : QDialog(parent),
      m_serie(current)
{
    ui.setupUi(this);
    
    connect(ui.buttonBox, SIGNAL(accepted()), this, SLOT(acceptClicked()));
    
    QString actplayer = m_serie->getPlayer();
    Settings* settings = Settings::Instance();
    QListIterator<QPair<QString, QString> > it(settings->getPlayerList());
    int index = 0;
    while(it.hasNext()) 
    {
        QPair<QString, QString> pair = it.next();
        ui.playerBox->addItem(pair.first);
        if(pair.first == actplayer)
            ui.playerBox->setCurrentIndex(index);
        index++;
    }
}

SetPlayerDialogImpl::~SetPlayerDialogImpl()
{
}



void SetPlayerDialogImpl::acceptClicked()
{
    m_serie->setPlayer(ui.playerBox->currentText());
    accept();
}
