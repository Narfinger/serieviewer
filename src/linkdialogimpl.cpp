#include "linkdialogimpl.h"

#include <QtDebug>
#include <QMessageBox>
#include <QList>
#include <QComboBox>

#include "serie.h"
#include "settings.h"

LinkDialogImpl::LinkDialogImpl(QWidget *parent)
    : QDialog(parent)
{
    ui.setupUi(this);
}

LinkDialogImpl::LinkDialogImpl(Serie* current, QList<Serie*> *list, QWidget *parent)
    :QDialog(parent),
     m_list(list)
{
    ui.setupUi(this);

    ui.sname->setText(current->getName());

    ui.linkBox->addItem("No Link", -1);
    for(int i=0; i<m_list->size(); i++)
    {
        Serie* serie = m_list->at(i);
        ui.linkBox->addItem(serie->getName(), i);
    }

    //sets the combobox
    for(int i=0; i<m_list->size(); i++)
    {
        const QUuid uuid = m_list->at(i)->getUuid();
        if(uuid==current->getLink())
        {
            ui.linkBox->setCurrentIndex(i + 1);
        }
    }
    if(current->getLink()==QUuid())
        ui.linkBox->setCurrentIndex(0);
}


LinkDialogImpl::~LinkDialogImpl()
{
}


QUuid LinkDialogImpl::getResult()
{
    if(ui.linkBox->currentIndex()==0)
        return QUuid();
    else
    {
        Serie* serie = m_list->at(ui.linkBox->currentIndex() -1);
        return serie->getUuid();
    }
}

