#ifndef LINKDIALOGIMPL_H
#define LINKDIALOGIMPL_H

#include <QtWidgets/QDialog>
#include <QString>
#include <QList>
#include <QUuid>

#include "ui_linkdialog.h"

class Serie;

/**
 	@class LinkDialogImpl
	Implementation of the set link dialog
*/
class LinkDialogImpl : public QDialog
{
    Q_OBJECT
  private:
    Ui::LinkDialog ui;	//!< User interface
    QList<Serie*> *m_list;
    
  public:
    LinkDialogImpl(QWidget *parent = 0);
    
    LinkDialogImpl(Serie* current, QList<Serie*> *list, QWidget *parent = 0);
    
    ~LinkDialogImpl();
    
    /**
       get the serie we constructed here
    */
    QUuid getResult();
};

#endif
