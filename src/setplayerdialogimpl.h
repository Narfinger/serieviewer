#ifndef SETPLAYERDIALOGIMPL_H
#define SETPLAYERDIALOGIMPL_H

#include <QtGui/QDialog>
#include <QString>

#include "ui_setplayerdialog.h"

class Serie;
/**
 	@class SetPlayerDialogImpl
	Implementation of the SetPlayerDialog which allows to change the player of a serie
*/

class SetPlayerDialogImpl : public QDialog
{
    Q_OBJECT
  private:
    Ui::SetPlayerDialog ui;
    
    Serie* m_serie; /* serie we change the player for */
                
  public:
    SetPlayerDialogImpl(QWidget *parent = 0);
    
    SetPlayerDialogImpl(Serie* current, QWidget *parent = 0);
    
    ~SetPlayerDialogImpl();
    
  public slots:
    void acceptClicked();
};

#endif
