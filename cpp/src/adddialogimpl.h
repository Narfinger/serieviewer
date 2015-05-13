#ifndef ADDDIALOGIMPL_H
#define ADDDIALOGIMPL_H

#include <QtWidgets/QDialog>
#include <QString>
#include <QList>
#include <QPair>

#include "ui_adddialog.h"

class Serie;

/** 	@class AddDialogImpl
	Implementation of the add serie dialog
*/
class AddDialogImpl : public QDialog
{
    Q_OBJECT
  private:
    Ui::AddDialog ui;	//!< User interface
    QString dirname;	//!< directory name
    bool dontshow;		//!< don't show it if something is wrong
    QList<Serie*> *m_list;
    QList<QPair<QString, QString> > replacelist;
    
  public:
    AddDialogImpl(QWidget *parent = 0);
    
    AddDialogImpl( QList<Serie*> *list, QString path = "", QWidget *parent = 0);
    
    ~AddDialogImpl();
		
    /**
       do we want to show this dialog because the user selected not valid files
    */
    bool getShow();
    
    /**
       @return returns the path we were in (one dir up)
    */
    QString getLastPath();
    
    /**
       get the serie we constructed here
    */
    Serie* getResult(QObject *parent = 0);
    
};

#endif
