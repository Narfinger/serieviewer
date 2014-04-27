#ifndef SETTINGSDIALOGIMPL_H
#define SETTINGSDIALOGIMPL_H

#include <QtGui/QDialog>
#include <QColor>

#include "ui_settingsdialog.h"

/** 	@class SettingsDialogImpl
	Class for settings dialog
*/
class SettingsDialogImpl : public QDialog
{
    Q_OBJECT
  private:
    Ui::SettingsDialog ui;	//!< Userinterface
    
    void addToWidget(QString player, QString argument); /* add a player to the widget */
    
    bool getSort(); /* do we want to sort? */
    
    bool getOngoingSort(); /* sort ongoing to the top? */
    
    bool getPriorSort(); /* prioratize sort? */
    
    bool getConvertNames(); /* convert names? */
    
    bool getReloadSort(); /* sort on reload? */
    
    bool getScanMedia(); /* scans all files for their running time */
    
    QString getSettingsFile(); /* return current settings file path */
    
    
  public:
    SettingsDialogImpl(QWidget *parent = 0);
    
    ~SettingsDialogImpl();
    
  public slots:
    void on_chooseButton_clicked();
        
    void on_addButton_clicked();
		
    void on_upButton_clicked();
    
    void on_downButton_clicked();
		
    void on_settingsfileButton_clicked();
    
    void acceptClicked();
    
    
  private slots:					
    void sortStateChanged(int sort);
};

#endif
