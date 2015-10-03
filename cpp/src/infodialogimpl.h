#ifndef INFODIALOGIMPL_H
#define INFODIALOGIMPL_H

#include "ui_infodialog.h"

#include <QtWidgets/QDialog>

#include "serie.h"

/* 
   Gives information about the serie
 */
class InfoDialogImpl : public QDialog
{ 
    Q_OBJECT
  private:
    Ui::InfoDialog ui;
    
  public:
  InfoDialogImpl(QWidget *parent = 0)
      : QDialog(parent)
    {
        ui.setupUi(this);
    }
    
  InfoDialogImpl(QWidget *parent = 0, const SeriePtr& serie = SeriePtr())
      : QDialog(parent)
    {
        ui.setupUi(this);
        Q_ASSERT(serie!=0);
        ui.nameEdit->setText(serie->getName());
        ui.nextepisodelabel->setText(serie->getNextEpisodeName());
        ui.directorylabel->setText(serie->getDir());
        ui.playerlabel->setText(serie->getPlayer());
        ui.filetextedit->setPlainText(serie->getDirectoryListing());
    }
    
    ~InfoDialogImpl()
    {
    }

    void setLinkName(const QString& name) { ui.linklabel->setText(name); };
    
    QString getResult()
    {
        return ui.nameEdit->text();
    }
};

#endif
