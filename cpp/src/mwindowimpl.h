#ifndef MWINDOWIMPL_H
#define MWINDOWIMPL_H

#include <QtWidgets/QMainWindow>
#include <QList>
#include <QHash>
#include <QUuid>
#include <QFuture>

#include "ui_mwindow.h"
#include "serie.h"

class XMLHandler;
class Serie;
class QSignalMapper;
class QAction;
class SerieModel;

/** 	@class MWindowImpl
	Our main class for representing the gui and doing stuff
*/
class MWindowImpl : public QMainWindow
{
    Q_OBJECT
  private:
    Ui::MainWindow ui;	//!< Userinterface
    XMLHandler* xmlhandler;	//!< Our xmlhandler
    
    QHash<QUuid, Serie*> hashmap;
    QSignalMapper* spinmapper;	//!< Signal mapper for mapping QSpinBox::valueChanged -> episodeChangedInGui
    
    //SeriePtr lastplayed;
    SeriePtr lastadded;
    SeriePtr currentlyplaying;             //!< serie which is currently playing otherwise null
    
    SerieModel* sm;
    QSortFilterProxyModel* pm;
    
    void buildmenus(); //!< builds the menu bar at the start and adds seriemenu
    
    void saveXML(bool sort = true);      //!< saves the XML file (we have some things we need to do before it)
  
  public:
    MWindowImpl(QWidget *parent = 0);
    
    ~MWindowImpl();
    
  public slots:
    // returns name of the last serie played (aka current)
    QString getCurrentName();
    
    void reload();                  //!< saves the xml and reloads the serie
    
    void about();                  //! the about box	
     
    void addSerieRecursive();	//adds all subfolders as a seperate entry
    
    void addGuiSerie(QString path=""); //!< adds a serie but gives the dialog
    
    void on_addButton_clicked() { addGuiSerie(); };
    
    void on_addDirRecursiveButton_clicked() { addSerieRecursive(); };
    
    void on_deleteButton_clicked();
    
    void on_playNextInSerieButton_clicked(bool from_dbus = false );
    
    void on_playNextButton_clicked();
    
    void on_playLastAddedButton_clicked();		
    
    void on_undoButton_clicked();

    void on_clearButton_clicked();
    
    void addToList(SeriePtr serie); //! add a serie to list and modify ui
    
    void askForSettings();
    
    
    void random(); //! starts a random serie
    
    void newRandom(); //! starts a random serie which has not started yet
 		
    void cleanupSeries();                 //! deletes all series with nodir

    void quitWithoutSaving(); //! quits the application and does not save
    
    //! show information from the serie, at the moment only filename of next episode
    void showSerieInfo();

    void rewind(); //! rewind the marked series by one episode
    
    void setLink();
    
    void setPlayer();
    
    void serieStarted(); //! serie started, change the gui
    
    void serieStopped(); //! serie stopped, change the gui
    
    void rightClickPopup(QPoint point); //! right click popup
    
    void setLastPlayedName();
};
#endif
