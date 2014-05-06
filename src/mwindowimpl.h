#ifndef MWINDOWIMPL_H
#define MWINDOWIMPL_H

#include <QtWidgets/QMainWindow>
#include <QList>
#include <QHash>
#include <QUuid>
#include <QFuture>

#include "ui_mwindow.h"

class XMLHandler;
class Serie;
class QSignalMapper;
class QAction;

/** 	@class MWindowImpl
	Our main class for representing the gui and doing stuff
*/
class MWindowImpl : public QMainWindow
{
    Q_OBJECT
  private:
    Ui::MainWindow ui;	//!< Userinterface
    XMLHandler* xmlhandler;	//!< Our xmlhandler
    
    bool changed;		//!< Has something changed and do we need to save the file
    QList<Serie*> list;	//!< List of our serie
    QHash<QUuid, Serie*> hashmap;
    QSignalMapper* spinmapper;	//!< Signal mapper for mapping QSpinBox::valueChanged -> episodeChangedInGui
    
    Serie* lastplayed;
    Serie* lastadded;
    Serie* currentlyplaying;             //!< serie which is currently playing otherwise null
    
    QList<QAction*> serieactionlist;       //!<list of the qactions in the serie menu, order is the same as in building
    
    QMenu* seriemenu;           //!< serie menu we need for rightlick stuff
    
    void buildmenus(); //!< builds the menu bar at the start and adds seriemenu
    
    void saveXML(bool sort = true);      //!< saves the XML file (we have some things we need to do before it)
  
  public:
    MWindowImpl(QWidget *parent = 0);
    
    ~MWindowImpl();
    
    /**
       compareing of pointer series, needed for qsort
    */
    static bool compareSerieP(Serie* s1, Serie* s2);
    
    /* returns the serie pointer for the uuid, could be null */
    Serie* getSerieForUuid(QUuid uuid);
    
  public slots:
    // returns name of the last serie played (aka current)
    QString getCurrentName();
  
    // returns a list of all series we have
    QStringList* getSerieListNames();

    void playIndex(int index);
    
    void reload();                  //!< saves the xml and reloads the serie
    
    void about();                  //! the about box		
    
    void addGuiSerie(QString path=""); //!< adds a serie but gives the dialog
    
    void on_addButton_clicked();
    
    void on_deleteButton_clicked();
    
    void on_playNextInSerieButton_clicked(bool from_dbus = false );
    
    void on_playNextButton_clicked();
    
    void on_playLastAddedButton_clicked();		
    
    void on_undoButton_clicked();

    void on_clearButton_clicked();
    
    void cellFocusChanged(int,int,int,int);
    
    void cellDoubleClicked(int, int);
    
    void addToList(Serie* serie); //! add a serie to list and modify ui
    
    void episodeChangedInSerie(int snumber); //! episode changed in serie (watched an episode)
    
    void episodeChangedInGui(int snumber); 		//! episode changed by spinbox
    
    void askForSettings();
    
    
    void random(); //! starts a random serie
    
    void newRandom(); //! starts a random serie which has not started yet
 		
    void cleanupSeries();                 //! deletes all series with nodir

    void quitWithoutSaving(); //! quits the application and does not save
    
    //! show information from the serie, at the moment only filename of next episode
    void showSerieInfo();
    
    void setOngoing(); //! set the marked serie as ongoing
    
    void setNotOngoing(); //! set the marked serie as not ongoing

    void rewind(); //! rewind the marked series by one episode
    
    void setLink();
    
    void setPlayer();
    
    void serieStarted(); //! serie started, change the gui
    
    void serieStopped(int snumber); //! serie stopped, change the gui
    
    void setGui(int snumber); //! set Gui to Finished or no left if had to (is called if episodeChanged*
    
    void on_searchEdit_textChanged(const QString & text); //! scrolls to the text in the search box
    
    void rightClickPopup(QPoint point); //! right click popup
    
    void setLastPlayedName();
    
    void setDuration();
};
#endif
