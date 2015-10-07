/*
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) 2015  Narfinger
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#include <QIcon>

#include "seriemodel.h"

SerieModel::SerieModel(QObject* parent): QAbstractTableModel(parent) {
}


QVariant SerieModel::data(const QModelIndex& index, int role) const {
  if (!index.isValid()) return QVariant();
  
  const SeriePtr s = list.at(index.row());
  if (role == Qt::DisplayRole) {
    switch(index.column()) {
      case 0: return s->getName();
      case 1: return getEpisodeNum(s);
      case 2: return getMax(s);
      case 3: //return s->isOngoing();
      case 4: return s->getDuration();
    }
  } else if (role == Qt::DecorationRole) {
    if (index.column()==3) {
      if (s->isOngoing())
	return QIcon(":/icons/button_ok.png");
      else
	return QIcon(":/icons/ongoing.png");
    }
  } else if (role == OwnSortRole) {
    return sortRole(index);
  }
  
  return QVariant();
}

bool SerieModel::setData(const QModelIndex& index, const QVariant& value, int role) {
  SeriePtr s = serieAtIndex(index);
  switch(index.column()) {
    case 0: s->setName(value.toString()); break;
    case 1: s->setEpisode(value.toInt()); break;
  }
  changed = true;
  return QAbstractTableModel::setData(index, value, role);
}

Qt::ItemFlags SerieModel::flags(const QModelIndex& index) const {
  if (!index.isValid()) return Qt::NoItemFlags;
  
  if (serieAtIndex(index)->isDisabled())
    return Qt::ItemIsSelectable;
  if (index.column()==1 || index.column() == 0)
    return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsEditable;
  return Qt::ItemIsEnabled | Qt::ItemIsSelectable;
  //return QAbstractTableModel::flags(index);
}

QVariant SerieModel::headerData(int section, Qt::Orientation orientation, int role) const {
  if (role != Qt::DisplayRole)
    return QVariant();
  if (orientation == Qt::Horizontal) {
    switch(section) {
      case 0: return "Serie Name";
      case 1: return "Episode";
      case 2: return "Max";
      case 3: return "Ongoing";
      case 4: return "Button";
      case 5: return "Duration";
    }
  }
  if (orientation == Qt::Vertical)
    return section;
  return QVariant();
}

void SerieModel::addSerie(const SeriePtr& ptr) {
  const int row = list.size();
  beginInsertRows(QModelIndex(), row, row);
  list.append(ptr);
  endInsertRows();
  
  const QModelIndex i = index(row,3);
  connect(ptr.data(), &Serie::durationRead, [=] () { this->dataChanged(i,i); });
  connect(ptr.data(), &Serie::changed, [=] () { serieChanged(row); });
  connect(ptr.data(), &Serie::stopped, [=] () { serieStopped(ptr); });
}

bool SerieModel::playRandom() {
  int first_available_index = 0;
  for(;first_available_index < list.size() && ( list.at(first_available_index)->isDisabled()
                                                || list.at(first_available_index)->isFinished());first_available_index++)
  {}

  if(first_available_index >=list.size() || list.at(first_available_index)->isDisabled() || list.at(first_available_index)->isFinished()) {
    return false;
  } else {
    int available_series = list.size() - first_available_index;  //is this really correct?
    int index = (rand() % available_series) + first_available_index;
    qDebug() << "RANDOM: first_available_index:" << first_available_index
             << "available_series:" << available_series
             << "index:" << index;
    list.at(index)->execActFile();
    return true;
  }
}

bool SerieModel::playNewRandom() {
  int first_available_index = 0;
  int not_played = 0;
  bool done = false;
  SeriePtr serie;
  foreach(serie, list) {
    qDebug() << "in:" << serie->isDisabled() << serie->getEpisodeNum() << "done:" << done;
    Q_ASSERT(serie!=0);
    if(!done && (serie->isDisabled() || serie->isFinished() || serie->getEpisodeNum()!=1))
      first_available_index++;
    else
      done = true;
   
    if(serie->getEpisodeNum()==1)
      not_played++;
  }

  const bool a = first_available_index >= list.size();
  const bool b =list.at(first_available_index)->isDisabled();
  const bool c = list.at(first_available_index)->getEpisodeNum()!=1;
  qDebug() << a << b << c;
    
  if(first_available_index >=list.size() || list.at(first_available_index)->isDisabled() || list.at(first_available_index)->isFinished() 
     || list.at(first_available_index)->getEpisodeNum()!=1)
  {
    return false;
  } else {
    int index = (rand() % not_played) + first_available_index;
    for(;list.at(index)->getEpisodeNum()!=1 && index < list.size(); index++)
    {}
      
    qDebug() << "RANDOM: first_available_index:" << first_available_index
             << "not_played:" << not_played
             << "index:" << index;
    Q_ASSERT(index!=list.size()); //i think this should not happen but if it could we should use an if
    list.at( index )->execActFile();
  }
  changed = true;
}

void SerieModel::cleanupSeries() {
  bool needtoreload = false;
  for(int i=0;i < list.size(); i++) {
    SeriePtr serie = list.at(i);
    if(serie->isDisabledNoDir()) {
      needtoreload = true;
      list.removeAt(i);
    }
  }
  changed = true;
}

QModelIndex SerieModel::playNext() {
  for (const SeriePtr& s : list) {
    if (s->isReadyToPlay()) {
      changed = true;
      s->execActFile();
      return QModelIndex();
    }
  }
  return QModelIndex();
}

SeriePtr SerieModel::getSerieFromUuid(const QUuid& uuid) const {
  for( const SeriePtr& s : list) {
    if (s->getUuid() == uuid)
      return s;
  }
  return SeriePtr();
}

int SerieModel::sortRole(const QModelIndex& i) const {
  //this needs a bunch of work
  const SeriePtr s = serieAtIndex(i);
  double dthis = static_cast<double>(s->getEpisodeNum()) / static_cast<double>(s->getMax());
  return dthis;
  
  /*Settings* instance = Settings::Instance();
    Q_ASSERT(instance != 0);
    if(! instance->getOngoingSort() )		//sort ongoing normal
    {
        if(this->isOngoing() )
            return false;
        if(s1.isOngoing() )
            return true;
    }
    if( instance->getPriorSort() )			//priortised sorting
    {
        if( this->getEpisodeNum() >1  && s1.getEpisodeNum() == 1)
        {
            return true;				
        }
        else
            if( this->getEpisodeNum() == 1 && s1.getEpisodeNum() >1 )
                return false;
        //if both >1 compare and if both ==1 compare
    }
    
    double dthis = static_cast<double>(this->getEpisodeNum()) / static_cast<double>(this->getMax());
    double dm = static_cast<double>(s1.getEpisodeNum()) / static_cast<double>(s1.getMax());
    return (dthis > dm );Model::sortRole(const QModelIndex& i) const {
  */
}

void SerieModel::serieChanged(int row) {
  changed = true;
  const QModelIndex start = index(row, 0);
  const QModelIndex end   = index(row, columnCount(start));
  dataChanged(start, end);
}

void SerieModel::serieStopped(SeriePtr ptr) {
  lastplayed = ptr;
}
