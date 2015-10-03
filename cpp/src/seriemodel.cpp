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
      case 1: return s->getEpisodeNum();
      case 2: return s->getMax();
      case 3: //return s->isOngoing();
      case 4: return s->getDuration().first;
    }const int OwnSortRole = Qt::UserRole + 1;
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
