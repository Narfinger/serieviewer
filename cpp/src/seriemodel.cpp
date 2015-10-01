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

#include "seriemodel.h"

QVariant SerieModel::data(const QModelIndex& index, int role) const {
  const SeriePtr s = list.at(index.row());
  if (role == Qt::DisplayRole) {
    switch(index.column()) {
      case 0: return s->getName();
      case 1: return s->getEpisodeNum();
      case 2: return s->getMax();
      case 3: return s->isOngoing();
      case 4: return s->getDuration().first;
    }
  }
  
  return QVariant();
}

QModelIndex SerieModel::parent(const QModelIndex& child) const {
  //return QAbstractTableModel::parent(child);
  Q_UNUSED(child)
  return QModelIndex();
}

QModelIndex SerieModel::index(int row, int column, const QModelIndex& parent) const {
  return QAbstractTableModel::index(row, column, parent);
  //return parent.child(row, column);
}

void SerieModel::addSerie(const SeriePtr& ptr) {
  beginInsertRows(QModelIndex(), list.size(), list.size()+1);
  list.append(ptr);
  endInsertRows();
}