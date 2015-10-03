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

#ifndef SERIEMODEL_H
#define SERIEMODEL_H

#include <QAbstractTableModel>

#include "serie.h"


class SerieModel : public QAbstractTableModel
{
    Q_OBJECT

public:
  SerieModel(QObject* parent = 0);
  virtual QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
  Qt::ItemFlags flags(const QModelIndex& index) const;
  virtual int columnCount(const QModelIndex& parent) const { if (parent.isValid()) return 0; else return 5; };
  virtual int rowCount(const QModelIndex& parent) const { if (parent.isValid()) return 0; else return list.size(); };
  virtual QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const;
  
  void addSerie(const SeriePtr& ptr);
  const SeriePtr serieAtIndex(const QModelIndex& i) const { return list.at(i.row()); };
  bool playRandom();
  bool playNewRandom();
  void cleanupSeries();
  QModelIndex playNext();  //returns the played index or invalid if nothing played
  
  const static int OwnSortRole = Qt::UserRole + 1;
private:
  int sortRole(const QModelIndex& i) const;
  
  QList<SeriePtr> list;
};

#endif // SERIEMODEL_H

