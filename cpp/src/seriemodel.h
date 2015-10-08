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
  virtual bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole);
  virtual Qt::ItemFlags flags(const QModelIndex& index) const;
  virtual int columnCount(const QModelIndex& parent = QModelIndex()) const { if (parent.isValid()) return 0; else return 5; };
  virtual int rowCount(const QModelIndex& parent = QModelIndex()) const { if (parent.isValid()) return 0; else return list.size(); };
  virtual QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const;
  
  void addSerie(const SeriePtr& ptr);
  const SeriePtr serieAtIndex(const QModelIndex& i) const { return list.at(i.row()); };
  bool playRandom();
  bool playNewRandom();
  void cleanupSeries();
  QModelIndex playNext();  //returns the played index or invalid if nothing played
  SeriePtr getSerieFromUuid(const QUuid& uuid) const;
  
  const static int OwnSortRole = Qt::UserRole + 1;
  bool changed = false;
  SeriePtr lastplayed;

signals:
  void serieStarted();
  void serieStopped();

private:
  int sortRole(const QModelIndex& i) const;
  void serieChanged(int row);
  void serieStoppedF(SeriePtr ptr);
  
  QVariant getEpisodeNum(const SeriePtr& s) const { if (s->isDisabled()) return s->getReason(); else return s->getEpisodeNum(); };
  QVariant getMax(const SeriePtr& s) const        { if (s->isOngoing())  return s->getMax() -1; else return s->getMax();        };
  
  QList<SeriePtr> list;
};

#endif // SERIEMODEL_H

