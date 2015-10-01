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
virtual QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
virtual int columnCount(const QModelIndex& parent) const { Q_UNUSED(parent) return 5; };
virtual int rowCount(const QModelIndex& parent) const { Q_UNUSED(parent) return list.size(); };
virtual QModelIndex parent(const QModelIndex& child) const;
virtual QModelIndex index(int row, int column, const QModelIndex& parent) const;
void addSerie(const SeriePtr& ptr);

private:
  QList<SeriePtr> list;
};

#endif // SERIEMODEL_H

