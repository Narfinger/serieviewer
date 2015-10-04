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

#ifndef SERIEMODELITERATOR_H
#define SERIEMODELITERATOR_H

#include <QObject>
#include <QSortFilterProxyModel>

#include "seriemodel.h"

/* 
 * This iterator goes over all series in the model ordered by sortfilterproxymodel
 */

class SerieModelIterator : public QObject
{
    Q_OBJECT
public:
    SerieModelIterator(QSortFilterProxyModel* m, QObject* parent = nullptr);
    bool hasNext();
    SeriePtr next();

private:
  QSortFilterProxyModel* sfpm_;
  SerieModel* sm_;
  QModelIndex sfpm_index_;
  int row_ = 0;
};

#endif // SERIEMODELITERATOR_H
