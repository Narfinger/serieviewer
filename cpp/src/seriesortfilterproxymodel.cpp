/*
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) 2015  
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

#include <QDebug>
#include "settings.h"
#include "seriemodel.h"
#include "serie.h"

#include "seriesortfilterproxymodel.h"

SerieSortFilterProxyModel::SerieSortFilterProxyModel(QObject* parent) : QSortFilterProxyModel(parent) {
  setFilterCaseSensitivity(Qt::CaseInsensitive);
}

void SerieSortFilterProxyModel::setSourceModel(QAbstractItemModel* sourceModel) {
    QSortFilterProxyModel::setSourceModel(sourceModel);
    sm_ = dynamic_cast<SerieModel*>(sourceModel);
}


bool SerieSortFilterProxyModel::lessThan(const QModelIndex& source_left, const QModelIndex& source_right) const {
  //if (source_left.column()!= 0 || source_right.column()!=0) return QSortFilterProxyModel::lessThan(source_left, source_right);
  
  Settings* instance = Settings::Instance();
  SeriePtr left = sm_->serieAtIndex(source_left);
  SeriePtr right = sm_->serieAtIndex(source_right);

  Q_ASSERT(instance != 0);
  if (!instance->getOngoingSort()) {
        if (left->isOngoing() )
            return false;
        if (right->isOngoing() )
            return true;
    }
    if (instance->getPriorSort()) {			//priortised sorting
        if (left->getEpisodeNum() >1  && right->getEpisodeNum() == 1)
            return true;				
        else
            if (left->getEpisodeNum() == 1 && right->getEpisodeNum() >1)
                return false;
        //if both >1 compare and if both ==1 compare
    }
    
    double dthis = static_cast<double>(left->getEpisodeNum()) / static_cast<double>(left->getMax());
    double dm = static_cast<double>(right->getEpisodeNum()) / static_cast<double>(right->getMax());
    return (dthis > dm );
}