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

#include "seriemodeliterator.h"

SerieModelIterator::SerieModelIterator(QSortFilterProxyModel* m, QObject* parent): QObject(parent), sfpm_(m) {
  sm_ = dynamic_cast<SerieModel*>(sfpm_->sourceModel());
  sfpm_index_ = sfpm_->index(0,0);
}

bool SerieModelIterator::hasNext() {
  return row_ < sfpm_->rowCount();
}

SeriePtr SerieModelIterator::next() {
  const QModelIndex sfpmi = sfpm_->index(row_, 0);
  const QModelIndex i = sfpm_->mapToSource(sfpmi);
  row_++;
  return sm_->serieAtIndex(i);  
}


