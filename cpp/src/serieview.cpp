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

#include <QDebug>
#include <QPushButton>

#include "serieview.h"

SerieView::SerieView(QWidget* parent) : QTableView(parent) {
}

void SerieView::rowsInserted(const QModelIndex& parent, int start, int end) {
  qDebug() << "blubber" << start << end;
  QAbstractItemView::rowsInserted(parent, start, end);
  for(int i = start; i< end; i++) {
    const QModelIndex id = model()->index(i,4);
    QPushButton* b = new QPushButton("Play", this);
    setIndexWidget(id, b);
  }
}


