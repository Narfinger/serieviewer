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

#include "serie.h"
#include "serieview.h"
#include "seriemodel.h"

SerieView::SerieView(QWidget* parent) : QTableView(parent) {
}

//WARNING: we might leak qpushbuttons if we resize this and might get even multiple signals.
//I need to find out if the view deletes the buttons or if i have to implement rowsDeleted
void SerieView::rowsInserted(const QModelIndex& parent, int start, int end) {
  qDebug() << "blubber" << start << end;
  QAbstractItemView::rowsInserted(parent, start, end);
  for(int i = start; i< end; i++) {
    const QModelIndex id = model()->index(i,4);
    QPushButton* b = new QPushButton("Play", this);
    connect(b, &QPushButton::clicked, this, &SerieView::playButtonPushed);
    setIndexWidget(id, b);
  }
}

void SerieView::playButtonPushed() {
  const QPushButton* b = qobject_cast<QPushButton*>(sender());
  if (b!= nullptr) {
    const QModelIndex i = indexAt(b->pos());
    const SerieModel* sm = dynamic_cast<SerieModel*>(model());
    qDebug() << "button pushed for" << i;
    const SeriePtr s = sm->serieAtIndex(i);
    qDebug() << "Playing" << s->getName();
  }
}



