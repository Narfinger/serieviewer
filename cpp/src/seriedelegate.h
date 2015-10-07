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

#ifndef SERIEDELEGATE_H
#define SERIEDELEGATE_H

#include <QStyledItemDelegate>
#include "serie.h"

class SerieDelegate : public QStyledItemDelegate
{
    Q_OBJECT
public:
    SerieDelegate(QObject* parent = 0);
    QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const;
    void setEditorData(QWidget* editor, const QModelIndex& index) const;
    void setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const;
    void updateEditorGemoetry(QWidget* editor, const QStyleOptionViewItem& option, const QModelIndex& index) const;

private:
  const SeriePtr getSeriePtrFromIndex(const QModelIndex& index) const;
};

#endif // SPINBOXDELEGATE_H