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

#include <QLineEdit>
#include <QSortFilterProxyModel>

#include "namedelegate.h"
#include "seriemodel.h"

NameDelegate::NameDelegate(QObject* parent): QStyledItemDelegate(parent) {
}

QWidget* NameDelegate::createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const {
    QLineEdit* editor = new QLineEdit(parent);
    editor->setFrame(false);
    
    return editor;
}

void NameDelegate::setEditorData(QWidget* editor, const QModelIndex& index) const {
  const QString value = index.model()->data(index, Qt::DisplayRole).toString();
  QLineEdit* le = dynamic_cast<QLineEdit*>(editor);
  le->setText(value);
}

void NameDelegate::setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const {
  QLineEdit* le = dynamic_cast<QLineEdit*>(editor);
  const QString value = le->text();
  const SeriePtr s = getSeriePtrFromIndex(index);
  s->setName(value);
  model->setData(index, value, Qt::DisplayRole);	//while we do not need to set this this calls update on the view which we need
}

void NameDelegate::updateEditorGemoetry(QWidget* editor, const QStyleOptionViewItem& option, const QModelIndex& index) const {
  editor->setGeometry(option.rect);
}

const SeriePtr NameDelegate::getSeriePtrFromIndex(const QModelIndex& index) const {
  const QSortFilterProxyModel* sfpm = dynamic_cast<const QSortFilterProxyModel*>(index.model());
  const SerieModel* sm = dynamic_cast<const SerieModel*>(sfpm->sourceModel());
  return sm->serieAtIndex(index);
}
