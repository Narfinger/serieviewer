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

#include <QSpinBox>
#include <QSortFilterProxyModel>

#include "spinboxdelegate.h"
#include "seriemodel.h"

SpinBoxDelegate::SpinBoxDelegate(QObject* parent): QStyledItemDelegate(parent) {
}

QWidget* SpinBoxDelegate::createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const {
    QSpinBox* editor = new QSpinBox(parent);
    editor->setFrame(false);
    editor->setMinimum(1);
    
    const QSortFilterProxyModel* sfpm = dynamic_cast<const QSortFilterProxyModel*>(index.model());
    const SerieModel* sm = dynamic_cast<const SerieModel*>(sfpm->sourceModel());
    const int max = sm->serieAtIndex(index)->getMax();
    editor->setMaximum(max);
    return editor;
}

void SpinBoxDelegate::setEditorData(QWidget* editor, const QModelIndex& index) const {
  const int value = index.model()->data(index, Qt::DisplayRole).toInt();
  QSpinBox* sb = dynamic_cast<QSpinBox*>(editor);
  sb->setValue(value);
}

void SpinBoxDelegate::setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const {
  QSpinBox* sb = dynamic_cast<QSpinBox*>(editor);
  sb->interpretText();
  const int value = sb->value();
  const SeriePtr s = getSeriePtrFromIndex(index);
  s->setEpisode(value);
  model->setData(index, value, Qt::DisplayRole);	//while we do not need to set this this calls update on the view which we need
}

void SpinBoxDelegate::updateEditorGemoetry(QWidget* editor, const QStyleOptionViewItem& option, const QModelIndex& index) const {
  editor->setGeometry(option.rect);
}

const SeriePtr SpinBoxDelegate::getSeriePtrFromIndex(const QModelIndex& index) const {
  const QSortFilterProxyModel* sfpm = dynamic_cast<const QSortFilterProxyModel*>(index.model());
  const SerieModel* sm = dynamic_cast<const SerieModel*>(sfpm->sourceModel());
  return sm->serieAtIndex(index);
}






