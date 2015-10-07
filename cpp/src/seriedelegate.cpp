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
#include <QSpinBox>
#include <QSortFilterProxyModel>

#include "seriedelegate.h"
#include "seriemodel.h"

SerieDelegate::SerieDelegate(QObject* parent): QStyledItemDelegate(parent) {
}

QWidget* SerieDelegate::createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const {
  Q_UNUSED(option)
  
  if (index.column()==0) {
    QLineEdit* editor = new QLineEdit(parent);
    editor->setFrame(false);
    return editor;
  } else if (index.column()==1) {
    QSpinBox* editor = new QSpinBox(parent);
    editor->setFrame(false);
    editor->setMinimum(1);

    const int max = getSeriePtrFromIndex(index)->getMax();
    editor->setMaximum(max);
    return editor;
  }
  return nullptr;
}

void SerieDelegate::setEditorData(QWidget* editor, const QModelIndex& index) const {
  if (index.column()==0) {
    const QString value = index.model()->data(index, Qt::DisplayRole).toString();
    QLineEdit* le = dynamic_cast<QLineEdit*>(editor);
    le->setText(value);
  } else if (index.column()==1) {
    const int value = index.model()->data(index, Qt::DisplayRole).toInt();
    QSpinBox* sb = dynamic_cast<QSpinBox*>(editor);
    sb->setValue(value);
  }
}

void SerieDelegate::setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const {
  if (index.column()==0) {
    QLineEdit* le = dynamic_cast<QLineEdit*>(editor);
    const QString value = le->text();
    model->setData(index, value, Qt::DisplayRole);	//while we do not need to set this this calls update on the view which we need
  } else if (index.column()==1) {
    QSpinBox* sb = dynamic_cast<QSpinBox*>(editor);
    sb->interpretText();
    const int value = sb->value();
    model->setData(index, value, Qt::DisplayRole);	//while we do not need to set this this calls update on the view which we need
  }
}

void SerieDelegate::updateEditorGemoetry(QWidget* editor, const QStyleOptionViewItem& option, const QModelIndex& index) const {
  Q_UNUSED(index)
  editor->setGeometry(option.rect);
}

const SeriePtr SerieDelegate::getSeriePtrFromIndex(const QModelIndex& index) const {
  const QSortFilterProxyModel* sfpm = dynamic_cast<const QSortFilterProxyModel*>(index.model());
  const SerieModel* sm = dynamic_cast<const SerieModel*>(sfpm->sourceModel());
  return sm->serieAtIndex(index);
}
