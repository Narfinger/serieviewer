project(serieviewer)
cmake_minimum_required(VERSION 2.8.12)

set(CMAKE_BUILD_TYPE Debug)

find_package(Qt5 5.2.1 COMPONENTS Widgets Xml DBus)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
find_package(FFMPEG)
include_directories(${FFMPEG_INCLUDE_DIRS})
if(${FFMPEG_FOUND})
  add_definitions(-DFFMPEG_LIB)
endif()

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

set(serieviewer_SRCS
    src/adddialogimpl.cpp
    src/defines.cpp
    src/linkdialogimpl.cpp
    src/main.cpp
    src/mwindowimpl.cpp
    src/namedelegate.cpp
    src/serie.cpp
    src/serieviewerdbus.cpp
    src/seriemodel.cpp
    src/seriemodeliterator.cpp
    src/serieview.cpp
    src/setplayerdialogimpl.cpp
    src/settings.cpp
    src/settingsdialogimpl.cpp
    src/spinboxdelegate.cpp
    src/xmlhandler.cpp
    src/infodialogimpl.h
)

SET(serieviewer_FORMS
    ui/adddialog.ui
    ui/infodialog.ui
    ui/linkdialog.ui
    ui/mwindow.ui
    ui/setplayerdialog.ui
    ui/settingsdialog.ui
)
QT5_ADD_RESOURCES(serieviewer_RESOURCES_RCC ui/icons.qrc)
QT5_WRAP_UI(serieviewer_FORMS_HDRS ${serieviewer_FORMS})

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -g")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wall")

list(APPEND CMAKE_CXX_FLAGS " -std=c++11 -march=native -Wall -pedantic -Wextra -Wundef -Wcast-align -Wwrite-strings -Wunreachable-code -Winit-self ")

add_executable(serieviewer ${serieviewer_SRCS}
		       ${serieviewer_FORMS_HDRS} ${serieviewer_RESOURCES_RCC})
target_link_libraries(serieviewer ${QT_QTCORE_LIBRARY} Qt5::Widgets Qt5::Xml Qt5::DBus ${FFMPEG_LIBRARIES})
install(TARGETS serieviewer RUNTIME DESTINATION bin)
