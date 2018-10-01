#!/bin/bash
#back my docs
#
# Usage:
#      backup.sh /Volumes/LACIE "--exclude=docker"

# * Deal Parameters

BACKUP_DEST=$1
if [ "$BACKUP_DEST" == "" ]; then
    BACKUP_DEST=$(pwd)
fi

EXTRA_EXCLUDES=$2

# * Source Path

WORKSPACE="$HOME/Workspace"
GO_BACK_SRC="$WORKSPACE/go"
JAVA_BACK_SRC="$WORKSPACE/java"
PY_BACK_SRC="$WORKSPACE/py"
JS_BACK_SRC="$WORKSPACE/js"
SHELL_BACK_SRC="$WORKSPACE/shell"
C_BACK_SRC="$WORKSPACE/c"
MISC_BACK_SRC="$WORKSPACE"
PICTURE_BACK_SRC="$HOME/Desktop/picture"
IMDATA_BACK_SRC="$HOME/Desktop/imdata"

# * Common Value

BACK_UP_VERSION=$(date "+%Y_%m_%d")
COMMON_EXCLUDES="--exclude=.svn --exclude=Icon? --exclude=.DS_Store --exclude=*.exe --exclude=*.tar.* --exclude=*.tar --exclude=*.zip --exclude=*.so --exclude=*.o --exclude=*.a --exclude=.idea --exclude=.cache/ --exclude=.local/"

# * Run Tar

# ** Back Go
GO_BACK_NAME=go_$BACK_UP_VERSION.tar
GO_EXCLUDES="--exclude=vendor --exclude=$GO_BACK_SRC/lib --exclude=pkg/ --exclude=$GO_BACK_SRC/bin"
echo "Back Go Source Files($GO_BACK_SRC) to $BACKUP_DEST/$GO_BACK_NAME   ...."
echo "-----------------------------------------------"
sh -c "tar -cv $COMMON_EXCLUDES $EXTRA_EXCLUDES $GO_EXCLUDES -f $BACKUP_DEST/$GO_BACK_NAME $GO_BACK_SRC"

# ** Back Java
JAVA_BACK_NAME=java_$BACK_UP_VERSION.tar
JAVA_EXCLUDES="--exclude=.metadata/ --exclude=.recommenders/"
echo "Back Java Source Files($JAVA_BACK_SRC) to $BACKUP_DEST/$JAVA_BACK_NAME   ...."
echo "-----------------------------------------------"
sh -c "tar -cv $COMMON_EXCLUDES $EXTRA_EXCLUDES $JAVA_EXCLUDES -f $BACKUP_DEST/$JAVA_BACK_NAME $JAVA_BACK_SRC"

# ** Back Python
PY_BACK_NAME=py_$BACK_UP_VERSION.tar
PY_EXCLUDES="--exclude=__pycache__ --exclude=env/ --exclude=venv/ --exclude=*.pyc"
echo "Back Python Source Files($PY_BACK_SRC) to $BACKUP_DEST/$PY_BACK_NAME   ...."
echo "-----------------------------------------------"
sh -c "tar -cv $COMMON_EXCLUDES $EXTRA_EXCLUDES $PY_EXCLUDES -f $BACKUP_DEST/$PY_BACK_NAME $PY_BACK_SRC"

# ** Back JS
JS_BACK_NAME=js_$BACK_UP_VERSION.tar
JS_EXCLUDES="--exclude=node_modules/"
echo "Back Javascript Source Files($JS_BACK_SRC) to $BACKUP_DEST/$JS_BACK_NAME   ...."
echo "-----------------------------------------------"
sh -c "tar -cv $COMMON_EXCLUDES $EXTRA_EXCLUDES $JS_EXCLUDES -f $BACKUP_DEST/$JS_BACK_NAME $JS_BACK_SRC"

# ** Back shell
SHELL_BACK_NAME=shell_$BACK_UP_VERSION.tar
SHELL_EXCLUDES="--exclude=*.elc  --exclude=elpa/ --exclude=elpa-devel/"
echo "Back Shell Source Files($SHELL_BACK_SRC) to $BACKUP_DEST/$SHELL_BACK_NAME   ...."
echo "-----------------------------------------------"
sh -c "tar -cv $COMMON_EXCLUDES $EXTRA_EXCLUDES $SHELL_EXCLUDES -f $BACKUP_DEST/$SHELL_BACK_NAME $SHELL_BACK_SRC"

# ** Back c
C_BACK_NAME=c_$BACK_UP_VERSION.tar
C_EXCLUDES="--exclude=.cquery-cache/"
echo "Back C Source Files($C_BACK_SRC) to $BACKUP_DEST/$C_BACK_NAME   ...."
echo "-----------------------------------------------"
sh -c "tar -cv $COMMON_EXCLUDES $EXTRA_EXCLUDES $C_EXCLUDES -f $BACKUP_DEST/$C_BACK_NAME $C_BACK_SRC"

# ** Back Misc
MISC_BACK_NAME=misc_$BACK_UP_VERSION.tar
MISC_SRC_EXCLUDES="--exclude=$GO_BACK_SRC --exclude=JAVA_BACK_SRC --exclude=$JS_BACK_SRC --exclude=$PY_BACK_SRC --exclude=$C_BACK_SRC --exclude=$SHELL_BACK_SRC"
MISC_EXCLUDES="$GO_EXCLUDES $JAVA_EXCLUDES $JS_EXCLUDES $SHELL_EXCLUDES $C_EXCLUDES $PY_EXCLUDES $MISC_SRC_EXCLUDES --exclude=.stack-work/"
echo "Back Misc Source Files($MISC_BACK_SRC) to $BACKUP_DEST/$MISC_BACK_NAME   ...."
echo "-----------------------------------------------"
sh -c "tar -cv $COMMON_EXCLUDES $EXTRA_EXCLUDES $MISC_EXCLUDES -f $BACKUP_DEST/$MISC_BACK_NAME $MISC_BACK_SRC"

# ** Back Picture
PICTURE_BACK_NAME=picture_$BACK_UP_VERSION.tar
PICTURE_EXCLUDES=""
echo "Back PICTURE Source Files($PICTURE_BACK_SRC) to $BACKUP_DEST/$PICTURE_BACK_NAME   ...."
echo "-----------------------------------------------"
sh -c "tar -cv $COMMON_EXCLUDES $EXTRA_EXCLUDES $PICTURE_EXCLUDES -f $BACKUP_DEST/$PICTURE_BACK_NAME $PICTURE_BACK_SRC"

# ** Back Imdata
IMDATA_BACK_NAME=imdata_$BACK_UP_VERSION.tar
IMDATA_EXCLUDES="$GO_EXCLUDES $JS_EXCLUDES $SHELL_EXCLUDES $PY_EXCLUDES $C_EXCLUDES"
echo "Back IMDATA Source Files($IMDATA_BACK_SRC) to $BACKUP_DEST/$IMDATA_BACK_NAME   ...."
echo "-----------------------------------------------"
sh -c "tar -cv $COMMON_EXCLUDES $EXTRA_EXCLUDES $IMDATA_EXCLUDES -f $BACKUP_DEST/$IMDATA_BACK_NAME $IMDATA_BACK_SRC"
