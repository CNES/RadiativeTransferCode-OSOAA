#!/usr/bin/env bash

if [ -z "$OSOAA_ROOT" ]
then
    cd ..
	export OSOAA_ROOT=`pwd`
	echo "Warning: OSOAA_ROOT not found..., assuming $OSOAA_ROOT as \$OSOAA_ROOT"
    cd bin
fi

if [ -z `which java` ]
then
    if [ -z "$JAVA_HOME" ]
    then
        echo "Error: JAVA_HOME environment variable is not defined: OSOAAUI.jar looks for java binary on \$JAVA_HOME/bin/java"
    	exit
    fi
else
    JAVA_CAND=`which java`
    if [ ! -e "$JAVA_CAND" ]
    then
        JAVA_CAND=`which java` | tail -1
        if [ ! -e "$JAVA_CAND" ]
        then
            echo "Warning: java not found"
            if [ -z "$JAVA_HOME" ]
            then
                echo "Error: JAVA_HOME environment variable is not defined: OSOAAUI.jar looks for java binary on \$JAVA_HOME/bin/java"
                exit
            fi
        fi
    fi
fi
if [ -z "$JAVA_HOME" ]
then
    JAVA_CAND=`which java`
    if [ ! -e "$JAVA_CAND" ]
    then
        JAVA_CAND=`which java` | tail -1
    fi 

    JAVA_VER=$(java -version 2>&1 | awk -F '"' '/version/ {print $2}')
    JAVA_VER_MAJOR=$(echo $JAVA_VER | cut -d "." -f 1)
    echo "Java major version found : $JAVA_VER_MAJOR"
    #End of change
    if [ "$JAVA_VER_MAJOR" -lt 16 ];
    then
        echo "Error: Java version lesser than 1.6 !!"
        exit
    fi
else    
    JAVA_VER=$(java -version 2>&1 | awk -F '"' '/version/ {print $2}')
    JAVA_VER_MAJOR=$(echo $JAVA_VER | cut -d "." -f 1)
    SUN_VM=$($JAVA_CAND -version 2>&1 | grep HotSpot)

    echo "Java major version found : $JAVA_VER_MAJOR"
    if [ "$JAVA_VER_MAJOR" -lt 16 ]
    then
        echo "Error : Java version lesser than 1.6 !!"
        exit
    fi
fi

if [ ! -e "$OSOAA_ROOT/inc/OSOAA.h" ]
then
    echo "Error: File not found: \$OSOAA_ROOT/inc/OSOAA.h"
    exit
fi

if [ ! -e "$OSOAA_ROOT/fic" ]
then
    echo "Error: Directory not found: \$OSOAA_ROOT/fic"
    exit
fi

if [ ! -e "$OSOAA_ROOT/exe/OSOAA_MAIN.exe" ]
then
    echo "Error: Exe file not found: \$OSOAA_ROOT/exe/OSOAA_MAIN.exe"
    exit
fi

if [ ! -e "$OSOAA_ROOT/log4j.properties" ]
then
    echo "Error: Log4j configuration file not found..."
    exit
fi

if [ ! -e "../lib" ]
then
    echo "Error: lib directory not found - OSOAAUI.jar assumes .jar libs are placed on ../lib"
    exit
fi

if [ ! -e "../lib/forms-1.3.0.jar" ]
then
    echo "Error: missing jar file - OSOAAUI.jar assumes .jar libs are placed on ../lib"
    exit
fi

if [ ! -e "../lib/log4j-1.2.17.jar" ]
then
    echo "Error: missing jar file - OSOAAUI.jar assumes .jar libs are placed on ../lib"
    exit
fi


if [ ! -e "../lib/miglayout15-swing.jar" ]
then
    echo "Error: missing jar file - OSOAAUI.jar assumes .jar libs are placed on ../lib"
    exit
fi

if [ -z "$JAVA_HOME" ]
then
    JAVA_CAND=`which java`
    if [ ! -e "$JAVA_CAND" ]
    then
        JAVA_CAND=`which java` | tail -1
    fi
    $JAVA_CAND -Dlog4j.configuration=file:$OSOAA_ROOT/log4j.properties -jar OSOAAUI.jar osoaa.Launcher
    # $JAVA_CAND -jar OSOAAUI.jar osoaa.Launcher
else
    $JAVA_HOME/bin/java -Dlog4j.configuration=file:$OSOAA_ROOT/log4j.properties -jar OSOAAUI.jar osoaa.Launcher
    # $JAVA_HOME/bin/java -jar OSOAAUI.jar osoaa.Launcher
fi
