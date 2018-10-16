#!/usr/bin/env csh
if (! $?OSOAA_ROOT) then
    cd ..
    setenv OSOAA_ROOT `pwd`
    echo "Warning: OSOAA_ROOT not found..., assuming $OSOAA_ROOT as \$OSOAA_ROOT"
    cd bin
endif

set JAVA_CAND=`which java`

if (! $?JAVA_CAND) then
    if (! $?JAVA_HOME) then
        echo "Error: JAVA_HOME environment variable is not defined: OSOAAUI.jar looks for java on \$JAVA_HOME/bin/java"
    	exit
    endif
else
    if (! -e $JAVA_CAND) then
        set JAVA_CAND=`which java` | tail -1
        if (! -e $JAVA_CAND) then
            echo "Warning: java not found"
            if (! $?JAVA_HOME) then
                echo "Error: JAVA_HOME environment variable is not defined: OSOAAUI.jar looks for java on \$JAVA_HOME/bin/java"
                exit
            endif
        endif
    endif
endif

if (! $?JAVA_HOME) then
    set JAVA_CAND=`which java`
    if (! -e $JAVA_CAND) then
        set JAVA_CAND=`which java` | tail -1
    endif
    $JAVA_CAND -version >& /tmp/t1.txt
    cat /tmp/t1.txt | sed 's/java version "\(.*\)\.\(.*\)\..*"/\1\2/; 1q' >& /tmp/t2.txt
    cat /tmp/t1.txt | grep HotSpot >& /tmp/t3.txt
    set JAVA_VER=`cat /tmp/t2.txt`
    set SUN_VM=`cat /tmp/t3.txt`
    rm /tmp/t1.txt
    rm /tmp/t2.txt
    rm /tmp/t3.txt
    set num=16
    if ($JAVA_VER < $num) then
        echo "Error: Java version lesser than 1.6 !!"
        exit
    endif
    if (! $?SUN_VM) then
        echo "Error: This is not an Oracle JVM !"
        exit
    endif
else
    $JAVA_HOME/bin/java -version >& /tmp/t1.txt
    cat /tmp/t1.txt | sed 's/java version "\(.*\)\.\(.*\)\..*"/\1\2/; 1q' >& /tmp/t2.txt
    cat /tmp/t1.txt | grep HotSpot >& /tmp/t3.txt
    set JAVA_VER=`cat /tmp/t2.txt`
    set SUN_VM=`cat /tmp/t3.txt`
    rm /tmp/t1.txt
    rm /tmp/t2.txt
    rm /tmp/t3.txt
    set num=16
    if ($JAVA_VER < $num) then
        echo "Error: Java version lesser than 1.6 !!"
        exit
    endif
    if (! $?SUN_VM) then
        echo "Error: This is not an Oracle JVM !"
        exit
    endif
endif

if (! -e $OSOAA_ROOT/inc/OSOAA.h) then
    echo "Error: File not found: \$OSOAA_ROOT/inc/OSOAA.h"
    exit
endif

if (! -e $OSOAA_ROOT/fic) then
    echo "Error: Directory not found: \$OSOAA_ROOT/fic"
    exit
endif

if (! -e $OSOAA_ROOT/exe/OSOAA_MAIN.exe) then
    echo "Error: Exe file not found: \$OSOAA_ROOT/exe/OSOAA_MAIN.exe"
    exit
endif

if (! -e $OSOAA_ROOT/log4j.properties) then
    echo "Error: Log4j configuration file not found..."
    exit
endif

if (! -e ../lib) then
    echo "Error: Missing lib directory - OSOAAUI.jar assumes .jar libs are placed on ../lib"
    exit
endif

if (! -e ../lib/forms-1.3.0.jar) then
    echo "Error: Missing jar file - OSOAAUI.jar assumes .jar libs are placed on ../lib"
    exit
endif

if (! -e ../lib/log4j-1.2.17.jar) then
    echo "Error: Missing jar file - OSOAAUI.jar assumes .jar libs are placed on ../lib"
    exit
endif


if (! -e ../lib/miglayout15-swing.jar) then
    echo "Error: Missing jar file - OSOAAUI.jar assumes .jar libs are placed on ../lib"
    exit
endif

if ($?JAVA_HOME) then
    set JAVA_CAND=`which java`
    if (! -e $JAVA_CAND) then
        set JAVA_CAND=`which java` | tail -1
    endif
    $JAVA_CAND -Dlog4j.configuration=file:$OSOAA_ROOT/log4j.properties -jar OSOAAUI.jar osoaa.Launcher
    # $JAVA_CAND -jar OSOAAUI.jar osoaa.Launcher
else
    $JAVA_HOME/bin/java -Dlog4j.configuration=file:$OSOAA_ROOT/log4j.properties -jar OSOAAUI.jar osoaa.Launcher
    # $JAVA_HOME/bin/java -jar OSOAAUI.jar osoaa.Launcher
endif
