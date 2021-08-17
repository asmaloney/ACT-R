#!/bin/sh

## 
## Hedderik van Rijn, ACT-R OS X startup script
## 
## Modified by Dan Bothell to make it a .command file
## and have it change the terminal's title directly
## which removes the need for the separate .term file
## and lets the ACT-R files run from anywhere instead
## of requiring they be in Applications.
##
## Path discovery code based on: 
##
## From: Ashwin Baskaran (no_spam@cisco.com)
## Subject: Re: Extracting the location of a script from its invocation 
## Newsgroups: comp.unix.shell
## Date: 2001-09-19 10:45:58 PST 
##

mypath=${0-.}
if expr "${mypath}" : '.*/.*' > /dev/null
then
    :
else
    IFS="${IFS=  }"; save_ifs="$IFS"; IFS="${IFS}:"
    for dir in $PATH
    do
 test -z "$dir" && dir=.
 if test -x "$dir/$mypath"
 then
     mypath=$dir/$mypath
     break
 fi
    done
    IFS="$save_ifs"
fi
execpath=`echo "${mypath}" | sed  -e 's@/[^/]*$@@'`

cd "$execpath"	

## Change the terminal's title
echo -n -e "\033]0;ACT-R Environment\007"

./start-environment-osx > /dev/null 2> /dev/null &
