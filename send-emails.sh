#!/usr/bin/env bash

SUBJECT=$(cat -)
HEADER="Subject: $SUBJECT\nContent-Type: text/html\n\n"
UNSUBSCRIBE="to unsubscribe, click this <a href=\"ka.dreadmaw.industries/unsubscribe.html\">link</a>"
CONTENT=$(cat -)

ARR=("msmtp" "-a" "gmail")
echo "sending emails..."
for EMAIL in $(cat emails.txt); do
    ARR+=( $EMAIL )
done

TEMP=$(printf "$CONTENT\n$UNSUBSCRIBE" | sed -z "s/\n/<br>/g")
printf "$HEADER$TEMP" | xargs -a <(cat emails.txt) msmtp -a gmail
