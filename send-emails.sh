#!/usr/bin/env bash
#SUBJECT="Subject: new blog post just dropped!\nContent-Type: text/html\n\n"
HEADER="Content-Type: text/html\n\n"
UNSUBCRIBE="to unsubscribe, click this <a href=\"ka.dreadmaw.industries/unsubscribe.html\">link</a>"
CONTENT=$(cat -)
ARR=("msmtp" "-a" "gmail")
echo "sending emails..."
for EMAIL in $(cat emails.txt); do
    ARR+=( $EMAIL )
done
printf "$HEADER$content\n\n$UNSUBCRIBE" | xargs -a <(cat emails.txt) msmtp -a gmail
