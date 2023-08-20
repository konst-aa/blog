#!/usr/bin/env bash
./server &
../proxy -cert "/etc/letsencrypt/live/$HOSTNAME/fullchain.pem" -key "/etc/letsencrypt/live/$HOSTNAME/privkey.pem" -from 0.0.0.0:4430 -to 127.0.0.1:8080
