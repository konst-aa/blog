#!/usr/bin/env bash
source env.sh
./server &
../proxy -cert "/etc/letsencrypt/live/$ADDRESS/fullchain.pem" -key "/etc/letsencrypt/live/$ADDRESS/privkey.pem" -from 0.0.0.0:443 -to 127.0.0.1:8080 &
disown -a
