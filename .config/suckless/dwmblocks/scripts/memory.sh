#! /bin/sh

mem="$(grep -i 'memory' /var/run/dmesg.boot | awk -F '[^0-9]*' '{print $3}' | awk -v ORS='/' '{getline x; print x;}1' | awk '{print substr($NF, 1, length($NF)-1)}')"
echo -e "$mem RAM "
