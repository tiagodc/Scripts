#!/bin/bash

#  give permission
## sudo chmod u+x pcap_many.sh

echo "tempo de espera antes de começar (s): "
read inicio
#inicio=10

echo "tempo de espera entre coletas (s): "
read espera
#espera=1

echo "duração da coleta (min): "
read tempo
#tempo=5
tempo=$((tempo*60))

timer=0
readTime=2

sleep $inicio

while ((timer <= tempo))
	do
	
		file="n"$timer".pcap"

		sleep $espera && timeout $readTime tcpdump src 192.168.1.201 and port 2368 or port 8308 -w $file && echo $file "salvo"

		timer=$((timer+espera+readTime))

	done

