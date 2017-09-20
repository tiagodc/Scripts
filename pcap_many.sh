#!/bin/bash

#  give permission
## chmod u+x pcap_many

echo "começar em (s): "
read inicio

echo "tempo de espera (s): "
read espera
#espera=$((espera))

echo "tempo de coleta (min): "
read tempo
tempo=$((tempo*60))

echo "intervalo de coleta (s): "
read intervalo
#inervalo=$((intervalo))

timer=0
readTime=2

sleep $inicio

while ((timer <= tempo))
	do
	
		file="n"$timer".pcap"

		sleep $espera && timeout $readTime tcpdump src 192.168.1.201 and port 2368 -w $file && echo $file "salvo"

		timer=$((timer+espera+readTime))

	done

