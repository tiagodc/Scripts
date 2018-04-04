@set folder=%1
for %%G in ("%folder%/"*.pcap) do pcap2cloud -i "%folder%/%%G" -v -s 0.33 -t