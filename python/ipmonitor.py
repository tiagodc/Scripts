import yagmail, time, os
from requests import get

wait = 2

while True:

    time.sleep(wait)

    if not os.path.exists('ip.txt'):
        ipfile = open('ip.txt', 'w')
        ipfile.close()

    ipfile = open('ip.txt', 'r')
    oldip = ipfile.read()
    ipfile.close()

    try:
        ip = get('https://api.ipify.org').text
    except Exception as error:
        ip = None
        print(error)

    if ip == None:
        continue
    elif oldip == ip:
        continue

    try: 
        contents = ["IP WAN novo:", ip]
        yagmail.SMTP(r'me@smth.com', r'#pass').send(r'to@gmail.com', 'IP casa', contents)
    except:
        contents = None

    if contents == None:
        continue

    ipfile = open('ip.txt', 'w')
    ipfile.write(ip)
    ipfile.close()