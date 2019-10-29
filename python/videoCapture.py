import cv2, os, datetime, sys

helpMsg = '''uso:
    python videoCapture.py <nome_do_arquivo> <tempo_de_coleta> <ip_da_camera> <login> <senha>
    
exemplo:
    python videoCapture.py meuvideo 30 192.168.0.50 admin pass#1234
        
output: meuvideo.mp4, com ~30s de duração
'''

if len(sys.argv) < 4:
    sys.exit(helpMsg)

login = ''
if(len(sys.argv) > 4):
    login = sys.argv[4] + r':' + sys.argv[5] + r'@'

address = r'rtsp://' + login + sys.argv[3]
filename = sys.argv[1] + r'.mp4'
recordTime = int(sys.argv[2]) # seconds

cap = cv2.VideoCapture(address)

if not cap.isOpened():
    sys.exit("Não foi possível abrir captura de vídeo para " + address)

out = cv2.VideoWriter( filename, cv2.VideoWriter_fourcc(*'mp4v'), cap.get(5) , (int(cap.get(3)), int(cap.get(4))) )

print('streaming ' + address)

t0 = datetime.datetime.now()
while True:
    ret, frame = cap.read()

    out.write(frame)
    tdiff = datetime.datetime.now() - t0
    
    if tdiff.total_seconds() > recordTime:
        break

    # cv2.imshow("capture", frame)
    # if cv2.waitKey(1) & 0xFF == ord('q'):
    #     break

cap.release()
out.release()
cv2.destroyAllWindows()
