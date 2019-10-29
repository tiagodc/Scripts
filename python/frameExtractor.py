import os, re

os.chdir('/home/tiago/Desktop/Sintecsys/videos/')

mp4 = list()
ffmpeg = 'ffmpeg -i "{}" -r 1/1 "{}"/$filename%03d.bmp'

for root, dirs, files in os.walk('.'):
    for f in files:
        if re.match(r'.+\.mp4$', f, re.DOTALL) is not None:
            fullpath = "{}/{}".format(root,f)
            f2 = re.sub(r'\.mp4$', '_frames', fullpath, 0, re.DOTALL)
            os.mkdir(f2)
            
            print('###')
            print(f2)

            os.system( ffmpeg.format(fullpath,f2) )

            