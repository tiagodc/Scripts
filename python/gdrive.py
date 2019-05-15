from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
import re, os, datetime, time

def authenticate(credentials_path = "mycreds.txt"):
    gauth = GoogleAuth()
    gauth.LoadCredentialsFile(credentials_path)

    if gauth.credentials is None:
        gauth.LocalWebserverAuth()
    elif gauth.access_token_expired:
        gauth.Refresh()
    else:
        gauth.Authorize()

    gauth.SaveCredentialsFile(credentials_path)

    drive = GoogleDrive(gauth)
    return drive

driveId = '0AOS7p2Q6ZZvUUk9PVA'
bagDir = '18JoqVGuoi9Dduc_aEbSW7-6n5YX7oqgg'
lazDir = '1EF6MO2NQDG8GtU8KxbVCf_9L-fyesLtF'

opt = {
    'q': "mimeType = 'application/vnd.google-apps.folder' and title = 'laz'",
    "driveId": driveId,
    "includeItemsFromAllDrives": True,
    "supportsAllDrives": True,
    "corpora": "drive"
    }

while True:

    time.sleep(10 * 60)

    try:
        drive = authenticate()
    except:
        continue

    try:
        dir_list = drive.ListFile(opt).GetList()
    except:
        continue

    opt['q'] = "'{}' in parents and trashed = false and title contains '.bag'".format(bagDir)
    try:
        bag_list = drive.ListFile(opt).GetList()
    except:
        continue

    opt['q'] = "'{}' in parents and trashed = false and title contains '.laz'".format(lazDir)
    try:
        laz_list = drive.ListFile(opt).GetList()
    except:
        continue

    base_names = [re.sub(r'\.laz$', '', i['title']) for i in laz_list]

    for bag in bag_list:
        base = re.sub(r'\.bag$','',bag['title'])
        
        if base in base_names:
            continue
        
        local_bag = 'bags/' + bag['title']
        if os.path.exists(local_bag):
            os.unlink(local_bag)

        try:
            bag.GetContentFile(local_bag)
        except:
            continue
        
        ### call slam and upload laz file
        
        #### SLAM ####
        
        local_laz = 'laz/' + base + '.laz'
        try:
            laz = drive.CreateFile({'uploadType':'resumable', 'parents': dir_list})
            laz.SetContentFile(local_laz)
            laz.Upload({'supportsAllDrives': True})
        except:
            continue
