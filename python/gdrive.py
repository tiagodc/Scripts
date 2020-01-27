from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
import re, os, datetime, time
import slam_api

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

driveId = '0ADkpMc84zyxQUk9PVA'
bagDir = '1v-_wd208p1JzrL9_TPafPUZ0Hm1h5CGe'
lazDir = '1M5Q_TceP_jML3Z98grSVkyNX2I51C9bP'

opt = {
    'q': "mimeType = 'application/vnd.google-apps.folder' and title = 'laz'",
    "driveId": driveId,
    "includeItemsFromAllDrives": True,
    "supportsAllDrives": True,
    "corpora": "drive"
    }

while True:

    time.sleep(1)

    try:
        drive = authenticate()
    except:
        continue
    
    opt["q"] = "mimeType = 'application/vnd.google-apps.folder' and title = 'laz'"
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
        
        local_laz = 'laz/' + base + '.laz'
        local_txt = 'txt/' + base + '.txt'

        try:
            side_prods = slam_api.runSLAM(local_bag, local_laz, local_txt, 30, 1)
        except:
            continue

        try:
            laz = drive.CreateFile({'uploadType':'resumable', 'parents': dir_list})
            laz.SetContentFile(local_laz)
            
            laz['title'] = re.sub('^laz/', '', laz['title'])
            
            laz.Upload({'supportsAllDrives': True})

            for sp in side_prods:
                os.unlink(sp)

        except:
            continue
