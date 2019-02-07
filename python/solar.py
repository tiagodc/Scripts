import processing as pr, os, re

os.chdir(r"E:\case_als\CLOUDS\2013\FarmB")
layersDir = r'3.dtm_derived'

files = os.listdir('3.dtm')
if not os.path.exists(layersDir ):
    os.mkdir(layersDir )

files = [f for f in  files if re.match(r'.+\.tif$', f) is not None]

for f in files:
    
    print("file: "+f)
    
    obj = pr.getObject("3.dtm/"+f)
    ext = obj.extent()
    ext = "{},{},{},{}".format(ext.xMinimum(), ext.xMaximum(), ext.yMinimum(), ext.yMaximum())

    slope = layersDir + r"/" + re.sub(r"dtm", r"slope", f)
    aspect = layersDir + r"/" + re.sub(r"dtm", r"aspect", f)

    print("... running aspect/slope")
    asp_slp = pr.runalg("grass7:r.slope.aspect", obj, 0, 0,True,1,0,ext,0,slope,aspect,None,None,None,None,None,None,None)

    for day in [1,180]:
        sunLayer = layersDir + r"/" + re.sub(r"dtm", r"sun_d" + str(day), f)

        irradiation = re.sub("sun","irradiation", sunLayer)
        insolation = re.sub("sun","insolation", sunLayer)
        diffuse = re.sub("sun","diffuse",sunLayer)
        reflected = re.sub("sun","reflected",sunLayer)
        globalrad = re.sub("sun","global",sunLayer)

        print("... running solar radiation for day " + str(day))
        sun = pr.runalg("grass7:r.sun", obj, aspect, slope, None, None, None, None, None, None, day, 1, 0, 1, False, False, ext, 0, irradiation, insolation, diffuse, reflected, globalrad)