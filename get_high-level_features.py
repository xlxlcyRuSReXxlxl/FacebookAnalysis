# These functions apply as well on those contexts in which we have several folders containing photos and we want to get back a dataframe including all their high-level
# features. Particulary, these lines provide a framework to extract from the folders the outputs of Vgg16, Place365 and Yolo neural networks.  


#' @name yolov2_vgg16_place365
#' @details get infos about comments of the photos of a profile.
#' @export 
#' @author Ciro Lista
#' @param string
#' @return a dataframe with n rows, one for each photo with the following values: 
#'  #'   "ImagePath"        "vgg16lab"     "ObjectsDetected"  "BlurIndex"   "Scene"   "IndooroOutdoor"
#'    the column "vgg16lab" contains 1000 n-dimensional vectors, whose each one provides the estimated probabilities of an image belonging to a given class 




import os
import csv
os.chdir('/usr/local/lib/python3.5/dist-packages')
import numpy as np
import pandas as pd
import cv2
import argparse
import imutils
os.chdir('/usr/local/lib/python3.5/dist-packages/tensorflow/contrib/keras/api')
from keras.applications import ResNet50
from keras.applications import InceptionV3
from keras.applications import Xception # TensorFlow ONLY
from keras.applications import VGG16
from keras.applications import VGG19
from keras.applications import imagenet_utils
from keras.applications.inception_v3 import preprocess_input
from keras.preprocessing.image import img_to_array
from keras.preprocessing.image import load_img
import sys
import numpy as np
import objectpath
import os
import functools
import csv
import glob 
import argparse 
import pandas as pd
from imutils import paths
from objectpath import *
import torch
from torch.autograd import Variable as V
import torchvision.models as models
from torchvision import transforms as trn
from torch.nn import functional as F
from PIL import Image
from scipy.misc import imresize as imresize
from scipy.misc import imread, imsave
from functools import reduce
os.chdir('/usr/local/lib/python3.5/dist-packages/darkflow')
from darkflow.net.build import TFNet



ap = argparse.ArgumentParser()
ap.add_argument("-i", "--image",help="path to the input image")
ap.add_argument("-model", "--model", type=str, default="vgg16",help="name of pre-trained network to use")
args = vars(ap.parse_args())
MODELS = {
    "vgg16": VGG16,
    "vgg19": VGG19,
    "inception": InceptionV3,
    "xception": Xception, 
    "resnet": ResNet50
}
if args["model"] not in MODELS.keys():
    raise AssertionError("The --model command line argument should " "be a key in the `MODELS` dictionary")

inputShape = (224, 224)
preprocess = imagenet_utils.preprocess_input
Network = MODELS[args["model"]]
model = Network(weights="imagenet")


options = {"model": "/home/ciro/darkflow/cfg/tiny-yolo-voc.cfg", "load": "/home/ciro/darkflow/darkflow/weights/tiny-yolo-voc.weights", "threshold": 0.25}
tfnet = TFNet(options)

def vgg19_classify(string): 
    os.chdir('/home/ciro')
    with open('synset.csv', 'rt') as csvfile:
        labels = csv.reader(csvfile)
        a=list(labels)
        AA=np.asarray(a)[:,0]
        os.chdir(string)
        folders=[d for d in os.listdir(os.getcwd()) if os.path.isdir(d)]
        folders=sorted(folders)
        Nfolders=len(folders)
        for j in range(Nfolders):
            wd='/'.join([string,folders[j]])
            os.chdir(wd)
            pattern = '*.jpg'
            jpgFiles=glob.glob(pattern)
            nFiles=len(jpgFiles)
            df = pd.DataFrame(0, columns=AA,index=range(nFiles))
            for idx in df.index:
                path='/'.join([wd,jpgFiles[idx]])
                image = load_img(path, target_size=inputShape)
                image = img_to_array(image)
                image = np.expand_dims(image, axis=0)
                image = preprocess(image)
                preds = model.predict(image)
                centrality=sorted(preds[0,:],reverse=True)[0]>0.90
                predsDF=pd.DataFrame(preds[0,]).T
                df.iloc[idx,0]=path
                df.iloc[idx,1]=centrality
                for idxI in range(1000):
                    df.iloc[idx,idxI+2]=predsDF.iloc[0,idxI]       
            df.to_csv('.'.join(['/'.join([string,'.'.join([str(j),'clas'])]),'csv']),index=False,sep=';')
        os.chdir(string)
        csv_files = sorted(glob.glob("*clas.csv"))
        header_saved = True
        with open('classFILE.csv','a') as fout:
            writer = csv.DictWriter(fout, fieldnames =AA,delimiter = ';')
            writer.writeheader()
            for filename in csv_files:
                with open(filename) as fin:
                    header = next(fin)
                    if not header_saved:
                        fout.write(header)
                        header_saved = True
                    for line in fin:
                        fout.write(line)



def yolov2_detection(string): 
    os.chdir(string)
    folders=[d for d in os.listdir(os.getcwd()) if os.path.isdir(d)]
    folders=sorted(folders)
    Nfolders=len(folders)
    for j in range(Nfolders):
        wd='/'.join([string,folders[j]])
        os.chdir(wd)
        pattern = '*.jpg'
        jpgFiles=glob.glob(pattern)
        nFiles=len(jpgFiles)
        df = pd.DataFrame(0, columns=['ImagePath', 'ObjectsDetected','BlurIndex'],
        index=range(nFiles))
        for idx in df.index:
            path='/'.join([wd,jpgFiles[idx]])
            imgcv = cv2.imread(path)
            result = tfnet.return_predict(imgcv)
            tree_obj =objectpath.Tree(result)
            labels='--'.join((tree_obj.execute('$..label')))
            imagCol = cv2.cvtColor(imgcv, cv2.COLOR_BGR2GRAY)
            blur_map = cv2.Laplacian(imgcv, cv2.CV_64F)
            score = np.var(blur_map)    
            df['ImagePath'].iloc[idx]=path
            df['ObjectsDetected'].iloc[idx]=labels
            df['BlurIndex'].iloc[idx]=score
        df.to_csv('.'.join(['/'.join([string,'.'.join([str(j),'det'])]),'csv']),index=False,sep=';')
    os.chdir(string)
    csv_files = sorted(glob.glob("*det.csv"))
    header_saved = True
    with open('objdetFILE.csv','a') as fout:
        writer = csv.DictWriter(fout, fieldnames=
        ['ImagePath','ObjectsDetected','BlurIndex'], 
        delimiter = ';')
        writer.writeheader()
        for filename in csv_files:
            with open(filename) as fin:
                header = next(fin)
                if not header_saved:
                    fout.write(header)
                    header_saved = True
                for line in fin:
                    fout.write(line)

  
def place_detection(string): 
    os.chdir('/usr/local/lib/python3.5/dist-packages/torchvision')
    arch = 'resnet18'
    model_weight = 'whole_%s_places365.pth.tar' % arch
    model = torch.load(model_weight)
    centre_crop = trn.Compose([
            trn.Scale(256),
            trn.CenterCrop(224),
            trn.ToTensor(),
            trn.Normalize([0.485, 0.456, 0.406], [0.229, 0.224, 0.225])])
    file_name = 'categories_places365.txt'
    classes = list()
    with open(file_name) as class_file:
        for line in class_file:
            classes.append(line.strip().split(' ')[0][3:])
    classes = tuple(classes)
    file_name_IO = 'IO_places365.txt'
    with open(file_name_IO) as f:
        lines = f.readlines()
        labels_IO = []
        for line in lines:
            items = line.rstrip().split()
            labels_IO.append(int(items[-1]) -1) 
    labels_IO = np.array(labels_IO)
    os.chdir(string)
    folders=[d for d in os.listdir(os.getcwd()) if os.path.isdir(d)]
    folders=sorted(folders)
    Nfolders=len(folders)
    for j in range(Nfolders):
        wd='/'.join([string,folders[j]])
        os.chdir(wd)
        pattern = '*.jpg'
        jpgFiles=glob.glob(pattern)
        nFiles=len(jpgFiles)
        df = pd.DataFrame(0, columns=['ImagePath', 'Scene','IndooroOutdoor'],
        index=range(nFiles))
        for idX in df.index:
            try:
                path='/'.join([wd,jpgFiles[idX]])
                img = imread(path)
                if (len(img.shape)<3):
                    df['ImagePath'].iloc[idX]=path
                    df['Scene'].iloc[idX]="NA"
                    df['IndooroOutdoor'].iloc[idX]="NA"
                else:
                    img = Image.open(path)
                    input_img = V(centre_crop(img).unsqueeze(0), volatile=True)
                    logit = model.forward(input_img)
                    h_x = F.softmax(logit).data.squeeze()
                    probs, idx = h_x.sort(0, True)
                    scene=classes[idx[0]]
                    io_image = np.mean(labels_IO[idx[:10].numpy()])
                    if probs[0]>0.3:
                        if io_image < 0.5:
                            io_image='indoor'
                        else:
                            io_image='outdoor'
                        df['ImagePath'].iloc[idX]=path
                        df['Scene'].iloc[idX]=scene
                        df['IndooroOutdoor'].iloc[idX]=io_image
                    else:
                        df['ImagePath'].iloc[idX]=path
                        df['Scene'].iloc[idX]='NoPlace'
                        df['IndooroOutdoor'].iloc[idX]='NA'
            except:
                print('errore gestito')      
        df.to_csv('.'.join(['/'.join([string,'.'.join([str(j),'place'])]),'csv']),index=False,sep=';')
    os.chdir(string)
    csv_files = sorted(glob.glob("*place.csv"))
    header_saved = True
    with open('placedetFILE.csv','a') as fout:
        writer = csv.DictWriter(fout, fieldnames=
        ['ImagePath','Scene','IndooroOutdoor'], 
        delimiter = ';')
        writer.writeheader()
        for filename in csv_files:
            with open(filename) as fin:
                header = next(fin)
                if not header_saved:
                    fout.write(header)
                    header_saved = True
                for line in fin:
                    fout.write(line) 
                   




def yolov2_vgg16_place365(string):
    vgg19_classify(string)
    yolov2_detection(string)
    place_detection(string)
    os.chdir(string)
    classFILE=pd.read_csv('classFILE.csv',sep=";")
    objdetFILE=pd.read_csv('objdetFILE.csv',sep=";")     
    placedetFILE=pd.read_csv('placedetFILE.csv',sep=";")
    dfs = [classFILE, objdetFILE, placedetFILE]
    DFmerge = reduce(lambda  left,right: pd.merge(left,right,on=['ImagePath'],
                                            how='outer'), dfs)
    DFmerge.sort_values(['ImagePath'],axis=0,ascending=True,inplace=True)    
    DFmerge.to_csv('/'.join([string,('.'.join([string.split("/")[5],'PY','csv']))]),index=False,sep=';')
    MERGEcsv=glob.glob('*.PY.csv')
    ALLFILEScsv=glob.glob('*.csv')
    ULFILES=list(set(ALLFILEScsv)-set(MERGEcsv))
    for f in ULFILES:
        os.remove(f)





