import glob
import os
import torch
import random
from pathlib import Path
from torch.utils.data import DataLoader
import torch.nn.functional as F
import torch.nn as nn
import torchvision
import shutil
import matplotlib
matplotlib.use('pdf')
import matplotlib.pyplot as plt
from   torch.autograd import Variable
import torch.optim as optim
from  torchvision import transforms
import numpy as np
import pandas as pd
from radam import *
from sklearn.cluster import *
import pickle
import json
from sklearn.metrics import silhouette_samples, silhouette_score
from sklearn.decomposition import PCA
from sklearn.manifold import TSNE
from Nets import AutoEncoderA
from dataset import PathogenTreeDataset
#from helper import visualize_feature_map, get_kernel_image

random.seed(17)
Net='A16_final_2'

os.mkdir('~/Net'+str(Net))
os.chdir('~/Net'+str(Net))

code_size = 256
num_epochs =200
batch_size = 64
lr = 0.00001
optimazeR = RAdam # New RAdam optimizer
optimizer_cls = optim.Adam

IMAGE_WIDTH = IMAGE_HEIGHT = 160
IMAGE_SIZE = IMAGE_WIDTH*IMAGE_HEIGHT

#all the  augmented images
img_dir = Path("~/images_Augmentation")
#non ovaerlapping images
imgO_dir = Path("~/images")
img_all=glob.glob("~/images_Augmentation/*.png")
imgO_all=glob.glob("~/images/*.png")

n=len(img_all)
# number of test/val elements
n_test = 10
n_val=int( n * .1 )
n_train= n - (n_test+n_val)

idx = list(range(n))  # indices to all elements
random.seed(17)
random.shuffle(idx)

train_idx = idx[:n_train]
val_idx = idx[n_train:(n_train + n_val)]
test_idx = idx[(n_train + n_val):(n_train + n_val+10)]

img_train = img_all[:n_train]

img_val = img_all[n_train:(n_train + n_val)]

img_test = img_all[(n_train + n_val):]


def dataset_preparation():

    custom_transform = transforms.Compose([transforms.Grayscale(),
                            transforms.ToTensor()])
    train_dataset = PathogenTreeDataset(img_train, img_dir, custom_transform)

    train_loader = DataLoader(dataset=train_dataset,
                              batch_size=batch_size,
                              shuffle=True,
                              num_workers=4)
    print(len(train_loader))
    custom_transform = transforms.Compose([transforms.Grayscale(),
                                         transforms.ToTensor()])

    test_dataset = PathogenTreeDataset(img_test, img_dir=img_dir, transform=custom_transform)
    print(len(test_dataset))

    test_loader = DataLoader(dataset=test_dataset,
                             batch_size=1,
                             shuffle=True,
                             num_workers=4)
    print(len(test_loader))

    val_dataset = PathogenTreeDataset(img_val, img_dir=img_dir, transform=custom_transform)

    val_loader = DataLoader(dataset=val_dataset,
                            batch_size=batch_size,
                            shuffle=True,
                            num_workers=4)
    print(len(val_loader))

    all_dataset = PathogenTreeDataset(imgO_all, img_dir=imgO_dir, transform=custom_transform)

    all_loader = DataLoader(dataset=all_dataset,
                            batch_size=1,
                            shuffle=False,
                            num_workers=4)
    print(len(all_loader))

    return train_loader, test_loader, val_loader, all_loader


names = [os.path.basename(x) for x in glob.glob('~/images/*.png')]

def main():
    train_loader, test_loader, val_loader, all_loader = dataset_preparation()
    autoencoder = AutoEncoderA(code_size, act='s')

    use_cuda = True

    if use_cuda and torch.cuda.is_available():
        autoencoder.cuda()

    loss_fn = nn.BCELoss()
    optimizer = optimizer_cls(autoencoder.parameters(), lr=lr)
    
    L_train = []
    L_t = []
    L_validation = []
    L_v = []
    for epoch in range(num_epochs):
        print("Epoch %d" % epoch)

        for batch_idx, x in enumerate(train_loader):
            # print(x.shape)
            if use_cuda and torch.cuda.is_available():
                x = x[0].cuda()
            autoencoder.train()
            b_x = x.view(-1, IMAGE_WIDTH * IMAGE_WIDTH)
            out, code = autoencoder(Variable(x))
            optimizer.zero_grad()
            loss = loss_fn(out, b_x)
            L_t.append(loss.data.cpu())
            loss.backward()
            optimizer.step()
        ML_t=np.mean(L_t)
        print("Loss = %.3f" % ML_t)
        L_train.append(ML_t)
        Csize = code.shape

        for batch_idx, x in enumerate(val_loader):
            # print(x.shape)
            if use_cuda and torch.cuda.is_available():
                x = x[0].cuda()
            autoencoder.eval()
            b_x = x.view(-1, IMAGE_WIDTH * IMAGE_WIDTH)
            out, code = autoencoder(Variable(x))

            loss = loss_fn(out, b_x)
            L_v.append(loss.data.cpu())
           
        ML_v=np.mean(L_v)    
        print("Loss = %.3f" % ML_v)
        L_validation.append(ML_v)


    x = np.arange(num_epochs)
    
    df = pd.DataFrame({'x': x, 'y1': L_validation, 'y2': L_train})
    

    # multiple line plot
    plt.plot('x', 'y1', data=df, marker='', color='red', linewidth=2, label="validation")
    plt.plot('x', 'y2', data=df, marker='', color='olive', linewidth=2, linestyle='dashed', label="train")
    plt.title('Net=' + str(Net) + 'code_size=' + str(code_size) + ',' + 'batch_size=' + str(
        batch_size) + ',' + 'learning rate=' + str(lr))
    plt.legend()
    plt.savefig('Net'+str(Net)+'.png')

    # Test the similarity between input and output after train
    im = []
    
    ind = 0
    for i, x in enumerate(test_loader):
        if use_cuda and torch.cuda.is_available():
            im = x[0].cuda()
        test_reconst, compress = autoencoder(Variable(im))
        torchvision.utils.save_image(im.data, 'orig' + '_' + str(ind) + '.png')
        torchvision.utils.save_image(test_reconst.data, 'reconst' + '_' + str(ind) + '.png')

        ind += 1
    return autoencoder

random.seed(17)
ae = main()


train_loader, test_loader, val_loader, all_loader = dataset_preparation()
hold_imgs = []
hold_reconstructions = []
hold_codes = []
hold_names = []
my_dict = {}
ae = ae.cpu()
os.mkdir('~/Net'+str(Net)+'/codes')
for batch_idx, batch in enumerate(all_loader):
    out, code = ae(batch[0])
    name=str(batch[1])
    name=name.split('/', -1)[-1]
    name=name.replace(',)','')
    name=name[:-1]
    my_dict[name] = code
    c = code.view([16,16])   
    torchvision.utils.save_image(c, '~/Net'+str(Net)+'/codes/'+str(name))

    hold_imgs.append(batch)
    hold_reconstructions.append(out)
    hold_codes.append(code)
    hold_names.append(name)


os.chdir('~/Net'+str(Net))
with open('filename.pickle', 'wb') as handle:
        pickle.dump(my_dict, handle, protocol=pickle.HIGHEST_PROTOCOL)

len(hold_imgs), len(hold_reconstructions), len(hold_codes)
stacked_codes = torch.cat(hold_codes, 0)

#=====================================================================================
#clustering on the codes resulted from autoencoder
#=====================================================================================
# with Kmeans
#=====================================================================================
os.mkdir('~/Net' + str(Net) +'/Kmean')
N=[3,4,5,6,7,8,12]
for n in N:
    os.mkdir('~/Net' + str(Net) +'/Kmean/'+str(n)+ 'cluster')
    random.seed(17)
    kmeans = KMeans(n_clusters=n, random_state=0, n_init = 50, max_iter=1000)
    x = stacked_codes.detach().numpy()
    kmeans.fit(x)
    clusters = kmeans.fit_predict(x)
    kmeans.cluster_centers_.shape
    pred = kmeans.predict(x)
    pred = list(pred)
    for i in range(n):
        os.mkdir('~/Net' + str(Net) +'/Kmean/'+str(n)+ 'cluster'+ '/cluster' + str(i))
        result = [j for j in range(len(pred)) if pred[j] == i]
        image_list = [hold_names[x] for x in result]
        source_folder = "~/images/"
        target_folder = '~/Net' + str(Net) +'/Kmean/'+str(n)+ 'cluster'+ '/cluster' + str(i)+ "/"
        dirs_list = [(source_folder, target_folder)]
        for img in image_list:
            for source_folder, destination_folder in dirs_list:
                shutil.copy(source_folder + img, destination_folder + img)

sse = {}
for k in range(1, 20):
    kmeans = KMeans(n_clusters=k, random_state=0, n_init = 50, max_iter=1000).fit(x)
    sse[k] = kmeans.inertia_
plt.figure()
plt.plot(list(sse.keys()), list(sse.values()))
plt.xlabel("Number of cluster")
plt.ylabel("SSE")
os.chdir('~/Net' + str(Net)+'/'+'Kmean')
plt.savefig('Number of clusters')

#Silhouette Analysis
from sklearn.metrics import silhouette_samples, silhouette_score
for i, k in enumerate([4,5,6,7,8,12]):
    fig, (ax1, ax2) = plt.subplots(1, 2)
    fig.set_size_inches(18, 7)
    
    # Run the Kmeans algorithm
    kmeans = KMeans(n_clusters=k, random_state=0, n_init = 50, max_iter=1000)
    labels = kmeans.fit_predict(x)
    centroids = kmeans.cluster_centers_

    # Get silhouette samples
    silhouette_vals = silhouette_samples(x, labels)

    # Silhouette plot
    y_ticks = []
    y_lower, y_upper = 0, 0
    for i, cluster in enumerate(np.unique(labels)):
        cluster_silhouette_vals = silhouette_vals[labels == cluster]
        cluster_silhouette_vals.sort()
        y_upper += len(cluster_silhouette_vals)
        ax1.barh(range(y_lower, y_upper), cluster_silhouette_vals, edgecolor='none', height=1)
        ax1.text(-0.03, (y_lower + y_upper) / 2, str(i + 1))
        y_lower += len(cluster_silhouette_vals)

    # Get the average silhouette score and plot it
    avg_score = np.mean(silhouette_vals)
    ax1.axvline(avg_score, linestyle='--', linewidth=2, color='green')
    ax1.set_yticks([])
    ax1.set_xlim([-0.1, 1])
    ax1.set_xlabel('Silhouette coefficient values')
    ax1.set_ylabel('Cluster labels')
    ax1.set_title('Silhouette plot for the various clusters', y=1.02);
    
    # Scatter plot of data colored with labels
    ax2.scatter(x[:, 0], x[:, 1], c=labels)
    ax2.scatter(centroids[:, 0], centroids[:, 1], marker='*', c='r', s=250)
    ax2.set_xlim([-2, 2])
    ax2.set_xlim([-2, 2])
    ax2.set_xlabel('First feature')
    ax2.set_ylabel('Second feature')
    ax2.set_title('Visualization of clustered data', y=1.02)
    ax2.set_aspect('equal')
    plt.tight_layout()
    plt.suptitle(f'Silhouette analysis using k = {k}',
                 fontsize=16, fontweight='semibold', y=1.05);
    plt.savefig('silhouette' + str(k)+'.png')
#=====================================================================================
#clustring with AffinityPropagation
os.mkdir('~/Net' + str(Net) + '/APcluster')
for p in [-15300,-14200,-14500,-12700,-13400,-13800,-14000,-7800,-8000,-8500,-9000,-9500,-10000,-10500,-11000,-11500,-12000,-12500,-13000,-16000,-16500,-17000,-18000,-18500,-19000]:
    os.mkdir('~/Net' + str(Net) + '/APcluster'+'/pref'+str(p))
    random.seed(17)
    x = stacked_codes.detach().numpy()
    clustering = AffinityPropagation(affinity='euclidean', convergence_iter=30, copy=True, damping=0.5, max_iter=2000, preference=p, verbose=False).fit(x)
    print(clustering)
    print(clustering.labels_)
    pred= clustering.predict(x)
    pred = list(pred)
    n=len(clustering.cluster_centers_)
    print(n)
    for i in range(n):
        os.mkdir('~/Net' + str(Net) + '/APcluster'+'/pref'+str(p) + '/cluster' + str(i))
        result = [j for j in range(len(pred)) if pred[j]==i]
        image_list=[hold_names[x] for x in result]
        source_folder= "~/images/"

        target_folder='~/Net' + str(Net) + '/APcluster'+'/pref'+str(p) + '/cluster' + str(i)+'/'
        dirs_list = [(source_folder, target_folder)]
        for img in image_list:
            for source_folder, destination_folder in dirs_list:
                shutil.copy(source_folder+img, destination_folder+img)

#clustring with hierarchical clustering
os.mkdir('~/Net' + str(Net) + '/HCcluster')
x = stacked_codes.detach().numpy()
N=[3,4,5,6,7,8,12]
for n in N:
    os.mkdir('~/Net' + str(Net) + '/HCcluster' +'/'+str(n)+ 'cluster')
    random.seed(17)
    HC = AgglomerativeClustering(affinity='euclidean', compute_full_tree='auto',connectivity=None, distance_threshold=None, linkage='ward', memory=None, n_clusters=n, pooling_func='deprecated').fit(x)
    
    pred=HC.labels_
    pred = list(pred)
    for i in range(n):
        os.mkdir('~/Net' + str(Net) + '/HCcluster' +'/'+str(n)+ 'cluster' + '/cluster' + str(i))
        result = [j for j in range(len(pred)) if pred[j]==i]
        image_list=[hold_names[x] for x in result]
        source_folder= "~/images/"

        target_folder= '~/Net' + str(Net) + '/HCcluster' +'/'+str(n)+ 'cluster' + '/cluster' + str(i)+'/'
        dirs_list = [(source_folder, target_folder)]
        for img in image_list:
            for source_folder, destination_folder in dirs_list:
                shutil.copy(source_folder+img, destination_folder+img)
