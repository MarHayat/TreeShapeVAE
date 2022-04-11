from __future__ import print_function
import glob
from pathlib import Path
from torch.utils.data import DataLoader
import torch.nn as nn
import torchvision
import matplotlib.pyplot as plt
from   torch.autograd import Variable
import torch.optim as optim
from  torchvision import transforms


from auto_encode import AutoEncoder
from dataset import PathogenTreeDataset
#from helper import visualize_feature_map, get_kernel_image

# Hyper Parameters
code_size = 20
num_epochs = 2
batch_size = 8
lr = 0.002
optimizer_cls = optim.Adam


img_dir_train = Path('~/train_images')
img_dir_test = Path('~/test_images')
img_train = glob.glob('~/train_images/*.png')
print(len(img_train))
img_test = glob.glob('~/test_images/*.png')
print(len(img_test))
IMAGE_SIZE = 1440000
IMAGE_WIDTH = IMAGE_HEIGHT = 1200





def dataset_preparation():

    custom_transform = transforms.Compose([transforms.Grayscale(),
                                           # transforms.Lambda(lambda x: x/255.),
                                           transforms.ToTensor()])
    # custom_transform = transforms.Compose([ #transforms.Lambda(lambda x: x/255.),
    #                                       transforms.ToTensor()])
    img_names = img_train
    train_dataset = PathogenTreeDataset(img_train, img_dir_train, custom_transform)

    train_loader = DataLoader(dataset=train_dataset,
                              batch_size=8,
                              shuffle=True,
                              num_workers=4)
    print(len(train_loader))
    custom_transform = transforms.Compose([transforms.Grayscale(),
                                           # transforms.Lambda(lambda x: x/255.),
                                           transforms.ToTensor()])
    img_names = img_test
    print(len(img_names))
    test_dataset = PathogenTreeDataset(img_names, img_dir=img_dir_test, transform=custom_transform)
    print(len(test_dataset))

    test_loader = DataLoader(dataset=test_dataset,
                             batch_size=8,
                             shuffle=True,
                             num_workers=4)
    print(len(test_loader))
    return train_loader, test_loader





def main():

    train_loader, test_loader =  dataset_preparation()
    autoencoder = AutoEncoder(code_size,  IMAGE_SIZE, IMAGE_WIDTH, IMAGE_HEIGHT)
    loss_fn = nn.BCELoss()
    optimizer = optimizer_cls(autoencoder.parameters(), lr=lr)

    # Training loop
    for epoch in range(num_epochs):
        print("Epoch %d" % epoch)

        for batch_idx, x in enumerate(train_loader):
            print(x.shape)
            # b_x = x.view(-1, 1200*1200)   # batch x, shape (batch, 1200*1200)
            b_y = x.view(-1, 1200 * 1200)
            out, code = autoencoder(Variable(x))

            optimizer.zero_grad()
            loss = loss_fn(out, b_y)
            loss.backward()
            optimizer.step()

        print("Loss = %.3f" % loss.data)


    # Test the similarity between input and output after train
    im = []
    for i, x in enumerate(test_loader):
        im = x
        test_reconst, compress = autoencoder(im)
        print(compress.shape)
        d = compress.view([-1])
        print(d.shape)
        print(d)
        c = compress.view([10, 16])
        plt.imshow(c.data.numpy(), cmap='gray')
        plt.show()
        torchvision.utils.save_image(im.data, 'orig1.png')
        torchvision.utils.save_image(test_reconst.data, 'reconst1.png')
        break


            #get_kernel_image(autoencoder)

if __name__ == '__main__':
    main()
