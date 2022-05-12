from torch.utils.data import Dataset
import os
from PIL import Image


class PathogenTreeDataset(Dataset):
    """"""

    def __init__(self, img_names, img_dir, transform=None):
        self.img_dir = img_dir
        self.img_names = img_names
        self.transform = transform

    def __getitem__(self, index):
        img = Image.open(os.path.join(self.img_dir,
                                      self.img_names[index]))

        if self.transform is not None:
            img = self.transform(img)

        return img, self.img_names[index]

    def __len__(self):
        return len(self.img_names)
