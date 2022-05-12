import torch
import torch.nn as nn
import torch.nn.functional as F
#from torchsummary import summary


class AutoEncoderA(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 32, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_2 = nn.Conv2d(32, 64, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_3 = nn.Conv2d(64, 128, kernel_size=3, stride=2, padding=0)

        self.enc_fc_1 = nn.Linear(128*19*19, code_size)
        self.relU = nn.ReLU()
        self.activations = {
            'r': nn.ReLU(),
            's': nn.SELU(),
        }
        self.activator = self.activations[act]

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 128*19*19)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            128, 64, kernel_size=3, stride=2, padding=0, output_padding=1)

        self.dec_cnn_2 = nn.ConvTranspose2d(
            64, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            32, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.activator(self.enc_cnn_2(code))
        code = self.activator(self.enc_cnn_3(code))
        code = code.view(-1, 128*19*19)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 128, 19, 19)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = torch.sigmoid(self.dec_cnn_3(out))
        # [20, 1, 480, 480]
        return out


class  AutoEncoderB(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 32, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_2 = nn.Conv2d(32, 64, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_3 = nn.Conv2d(64, 128, kernel_size=5, stride=2, padding=2)
        self.enc_cnn_4 = nn.Conv2d(
            128, 256, kernel_size=3, stride=2, padding=0)

        self.enc_fc_1 = nn.Linear(256*9*9, code_size)
        self.activations = {
            'r': nn.ReLU(),
            's': nn.SELU(),
        }
        self.activator = self.activations[act]

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 256*9*9)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            256, 128, kernel_size=3, stride=2, padding=0, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            128, 64, kernel_size=3, stride=2, padding=1, output_padding=1)

        self.dec_cnn_3 = nn.ConvTranspose2d(
            64, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_4 = nn.ConvTranspose2d(
            32, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.activator(self.enc_cnn_2(code))
        code = self.activator(self.enc_cnn_3(code))
        code = self.activator(self.enc_cnn_4(code))
        code = code.view(-1, 256*9*9)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 256, 9, 9)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = torch.sigmoid(self.dec_cnn_4(out))
        # [20, 1, 480, 480]
        return out


class AutoEncoderC(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 32, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_2 = nn.Conv2d(32, 64, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_3 = nn.Conv2d(64, 128, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_4 = nn.Conv2d(
            128, 256, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_5 = nn.Conv2d(
            256, 512, kernel_size=3, stride=2, padding=1)

        self.enc_fc_1 = nn.Linear(512*5*5, code_size)

        self.activations = {
            'r': nn.ReLU(),
            's': nn.SELU(),
        }
        self.activator = self.activations[act]

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 512*5*5)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            512, 256, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            256, 128, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            128, 64, kernel_size=3, stride=2, padding=1, output_padding=1)

        self.dec_cnn_4 = nn.ConvTranspose2d(
            64, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_5 = nn.ConvTranspose2d(
            32, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.activator(self.enc_cnn_2(code))
        code = self.activator(self.enc_cnn_3(code))
        code = self.activator(self.enc_cnn_4(code))
        code = self.activator(self.enc_cnn_5(code))
        code = code.view(-1, 512*5*5)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 512, 5, 5)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = self.activator(self.dec_cnn_4(out))
        out = torch.sigmoid(self.dec_cnn_5(out))
        # [20, 1, 480, 480]
        return out


class AutoEncoderD(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_2 = nn.Conv2d(32, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_3 = nn.Conv2d(32, 32, kernel_size=3, stride=1, padding=1)

        self.enc_fc_1 = nn.Linear(32*20*20, code_size)
        self.activations = {
            'r': nn.ReLU(inplace=False),
            's': nn.SELU(inplace=False),
        }
        self.activator = self.activations[act]

        # MaxPool

        self.pool = nn.MaxPool2d(2, 2)

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 32*20*20)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            32, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            32, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            32, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_2(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_3(code))
        code = self.pool(code)

        code = code.view(-1, 32*20*20)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 32, 20, 20)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = torch.sigmoid(self.dec_cnn_3(out))
        return out


class AutoEncoderE(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_2 = nn.Conv2d(32, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_3 = nn.Conv2d(32, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_4 = nn.Conv2d(32, 32, kernel_size=3, stride=1, padding=1)

        self.enc_fc_1 = nn.Linear(32*10*10, code_size)
        self.activations = {
            'r': nn.ReLU(inplace=False),
            's': nn.SELU(inplace=False),
        }
        self.activator = self.activations[act]

        # MaxPool

        self.pool = nn.MaxPool2d(2, 2)

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 32*10*10)

        self.dec_cnn_1 = nn.ConvTranspose2d(
            32, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            32, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            32, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_4 = nn.ConvTranspose2d(
            32, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_2(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_3(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_4(code))
        code = self.pool(code)
        code = code.view(-1, 32*10*10)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 32, 10, 10)

        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = torch.sigmoid(self.dec_cnn_4(out))
        return out


class AutoEncoderF(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_2 = nn.Conv2d(32, 16, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_3 = nn.Conv2d(16, 8, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_4 = nn.Conv2d(8, 4, kernel_size=3, stride=1, padding=1)

        self.enc_fc_1 = nn.Linear(4*10*10, code_size)
        self.activations = {
            'r': nn.ReLU(inplace=False),
            's': nn.SELU(inplace=False),
        }
        self.activator = self.activations[act]

        # MaxPool

        self.pool = nn.MaxPool2d(2, 2)

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 4*10*10)

        self.dec_cnn_1 = nn.ConvTranspose2d(
            4, 8, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            8, 16, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            16, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_4 = nn.ConvTranspose2d(
            32, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_2(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_3(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_4(code))
        code = self.pool(code)
        code = code.view(-1, 4*10*10)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 4, 10, 10)

        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = torch.sigmoid(self.dec_cnn_4(out))
        return out

class AutoEncoderG(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_2 = nn.Conv2d(32, 64, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_3 = nn.Conv2d(64, 128, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_4 = nn.Conv2d(
            128, 256, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_5 = nn.Conv2d(
            256, 512, kernel_size=3, stride=1, padding=1)

        self.enc_fc_1 = nn.Linear(512*5*5, code_size)

        self.activations = {
            'r': nn.ReLU(),
            's': nn.SELU(),
        }
        self.activator = self.activations[act]
        self.pool = nn.MaxPool2d(2, 2)
        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 512*5*5)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            512, 256, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            256, 128, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            128, 64, kernel_size=3, stride=2, padding=1, output_padding=1)

        self.dec_cnn_4 = nn.ConvTranspose2d(
            64, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_5 = nn.ConvTranspose2d(
            32, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_2(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_3(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_4(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_5(code))
        code = self.pool(code)
        code = code.view(-1, 512*5*5)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 512, 5, 5)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = self.activator(self.dec_cnn_4(out))
        out = torch.sigmoid(self.dec_cnn_5(out))
        # [20, 1, 480, 480]
        return out

class AutoEncoderH(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_2 = nn.Conv2d(32, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_3 = nn.Conv2d(32, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_4 = nn.Conv2d(32, 32, kernel_size=3, stride=1, padding=1)
        self.enc_cnn_5 = nn.Conv2d(32, 16, kernel_size=3, stride=1, padding=1)

        self.enc_fc_1 = nn.Linear(16*5*5, code_size)
        self.activations = {
            'r': nn.ReLU(inplace=False),
            's': nn.SELU(inplace=False),
        }
        self.activator = self.activations[act]

        # MaxPool

        self.pool = nn.MaxPool2d(2, 2)

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 16*5*5)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            16, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            32, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            32, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_4 = nn.ConvTranspose2d(
            32, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_5 = nn.ConvTranspose2d(
            32, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_2(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_3(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_4(code))
        code = self.pool(code)
        code = self.activator(self.enc_cnn_5(code))
        code = self.pool(code)
        code = code.view(-1, 16*5*5)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 16, 5, 5)

        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = self.activator(self.dec_cnn_4(out))
        out = torch.sigmoid(self.dec_cnn_5(out))
        return out


class AutoEncoderI(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 32, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_2 = nn.Conv2d(32, 64, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_3 = nn.Conv2d(64, 32, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_4 = nn.Conv2d(
            32, 16, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_5 = nn.Conv2d(
            16, 8, kernel_size=3, stride=2, padding=1)

        self.enc_fc_1 = nn.Linear(8*5*5, code_size)

        self.activations = {
            'r': nn.ReLU(),
            's': nn.SELU(),
        }
        self.activator = self.activations[act]

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 8*5*5)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            8, 16, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            16, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            32, 64, kernel_size=3, stride=2, padding=1, output_padding=1)

        self.dec_cnn_4 = nn.ConvTranspose2d(
            64, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_5 = nn.ConvTranspose2d(
            32, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.activator(self.enc_cnn_2(code))
        code = self.activator(self.enc_cnn_3(code))
        code = self.activator(self.enc_cnn_4(code))
        code = self.activator(self.enc_cnn_5(code))
        code = code.view(-1, 8*5*5)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 8, 5, 5)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = self.activator(self.dec_cnn_4(out))
        out = torch.sigmoid(self.dec_cnn_5(out))
        # [20, 1, 480, 480]
        return out

class AutoEncoderK(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 4, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_2 = nn.Conv2d(4, 16, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_3 = nn.Conv2d(16, 32, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_4 = nn.Conv2d(
            32, 64, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_5 = nn.Conv2d(
            64, 128, kernel_size=3, stride=2, padding=1)

        self.enc_fc_1 = nn.Linear(128*5*5, code_size)

        self.activations = {
            'r': nn.ReLU(),
            's': nn.SELU(),
        }
        self.activator = self.activations[act]

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 128*5*5)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            128, 64, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            64, 32, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            32, 16, kernel_size=3, stride=2, padding=1, output_padding=1)

        self.dec_cnn_4 = nn.ConvTranspose2d(
            16, 4, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_5 = nn.ConvTranspose2d(
            4, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.activator(self.enc_cnn_2(code))
        code = self.activator(self.enc_cnn_3(code))
        code = self.activator(self.enc_cnn_4(code))
        code = self.activator(self.enc_cnn_5(code))
        code = code.view(-1, 128*5*5)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 128, 5, 5)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = self.activator(self.dec_cnn_4(out))
        out = torch.sigmoid(self.dec_cnn_5(out))
        # [20, 1, 480, 480]
        return out

class AutoEncoderL(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 6, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_2 = nn.Conv2d(6, 6, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_3 = nn.Conv2d(6, 16, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_4 = nn.Conv2d(16, 16, kernel_size=3, stride=2, padding=1)

        self.enc_fc_1 = nn.Linear(16*10*10, 120)
        self.enc_fc_2 = nn.Linear(120 , 84)
        self.enc_fc_3 = nn.Linear(84, code_size)

        self.activations = {
            'r': nn.ReLU(),
            's': nn.SELU(),
        }
        self.activator = self.activations[act]

        #Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 84)
        self.dec_fc_2 = nn.Linear(84, 120)
        self.dec_fc_3 = nn.Linear(120, 16 * 10 * 10)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            16, 16, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            16, 6, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_3 = nn.ConvTranspose2d(
            6, 6, kernel_size=3, stride=2, padding=1, output_padding=1)

        self.dec_cnn_4 = nn.ConvTranspose2d(
            6, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.activator(self.enc_cnn_2(code))
        code = self.activator(self.enc_cnn_3(code))
        code = self.activator(self.enc_cnn_4(code))
        code = code.view(-1, 16*10*10)
        code = self.activator(self.enc_fc_1(code))
        code = self.activator(self.enc_fc_2(code))
        code = self.activator(self.enc_fc_3(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = self.activator(self.dec_fc_2(out))
        out = self.activator(self.dec_fc_3(out))
        out = out.view(-1, 16, 10, 10)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = torch.sigmoid(self.dec_cnn_4(out))
        # [20, 1, 480, 480]
        return out

class AutoEncoderM(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 8, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_2 = nn.Conv2d(8, 16, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_3 = nn.Conv2d(16, 32, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_4 = nn.Conv2d(
            32, 64, kernel_size=3, stride=2, padding=0)

        self.enc_fc_1 = nn.Linear(64 * 9 * 9, code_size)
        self.activations = {
            'r': nn.ReLU(),
            's': nn.SELU(),
        }
        self.activator = self.activations[act]

        # Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 64 * 9 * 9)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            64, 32, kernel_size=3, stride=2, padding=0, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            32, 16, kernel_size=3, stride=2, padding=1, output_padding=1)

        self.dec_cnn_3 = nn.ConvTranspose2d(
            16, 8, kernel_size=3, stride=2, padding=1, output_padding=1)
        self.dec_cnn_4 = nn.ConvTranspose2d(
            8, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.activator(self.enc_cnn_2(code))
        code = self.activator(self.enc_cnn_3(code))
        code = self.activator(self.enc_cnn_4(code))
        code = code.view(-1, 64 * 9 * 9)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 64, 9, 9)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = self.activator(self.dec_cnn_3(out))
        out = torch.sigmoid(self.dec_cnn_4(out))
        # [20, 1, 480, 480]
        return out

class AutoEncoderN(nn.Module):
    def __init__(self, code_size, act='s'):
        super().__init__()
        self.code_size = code_size

        # Encoder specification
        self.enc_cnn_1 = nn.Conv2d(1, 16, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_2 = nn.Conv2d(16, 32, kernel_size=3, stride=2, padding=1)
        self.enc_cnn_3 = nn.Conv2d(32, 64, kernel_size=3, stride=2, padding=0)
        self.enc_fc_1 = nn.Linear(64 * 19 * 19, code_size)
        self.activations = {
            'r': nn.ReLU(),
            's': nn.SELU(),
        }
        self.activator = self.activations[act]

        # Decoder specification
        self.dec_fc_1 = nn.Linear(code_size, 64 * 19 * 19)
        self.dec_cnn_1 = nn.ConvTranspose2d(
            64, 32, kernel_size=3, stride=2, padding=0, output_padding=1)
        self.dec_cnn_2 = nn.ConvTranspose2d(
            32, 16, kernel_size=3, stride=2, padding=1, output_padding=1)

        self.dec_cnn_3 = nn.ConvTranspose2d(
            16, 1, kernel_size=3, stride=2, padding=1, output_padding=1)

    def forward(self, images):
        code = self.encode(images)
        out = self.decode(code)
        return out, code

    def encode(self, images):
        code = self.activator(self.enc_cnn_1(images))
        code = self.activator(self.enc_cnn_2(code))
        code = self.activator(self.enc_cnn_3(code))
        code = code.view(-1, 64 * 19 * 19)
        code = self.activator(self.enc_fc_1(code))
        # 20, 32, 120, 120

        return code

    def decode(self, code):
        out = self.activator(self.dec_fc_1(code))
        out = out.view(-1, 64, 19, 19)
        out = self.activator(self.dec_cnn_1(out))
        out = self.activator(self.dec_cnn_2(out))
        out = torch.sigmoid(self.dec_cnn_3(out))

        # [20, 1, 480, 480]
        return out
# summary is a really helpful function to see you architecture end to end.

# You can call it like this summary(aeC(10, 'r'), (1,160,160))

# First parameter is the model, and the second is the input dimensions.
'''
----------------------------------------------------------------
        Layer (type)               Output Shape         Param #
================================================================
            Conv2d-1           [-1, 32, 80, 80]             320
              ReLU-2           [-1, 32, 80, 80]               0
            Conv2d-3           [-1, 64, 40, 40]          18,496
              ReLU-4           [-1, 64, 40, 40]               0
            Conv2d-5          [-1, 128, 20, 20]          73,856
              ReLU-6          [-1, 128, 20, 20]               0
            Conv2d-7          [-1, 256, 10, 10]         295,168
              ReLU-8          [-1, 256, 10, 10]               0
            Conv2d-9            [-1, 512, 5, 5]       1,180,160
             ReLU-10            [-1, 512, 5, 5]               0
           Linear-11                   [-1, 10]         128,010
             ReLU-12                   [-1, 10]               0
           Linear-13                [-1, 12800]         140,800
             ReLU-14                [-1, 12800]               0
  ConvTranspose2d-15          [-1, 256, 10, 10]       1,179,904
             ReLU-16          [-1, 256, 10, 10]               0
  ConvTranspose2d-17          [-1, 128, 20, 20]         295,040
             ReLU-18          [-1, 128, 20, 20]               0
  ConvTranspose2d-19           [-1, 64, 40, 40]          73,792
             ReLU-20           [-1, 64, 40, 40]               0
  ConvTranspose2d-21           [-1, 32, 80, 80]          18,464
             ReLU-22           [-1, 32, 80, 80]               0
  ConvTranspose2d-23          [-1, 1, 160, 160]             289
================================================================
Total params: 3,404,299
Trainable params: 3,404,299
Non-trainable params: 0
----------------------------------------------------------------
Input size (MB): 0.10
Forward/backward pass size (MB): 12.30
Params size (MB): 12.99
Estimated Total Size (MB): 25.39
----------------------------------------------------------------
'''
