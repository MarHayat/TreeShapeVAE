
import matplotlib.pyplot as plt

def get_kernel_image(autoencoder):
    kernels = autoencoder.enc_cnn_1.weight.detach()
    fig, ax = plt.subplots(2, 5, figsize=(5, 2))
    for axi, k in zip(ax.flat, kernels):
        axi.set(xticks=[], yticks=[])
        axi.imshow(k.squeeze(), cmap='gray')




def visualize_feature_map(autoencoder,image):
    # Visualize feature maps
    activation = {}

    def get_activation(name):
        def hook(model, input, output):
            activation[name] = output.detach()

        return hook

    autoencoder.enc_cnn_1.register_forward_hook(get_activation('enc_cnn_1'))

    output, _ = autoencoder(image)
    act = activation['enc_cnn_1'].squeeze()
    print(act.shape)
    fig, ax = plt.subplots(2, 5, figsize=(5, 2))
    for axi, a in zip(ax.flat, act):
        axi.set(xticks=[], yticks=[])
        axi.imshow(a)
