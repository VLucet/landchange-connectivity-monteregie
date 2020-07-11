# SURF Module.


# Class of annotated images returned by surf
class Annotated:
    def __init__(self, kp, des, img):
        self.kp = kp
        self.des = des
        self.img = img

    def plot(self):
        import matplotlib.pyplot as plt
        fig = plt.figure()
        plt.imshow(self.img)
        plt.show()


class Raster:
    def __init__(self, name, img):
        self.name = name
        self.img = img

    def get_ts(self):
        ts = self.name.split("ts_")[1].split("_")[0]
        return int(ts)

    def get_iter(self):
        it = self.name.split("it_")[1].split("_")[0]
        return int(it)

    def get_spe(self):
        spe = self.name.split("_")[-1].split(".")[0]
        return str(spe)

# ------ Image processing functions ------


def read_img(img):
    import cv2
    from os.path import isfile
    if isinstance(img, str):
        if isfile(img):
            # Read image as is
            print(img)
            img_array = cv2.imread(img, -1)
        else:
            print("Incorrect file path:", img)
            raise
    img = Raster(img=img_array, name=img)
    return img


# Scale the image
def scale_img(img, the_max=255.0):
    img_scaled = (img * the_max / img.max())
    return img_scaled


def transform_img(img):
    import numpy as np
    img_db = 10 * np.log10(img)
    img_db_pos = img_db + abs(np.min(img_db))
    return img_db_pos


# def get_peak_img(img):
#     import numpy as np
#     from scipy import signal
#
#     data = img.flatten()
#     n, bins = np.histogram(data, 100)
#
#     # trim data
#     x = np.linspace(np.min(data), np.max(data), num=100)
#
#     # find index of minimum between two modes
#     ind_max = signal.argrelmax(n)
#     x_max = x[ind_max]
#     y_max = n[ind_max]
#
#     # plot
#     # plt.hist(data, bins=100, color='y')
#     # plt.scatter(x_max, y_max, color='b')
#     # plt.show()
#
#     y_main_peak = np.max(y_max)
#     x_main_peak = x_max[np.argmax(y_max)]
#
#     if x_main_peak == 0:
#         raise Exception("Error x is 0")
#
#     return x_main_peak, y_main_peak


# Process image
def process_img(img, mask=None):
    # from skimage.exposure import rescale_intensity
    import cv2
    from skimage.exposure import equalize_hist

    # img_scaled = scale_img(transform_img(read_img(img)))
    # x, y = get_peak_img(img_scaled)
    # img_processed = rescale_intensity(img_scaled, (x-30, x+30), (0, 255)).astype("uint8")

    img_transformed = transform_img(read_img(img).img)
    #img_processed = scale_img(equalize_hist(img_transformed, mask=mask)).astype("uint8")
    img_processed = scale_img(img_transformed).astype("uint8")
    return img_processed


# Customized version of SURF algorithm from cv2, returns an annotated image
def surf_detect(img, mask=None, h_threshold=2000, oct_layers=3, oct_nb=3, upright=False, verbose=False,
                kp_only=False, bright_only=True):
    import cv2
    import numpy as np

    # Read image
    if isinstance(img, str):
        img = cv2.imread(img, 0)

    # Set up the SURF algorithm
    surf_engine = cv2.xfeatures2d.SURF_create()
    surf_engine.setHessianThreshold(h_threshold)
    surf_engine.setNOctaveLayers(oct_layers)
    surf_engine.setNOctaves(oct_nb)
    surf_engine.setUpright(upright)

    # Process image
    kp, des = surf_engine.detectAndCompute(img, mask)

    if bright_only:
        laplacian = [kp[idx].class_id for idx in range(0, len(kp))]
        indices = np.where(np.array(laplacian) < 0)[0]
        kp_final = [kp[idx] for idx in indices]
    else:
        kp_final = kp

    if verbose:
        print("Keypoints:", len(kp_final))
        print("Descriptors:", len(des))

    if kp_only:
        return kp_final, des
    else:
        img2 = cv2.drawKeypoints(img, kp_final, None, (255, 0, 0), 4)
        return Annotated(kp_final, des, img2)


# Process flow, combine all functions
def process_flow(img, mask=None, h_threshold=2000, oct_layers=3, oct_nb=3, upright=False, verbose=False,
                 kp_only=False, bright_only=True):
    img_processed = process_img(img, mask)
    img_annotated = surf_detect(img_processed, mask, h_threshold, oct_layers, oct_nb, upright, verbose,
                                kp_only, bright_only)
    return img_annotated


def get_kp_lengths(img, mask=None):
    img_annotated = process_flow(img, mask=mask)
    the_length = len(img_annotated.kp)
    return the_length


def get_annotated(img, mask=None):
    img_annotated = process_flow(img, mask=mask)
    return img_annotated.img


def process_and_plot(img):
    img_annotated = process_flow(img)
    img_annotated.plot()


def compute_and_save(files, mask):
    import numpy as np
    from matplotlib import pyplot as plt

    for file in files:
        raster = read_img(file)
        if (raster.get_ts() in [2, 11]) & (raster.get_iter() == 1):
            img = process_flow(file, mask=mask)
            fig = plt.figure()
            plt.imshow(img)
            plt.savefig("/outputs/figures/"+file+"_annotated.png")
        else:
            pass





# ------ Plotting functions ------


# plot image with hist
def plot_hist(img, mask=None):

    if mask is not None:
        data = img[mask==1]
    else:
        data = img.flatten()

    import matplotlib.pyplot as plt
    # Display the image.
    fig, (ax1, ax2) = plt.subplots(1, 2,
                                   figsize=(12, 3))

    ax1.imshow(img, cmap=plt.cm.gray)
    ax1.set_axis_off()

    # Display the histogram.
    ax2.hist(data, lw=0, bins=256)
    ax2.set_xlim(0, img.max())
    ax2.set_yticks([])

    plt.show()


# Plot an image
def plot(img):
    import matplotlib.pyplot as plt
    fig = plt.figure()
    plt.imshow(img)
    plt.show()
