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


# ------ Image processing functions ------


def read_img(img):
    import cv2
    from os.path import isfile
    if isinstance(img, str):
        if isfile(img):
            # Read image as is
            print(img)
            img = cv2.imread(img, -1)
        else:
            print("Incorrect file path:", img)
            raise
    return img


# Process image
def process_img(img, in_range=(1, 20), out_range=(0, 255)):
    import cv2
    from skimage.exposure import rescale_intensity
    img = read_img(img)
    img_scaled = (img * 255.0 / img.max())
    img_processed = rescale_intensity(img_scaled, in_range=in_range, out_range=out_range).astype("uint8")
    return img_processed


# Customized version of SURF algorithm from cv2, returns an annotated image
def surf_detect(img, h_threshold=1000, oct_layers=3, oct_nb=4, upright=False, verbose=False, kp_only=True):
    import cv2
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
    kp, des = surf_engine.detectAndCompute(img, None)

    if verbose:
        print("Keypoints:", len(kp))
        print("Descriptors:", len(des))

    if kp_only:
        return kp, des
    else:
        img2 = cv2.drawKeypoints(img, kp, None, (255, 0, 0), 4)
        return Annotated(kp, des, img2)


# Process flow, combine all functions
def process_flow(img, in_range=(2, 20), out_range=(0, 255), h_threshold=30000,
                 oct_layers=5, oct_nb=5, upright=True, verbose=False, kp_only=False):
    img_processed = process_img(img, in_range, out_range)
    img_annotated = surf_detect(img_processed, h_threshold, oct_layers, oct_nb, upright, verbose, kp_only)
    return img_annotated


def get_kp_lengths(img):
    img_annotated = process_flow(img)
    the_length = len(img_annotated.kp)
    return the_length


# ------ Plotting functions ------


# plot image with hist
def plot_hist(img):
    import matplotlib.pyplot as plt
    # Display the image.
    fig, (ax1, ax2) = plt.subplots(1, 2,
                                   figsize=(12, 3))

    ax1.imshow(img, cmap=plt.cm.gray)
    ax1.set_axis_off()

    # Display the histogram.
    ax2.hist(img.ravel(), lw=0, bins=256)
    ax2.set_xlim(0, img.max())
    ax2.set_yticks([])

    plt.show()


# Plot an image
def plot(img):
    import matplotlib.pyplot as plt
    fig = plt.figure()
    plt.imshow(img)
    plt.show()
