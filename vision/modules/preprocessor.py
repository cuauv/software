from vision import options
import numpy
import cv2

class Preprocessor:
    def __init__(self, module):
        self.module = module
        self.options = [
            options.BoolOption('PPX_grayscale', False),
            options.BoolOption('PPX_lab', False),
            options.BoolOption('PPX_rgb_split', False),
            options.BoolOption('PPX_lab_split', False),
            options.BoolOption('PPX_hsv_split', False),
            options.BoolOption('PPX_hls_split', False),
            options.BoolOption('PPX_ycrcb_split', False),
            options.BoolOption('PPX_luv_split', False),
            options.BoolOption('PPX_color_correction', False),
            options.IntOption('PPX_r_bias', 0, -255, 255),
            options.IntOption('PPX_g_bias', 0, -255, 255),
            options.IntOption('PPX_b_bias', 0, -255, 255),
            options.DoubleOption('PPX_contrast', 1, 0, 5),
            options.IntOption('PPX_brightness', 0, -255, 255),
            options.BoolOption('PPX_gaussian_blur', False),
            options.IntOption('PPX_gaussian_blur_kernel', 1, 1, 100),
            options.IntOption('PPX_gaussian_noise', 0, 0, 255),
            options.BoolOption('PPX_erode', False),
            options.IntOption('PPX_erode_kernel', 1, 1, 50),
            options.BoolOption('PPX_dilate', False),
            options.IntOption('PPX_dilate_kernel', 1, 1, 50),
            options.IntOption('PPX_rotate', 0, 0, 359),
            options.BoolOption('PPX_resize', False),
            options.IntOption('PPX_resize_width', 512, 1, 2048, lambda x:
                x * self.options_dict['PPX_resize_height'].value * 3 <=
                self.module.max_buffer_size),
            options.IntOption('PPX_resize_height', 512, 1, 2048, lambda x:
                x * self.options_dict['PPX_resize_width'].value * 3 <=
                self.module.max_buffer_size),
            options.DoubleOption('PPX_resize_ratio', 1, 0.01, 1),
            options.IntOption('PPX_translate_x', 0, -2048, 2048),
            options.IntOption('PPX_translate_y', 0, -2048, 2048),
        ]
        self.options_dict = {option.name : option for option in self.options}
        for option in self.options:
            self.module.options_dict[option.name] = option

    def process(self, *images):
        from vision.modules.color_balance import balance
        preprocessed_images = []
        for mat in images:
            if self.options_dict['PPX_rgb_split'].value:
                bgr_split = cv2.split(mat)
                self.module.post("PPX_rgb_r_channel", bgr_split[2])
                self.module.post("PPX_rgb_g_channel", bgr_split[1])
                self.module.post("PPX_rgb_b_channel", bgr_split[0])
            if self.options_dict['PPX_lab_split'].value:
                lab_split = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2LAB))
                self.module.post("PPX_lab_l_channel", lab_split[0])
                self.module.post("PPX_lab_a_channel", lab_split[1])
                self.module.post("PPX_lab_b_channel", lab_split[2])
            if self.options_dict['PPX_hsv_split'].value:
                hsv_split = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2HSV))
                self.module.post("PPX_hsv_h_channel", hsv_split[0])
                self.module.post("PPX_hsv_s_channel", hsv_split[1])
                self.module.post("PPX_hsv_v_channel", hsv_split[2])
            if self.options_dict['PPX_hls_split'].value:
                hls_split = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2HLS))
                self.module.post("PPX_hls_h_channel", hls_split[0])
                self.module.post("PPX_hls_l_channel", hls_split[1])
                self.module.post("PPX_hls_s_channel", hls_split[2])
            if self.options_dict['PPX_ycrcb_split'].value:
                ycrcb_split = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2YCrCb))
                self.module.post("PPX_ycrcb_y_channel", ycrcb_split[0])
                self.module.post("PPX_ycrcb_cr_channel", ycrcb_split[1])
                self.module.post("PPX_ycrcb_cb_channel", ycrcb_split[2])
            if self.options_dict['PPX_luv_split'].value:
                luv_split = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2LUV))
                self.module.post("PPX_luv_l_channel", luv_split[0])
                self.module.post("PPX_luv_u_channel", luv_split[1])
                self.module.post("PPX_luv_v_channel", luv_split[2])
            if self.options_dict['PPX_grayscale'].value:
                grayscale = cv2.cvtColor(mat, cv2.COLOR_BGR2GRAY)
                self.module.post("PPX_grayscale", grayscale)
            if self.options_dict['PPX_lab'].value:
                lab = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
                self.module.post('PPX_lab', lab)
            if self.options_dict['PPX_color_correction'].value:
                mat = balance(mat)
            if self.options_dict['PPX_r_bias'].value != 0:
                bgr_split = cv2.split(mat)
                bgr_split[2] = cv2.add(self.options_dict['PPX_r_bias'].value, bgr_split[2])
                mat = cv2.merge(bgr_split)
            if self.options_dict['PPX_g_bias'].value != 0:
                bgr_split = cv2.split(mat)
                bgr_split[1] = cv2.add(self.options_dict['PPX_g_bias'].value, bgr_split[1])
                mat = cv2.merge(bgr_split)
            if self.options_dict['PPX_b_bias'].value != 0:
                bgr_split = cv2.split(mat)
                bgr_split[0] = cv2.add(self.options_dict['PPX_b_bias'].value, bgr_split[0])
                mat = cv2.merge(bgr_split)
            if self.options_dict['PPX_contrast'].value != 1:
                temp = mat * self.options_dict['PPX_contrast'].value
                mat = numpy.clip(temp, 0., 255.).astype(numpy.uint8)
            if self.options_dict['PPX_brightness'].value != 0:
                temp = mat + float(self.options_dict['PPX_brightness'].value)
                mat = numpy.clip(temp, 0., 255.).astype(numpy.uint8)
            if self.options_dict['PPX_gaussian_blur'].value:
                mat = cv2.GaussianBlur(mat,
                        (self.options_dict['PPX_gaussian_blur_kernel'].value * 2 + 1,
                         self.options_dict['PPX_gaussian_blur_kernel'].value * 2 + 1),
                        0)
            if self.options_dict['PPX_gaussian_noise'].value != 0:
                noise = numpy.random.randn(*mat.shape) * self.options_dict['PPX_gaussian_noise'].value
                mat = mat + noise
                mat = numpy.clip(mat, 0., 255.).astype(numpy.uint8)
            if self.options_dict['PPX_erode'].value:
                mat = cv2.erode(mat,
                        cv2.getStructuringElement(cv2.MORPH_ELLIPSE,
                            (self.options_dict['PPX_erode_kernel'].value * 2 + 1,
                             self.options_dict['PPX_erode_kernel'].value * 2 + 1)))
            if self.options_dict['PPX_dilate'].value:
                mat = cv2.dilate(mat,
                        cv2.getStructuringElement(cv2.MORPH_ELLIPSE,
                            (self.options_dict['PPX_dilate_kernel'].value * 2 + 1,
                             self.options_dict['PPX_dilate_kernel'].value * 2 + 1)))
            if self.options_dict['PPX_rotate'].value != 0:
                rot_mat = cv2.getRotationMatrix2D(
                        (mat.shape[1] / 2, mat.shape[0] / 2),
                        self.options_dict['PPX_rotate'].value, 1)
                mat = cv2.warpAffine(mat, rot_mat, (mat.shape[1], mat.shape[0]),
                        borderMode=cv2.BORDER_REPLICATE)
            if self.options_dict['PPX_resize'].value:
                mat = cv2.resize(mat,
                        (self.options_dict['PPX_resize_width'].value,
                         self.options_dict['PPX_resize_height'].value))
            if self.options_dict['PPX_resize_ratio'].value != 1:
                mat = cv2.resize(mat,
                        (int(mat.shape[1] * self.options_dict['PPX_resize_ratio'].value),
                         int(mat.shape[0] * self.options_dict['PPX_resize_ratio'].value)))
            if self.options_dict['PPX_translate_x'].value != 0 or \
               self.options_dict['PPX_translate_y'].value != 0:
                trans_mat = numpy.float32([[1, 0, self.options_dict['PPX_translate_x'].value],
                                           [0, 1, self.options_dict['PPX_translate_y'].value]])
                mat = cv2.warpAffine(mat, trans_mat, (mat.shape[1], mat.shape[0]))
            preprocessed_images.append(mat)
        return preprocessed_images

