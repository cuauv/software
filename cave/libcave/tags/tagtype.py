class TagType:

    ######Overloaded methods

    @classmethod
    def get_name(cls):
        raise NotImplemented("Must implement get_name")

    @classmethod
    def get_schema(cls):
        raise NotImplemented("Data schema must be provided")

    @classmethod
    def test_frame(cls, frame, mission_element_instance):
        """
            Returns true on a positive result and false otherwise.
        """
        raise NotImplemented("Must implement frame test")
    
    @classmethod
    def load_shm_for_frame(cls, frame):
        raise NotImplemented("Must implement load_shm_for_frame")

    def __init__(self, tag, get_frame):
        raise NotImplemented("Init method not implemented")

    def hover(self, area, event, frame_x, frame_y):
        pass

    def click(self, area, event, frame_x, frame_y):
        return False

    def enter(self, area, event, frame_x, frame_y):
        pass
    
    def exit(self, area, event, frame_x, frame_y):
        pass

    def scroll(self, area, event, frame_x, frame_y):
        pass

    def keypress(self, area, event):
        pass

    def draw(self, widget, cr, x_origin, y_origin):
        raise NotImplemented("Must implement draw")

