
class Screen:
    """
    Abstract class for drawing to the LCD
    The main program will transition between
    displaying various "screens" based on
    high-level logic
    """
    def draw(self, cr):
        raise NotImplementedError("Must implement draw")

    def stop(self):
        """
        Some screens may need to initialize threads in order
        to display content. These screens should terminate
        those threads when this method is called.
        """
        pass

    """
    Name of this screen (for manual triggering via shared memory)
    """
    def get_name(self):
        raise NotImplementedError("Must implement get_name")


